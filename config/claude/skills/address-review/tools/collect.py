#!/usr/bin/env python3
"""Do the mechanical half of the address-review scout.

Usage (from the checkout, on the PR branch):
  python3 -I ~/.claude/skills/address-review/tools/collect.py <scratchpad> [<pr>]

Runs the branch check, fetches every thread, review, and PR comment, and writes:
  pr-body-original.md  the PR body
  findings.md          one entry per open thread
  bodies.md            every review body and PR comment that may hold items
  collect.md           the PR fields, the review rounds, the answered item keys,
                       and the skip counts
It implements "Branch check", "Collect", and the thread half of "Delta" in
briefs.md: change both together. Splitting a body into items stays with the scout.

A branch check that fails prints FAIL and the reason, and exits 1.
"""
import json
import os
import re
import subprocess
import sys

ANSWERED = "<!-- address-review -->"
REVIEW = "<!-- review -->"
QUERIES = {
    "reviewThreads": """reviewThreads(first:100,after:$after){pageInfo{hasNextPage endCursor}
      nodes{id isResolved isOutdated path line originalLine
        comments(first:50){nodes{databaseId author{login __typename} body createdAt
          pullRequestReview{databaseId}}}}}""",
    "reviews": """reviews(first:100,after:$after){pageInfo{hasNextPage endCursor}
      nodes{databaseId author{login __typename} state body submittedAt commit{oid}}}""",
    "comments": """comments(first:100,after:$after){pageInfo{hasNextPage endCursor}
      nodes{databaseId author{login __typename} body createdAt}}""",
}


def run(*args, check=True):
    done = subprocess.run(args, capture_output=True, text=True)
    if check and done.returncode:
        raise SystemExit(f"FAIL {' '.join(args[:4])}: {done.stderr.strip()[:300]}")
    return done.stdout.strip()


def write(path, text):
    with open(path, "w") as f:
        f.write(text)


def fetch_all(owner, repo, n, field):
    nodes, after = [], None
    while True:
        query = (
            "query($owner:String!,$repo:String!,$n:Int!,$after:String){repository(owner:$owner,name:$repo)"
            "{pullRequest(number:$n){" + QUERIES[field] + "}}}"
        )
        args = ["gh", "api", "graphql", "-F", f"owner={owner}", "-F", f"repo={repo}", "-F", f"n={n}", "-f", f"query={query}"]
        if after:
            args += ["-f", f"after={after}"]
        page = json.loads(run(*args))["data"]["repository"]["pullRequest"][field]
        nodes += page["nodes"]
        if not page["pageInfo"]["hasNextPage"]:
            return nodes
        after = page["pageInfo"]["endCursor"]


def who(author):
    if not author:
        return "ghost (human)"
    return f"{author['login']} ({'bot' if author['__typename'] == 'Bot' else 'human'})"


def login(node):
    return (node["author"] or {}).get("login")


def first_line(body):
    lines = [line for line in body.strip().splitlines() if line.strip() and not line.startswith("<!--")]
    return lines[0] if lines else ""


def branch_check(pr):
    branch = run("git", "rev-parse", "--abbrev-ref", "HEAD")
    if branch != pr["headRefName"]:
        raise SystemExit(f"FAIL the current branch is {branch}, the PR branch is {pr['headRefName']}")
    if run("git", "status", "--porcelain"):
        raise SystemExit("FAIL git status --porcelain is not empty")
    run("git", "fetch", "origin", pr["headRefName"])
    oid = pr["headRefOid"]
    if subprocess.run(["git", "merge-base", "--is-ancestor", oid, "HEAD"]).returncode:
        raise SystemExit(f"FAIL the PR head {oid} is not an ancestor of HEAD")
    return run("git", "log", "--format=%h %s", f"{oid}..HEAD").splitlines()


def thread_entry(k, thread):
    top, later = thread["comments"]["nodes"][0], thread["comments"]["nodes"][1:]
    line = thread["line"] or thread["originalLine"]
    fence = re.search(r"```suggestion.*?```", top["body"], re.S)
    replies = [f"{login(c)}: {first_line(c['body'])}" for c in later]
    return "\n".join([
        f"## F{k}",
        "",
        f"Source: thread {top['databaseId']} {thread['id']}",
        "Item: none",
        f"Reviewer: {who(top['author'])}",
        f"Anchor: {thread['path']}:{line} (outdated: {'yes' if thread['isOutdated'] else 'no'})",
        "Suggestion: " + (fence.group(0) if fence else "none"),
        "Finding: " + top["body"].strip(),
        "Thread: " + ("\n".join(replies) if replies else "none"),
        "",
    ])


def main():
    scratch = os.path.abspath(sys.argv[1])
    target = sys.argv[2:3]
    os.makedirs(scratch, exist_ok=True)
    fields = "number,url,author,headRefName,headRefOid,baseRefName,body"
    pr = json.loads(run("gh", "pr", "view", *target, "--json", fields))
    author = pr["author"]["login"]
    write(os.path.join(scratch, "pr-body-original.md"), pr["body"] + "\n")
    ahead = branch_check(pr)
    owner, repo = pr["url"].split("/")[3:5]

    threads = fetch_all(owner, repo, pr["number"], "reviewThreads")
    reviews = fetch_all(owner, repo, pr["number"], "reviews")
    comments = fetch_all(owner, repo, pr["number"], "comments")

    entries, rows, skipped = [], [], {"resolved": 0, "answered": 0, "author spoke last": 0}
    for thread in threads:
        nodes = thread["comments"]["nodes"]
        newest = nodes[-1]
        if thread["isResolved"] and not any(c["body"].startswith(ANSWERED) for c in nodes):
            skipped["resolved"] += 1
        elif newest["body"].startswith(ANSWERED):
            skipped["answered"] += 1
        elif login(newest) == author and not newest["body"].startswith(REVIEW):
            skipped["author spoke last"] += 1
        else:
            k = len(entries) + 1
            entries.append(thread_entry(k, thread))
            line = thread["line"] or thread["originalLine"]
            rows.append(f"- F{k}: {who(nodes[0]['author'])}, {thread['path']}:{line}")
    write(os.path.join(scratch, "findings.md"), "\n".join(entries))

    answered_keys, sources = [], []
    for kind, node in [("review", r) for r in reviews] + [("comment", c) for c in comments]:
        body = node["body"].strip()
        if body.startswith(ANSWERED):
            answered_keys += re.findall(r"<!-- items:(.*?)-->", body)
        elif body:
            state = f" {node['state']}" if kind == "review" else ""
            sources.append(f"## {kind} {node['databaseId']}{state}\n\nReviewer: {who(node['author'])}\n\n{body}\n")
    write(os.path.join(scratch, "bodies.md"), "\n".join(sources))

    round_ids = {
        c["pullRequestReview"]["databaseId"]
        for t in threads for c in t["comments"]["nodes"]
        if c["body"].startswith(REVIEW) and c["pullRequestReview"]
    }
    rounds = [f"{r['commit']['oid']} (review {r['databaseId']})" for r in reviews if r["databaseId"] in round_ids and r["commit"]]

    out = [
        f"pr: {owner}/{repo}#{pr['number']} {pr['url']}",
        f"author: {author}",
        f"headRefName: {pr['headRefName']}",
        f"headRefOid: {pr['headRefOid']}",
        f"baseRefName: {pr['baseRefName']}",
        "local commits ahead: " + ("; ".join(ahead) or "none"),
        f"pr body: {scratch}/pr-body-original.md",
        f"findings: {scratch}/findings.md ({len(entries)} thread findings)",
        f"bodies: {scratch}/bodies.md ({len(sources)} sources to split into items)",
        "review rounds: " + ("; ".join(rounds) or "none"),
        "answered item keys: " + (" ".join(" ".join(answered_keys).split()) or "none"),
        "threads skipped: " + ", ".join(f"{count} {reason}" for reason, count in skipped.items()),
        "",
        "## Thread findings",
    ] + rows
    write(os.path.join(scratch, "collect.md"), "\n".join(out) + "\n")
    print(os.path.join(scratch, "collect.md"))


if __name__ == "__main__":
    main()
