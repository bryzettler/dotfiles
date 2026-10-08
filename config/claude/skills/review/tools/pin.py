#!/usr/bin/env python3
"""Do the mechanical half of the review scout and write <scratchpad>/pin.md.

Usage (from the checkout):
  python3 -I ~/.claude/skills/review/tools/pin.py <scratchpad> branch [<base>] [--since <last>]
  python3 -I ~/.claude/skills/review/tools/pin.py <scratchpad> pr <n>

Pins the fixed point, the head, and the checkout; drafts spec.md (and pr-body.md);
gives every changed file its domains; runs the domain greps and ts-signatures.mjs;
and in PR mode fetches the checks, one tail per failing check, and the prior round.
It implements "By mode", "Domain", and the grep and signature lines of "Tooling"
in scout.md: change both together. The greps themselves are read from the
Tooling section of each domain-*.md.

A step that cannot run prints a FAIL line in pin.md and the rest still runs.
"""
import json
import os
import re
import subprocess
import sys

SKILL = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
MARKER = "<!-- review -->"
SOLANA_CRATE = re.compile(r"anchor-lang|solana-program|pinocchio|\bspl-")
SOLANA_USE = re.compile(
    r"@coral-xyz/anchor|@solana/web3\.js|@solana/kit|solana[-_]client"
    r"|anchor (build|idl)|solana program|solana-verify|squads",
    re.I,
)
EVM_USE = re.compile(r"""(from|require\(|import)\s*['"(]*\s*['"]?(ethers|viem|wagmi|web3)\b""")
DATABASE_USE = re.compile(
    r"drizzle-orm|@prisma/client|\bkysely\b|\bknex\b|\bsequelize\b|['\"]pg['\"]"
    r"|['\"]postgres['\"]|\bsqlx\b|Ecto\.Query"
)
CODE = (".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs", ".rs", ".py", ".sh", ".ex", ".exs", ".yml", ".yaml")
DATABASE_PATH = re.compile(r"\.sql$|(^|/)migrations/|schema\.prisma$|drizzle\.config\.")


def run(*args, cwd=None, check=True):
    done = subprocess.run(args, cwd=cwd, capture_output=True, text=True)
    if check and done.returncode:
        raise RuntimeError(f"{' '.join(args)}: {done.stderr.strip()[:300]}")
    return done.stdout.strip()


def resolves(ref, cwd=None):
    done = subprocess.run(
        ["git", "rev-parse", "--verify", "--quiet", ref + "^{commit}"],
        cwd=cwd, capture_output=True, text=True,
    )
    return done.stdout.strip() if done.returncode == 0 else None


def read(path):
    try:
        with open(path, errors="replace") as f:
            return f.read()
    except OSError:
        return ""


def write(path, text):
    with open(path, "w") as f:
        f.write(text)
    return path


def branch_base(named):
    if named:
        return named
    for ref in ("origin/develop", "develop"):
        if resolves(ref):
            return ref
    try:
        return "origin/" + json.loads(run("gh", "pr", "view", "--json", "baseRefName"))["baseRefName"]
    except (RuntimeError, ValueError):
        return "origin/HEAD"


def link_node_modules(main, tree):
    for manifest in run("git", "ls-files", "package.json", "*/package.json", cwd=tree).splitlines():
        rel = os.path.join(os.path.dirname(manifest), "node_modules")
        src, dst = os.path.join(main, rel), os.path.join(tree, rel)
        if os.path.isdir(src) and not os.path.lexists(dst):
            os.symlink(src, dst)


def pr_checkout(scratch, n, oid):
    if run("git", "rev-parse", "HEAD") == oid:
        return os.getcwd()
    tree = os.path.join(scratch, "checkout")
    if not os.path.isdir(tree):
        run("git", "fetch", "origin", f"pull/{n}/head")
        run("git", "worktree", "add", "--detach", tree, oid)
        link_node_modules(os.getcwd(), tree)
    return tree


def prior_round(n, oid, slug):
    """The newest review by the current user whose inline comments carry the marker."""
    login = run("gh", "api", "user", "--jq", ".login")
    marked = set(run(
        "gh", "api", "--paginate", f"repos/{slug}/pulls/{n}/comments", "--jq",
        f'.[] | select(.body | startswith("{MARKER}")) | .pull_request_review_id',
    ).split())
    reviews = run(
        "gh", "api", "--paginate", f"repos/{slug}/pulls/{n}/reviews", "--jq",
        f'.[] | select(.user.login == "{login}") | "\\(.id) \\(.commit_id)"',
    ).splitlines()
    rounds = [line.split()[1] for line in reviews if line.split()[0] in marked]
    return rounds[-1] if rounds else None


def crate_is_solana(path, tree):
    folder = os.path.dirname(path)
    while True:
        manifest = read(os.path.join(tree, folder, "Cargo.toml"))
        if manifest:
            return bool(SOLANA_CRATE.search(manifest))
        if not folder:
            return False
        folder = os.path.dirname(folder)


def domains(path, tree, workflows):
    text = read(os.path.join(tree, path)) if path.endswith(CODE) else ""
    name = os.path.basename(path)
    found = []
    if path.endswith(".rs"):
        if crate_is_solana(path, tree) or SOLANA_USE.search(text):
            found.append("solana")
    elif "target/idl/" in path or SOLANA_USE.search(text):
        found += ["solana", "web"]
    if path.endswith(".sol") or name == "foundry.toml" or name.startswith("hardhat.config."):
        found.append("evm")
    elif EVM_USE.search(text):
        found += ["evm", "web"]
    if not found:
        found.append("web")
    if path.startswith((".github/workflows/", ".github/actions/")) or path in workflows:
        found.append("ci")
    if DATABASE_PATH.search(path) or DATABASE_USE.search(text):
        found.append("database")
    return list(dict.fromkeys(found))


def domain_grep(domain):
    section = read(os.path.join(SKILL, f"domain-{domain}.md")).split("## Tooling", 1)[-1]
    fence = re.search(r"```\n(rg .+?)\n```", section, re.S)
    return re.match(r"rg (-n(?: -i)?) '(.+)'$", fence.group(1)).groups() if fence else None


def run_greps(scratch, tree, by_domain):
    outputs = []
    for domain, files in sorted(by_domain.items()):
        grep = domain_grep(domain)
        if domain == "ci":
            files = run("git", "ls-files", ".github/workflows", cwd=tree).splitlines()
        if domain == "database":
            files = [f for f in files if "drizzle/meta/" not in f]
        files = [f for f in files if os.path.isfile(os.path.join(tree, f))]
        if not grep or not files:
            continue
        flags, pattern = grep
        hits = run("rg", *flags.split(), pattern, "--", *files, cwd=tree, check=False)
        outputs.append(write(os.path.join(scratch, f"grep-{domain}.txt"), hits + "\n"))
    return outputs


def pr_checks(scratch, n):
    lines, tails = [], []
    checks = json.loads(run("gh", "pr", "checks", str(n), "--json", "name,bucket,link", check=False) or "[]")
    for check in checks:
        lines.append(f"{check['name']}: {check['bucket']}")
        run_id = re.search(r"/runs/(\d+)", check.get("link") or "")
        if check["bucket"] != "fail":
            continue
        log = run("gh", "run", "view", run_id.group(1), "--log-failed", check=False) if run_id else ""
        if not log:
            lines[-1] += " (log not fetched)"
            continue
        name = re.sub(r"[^\w.-]+", "-", check["name"])
        tails.append(write(os.path.join(scratch, f"ci-{name}.txt"), "\n".join(log.splitlines()[-40:]) + "\n"))
    return lines or ["no CI"], tails


def main():
    args = sys.argv[1:]
    since = None
    if "--since" in args:
        at = args.index("--since")
        since = args[at + 1]
        del args[at:at + 2]
    scratch, mode, target = os.path.abspath(args[0]), args[1], (args[2] if len(args) > 2 else None)
    os.makedirs(scratch, exist_ok=True)
    out, fails = [f"mode: {mode}"], []

    if mode == "pr":
        slug = run("gh", "repo", "view", "--json", "nameWithOwner", "--jq", ".nameWithOwner")
        pr = json.loads(run("gh", "pr", "view", target, "--json", "number,baseRefName,headRefOid,body"))
        n, head = pr["number"], pr["headRefOid"]
        tree = pr_checkout(scratch, n, head)
        run("git", "fetch", "origin", pr["baseRefName"])
        base = "origin/" + pr["baseRefName"]
        write(os.path.join(scratch, "pr-body.md"), pr["body"] + "\n")
        spec = "# PR body\n\n" + pr["body"] + "\n\n# Commits\n\n"
        out += [f"pr: {slug}#{n}", f"pr-body: {scratch}/pr-body.md",
                f"state file: ~/.claude/review-state/{slug.replace('/', '__')}__{n}.md"]
        tip = head
        since = prior_round(n, head, slug)
        if since and since != head and subprocess.run(["git", "fetch", "origin", since], capture_output=True).returncode:
            fails.append(f"git fetch origin {since}: prior round not fetchable, run a full review")
            since = None
    else:
        tree, base, spec = os.getcwd(), branch_base(target), "# Commits\n\n"
        tip = "HEAD"
        head = run("python3", "-I", os.path.join(SKILL, "tools", "snapshot.py"))
        if since and not resolves(since):
            fails.append(f"git rev-parse {since}: not a commit, run a full review")
        since = since and resolves(since)

    fixed = resolves(base, tree)
    if not fixed:
        raise SystemExit(f"FAIL git rev-parse {base}")
    full = f"{fixed}...{head}"
    if since and not run("git", "diff", "--name-only", since, head, cwd=tree):
        note = "; in PR mode, check the PR body against the state file" if mode == "pr" else ""
        write(os.path.join(scratch, "pin.md"), "\n".join(out + [f"head: {head}", f"nothing new since {since}{note}"]) + "\n")
        print(os.path.join(scratch, "pin.md"))
        return
    target_range = [since, head] if since else [full]
    spec += run("git", "log", "--format=## %h %s%n%n%b", f"{fixed}..{tip}", cwd=tree)
    write(os.path.join(scratch, "spec.md"), spec + "\n")

    changed = run("git", "diff", "--name-only", *target_range, cwd=tree).splitlines()
    workflows = set()
    for workflow in run("git", "ls-files", ".github/workflows", cwd=tree).splitlines():
        text = read(os.path.join(tree, workflow))
        workflows.update(f for f in changed if f in text)
    by_file = {f: domains(f, tree, workflows) for f in changed}
    by_domain = {}
    for f, found in by_file.items():
        for domain in found:
            by_domain.setdefault(domain, []).append(f)

    tooling = run_greps(scratch, tree, by_domain)
    if any(f.endswith((".ts", ".tsx")) for f in changed):
        sig = subprocess.run(
            ["node", os.path.join(SKILL, "tools", "ts-signatures.mjs"), since or fixed, head],
            cwd=tree, capture_output=True, text=True,
        )
        if sig.returncode:
            fails.append(f"ts-signatures.mjs: {sig.stderr.strip()[:200]}")
        else:
            tooling.append(write(os.path.join(scratch, "signatures.txt"), sig.stdout))

    out += [
        f"fixed point: {fixed} ({base})",
        f"head: {head}",
        f"checkout: {tree}",
        f"spec draft: {scratch}/spec.md",
        f"full diff: git diff {full}",
    ]
    if len(target_range) == 2:
        out += [f"last: {since}", f"delta diff: git diff {since} {head}"]
    out.append("diff size: " + (run("git", "diff", "--shortstat", *target_range, cwd=tree) or "empty"))
    out += ["", "## Files"] + [f"- {f}: {', '.join(found)}" for f, found in by_file.items()]
    out += ["", "## Tooling run"] + [f"- {path}" for path in tooling]
    if mode == "pr":
        checks, tails = pr_checks(scratch, n)
        out += ["", "## Checks"] + [f"- {line}" for line in checks] + [f"- tail: {path}" for path in tails]
    out += [""] + [f"FAIL {line}" for line in fails]
    write(os.path.join(scratch, "pin.md"), "\n".join(out) + "\n")
    print(os.path.join(scratch, "pin.md"))


if __name__ == "__main__":
    main()
