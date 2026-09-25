# address-review: briefs

Read by the scout and the verifiers by path. The main loop never opens this file.

## Scout

**Branch check.** Resolve the PR: `gh pr view <target> --json number,url,author,headRefName,headRefOid,baseRefName,body`. Write the body to `<scratchpad>/pr-body-original.md`. The run edits your working tree and pushes to the PR branch, so all of these must hold, and the first that fails ends the run with that failure as the return:

- The current branch is `headRefName`.
- `git status --porcelain` is empty.
- After `git fetch origin <headRefName>`, `headRefOid` is an ancestor of `HEAD`. Local commits ahead of it are allowed; name them in the manifest.

**Collect.** One GraphQL call, paginated with `after:` cursors when a connection returns 100 nodes:

```sh
gh api graphql -F owner=<o> -F repo=<r> -F n=<n> -f query='
query($owner:String!,$repo:String!,$n:Int!){repository(owner:$owner,name:$repo){pullRequest(number:$n){
  reviewThreads(first:100){nodes{isResolved isOutdated path line originalLine
    comments(first:50){nodes{databaseId author{login} body createdAt}}}}
  reviews(first:100){nodes{databaseId author{login} state body submittedAt}}
  comments(first:100){nodes{databaseId author{login} body createdAt}}}}}'
```

A thread is a finding when it is unresolved and its newest comment is by someone other than the PR author, or by the PR author and starts with `<!-- review -->` (the `review` skill posting as the author). A thread whose newest comment starts with `<!-- address-review -->` is already answered: list it as skipped.

A review body or PR comment is a source of findings when it asks for a change or asks a question. Split it into one **item** per change or question, numbered as the reviewer numbered them (a table row, a list entry), else in order: bot reviews (CodeRabbit, Copilot, Sourcery) often pack many items into one body. An item that an inline thread by the same reviewer covers (the item says "inline", or names the thread's file, line, or mechanism) is not a finding of its own: write its label on each thread finding it covers. Every other item is a finding. CI reports, deploy previews, coverage bots, and approvals with no items are not findings. A PR comment that already has a later PR comment by the author starting with `<!-- address-review -->` quoting it is answered: list it as skipped.

**Write** `<scratchpad>/findings.md`, one entry per finding:

```markdown
## F<k>

Source: thread <top comment databaseId> | review <databaseId> | comment <databaseId>
Item: <review or comment databaseId>#<the reviewer's item number>, or "none"
Reviewer: <login> (bot | human)
Anchor: <path>:<line> (outdated: <yes|no>) | none
Suggestion: <the suggestion fence verbatim, or "none">
Finding: <the full comment text for this item, verbatim>
Thread: <later comments in the thread, author: first line each, or "none">
```

Also find the project's lint and test commands (package scripts, `Makefile`, `Cargo.toml`, CI workflow steps).

**Return** a manifest under 300 words: PR number and URL, `headRefName`, `headRefOid`, local commits ahead of it (sha and subject), the `findings.md` path, the PR body path, the lint and test commands, one row per finding (id, item, reviewer, anchor, a ten-word gist), each body item that threads cover with their finding ids, and one row per skipped thread or comment with the reason. Done when every item of every source review body and comment is a finding or names the findings that cover it.

## Verifier

You judge review findings against the code. You edit nothing. The reviewer may be right, wrong, or right about a different line than the one they commented on; the code settles it. Read the code each finding points at and whatever it depends on. For an outdated anchor, find where that code lives now at `HEAD`. Check `git log <headRefOid>..HEAD` too: a local commit may already fix the finding.

For each finding id, return one of these, with the `file:line` that settles it quoted:

- **VALID** — the scenario, input or state then the wrong outcome, in your own words under 80 words; then the minimal edit as file, line, and replacement. When the finding carries a `suggestion` fence, say whether it still applies verbatim at `HEAD`, and if it does not, give the corrected edit. Add `scope: out` when the fix needs changes to code this PR does not touch, a new design, or a public interface change.
- **FIXED** — a local commit or a later line already resolves it; name the sha.
- **INVALID** — the specific reason the scenario cannot happen or the finding misreads the code.
- **CLAIM** — the code is right and a comment, doc, or the PR body says otherwise; quote the wrong text and give its corrected wording.
- **QUESTION** — the finding asks something and no change follows; the answer, from the code, in under 60 words.
- **SPEC** — the finding asks for behaviour the PR body contradicts, or the choice is a product call; the PR body line and the question the author must answer.
- **NIT** — a lateral preference (naming, ordering, style). Say whether a repo convention (a lint rule, a style guide, the dominant pattern in sibling files) supports or contradicts it, and give the edit.

Two reasons never make a finding INVALID: that the flaw predates the PR, and that the PR body asks for the behaviour. The first is VALID with `scope: out` when the PR does not reach the flaw; the second is SPEC.

Write the full reasoning to your report path. Return only the per-finding verdicts.
