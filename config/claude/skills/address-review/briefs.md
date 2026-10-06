# address-review: briefs

Find your section by the role your prompt names. Return per `~/.claude/skills/review/return-rule.md`.

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

A review body or PR comment is a source of findings when it asks for a change or asks a question. Split it into one **item** per change or question, numbered as the reviewer numbered them (a table row, a list entry), else in order: bot reviews (CodeRabbit, Copilot, Sourcery) often pack many items into one body. An item that an inline thread by the same reviewer covers (the item says "inline", or names the thread's file, line, or mechanism) is covered: write its label on each thread finding it covers. Every other item is a finding. Skip CI reports, deploy previews, coverage bots, and approvals with no items. A PR comment that already has a later PR comment by the author starting with `<!-- address-review -->` quoting it is answered: list it as skipped.

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

Also find the project's lint and test commands (package scripts, `Makefile`, `Cargo.toml`, CI workflow steps), and the backend command for any database or chain code the findings touch (how to start a disposable instance with the schema or program loaded), or "none".

**Return** a manifest under 300 words: PR number and URL, `headRefName`, `headRefOid`, local commits ahead of it (sha and subject), the `findings.md` path, the PR body path, the lint and test commands, the backend commands, one row per finding (id, item, reviewer, anchor, a ten-word gist), each body item that threads cover with their finding ids, and one row per skipped thread or comment with the reason. Done when every item of every source review body and comment is a finding or names the findings that cover it.

## Verifier

Read `~/.claude/skills/review/verdicts.md` and follow its verifier brief. The prompt carries your finding ids, the `findings.md` path, the PR body path, and the head sha from the manifest. Each finding is a hit; its `Suggestion:` line is the `suggestion` fence the verdict checks. All five verdicts apply.
