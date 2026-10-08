# address-review: briefs

Find your section by the role your prompt names. Return per `~/.claude/skills/review/return-rule.md`.

## Scout

Start with `python3 -I ~/.claude/skills/address-review/tools/collect.py <scratchpad> <target>`, run in the checkout. It does Branch check, Collect, and the thread half of Delta, and writes `pr-body-original.md`, `findings.md` with one entry per open thread, `bodies.md` with every review body and PR comment, and `collect.md` with the PR fields, the review rounds, the answered item keys, and the skip counts. A `FAIL` line is the branch-check failure: return it. What stays with you: split `bodies.md` into items, append the item findings to `findings.md`, write each covering item key on its thread's `Item:` line, and find the commands. The sections below are the rules the script follows and the ones you apply to the bodies.

**Branch check.** Resolve the PR: `gh pr view <target> --json number,url,author,headRefName,headRefOid,baseRefName,body`. Write the body to `<scratchpad>/pr-body-original.md`. The run edits your working tree and pushes to the PR branch, so all of these must hold, and the first that fails ends the run with that failure as the return:

- The current branch is `headRefName`.
- `git status --porcelain` is empty.
- After `git fetch origin <headRefName>`, `headRefOid` is an ancestor of `HEAD`. Local commits ahead of it are allowed; name them in the manifest.

**Collect.** One GraphQL call, paginated with `after:` cursors when a connection returns 100 nodes:

```sh
gh api graphql -F owner=<o> -F repo=<r> -F n=<n> -f query='
query($owner:String!,$repo:String!,$n:Int!){repository(owner:$owner,name:$repo){pullRequest(number:$n){
  reviewThreads(first:100){nodes{id isResolved isOutdated path line originalLine
    comments(first:50){nodes{databaseId author{login} body createdAt}}}}
  reviews(first:100){nodes{databaseId author{login} state body submittedAt commit{oid}}}
  comments(first:100){nodes{databaseId author{login} body createdAt}}}}}'
```

**Delta.** An earlier run of this skill leaves two marks, and a rerun collects only what came after them: a thread reply that starts with `<!-- address-review -->`, and a summary PR comment by the author that starts with `<!-- address-review -->` and lists the item keys it answered on its `<!-- items: ... -->` line. An item key is `<review or comment databaseId>#<the reviewer's item number>`.

A thread is open when it is unresolved, or when it holds an `<!-- address-review -->` reply: the earlier run resolved the threads it answered, so a comment after that reply reopens the thread whatever `isResolved` says. An open thread is a finding when its newest comment is by someone other than the PR author, or by the PR author and starts with `<!-- review -->` (the `review` skill posting as the author). A thread whose newest comment starts with `<!-- address-review -->` is answered: skip it.

A review body or PR comment is a source of findings when it asks for a change or asks a question. Split it into one **item** per change or question, numbered as the reviewer numbered them (a table row, a list entry), else in order: bot reviews (CodeRabbit, Copilot, Sourcery) often pack many items into one body. An item that an inline thread by the same reviewer covers (the item says "inline", or names the thread's file, line, or mechanism) is covered: write its label on each thread finding it covers. An item whose key is on the items line of a summary is answered: skip it, with or without a covering thread. Every other item is a finding. Skip CI reports, deploy previews, coverage bots, and approvals with no items.

**Write** `<scratchpad>/findings.md`, one entry per finding:

```markdown
## F<k>

Source: thread <top comment databaseId> <thread node id> | review <databaseId> | comment <databaseId>
Item: <each item key this finding is or covers>, or "none"
Reviewer: <login> (bot | human)
Anchor: <path>:<line> (outdated: <yes|no>) | none
Suggestion: <the suggestion fence verbatim, or "none">
Finding: <the full comment text for this item, verbatim>
Thread: <later comments in the thread, author: first line each, or "none">
```

**Review rounds.** List each review whose inline comments start with `<!-- review -->` (a `review` skill round), with its `commit{oid}`, or "none".

Also find the project's lint and test commands (package scripts, `Makefile`, `Cargo.toml`, CI workflow steps), and the backend command for any database or chain code the findings touch (how to start a disposable instance with the schema or program loaded), or "none". Also list the **dependency sources**, per that entry in `~/.claude/skills/review/scout.md`, for the packages the findings' files import.

**Return** a manifest under 300 words, and write the same text to `<scratchpad>/manifest.md`: PR number and URL, `headRefName`, `headRefOid`, local commits ahead of it (sha and subject), the `findings.md` path, the PR body path, the lint and test commands, the backend commands, the dependency sources, the review rounds with their commit shas, one row per finding (id, item, reviewer, anchor, a ten-word gist), each body item that threads cover with their finding ids, and the count of skipped threads and items per reason. Done when every item of every source review body and comment is a finding or names the findings that cover it.

## Verifier

Read `~/.claude/skills/review/verdicts.md` and follow its verifier brief. The prompt carries your finding ids, the `findings.md` path, the PR body path, and the head sha and dependency sources from the manifest. Each finding is a hit; its `Suggestion:` line is the `suggestion` fence the verdict checks. All five verdicts apply.
