# Review: PR mode

What PR mode adds to the steps in `SKILL.md`: the gate, steps 5 to 9, anchoring, and the report additions. Every body this mode posts follows `public-text.md` in this folder.

## Gate (step 4)

- **Approve** when every dispatched axis agrees the change is valid and the execution evidence is green: every check in `gh pr checks <n>` passed, none pending, none failing; and when no check runs the tests, the test run from step 2 passed. Run `gh pr review <n> --approve --body "LGTM"`, write the report, and stop.
- **Red or pending check, or a failing test run:** this blocks approval regardless of the axes. Submit a comment review that names the check or the failing test, and write the report. Approve no pending check and wait on none: the next round approves when it is green.
- **Zero confirmed findings, one or more open Value suspicions:** submit `gh pr review <n> --comment --body <file>`, whose body asks the one question per suspicion that would close it. Write the report and stop.
- **Confirmed findings:** continue to step 5.

## Steps after the gate

5. **Draft** — one `fixer` run in the checkout. It edits the code as if fixing it, runs the project's lint and tests, then converts each change into a suggestion anchored to the PR diff. Its prompt carries the PR head sha, the base branch, the checkout path, the lint and test commands, the JSON path and shape, the path of this file for Anchoring, and the path of `public-text.md` for comment bodies. Its deliverable is a JSON file in the scratchpad, one entry per comment:

   ````json
   {
     "path": "...",
     "start_line": 12,
     "line": 14,
     "body": "<one-line reason>\n\n```suggestion\n<replacement for lines 12-14>\n```"
   }
   ````

   - Every entry starts as a `suggestion` fence. A plain comment is the exception, reached only through Anchoring below. An entry without a fence carries `"plain_reason"`, so the main loop can check each one before posting.
   - Anchor an entry where the fix lands, not where the symptom shows: when the edit belongs on another `+` line, the suggestion goes there.
   - Leave as is posts nothing; its reasoning lives in the terminal report. Redesign gets no entry; it goes in the review body per Post.
   - One confirmed finding is one entry, and every entry carries its full fix as code. Give each finding its own entry and its own code: never fold it into another entry's prose, post it as "also consider" or "a similar test for X", or hand it to the fixer as "describe but do not implement". A fix larger than one fence is a plain comment with a complete fenced code block, per Anchoring.
   - The fixer proves each edit with the narrowest command that covers it: the type check, the lint script, and the unit test file it touched. An edit that touches a migration, a lock, a transaction boundary, or a cursor is proven against the real backend per "Fix re-review" in `review-rules.md`. Otherwise it starts no validator, database, or chain node, and runs no end-to-end suite. An edit whose only proof is such a suite carries `"unproven": "<suite name>"` in the JSON, and the report lists those.

   Done when every item has an entry and the checks it ran pass on the edited checkout, or a failure is shown to pre-exist at the PR head.

6. **Fix re-review** — per "Fix re-review" in `review-rules.md` in this folder, against the fixer's diff in the checkout. Corrections land in the entries.

7. **Redact** — read every `body` as a stranger on the internet would, against `public-text.md`. Rewrite only the ones that fail; leave a passing body byte for byte. Done when every body passes each line of the list in `public-text.md`, and each reason names only code visible in the PR diff.

8. **Post** —
   - List existing marker comments as path and line only: `gh api --paginate repos/{owner}/{repo}/pulls/<n>/comments --jq '.[] | select(.body | startswith("<!-- review -->")) | {path, line}'`. Drop any entry whose path and line already appear, so a rerun adds only new findings.
   - Submit one review: `gh api repos/{owner}/{repo}/pulls/<n>/reviews --input <file>`, with the payload `{ "event": "COMMENT", "body": "<body>", "comments": [...] }`.
   - The body is a one-line summary; then, when there is any Redesign, a **Blocking: design** section with one entry per Redesign (the scenario, then the smaller design); then, when any entry is `unproven`, one line that names each `unproven` comment and the suite it was not run against. The comments carry no such footer.
   - Every comment `body` starts with the marker `<!-- review -->`.
   - This skill submits two events only: `COMMENT` here and `APPROVE` at the gate.

   Done when the API returns the review URL.

9. **Persist** — after the gate or the post, write `~/.claude/review-state/<owner>__<repo>__<n>.md` (create the directory), overwriting it each round: the head sha reviewed, the PR body as reviewed, every open suspicion with what was checked and what was not, and every dismissal with its reason. Copy every `return-*.md` from step 2 to `~/.claude/review-state/<owner>__<repo>__<n>.returns/<head sha>/`, so `address-review` can tell a lens that never fired from one an agent cleared.

## Anchoring

GitHub accepts inline comments only on lines that appear in the PR diff, on `side: "RIGHT"`. A `suggestion` block replaces exactly `start_line`..`line` (single line: omit `start_line`), both inside one hunk. A fix that lands outside the diff goes on the nearest diff line as a plain comment with a fenced code block and no `suggestion` fence, saying where the change belongs.

GitHub refuses to apply a `suggestion` whose range has `-` lines interleaved with it in the hunk ("Applying suggestions on deleted lines is currently not supported"). Before emitting a fence, check the hunk:

1. A deleted line between the added lines of the range: shrink the range to a run of pure `+` lines.
2. Look for a smaller committable fix that fits such a run: the part of the change that stands alone (a hoisted binding, a field assignment on the line that already mutates the account) goes out as a `suggestion`. The rest of the edit follows in the same body as a fenced code block headed by the exact line range it replaces.
3. Only when no pure-`+` run can carry a self-contained edit: a plain fenced code block, with the language tag, headed by the exact line range it replaces.

A one-line suggestion on a `+` line is always safe.

A multi-region refactor (a test fixture rewritten to use an existing encoder, helpers replaced by a shared package) is the other plain comment: the fence would have to replace more than one hunk. Name the helper and the file that already uses it; that is the whole body.

Before posting, count fences: every entry without a `suggestion` fence has a stated reason from this section, and the report lists them.

## Report additions

The review URL and whether it was an approval or a comment review. Per posted comment: `file:line`, a one-line summary, and `suggestion` or `plain: <reason>`.
