# Review: PR mode

Read by the main loop in PR mode only, beside `SKILL.md` and `review-core.md`. It holds what PR mode adds to the shared steps, the PR steps after the gate, and the standards every posted body meets.

## Prior round

Part of step 1, Pin. The scout also looks for an earlier round on this PR: the newest review by the current user (`gh api user --jq .login`) whose inline comments start with `<!-- review -->`, and the state file at `~/.claude/review-state/<owner>__<repo>__<n>.md`. When neither exists this is round one and the rest of this section does not apply. When a prior review exists, its `commit_id` is `<last>`. `git fetch origin <last>` (a force-pushed head is still fetchable by sha); if that fails, the round is a full review and the manifest says so. Otherwise the review is a **delta round**: the review diff is `git diff <last> HEAD`, the fixed point for the axes is `<last>`, and the full-PR diff command is still named in every prompt as context, never as the target. The scout writes `<scratchpad>/prior.md`: one entry per prior inline comment with path, line, the first line of the body, the author's reply if any, and whether the delta touches that file; plus the carried suspicions and dismissals from the state file; plus whether the PR body changed since `<last>` (compare against the body recorded in the state file). The manifest adds `<last>`, the delta line count, the `prior.md` path, and whether the PR body changed.

## Delta round

Part of step 2, Review. Every prompt carries the `prior.md` path and says the target is the delta diff. Defects and Value always run. Standards runs only when the delta touches a file outside the test tree. Spec runs only when the PR body changed or the delta touches a file outside the test tree. A skipped axis is named in the report. Defects also owns the follow-up check in `prior.md`: for each prior comment the author replied to, confirm the delta does what the reply says; a reply the code does not back is a hit.

## Gate

Step 4 in PR mode. Approve when all four axes agree the change is valid and the execution evidence is green: every check in `gh pr checks <n>` passed, none pending, none failing; and when no check runs the tests, the scout's test run passed. The approval is `gh pr review <n> --approve --body "LGTM"`. Write the report and stop. A red or pending check, or a failing test run, blocks approval regardless of the axes: submit a comment review that names the check or the failing test, and write the report. Pending is never approved and never waited on; the next round approves when it is green. Zero confirmed findings but one or more open Value suspicions: no approval. Submit `gh pr review <n> --comment --body <file>` whose body asks the one question per suspicion that would close it, written to the "Public text" standard, then write the report and stop.

## Steps after the gate

5. **Draft** — one `fixer` run in the checkout. It edits the code as if fixing it, runs the project's lint and tests, then converts each change into a suggestion anchored to the PR diff. Its deliverable is a JSON file in the scratchpad, one entry per comment:

   ````json
   {
     "path": "...",
     "start_line": 12,
     "line": 14,
     "body": "<one-line reason>\n\n```suggestion\n<replacement for lines 12-14>\n```"
   }
   ````

   Every entry starts as a `suggestion` fence. A plain comment is the exception, reached only through the rules under "Anchoring suggestions" below, and the JSON carries `"plain_reason"` on any entry without a fence so the main loop can check each one before posting. An entry is anchored where the fix lands, not where the symptom shows: if the finding was spotted at one diff line but the edit belongs on another `+` line, the suggestion goes on the latter. A "leave as is" resolution posts nothing; its reasoning lives in the terminal report.

   One confirmed finding is one entry, and every entry carries its full fix as code. A finding is never folded into another entry's prose, never posted as "also consider" or "a similar test for X", and never handed to the fixer as "describe but do not implement". Authors apply fences and skim prose: a finding that leaves this step without its own code block comes back next round as a new finding, at full price. When the fix is larger than one fence it is a plain comment with a complete fenced code block, per "Anchoring suggestions" below.

   The fixer proves each edit with the narrowest command that covers it: the type check, the lint script, and the unit test file it touched. It does not start a validator, a database, or a chain node, and it does not run an end-to-end suite. An edit whose only proof is such a suite goes out with the entry marked `"unproven": "<suite name>"` in the JSON, its comment `body` ends with the line "Not run against `<suite name>`.", and the report lists those. The author sees the flag where the fence is, not only the reviewer in the terminal. The prompt also carries the PR head sha, the base branch, the checkout path, the lint and test commands, the JSON path and shape, the anchoring rules, and the "Public text" standard for comment bodies. Done when every item has an entry and the checks it ran pass on the edited checkout, or a failure is shown to pre-exist at the PR head.

6. **Re-review the fix** — per `review-core.md`, against the fixer's diff in the checkout. Done when every drafted change passes the five lenses, or one follow-up `fixer` run has corrected the entries.

7. **Redact** — the fixer wrote each `body` to the "Public text" standard already. Read every body as a stranger on the internet would, and rewrite only the ones that fail; a passing body is left byte for byte. Done when each body's reason is stated purely in terms of the code visible in the PR diff.

8. **Post** — list existing marker comments as path and line only (`gh api --paginate repos/{owner}/{repo}/pulls/<n>/comments --jq '.[] | select(.body | startswith("<!-- review -->")) | {path, line}'`) and drop any entry whose path and line already appear, so a rerun adds only new findings. In a delta round also drop any entry that restates a prior comment the author's reply resolved, per `prior.md`. Submit one review with `gh api repos/{owner}/{repo}/pulls/<n>/reviews --input <file>` where the payload is `{ "event": "COMMENT", "body": "<one-line summary>", "comments": [...] }` and every comment `body` starts with the marker `<!-- review -->`. `COMMENT` here and `APPROVE` only at the gate are the only two events this skill submits. Done when the API returns the review URL.

9. **Persist** — after the gate or the post, write `~/.claude/review-state/<owner>__<repo>__<n>.md` (create the directory) so the next round starts from it instead of re-deriving: the head sha reviewed, the PR body as reviewed, every open suspicion with what was checked and what was not, and every dismissal with its reason. Overwrite the file each round.

## Public text

The repo may be open source, so every review body and comment body is public the moment it posts. Write each one from the PR diff alone: name the code, the line, and the property of the code that makes the change worth taking. Context that came from anywhere else stays in the terminal report: the reviewer's own memory files, `.scratch/`, `CONTEXT.md`, session history, and anything learned about how the code runs in production. Concretely, the terminal report is the only home for: incident and outage details, log excerpts, replica counts, tick rates, hostnames, RPC endpoints, dashboards, Sentry or ticket links, wallet or program addresses, key names, multisig details, deploy schedules, and the names of people or teams. A reason that needs any of those to make sense is rewritten to lean on the code instead; where the code alone cannot carry it, post the suggestion with a shorter reason rather than the sensitive one.

Disclosure is a separate limit. A Value finding whose exploit path also exists in code that is already deployed (a pre-existing missing check the PR now makes reachable, or a helper the PR reuses) is a live vulnerability, and a PR comment describing it is public disclosure. Post only the fix with a neutral one-line reason ("bind the destination to the signer"), never the attack. The attack, the affected deployed paths, and the urgency go to the terminal report and to the user, who decides how to notify the team.

## Anchoring suggestions

GitHub only accepts inline comments on lines that appear in the PR diff, on `side: "RIGHT"`, and a `suggestion` block replaces exactly `start_line`..`line` (single line: omit `start_line`), both inside one hunk. Fixes that land outside the diff go on the nearest diff line as a plain comment with a fenced code block and no `suggestion` fence, saying where the change belongs.

GitHub also refuses to apply a `suggestion` whose `start_line`..`line` range has `-` lines interleaved with it in the hunk ("Applying suggestions on deleted lines is currently not supported"). Before emitting a `suggestion` fence, check the hunk: if any deleted line sits between the added lines of the range, the range must shrink to a run of pure `+` lines with no deletions between them. Before giving up on a fence, look for a smaller committable fix that fits such a run: the part of the change that stands alone (a hoisted binding, a field assignment on the line that already mutates the account) goes out as a `suggestion`, and the rest of the edit follows in the same body as a fenced code block headed by the exact line range it replaces. Only when no pure-`+` run can carry a self-contained edit does the fix go out as a plain fenced code block (with the language tag) headed by the exact line range it replaces. A one-line suggestion on a `+` line is always safe.

Multi-region refactors (a test fixture rewritten to use an existing encoder, helpers replaced by a shared package) are the other legitimate plain comment: the fence would have to replace more than one hunk. Name the helper and the file that already uses it; that is the whole body.

Before posting, count fences: every entry without a `suggestion` fence must have a stated reason from this section, and the report lists them.

## Report additions

The review URL and whether it was an approval or a comment review, and per posted comment `file:line`, a one-line summary, and `suggestion` or `plain: <reason>`.
