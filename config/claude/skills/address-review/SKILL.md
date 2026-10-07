---
name: address-review
description: Use when the user asks to address, answer, or respond to review comments or findings on their PR. Gives every open finding an outcome (fix, decline, defer, or answer), lands the fixes as one commit, pushes, and replies in each thread.
---

# address-review

The other half of `review`. A reviewer (you through `/review pr`, a teammate, or a bot) left findings on your PR. This skill gives every one an **outcome** from the Outcomes table below, lands the fixes as one commit, and replies in each thread with what happened and why. The code settles every outcome, never the reviewer's standing: a bot's finding that holds gets fixed, and a senior reviewer's finding that the code refutes gets declined with the `file:line` that refutes it.

Keep the main loop's context for outcomes. Comment bodies, code reads, and lint and test output go to sub-agents and scratchpad files and reach the main loop as paths and short returns. `briefs.md` in this folder holds the scout and verifier briefs; pass its path. `fixer` is the only agent that edits code; the main loop writes the tickets, the commit, the replies, and the miss log. Nothing is pushed or posted before the gate.

Every `general-purpose` prompt this skill sends ends with: "Do not call the advisor." Those agents orchestrate, and an advisor call resends their whole context to Fable uncached.

**Invocation:** `/address-review [pr | <n> | #<n> | <PR URL>]`. No argument or `pr`: the PR open for the current branch.

## Steps

1. **Pin** — one scout (`general-purpose`, `model: "opus"`) with the target, the scratchpad path, and the path of `briefs.md`. Done when the manifest names the PR, the head sha, the lint and test commands, the backend commands, and one row per finding. A branch-check failure stops the run and goes to the user as the scout reported it. Zero findings: say so and stop.

2. **Verify** — at most three `verifier` agents: findings grouped by top-level directory, the smallest groups merged until three remain, with the findings that have no file in the smallest group. Each prompt carries its finding ids, the `findings.md` path, the PR body path, the head sha from the manifest, and the path of `briefs.md`. Done when every finding id has a verdict per `~/.claude/skills/review/verdicts.md`.

3. **Decide** — in the main loop, read `~/.claude/skills/review/verdicts.md`, then give every finding an outcome from its verdict:

   | Verdict            | Outcome                                        |
   | ------------------ | ---------------------------------------------- |
   | CONFIRMED `defect` | **fix**; with `scope: out`, **defer**          |
   | CONFIRMED `claim`  | **claim**                                      |
   | CONFIRMED `spec`   | **ask**                                        |
   | CONFIRMED `design` | **defer**                                      |
   | CONFIRMED `nit`    | **fix** or **decline**, per the Outcomes table |
   | DISMISSED          | **decline**                                    |
   | OPEN               | **ask**                                        |
   | FIXED              | **fix**, with that sha                         |
   | ANSWER             | **answer**                                     |

   - Send a verdict you cannot follow back to the same verifier once.
   - A **fix** whose edit touches more than one module, changes a public interface, or needs a new design is a **defer**. When the reviewer marked it blocking (a changes-requested review, a "Blocking" heading), it is an **ask**.
   - The +EV bar in the Outcomes table is "The +EV bar" in `verdicts.md`.

   - **Misses:** when the manifest lists a `review` round, a finding from another reviewer is a miss when its verdict is CONFIRMED `defect`, `claim`, or `design` and its anchor line exists at that round's commit (`git show <sha>:<path>`). Append one entry per miss to `~/.claude/review-misses.md`: the date, `<owner>/<repo>#<n>`, the round's sha, the mechanism as a class in one sentence (never the instance), the lens in `~/.claude/skills/review/` that should have fired, or "no lens", and **Seen:** what that round did with the anchor, from its kept returns in `~/.claude/review-state/<owner>__<repo>__<n>.returns/<round sha>/`: "cleared as '<the coverage reason, quoted>' by <axis/slice>, audit <passed|rejected>" (from the kept `audit.txt`), "suspicion, not verified", "hit, dismissed at triage", "in no return", or "returns not kept". "Returns not kept" is itself a process miss against Persist in `~/.claude/skills/review/pr-mode.md`: log it once per round. A clearance the audit passed is a miss in the field definitions of `return-rule.md` or the checks of `tools/audit-coverage.py`, not in the lens. A miss Seen as cleared or unverified is a process miss: its fix goes to the brief rule or triage step that let it through, not to the lens wording. A finding whose anchor line an earlier `fix: address review` commit wrote is a miss of that run's Fix re-review or Delta review: log it with that commit's sha and the step that should have caught it.

   Done when every finding has an outcome and a one-line reason, every miss has an entry, and every **fix** and **claim** has a fixer item: file, line, what is wrong, the minimal edit. A reviewer's `suggestion` fence that the verifier confirmed still applies is the edit, verbatim.

4. **Fix** — one `fixer` run with every **fix** and **claim** item on the code, the lint and test commands, the backend commands, and a scratchpad path for their output.
   - Then "Fix re-review" in `~/.claude/skills/review/review-rules.md` on its diff, with at most one follow-up `fixer` run. A fix still `unproven` after that stays a **fix**, and its reply names the backend it was not run against. A fix whose Remainder names an open part is a partial fix: its reply names that part.
   - **Delta review** — before the commit, call the Skill tool with `review` and the head sha from the manifest as its argument. In branch mode with that base, it reviews only the uncommitted fix diff, and its own fixer lands what it confirms. Its prompts carry no findings, verdicts, or replies: the review is blind, so it reads the fix as a reviewer meets it. Run it once. A finding it leaves unfixed goes to the gate as **ask**.
   - Commit the result on the PR branch as `fix: address review on #<n>` with the attribution trailer. Then copy the revert-check lines and the Delta review's `return-*.md` and `audit.txt`, byte for byte, to `~/.claude/review-state/<owner>__<repo>__<n>.returns/<fix commit sha>/`, so the next round can tell what this fix round saw.
   - A **claim** on the PR description, or any **defer**: write a new body to `<scratchpad>/pr-body.md`, posted at step 7. Call the Skill tool with `pr` and write it in that shape, keep every claim in `pr-body-original.md` that still holds, and follow `~/.claude/skills/review/public-text.md`.

   Done when the Delta review has reported, every item is in the commit or reported as a fixer mismatch (a mismatch becomes **ask**), and lint and tests pass or their failures are shown to pre-exist at the head sha from the manifest.

5. **Defer** — one ticket per **defer** in `.scratch/<feature>/issues/` when a folder matches the branch, else `.scratch/pr-<n>-followups/issues/`, per "Ticket grammar" in `~/.claude/skills/implement-tickets/tickets.md`: `**Status:** needs-triage`, a body that quotes the finding and the verifier's scenario, and one acceptance criterion. The ticket is private, so the reviewer cannot see it: also add one line per **defer** to a `## Follow-ups` section at the end of `<scratchpad>/pr-body.md` (create the section when it is missing), naming the finding in a few words and why it is out of this PR's scope. Done when every **defer** has a ticket path and a Follow-ups line.

6. **Draft and gate** — all text follows `~/.claude/skills/review/public-text.md`.
   - **Replies:** write `<scratchpad>/replies.json`, one entry per finding that is not **ask**: the reply target and thread node id from `findings.md` and a body that starts with `<!-- address-review -->`. Each body opens by restating the finding in a few words, so the reviewer sees it was read, then says what the Outcomes table's reply column says. A **defer** reply says it is listed under Follow-ups in the PR description, and names no ticket path: the path is private. A **fix** reply names the test that pins it; a partial fix also names the part that remains and its outcome.
   - **Summary:** one comment for the PR at `<scratchpad>/summary.md`, covering only the findings this run answered. Its first line is `<!-- address-review -->` and its second is `<!-- items: <key> <key> -->`, with every key on the `Item:` line of those findings, so the next run's scout skips them. The text opens with one sentence: the commit sha and the count per outcome. Then, per reviewer, a table in the reviewer's own order: the item label (the reviewer's number, else `F<k>`), the finding in a few words, the outcome, and its result: the sha, "Follow-ups in the PR description", the answer in one line, or `{{reply:F<k>}}` for each thread reply that covers the item. Below the table, each finding with no thread gets its full reply under its item label, quoting the item's first line. The summary answers every finding it covers, so a reviewer who reads only it sees each outcome.
   - **Gate:** show the user one row per finding (id, reviewer, `file:line`, outcome, one-line reason), each **ask** with the one question that settles it, the commit sha and its lint and test results, the revert-check lines and the Delta review's severity table, the ticket paths, the Follow-ups section, the reply bodies (each post resolves its thread), and the summary. Ask with `AskUserQuestion`: post all, change outcomes first, or stop here. A changed outcome loops back to step 4 or 5 for that finding only. Stop leaves the commit and tickets local and posts nothing.

   Done when the user picks post or stop.

7. **Push and post** —
   1. `git push`, never with force.
   2. Post each thread reply with `gh api -X POST repos/{owner}/{repo}/pulls/<n>/comments/<top comment id>/replies -F body=@<file> --jq .html_url`, and replace each `{{reply:F<k>}}` in the summary with a link to that URL. Then resolve that thread: `gh api graphql -f query='mutation($id:ID!){resolveReviewThread(input:{threadId:$id}){thread{isResolved}}}' -f id=<thread node id> --jq .data.resolveReviewThread.thread.isResolved`.
   3. Post the summary last, with `gh pr comment <n> --body-file <scratchpad>/summary.md`, so every link resolves.
   4. Post `<scratchpad>/pr-body.md`, when a **claim** or a **defer** wrote it, with `gh pr edit <n> --body-file`.

   An **ask** thread has no reply and stays open. Done when every reply returns a URL and its thread returns `true`.

## Outcomes

Each finding ends with exactly one:

| Outcome     | When                                                                                                                                                                 | Reply says                                                    |
| ----------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------- |
| **fix**     | the finding holds and the fix fits this PR; also a lateral nit whose edit is local and that no repo convention contradicts, since taking it costs less than a debate | what changed, the commit sha, the test that pins it, the part that remains when the fix is partial, and the backend when `unproven` |
| **claim**   | the code is right and a comment, doc, or PR description is wrong                                                                                                     | which text changed, and the sha (or "PR description updated") |
| **decline** | the scenario cannot happen, the finding misreads the code, or a nit contradicts a repo convention or falls below the +EV bar                                         | the reason in code terms, with the `file:line` that shows it  |
| **defer**   | the finding holds but the fix is out of scope: a separate feature, a redesign, or code this PR does not touch                                                        | that it is valid and listed under Follow-ups in the PR description |
| **answer**  | the reviewer asked a question and no change follows                                                                                                                  | the answer                                                    |
| **ask**     | only the user can decide: the reviewer asks for behaviour the PR body contradicts, the choice is a product call, or funds, authority, or data could be lost and the code cannot settle it (OPEN)                                                | nothing until the user rules at the gate                      |

## Report

The gate table with each posted reply's URL added, the commit sha, the ticket paths, the lint and test results per command, each **ask** with the user's ruling, the miss entries appended, and the count of threads and items skipped as already answered.
