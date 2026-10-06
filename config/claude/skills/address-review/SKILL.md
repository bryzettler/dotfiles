---
name: address-review
description: Use when the user asks to address, answer, or respond to review comments or findings on their PR. Gives every open finding an outcome (fix, decline, defer, or answer), lands the fixes as one commit, pushes, and replies in each thread.
---

# address-review

The other half of `review`. A reviewer (you through `/review pr`, a teammate, or a bot) left findings on your PR. This skill gives every one an **outcome** from the Outcomes table below, lands the fixes as one commit, and replies in each thread with what happened and why. The code settles every outcome, never the reviewer's standing: a bot's finding that holds gets fixed, and a senior reviewer's finding that the code refutes gets declined with the `file:line` that refutes it.

Keep the main loop's context for outcomes. Comment bodies, code reads, and lint and test output go to sub-agents and scratchpad files and reach the main loop as paths and short returns. `briefs.md` in this folder holds the scout and verifier briefs; pass its path. `fixer` is the only agent that edits code; the main loop writes the tickets, the commit, the replies, and the miss log. Nothing is pushed or posted before the gate.

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

   - **Misses:** when the manifest lists a `review` round, a finding from another reviewer is a miss when its verdict is CONFIRMED `defect`, `claim`, or `design` and its anchor line exists at that round's commit (`git show <sha>:<path>`). Append one entry per miss to `~/.claude/review-misses.md`: the date, `<owner>/<repo>#<n>`, the round's sha, the mechanism as a class in one sentence (never the instance), and the lens in `~/.claude/skills/review/` that should have fired, or "no lens".

   Done when every finding has an outcome and a one-line reason, every miss has an entry, and every **fix** and **claim** has a fixer item: file, line, what is wrong, the minimal edit. A reviewer's `suggestion` fence that the verifier confirmed still applies is the edit, verbatim.

4. **Fix** — one `fixer` run with every **fix** and **claim** item on the code, the lint and test commands, the backend commands, and a scratchpad path for their output.
   - Then "Fix re-review" in `~/.claude/skills/review/review-rules.md` on its diff, with at most one follow-up `fixer` run. A fix still `unproven` after that stays a **fix**, and its reply names the backend it was not run against.
   - Commit the result on the PR branch as `fix: address review on #<n>` with the attribution trailer.
   - A **claim** on the PR description: write a new body to `<scratchpad>/pr-body.md`, posted at step 7. Call the Skill tool with `pr` and write it in that shape, keep every claim in `pr-body-original.md` that still holds, and follow `~/.claude/skills/review/public-text.md`.

   Done when every item is in the commit or reported as a fixer mismatch (a mismatch becomes **ask**), and lint and tests pass or their failures are shown to pre-exist at the head sha from the manifest.

5. **Defer** — one ticket per **defer** in `.scratch/<feature>/issues/` when a folder matches the branch, else `.scratch/pr-<n>-followups/issues/`, per "Ticket grammar" in `~/.claude/skills/implement-tickets/tickets.md`: `**Status:** needs-triage`, a body that quotes the finding and the verifier's scenario, and one acceptance criterion. Done when every **defer** has a ticket path.

6. **Draft and gate** — all text follows `~/.claude/skills/review/public-text.md`.
   - **Replies:** write `<scratchpad>/replies.json`, one entry per finding that is not **ask**: the reply target from `findings.md` and a body that starts with `<!-- address-review -->`. Each body opens by restating the finding in a few words, so the reviewer sees it was read, then says what the Outcomes table's reply column says. A **defer** reply says "tracked as a follow-up" and names no ticket path: the path is private.
   - **Summary:** one comment for the PR at `<scratchpad>/summary.md`, also starting with `<!-- address-review -->`, covering only the findings this run answered. It opens with one sentence: the commit sha and the count per outcome. Then, per reviewer, a table in the reviewer's own order: the item label (the reviewer's number, else `F<k>`), the finding in a few words, the outcome, and its result: the sha, "tracked as a follow-up", the answer in one line, or `{{reply:F<k>}}` for each thread reply that covers the item. Below the table, each finding with no thread gets its full reply under its item label, quoting the item's first line. The summary answers every finding it covers, so a reviewer who reads only it sees each outcome.
   - **Gate:** show the user one row per finding (id, reviewer, `file:line`, outcome, one-line reason), each **ask** with the one question that settles it, the commit sha and its lint and test results, the ticket paths, the reply bodies, and the summary. Ask with `AskUserQuestion`: post all, change outcomes first, or stop here. A changed outcome loops back to step 4 or 5 for that finding only. Stop leaves the commit and tickets local and posts nothing.

   Done when the user picks post or stop.

7. **Push and post** —
   1. `git push`, never with force.
   2. Post each thread reply with `gh api -X POST repos/{owner}/{repo}/pulls/<n>/comments/<top comment id>/replies -F body=@<file> --jq .html_url`, and replace each `{{reply:F<k>}}` in the summary with a link to that URL.
   3. Post the summary last, with `gh pr comment <n> --body-file <scratchpad>/summary.md`, so every link resolves.
   4. Post a **claim** on the PR description with `gh pr edit <n> --body-file`.

   Leave the threads unresolved: the reviewer resolves them after checking the fix. Done when every reply returns a URL.

## Outcomes

Each finding ends with exactly one:

| Outcome     | When                                                                                                                                                                 | Reply says                                                    |
| ----------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------- |
| **fix**     | the finding holds and the fix fits this PR; also a lateral nit whose edit is local and that no repo convention contradicts, since taking it costs less than a debate | what changed, the commit sha, and the backend when `unproven` |
| **claim**   | the code is right and a comment, doc, or PR description is wrong                                                                                                     | which text changed, and the sha (or "PR description updated") |
| **decline** | the scenario cannot happen, the finding misreads the code, or a nit contradicts a repo convention or falls below the +EV bar                                         | the reason in code terms, with the `file:line` that shows it  |
| **defer**   | the finding holds but the fix is out of scope: a separate feature, a redesign, or code this PR does not touch                                                        | that it is valid and tracked as a follow-up                   |
| **answer**  | the reviewer asked a question and no change follows                                                                                                                  | the answer                                                    |
| **ask**     | only the user can decide: the reviewer asks for behaviour the PR body contradicts, the choice is a product call, or funds, authority, or data could be lost and the code cannot settle it (OPEN)                                                | nothing until the user rules at the gate                      |

## Report

The gate table with each posted reply's URL added, the commit sha, the ticket paths, the lint and test results per command, each **ask** with the user's ruling, the miss entries appended, and the findings skipped as already answered.
