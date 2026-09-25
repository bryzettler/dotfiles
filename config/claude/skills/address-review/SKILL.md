---
name: address-review
description: Answer every open finding on your PR — fix it, decline it, defer it to a ticket, or answer it — then push and reply in each thread.
disable-model-invocation: true
---

# address-review

The other half of `review`. A reviewer (you through `/review pr`, a teammate, or a bot) left findings on your PR; this skill gives every one a **verdict**, lands the fixes as one commit, and replies in each thread with what happened and why. The code settles every verdict, never the reviewer's standing: a bot's finding that holds gets fixed, and a senior reviewer's finding that the code refutes gets declined with the `file:line` that refutes it.

The main loop's context is the scarce resource. Comment bodies, code reads, and lint and test output go to sub-agents and scratchpad files and reach the main loop as paths and short returns. `briefs.md` in this folder holds the scout and verifier briefs; sub-agents read it by path.

Shared reference lives in the `review` skill folder (`~/.claude/skills/review/`) and is read there, never restated: the **+EV bar** and **Fix re-review** in `review-core.md`, the **Public text** standard in `pr-mode.md`. The ticket grammar is in `~/.claude/skills/implement-tickets/briefs.md`.

**Invocation:** `/address-review [pr | <n> | #<n> | <PR URL>]`. No argument or `pr`: the PR open for the current branch.

## Verdicts

Each finding ends with exactly one:

| Verdict     | When                                                                                                                                                                 | Reply says                                                    |
| ----------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------- |
| **fix**     | the finding holds and the fix fits this PR; also a lateral nit whose edit is local and that no repo convention contradicts, since taking it costs less than a debate | what changed, and the commit sha                              |
| **claim**   | the code is right and a comment, doc, or PR description is wrong                                                                                                     | which text changed, and the sha (or "PR description updated") |
| **decline** | the scenario cannot happen, the finding misreads the code, or a nit contradicts a repo convention or falls below the +EV bar                                         | the reason in code terms, with the `file:line` that shows it  |
| **defer**   | the finding holds but the fix is out of scope: a separate feature, a redesign, or code this PR does not touch                                                        | that it is valid and tracked as a follow-up                   |
| **answer**  | the reviewer asked a question and no change follows                                                                                                                  | the answer                                                    |
| **ask**     | only the user can decide: the reviewer asks for behaviour the PR body contradicts, or the choice is a product call                                                   | nothing until the user rules at the gate                      |

## Steps

1. **Pin** — one `scout` (`general-purpose`, `model: "opus"`) with the target, the scratchpad path, and the path of `briefs.md`. It checks the branch, collects every open finding into `<scratchpad>/findings.md`, and returns the manifest the Scout brief defines. Done when the manifest names the PR, the head sha, the lint and test commands, and one row per finding. A branch-check failure stops the run and goes to the user as the scout reported it. Zero findings: say so and stop.

2. **Verify** — verifiers (`general-purpose`, `model: "opus"`), one per file with findings, one per top-level directory when more than six files have findings, one for all findings with no file. Each prompt carries its finding ids, the `findings.md` path, the PR body path, the path of `briefs.md`, and a report path under the scratchpad. Done when every finding id has a return in the shape of the Verifier brief.

3. **Decide** — main loop. Give every finding a verdict from the table, working from the verifier returns: VALID → **fix** (`scope: out` → **defer**), FIXED → **fix** with that sha, INVALID → **decline**, CLAIM → **claim**, QUESTION → **answer**, SPEC → **ask**, NIT → **fix** or **decline** per the table. A return the main loop cannot follow goes back to the same verifier once. A **fix** whose edit touches more than one module, changes a public interface, or needs a new design is a **defer**. Done when every finding has a verdict and a one-line reason, and every **fix** and **claim** has a fixer item: file, line, what is wrong, the minimal edit. A reviewer's `suggestion` fence that the verifier confirmed still applies is the edit, verbatim.

4. **Fix** — one `fixer` run with every **fix** and **claim** item on the code, the lint and test commands, and a scratchpad path for their output. Then the Fix re-review from `review-core.md` on its diff, with at most one follow-up `fixer` run. Commit the result on the PR branch as `fix: address review on #<n>` with the attribution trailer. A **claim** on the PR description is a new body written to `<scratchpad>/pr-body.md`, posted at step 7. Done when every item is in the commit or reported as a fixer mismatch (a mismatch becomes **ask**), and lint and tests pass or their failures are shown to pre-exist at the head sha from the manifest.

5. **Defer** — one ticket per **defer** in `.scratch/<feature>/issues/` when a folder matches the branch, else `.scratch/pr-<n>-followups/issues/`, in the ticket grammar from `implement-tickets/briefs.md`: `**Status:** needs-triage`, a body that quotes the finding and the verifier's scenario, and one acceptance criterion. Done when every **defer** has a ticket path.

6. **Draft and gate** — write `<scratchpad>/replies.json`, one entry per finding that is not **ask**: the reply target from `findings.md` and a body that starts with `<!-- address-review -->`. Each body opens by restating the finding in a few words, so the reviewer sees it was read, then says what the table's reply column says, to the Public text standard. A ticket path is private: a **defer** reply says "tracked as a follow-up" and names no path.

   Then one **summary** comment for the PR, at `<scratchpad>/summary.md`, also starting with `<!-- address-review -->`. It opens with one sentence: the commit sha and the count per verdict. Then, per reviewer, a table in the reviewer's own order: the item label (the reviewer's number, else `F<k>`), the finding in a few words, the verdict, and the outcome: the sha, "tracked as a follow-up", the answer in one line, or `{{reply:F<k>}}` for each thread reply that covers the item. Below the table, each finding with no thread gets its full reply under its item label, quoting the item's first line. The summary answers every finding, so a reviewer who reads only it sees each verdict.

   Then show the user: one row per finding (id, reviewer, `file:line`, verdict, one-line reason), each **ask** with the one question that settles it, the commit sha and its lint and test results, the ticket paths, the reply bodies, and the summary. Ask with `AskUserQuestion`: post all, change verdicts first, or stop here. A changed verdict loops back to step 4 or 5 for that finding only. Done when the user picks post or stop. Stop leaves the commit and tickets local and posts nothing.

7. **Push and post** — `git push` (never force). Then post each thread reply with `gh api -X POST repos/{owner}/{repo}/pulls/<n>/comments/<top comment id>/replies -F body=@<file> --jq .html_url`, and replace each `{{reply:F<k>}}` in the summary with a link to that URL. Post the summary last, with `gh pr comment <n> --body-file <scratchpad>/summary.md`, so every link resolves. Post a **claim** on the PR description with `gh pr edit <n> --body-file`. Threads stay unresolved; the reviewer resolves them after checking the fix. Done when every reply returns a URL.

## Rerun

A thread whose newest comment carries `<!-- address-review -->` is answered, and the scout skips it; a reviewer reply after that comment reopens it. A rerun posts a new summary that covers only the findings it answered. A rerun after the user stopped at the gate finds the earlier commit ahead of the remote head; the verifiers return **FIXED** with its sha for the findings it covers, so nothing is fixed twice.

## Cost budget

One scout, at most seven verifiers (six file groups plus one for unanchored findings), one `fixer` plus at most one follow-up. Every agent runs on opus. `fixer` is the only agent that edits code; the main loop writes tickets, the commit, and the replies. Nothing is pushed or posted before the gate.

## Report

The gate table with each posted reply's URL added, the commit sha, the ticket paths, the lint and test results per command, each **ask** with the user's ruling, and the findings skipped as already answered.
