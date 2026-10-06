---
name: implement-tickets
description: Work a folder of ticket markdown files to done, across repos - one implementer per ticket, one review per PR group.
disable-model-invocation: true
---

# implement-tickets

Work a folder of ticket markdown files to done. Tickets are a **task graph**: `Blocked by:` edges define a **frontier** of tickets ready to run. One fresh-context implementer per ticket, routed to the cheapest tier the ticket allows. State lives in each ticket's `Status:` field, so a rerun after a usage-limit stop, crash, or `/clear` resumes where it left off.

Keep the main loop's context for decisions. Ticket bodies, spec text, verification output, and review reports go to sub-agents and scratchpad files and reach the main loop as paths and short returns. `briefs.md` in this folder holds the sub-agent briefs; pass its path, and keep its text out of the main loop. The main loop writes only these: `Status:` edits and `## Agent result` sections in ticket files it dispatched, `spec-questions.md`, the tickets that `answers.md` and step 5 create, and the PR bodies that step 6 drafts. Nothing is pushed and no PR is opened.

**Invocation:** `/implement-tickets [issues-folder] [--dry-run] [--only NN,NN] [--max N] [--answers]`

Folder given: use it. None: glob `.scratch/*/issues` from the current directory. One match: use it and say which. Several: list them and ask. None: ask for the path.

**Model:** when the system prompt names a model other than opus, stop before anything else and say to relaunch with `claude --model opus`. `--dry-run` is the one exception.

**`--answers` given:** first apply the answers per `answers.md` in this folder, then continue at step 1.

## Steps

1. **Plan** — one scout (`general-purpose`, `model: "opus"`). Its prompt carries the issues folder path, the `--only` list, the scratchpad path, and the path of `briefs.md`. Done when every `*.md` in the folder has a row in the manifest and every repo path is verified or listed as unresolved. Ask the user about unresolved repos. Print the table. `--dry-run` stops here.

2. **Explore** — only when the scout recommends it: one explorer (`general-purpose`, `model: "opus"`) per research topic. Its prompt carries the topic, the spec path, the repo paths, the notes folder `<issues-folder>/../notes/`, and the path of `briefs.md`. Done when it returns the list of note files it wrote.

3. **Execute** — the frontier loop. Each PR group has one branch, named after the group, in one worktree. Each ticket runs in a worktree of its own, on a ticket branch cut from the group tip, and lands on the group branch by fast-forward when it returns `done`. Review no ticket on its own: step 4 reviews each group's tickets together.
   - **Bad group name:** when the scout flags a bad name from a `**PR:**` header, ask the user for a better one before the group's first ticket, and update the headers.
   - **Group worktree:** before a group's first ticket, run `git worktree add -b <group> <path> <base>` from the group's base (the tip of the group it stacks on, else the repo's default branch), at the path the repo's conventions name, else `<repo>-<group>` beside the repo. On a rerun, `git worktree list` finds it. A branch with no worktree (step 4 removed it) gets one with `git worktree add <path> <group>`.
   - **Dispatch** every frontier ticket at once, in the background, whatever its repo. A ticket whose blocker is still running is waiting. Wait for each return as a task notification, never with a sleep loop. Each `done` recomputes the frontier and unlocks dependents within the same run.

   Per dispatched ticket:
   1. **Claim** — edit the ticket, `Status: ready-for-agent` → `Status: in-progress`, and note the group branch's head sha as the ticket's start sha. Cut the ticket branch and worktree from it: `git worktree add -b NN-slug <path> <group>`, at `<repo>-NN-slug` beside the repo unless the repo's conventions name another place.
   2. **Dispatch** one implementer at the ticket's tier: standard → `implementer`, deep → `implementer-deep`. The prompt carries only pointers: the ticket path; the spec path plus the ticket's section numbers; the notes folder when exploration ran; the repo path(s); the group branch, the ticket branch, and the ticket worktree path; the scratchpad path `<scratchpad>/<NN>/` for verification output; and the path of `briefs.md`. Done when the return arrives in the shape `briefs.md` names.
   3. **Record** — by the return's outcome:
      - `done` → land it, per Landing below. Then `Status: done`, tick only the acceptance-criteria boxes the return lists as verified, and append the Agent result below, with the group tip after the fast-forward as the head sha.
      - `mismatch` from a standard run (the ticket or spec leaves a design decision open, the plan conflicts with the code, or the spec contradicts the real system) → escalate once: leave `in-progress`, keep the partial branch per Partial work below, cut a fresh ticket branch and worktree from the group tip as in Claim, re-dispatch at deep with the same pointers plus the standard return's mismatch text, and note the escalation in the Agent result. Escalation is the only main-loop override of a tier, and only upward.
      - `mismatch` from a deep run → `Status: failed`; its question goes to step 5.
      - `blocked` (a missing tool, broken test infrastructure, a dependency that does not build) → `Status: failed`, no retry; its dependents become waiting.
   4. Stop when the frontier is empty or `--max` is reached.

4. **Harden** — always run: it reviews what landed. Name a waiting, failed, or unrun ticket in the report and continue. Per PR group, one reviewer (`general-purpose`, `model: "opus"`) round at a time, on the group's tip, against the base its PR will target: the previous group's tip when groups stack, else the fork point on the default branch. Groups in different repos run concurrently. Groups in one stack run bottom up; before a stacked group's first round, merge the lower group's final tip into its branch.
   - **Reviewer prompt:** the repo path, the worktree path when there is one, the branch, the base ref, the follow-ups, the spec files (the group's ticket paths and the spec), the round number, the scratchpad path, and the path of `briefs.md`. A delta round adds the sha the previous round reviewed, that round's report path, and its scout manifest path when the same run produced it.
   - **First hardening** (no `Hardened:` line on any ticket of the group):
     - Round 1 is a full review of the group's diff. Its follow-ups are the `Unverified:` lines of the group's `done` tickets, each tagged with its ticket number.
     - Round 2 is a delta round on round 1's fixes. Run it only when round 1 confirmed a finding of medium or higher severity.
   - **Group already hardened** (a ticket has a `Hardened:` line): skip the group when its tip sha has a `Hardened:` line. When the tip moved, read `re-harden.md` in this folder.
   - A fix earns no round of its own: the fix re-review inside the round already read its diff, and the round runs the full lint and test suite on the head after the fix commit.

   Append `Hardened: <tip sha> · <rounds> rounds · <clean|cap|carried> (<report path>)` to the Agent result of the group's tip ticket. Carry each spec question and follow-up to step 5. A failing lint or test run in a round's return leaves the group with no `Hardened:` line, and the report leads with it. Done when every `done` ticket's head sha is an ancestor of a tip with a `Hardened:` line (`git merge-base --is-ancestor`), and each group's last round either confirmed nothing of medium or higher severity or was the last its rules allow. Then remove the group worktrees (`git worktree remove`) and keep the branches.

5. **Ask** — **follow-ups or open questions from this run:** read `ask.md` in this folder and do it. None: go to step 6.

6. **Report** — first draft each PR body, then the report.
   - **PR body:** per PR group with a `Hardened:` line, call the Skill tool with `pr` and write `<issues-folder>/../pr-body-<group>.md` in its shape. Its sources are the group's Agent results, `pr-notes-<group>.md`, the pre-deploy checklist, and the last round's report. Evidence names the verification commands and their results, and the pr notes go next to the claims they support. Each claim in the body traces to a verification output file or a `file:line`; cut a claim with neither. A ticket with Value as its tier reason makes the door a question: an on-chain upgrade, a migration, or anything that moves funds or authority is one-way unless the ticket names a rollback. Blast Radius names every consumer that the checklist or the report names. Write the body per `~/.claude/skills/review/public-text.md`. Overwrite the file on each run, so it follows the group tip.
   - **Report**, in this order:
     1. The questions that still have an empty `Answer:`, and the path of `spec-questions.md`.
     2. The answers applied this run, one line each: ticket, answer, and the ticket it created or reset.
     3. The follow-ups sorted in step 5: per group, the count added to each of the checklist, `pr-notes-<group>.md`, and `followups-<group>.md`, with their paths.
     4. A table: ticket · outcome · tier · repo · branch · head sha · one-line note.
     5. Per PR group: rounds run this run and over its life, each round's confirmed count and whether it was full or delta, the tickets a `carried` line names, the last round's severity table, open suspicions, lint and test results, and whether it ended clean or at the cap.
     6. The path of each PR body.
     7. Waiting tickets with what unblocks each, escalations, and the reminder that branches are unpushed and the same command resumes safely.

## Landing

For a `done` return, in the group worktree: `git merge --ff-only NN-slug`.

When that fails because another ticket landed after the implementer merged the group tip:

1. In the ticket worktree, `git merge <group>`.
2. On a clean merge, rerun the return's verification commands there in the background, one output file each under `<scratchpad>/<NN>/`. Read only the tail of each. Fast-forward when every one passes.
3. On a conflict, `git merge --abort`. On a conflict or a failing command, dispatch the implementer again at the same tier with the same pointers and one instruction: merge the group tip and resolve the conflict, or fix the failure at the output path named; rerun verification; return. Then fast-forward again.

Then `git worktree remove` the ticket worktree and `git branch -d NN-slug`.

## Partial work

A `mismatch` or `blocked` return leaves its partial work committed as `wip(NN): partial` on the ticket branch, and the group branch untouched. Before any further dispatch of the ticket, the escalation included, keep that work: `git branch -m NN-slug NN-slug-partial-<tier>` (suffix `-2`, `-3` when the name is taken), then `git worktree remove` the ticket worktree. Name the branch in the Agent result.

## Agent result

```markdown
## Agent result (<date>)

Repo: <path> · Branch: <group branch> · Ticket branch: <NN-slug, merged | NN-slug-partial-<tier>> · Tier: <standard|deep, escalated?>
Commits: <the ticket's own hashes from the return> · Head: <head sha>
<2-4 sentences: what landed, verification evidence paths>
Unverified: <the return's unverified claims, one per line, or "none">
```

## Guardrails

- A `claimed` ticket belongs to another session or a human. Report it, and leave its file, worktree, and branch as found.
- An `in-progress` ticket at the start of a run is a previous run that died. Report it and let the user decide; the user's word is what re-dispatches it. Its ticket branch and worktree stay as found.
- Retry a `failed` ticket only when the user says so. An answer to its `mismatch` question counts as that word.
- A blocker on a human (a sign-off, a partner response) is beyond an agent: leave dependents waiting and name who is being waited on.
