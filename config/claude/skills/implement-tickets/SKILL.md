---
name: implement-tickets
description: Use when the user asks to implement, work through, or orchestrate a folder of ticket/issue markdown files (e.g. a .scratch/*/issues directory), possibly spanning multiple repos.
---

# implement-tickets

Work a folder of ticket markdown files to done. Tickets are a **task graph**: `Blocked by:` edges define a **frontier** of tickets ready to run. One fresh-context implementer per ticket, routed to the cheapest tier the ticket allows. State lives in each ticket's `Status:` field, so a re-run after a usage-limit stop, crash, or `/clear` resumes where it left off.

The main loop's context is the scarce resource. Ticket bodies, spec text, verification output, and review reports go to sub-agents and scratchpad files and reach the main loop as paths and short returns. `briefs.md` in this folder holds every sub-agent brief, the ticket grammar, and the tier rubric; sub-agents read it by path. The main loop's only writes are `Status:` edits and `## Agent result` sections in ticket files it dispatched, `spec-questions.md`, and the follow-up tickets that step 0 creates.

**Invocation:** `/implement-tickets [issues-folder] [--dry-run] [--only NN,NN] [--max N] [--answers]`

Folder given: use it. None: glob `.scratch/*/issues` from the current directory. One match: use it and say which. Several: list them and ask. None: ask for the path.

## Steps

0. **Answers** — only with `--answers`. Read `<issues-folder>/../spec-questions.md`, the file step 5 writes. Each entry has a `Ticket:`, a `PR group:`, the question and its spec line, and an `Answer:` line that the user fills in:
   - `keep` → append `Spec answer: keep — <question>` to that ticket's Agent result, so later reviews read the decision as spec.
   - `change: <behaviour>` → write a new ticket at the next free number, `NN-spec-<slug>.md`: `**Type:** fix`, `**Status:** ready-for-agent`, `**PR:** <group>`, `**Blocked by:**` the ticket at the group's tip, and a body that quotes the question, the spec line, and the answer. Its one acceptance criterion is the behaviour the answer names, pinned by a test.
   - An empty answer → leave the entry as it is.

   Under every entry acted on, add `Applied: <date> → <ticket>`. An entry that already has an `Applied:` line is skipped, so a rerun applies each answer once. Done when every answered entry has an `Applied:` line. Then continue at step 1: the new tickets are on the frontier, and every other ticket resumes by its `Status:`.

1. **Plan** — one `scout` (`general-purpose`, `model: "opus"`). Its prompt carries the issues folder path, the `--only` list, the scratchpad path, and the path of `briefs.md`. It parses every ticket, reads the spec, resolves and verifies repo paths, classifies each ticket, computes the topological order and the frontier, proposes a tier per ticket from the rubric, assigns each ticket its PR group, writes `<scratchpad>/plan.md`, and returns a manifest under 300 words: one row per ticket (number, title, state, repo paths, blockers, tier with a one-phrase reason, PR group), the spec path, repos it could not resolve, and whether exploration would save repeated research and on what. Done when every `*.md` in the folder has a row and every repo path is verified or listed as unresolved. Ask the user about unresolved repos. Print the table. `--dry-run` stops here.

2. **Explore** — only when the scout recommends it: one `explorer` (`general-purpose`, `model: "opus"`) per research topic. Its prompt carries the topic, the spec path, the repo paths, the notes folder `<issues-folder>/../notes/`, and the path of `briefs.md`. Done when it returns the list of note files it wrote.

3. **Execute** — the frontier loop. Each PR group has one branch, named after the group, in one worktree. Each ticket lands as one or more commits on its group's branch, and no ticket is reviewed on its own: step 4 reviews each group's tickets together, which also finds the bugs that sit between tickets. Before a group's first ticket, create its branch and worktree from the group's base (the tip of the group it stacks on, else the repo's default branch) with `git worktree add -b <group> <path> <base>`, at the path the repo's conventions name, else `<repo>-<group>` beside the repo. On a rerun, `git worktree list` finds it. Tickets in different repos run concurrently; tickets in the same repo run one at a time in topological order, because they share files and their blockers encode real ordering. A ticket holds its repo's slot until its return is recorded. Each `done` recomputes the frontier and unlocks dependents within the same run. Per dispatched ticket:
   1. **Claim** — edit the ticket, `Status: ready-for-agent` → `Status: in-progress`, and note the group branch's head sha as the ticket's start sha.
   2. **Dispatch** one `implementer` at the ticket's tier: standard tier is `subagent_type: "implementer"` (effort medium); deep tier is `subagent_type: "implementer-deep"` (effort high). Both run on opus. The prompt carries only pointers: the ticket path; the spec path plus the ticket's section numbers; the notes folder when exploration ran; the repo path(s); the group branch and its worktree path; the scratchpad path `<scratchpad>/<NN>/` for verification output; and the path of `briefs.md`. Done when the return arrives in the shape of the Return rule in `briefs.md`.
   3. **Record** — by the return's outcome:
      - `done` → `Status: done`, tick only the acceptance-criteria boxes the return lists as verified, append the Agent result section below.
      - `mismatch` from a standard run (the ticket or spec leaves a design decision open, the plan conflicts with the code, or the spec contradicts the real system) → escalate once: leave `in-progress`, re-dispatch at deep with the same pointers plus the standard return's mismatch text, and note the escalation in the Agent result. A deep `mismatch` → `Status: failed`; its question goes to the report for the user.
      - `blocked` (a missing tool, broken test infrastructure, a dependency that does not build) → `Status: failed`, no retry; its dependents become waiting.

      A `mismatch` or `blocked` return leaves its partial work committed as `wip(NN): partial`. Before the next dispatch on the group branch, the escalation included, keep that work and put the branch back where the ticket found it: in the group worktree, `git branch NN-slug-partial` and then `git switch -C <group> <start sha>`. Name the side branch in the Agent result.

      ```markdown
      ## Agent result (<date>)

      Repo: <path> · Branch: <group branch> · Worktree: <path> · Tier: <standard|deep, escalated?>
      Commits: <start sha>..<head sha>
      <2-4 sentences: what landed, verification evidence paths>
      Unverified: <the return's unverified claims, one per line, or "none">
      ```

   4. Stop when the frontier is empty or `--max` is reached.

4. **Harden** — never skipped: it reviews what landed, and a waiting, failed, or unrun ticket is named in the report, not a reason to stop. Per PR group, one `reviewer` round at a time on the group's tip against the base its PR will target: the previous group's tip when groups stack, else the fork point on the default branch. Groups in different repos run concurrently. Groups in one stack run bottom up, and before a stacked group's first round, merge the lower group's final tip into its branch, so the lower group's review fixes reach it.
   - Round 1 is a full review of the group's diff. Its follow-ups are the `Unverified:` lines of the group's `done` tickets, each tagged with its ticket number, and its spec files are the group's ticket paths plus the spec.
   - Rounds 2 and 3 are delta rounds: the prompt also carries the sha the previous round reviewed and that round's report path, so the round reviews the previous round's fixes with the full diff as context.
   - Run another round only when the last one confirmed a finding of medium or higher severity, or its fixer changed logic (anything beyond formatting, comments, docs, and changesets). Stop after three rounds per group.

   Append `Hardened: <tip sha> · <rounds> rounds · <clean|cap> (<report path>)` to the Agent result of the group's tip ticket, and carry each spec question to the report. A group whose tip sha already has a `Hardened:` line is skipped, so a rerun hardens only the groups that changed. A failing lint or test run in a round's return leaves the group with no `Hardened:` line, and the report leads with it. Done when every `done` ticket's head sha is an ancestor of a tip with a `Hardened:` line (`git merge-base --is-ancestor`), and each group's last round either met neither condition for another round or is the third. Then remove the group worktrees (`git worktree remove`) and keep the branches.

5. **Report** — append each new spec question to `<issues-folder>/../spec-questions.md` as an entry with `Ticket:`, `PR group:`, the question verbatim, its spec line, and an empty `Answer:`, and add a line at the top of the file saying how to answer: `keep`, or `change: <behaviour>`, then rerun with `--answers`. A question already in the file is not added again. Lead the report with those questions and the file's path. Then a table: ticket · outcome · tier · repo · branch · head sha · one-line note. Then per PR group: rounds run, each round's confirmed count and whether it was full or delta, the last round's severity table, open suspicions, and lint and test results, and whether it ended clean or at the cap. Then waiting tickets with what unblocks each, escalations, and the reminder that branches are unpushed and the same command resumes safely.

## Tiers

Standard is the default: the `implementer` agent. Deep is the `implementer-deep` agent, the same contract at effort high, for tickets the rubric in `briefs.md` marks as significant: money, authority, on-chain, migration, or concurrency paths, and design decisions the spec leaves to the implementer. A human pins a tier with `**Tier:** deep` or `**Tier:** standard` in the ticket header (`fable` and `opus` are read as `deep` and `standard`), and that wins over the rubric. Escalation is the only main-loop override, and only upward.

## Cost budget

One scout, explorers only on the scout's recommendation, one implementer per ticket plus at most one deep escalation, and one to three reviewer rounds per PR group, rounds 2 and 3 as delta rounds. No ticket gets a review of its own. Each reviewer runs the whole review skill, so review is the larger share of a run's agents; that is the price of a branch that lands clean. Every agent runs on opus. The implementer's own code-review step is skipped, since the group review replaces it. Nothing is pushed and no PR is opened.

## Guardrails

- A `claimed` ticket belongs to another session or a human. Report it, and leave its file, worktree, and branch as found.
- An `in-progress` ticket at the start of a run is a previous run that died. Report it and let the user decide; the user's word is what re-dispatches it.
- A `failed` ticket is retried only when the user says so.
- A blocker on a human (a sign-off, a partner response) is not agent-satisfiable: leave dependents waiting and name who is being waited on.
