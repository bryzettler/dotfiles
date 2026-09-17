---
name: implement-tickets
description: Use when the user asks to implement, work through, or orchestrate a folder of ticket/issue markdown files (e.g. a .scratch/*/issues directory), possibly spanning multiple repos.
---

# implement-tickets

Work a folder of ticket markdown files to done. Tickets are a **task graph**: `Blocked by:` edges define a **frontier** of tickets ready to run. One fresh-context implementer per ticket, routed to the cheapest tier the ticket allows. State lives in each ticket's `Status:` field, so a re-run after a usage-limit stop, crash, or `/clear` resumes where it left off.

The main loop's context is the scarce resource. Ticket bodies, spec text, verification output, and review reports go to sub-agents and scratchpad files and reach the main loop as paths and short returns. `briefs.md` in this folder holds every sub-agent brief, the ticket grammar, and the tier rubric; sub-agents read it by path and the main loop never opens it. The main loop's only writes are `Status:` edits and `## Agent result` sections in ticket files, and the only ticket files it edits are the ones it dispatched.

**Invocation:** `/implement-tickets [issues-folder] [--dry-run] [--only NN,NN] [--max N]`

Folder given: use it. None: glob `.scratch/*/issues` from the current directory. One match: use it and say which. Several: list them and ask. None: ask for the path.

## Steps

1. **Plan** — one `scout` (`general-purpose`, `model: "opus"`). Its prompt carries the issues folder path, the `--only` list, the scratchpad path, and the path of `briefs.md`. It parses every ticket, reads the spec, resolves and verifies repo paths, classifies each ticket, computes the topological order and the frontier, proposes a tier per ticket from the rubric, writes `<scratchpad>/plan.md`, and returns a manifest under 300 words: one row per ticket (number, title, state, repo paths, blockers, tier with a one-phrase reason), the spec path, repos it could not resolve, and whether exploration would save repeated research and on what. Done when every `*.md` in the folder has a row and every repo path is verified or listed as unresolved. Ask the user about unresolved repos. Print the table. `--dry-run` stops here.

2. **Explore** — only when the scout recommends it: one `explorer` (`general-purpose`, `model: "opus"`) per research topic. Its prompt carries the topic, the spec path, the repo paths, the notes folder `<issues-folder>/../notes/`, and the path of `briefs.md`. Done when it returns the list of note files it wrote.

3. **Execute** — the frontier loop. Tickets in different repos run concurrently; tickets in the same repo run one at a time in topological order, because they share files and their blockers encode real ordering. Each `done` recomputes the frontier and unlocks dependents within the same run. Per dispatched ticket:
   1. **Claim** — edit the ticket, `Status: ready-for-agent` → `Status: in-progress`.
   2. **Dispatch** one `implementer` at the ticket's tier: opus tier is `subagent_type: "implementer"` as is; fable tier adds `model: "fable"` (effort stays at the agent's medium). The prompt carries only pointers: the ticket path; the spec path plus the ticket's section numbers; the notes folder when exploration ran; the repo path(s); the base ref to branch from (the branch of the most recent `done` blocker in the same repo when one exists, else the repo's default branch); the scratchpad path `<scratchpad>/<NN>/` for verification output; and the path of `briefs.md`. Done when the return arrives in the shape of the Return rule in `briefs.md`.
   3. **Record** — by the return's outcome:
      - `done` → `Status: done`, tick only the acceptance-criteria boxes the return lists as verified, append the Agent result section below.
      - `mismatch` from an opus run (the ticket or spec leaves a design decision open, or the plan conflicts with the code) → escalate once: leave `in-progress`, re-dispatch at fable with the same pointers plus the opus return's mismatch text, and note the escalation in the Agent result. A fable `mismatch` → `Status: failed`; its question goes to the report for the user.
      - `blocked` (a missing tool, broken test infrastructure, a dependency that does not build) → `Status: failed`, no retry; its dependents become waiting.

      ```markdown
      ## Agent result (<date>)

      Repo: <path> · Branch: <name> · Worktree: <path> · Tier: <opus|fable, escalated?>
      <2-4 sentences: what landed, verification evidence paths, anything left unverified>
      ```

   4. Stop when the frontier is empty or `--max` is reached.

4. **Review** — skip when `--max` stopped early or tickets are still waiting, and say so. Per repo that received work, one `reviewer` (`general-purpose`, `model: "fable"`), all repos concurrently. Its prompt carries the repo path, each branch to review with its base ref (the tip of each stacked chain against the chain's fork point on the default branch, and each independent branch against its own fork point), the scratchpad path, and the path of `briefs.md`. It runs the review skill in branch mode with fixes applied. Done when its return holds the severity table and per-command lint and test results for every branch. Then remove implementer worktrees (`git worktree remove`) and keep the branches.

5. **Report** — a table: ticket · outcome · tier · repo · branch · one-line note. Then waiting tickets with what unblocks each, escalations, the reviewer's severity tables and lint and test results per repo, and the reminder that branches are unpushed and the same command resumes safely.

## Tiers

Opus is the default: the `implementer` agent as defined. Fable is for tickets the rubric in `briefs.md` marks as significant: cross-module or cross-repo cuts, money, authority, on-chain, migration, or concurrency paths, and design decisions the spec leaves to the implementer. A human pins a tier with `**Tier:** fable` or `**Tier:** opus` in the ticket header, and that wins over the rubric. Escalation is the only main-loop override, and only upward.

## Cost budget

One scout, explorers only on the scout's recommendation, one implementer per ticket plus at most one fable escalation, one reviewer per repo. Every agent runs on opus except fable-tier implementers and the reviewer. The implementer's own code-review step is skipped; the reviewer covers each branch once. Nothing is pushed and no PR is opened.

## Guardrails

- A `claimed` ticket belongs to another session or a human. Report it, and leave its file, worktree, and branch as found.
- An `in-progress` ticket at the start of a run is a previous run that died. Report it and let the user decide; the user's word is what re-dispatches it.
- A `failed` ticket is retried only when the user says so.
- A blocker on a human (a sign-off, a partner response) is not agent-satisfiable: leave dependents waiting and name who is being waited on.
