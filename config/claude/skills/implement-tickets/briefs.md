# implement-tickets briefs

Find your section by the role your prompt names. Return per `~/.claude/skills/review/return-rule.md`, under your word cap. Write your full output to the scratchpad path you were given; the return carries the shape below.

## Scout

1. Read every `*.md` in the issues folder and parse each per "Ticket grammar" in `~/.claude/skills/implement-tickets/tickets.md`.
2. Read the spec the tickets reference (usually `../spec.md`); its section structure names the target repo per ticket.
3. Resolve repo names to absolute paths against, in order: explicit paths in the spec or ticket, the current directory, and the parent of the directory containing `.scratch`. Verify each path exists with `ls`.
4. Classify every ticket by the State classes table in `tickets.md`, compute the topological order from `Blocked by:`, mark the frontier, and propose a tier per frontier and waiting ticket by its Tier rubric.
5. Assign every candidate ticket a PR group by the PR groups and Branch names rules below: its `**PR:**` header, else one group per repo. A spec or map that splits the work into more PRs (a "part 1", a PR list, one PR per issue) counts only when it gives a split reason those rules accept. When a `**PR:**` header breaks the Branch names rule, keep the header and flag the name in the manifest. Record for each group which group it stacks on, if any.
6. Apply `--only` as a filter on candidates, not on blockers.
7. Write the full plan to `<scratchpad>/plan.md`: the table, the spec section numbers per ticket, the repo paths, and the PR groups.
8. Recommend exploration only when two or more tickets would repeat the same codebase or external research, and name the topic.

Return under 300 words: one row per file (number, title, state, repo paths, blockers, tier with a one-phrase reason, PR group, or the skip reason), the spec path, unresolved repo names, and the exploration recommendation.

### PR groups

Fewer PRs are better. One PR per ticket is wrong, and so is one PR per spec issue. Put tickets in one group when they are in the same repo and ship or deploy together. Split a group only for one of these reasons: the tickets are in different repos; a spec gives a reason that the parts must roll out apart (not only "one PR each"); or the diff is too large for one review. Name the reason for each split in the plan.

### Branch names

A group's name is its branch name, and a `**PR:**` header name follows this rule. Use the repo's convention: read `git branch -r` and the repo's `CLAUDE.md`. With no convention, use `<type>/<slug>`: the type is `feat`, `fix`, or `chore`, and the slug says what the change does in 3–6 words (`fix/sink-refresh-stall-publisher-cursor-bound`). Keep ticket numbers, `pr-NN`, and sequence numbers out of the name: they tell a reviewer nothing and go stale when groups merge.

## Explorer

Research the topic against the repos and the spec: the existing code paths, conventions, interfaces, and external facts an implementer would otherwise rediscover. Write markdown notes to the notes folder, one file per topic, each note leading with the file paths and symbols an implementer should open. When the topic has more than one design (where a write lands, one transaction or two, which layer holds a guard), write each option with the failure paths it opens and what repairs each one, and leave the choice to the implementer and the spec. Write only in the notes folder, which sits outside every repo.

Return the list of note paths written and one line per note saying what it covers.

## Implementer

**Before writing code:** read the ticket at the path given (its relative links resolve against its own directory), the spec sections named, the notes folder if given, and the target repo's `CLAUDE.md`.

**Where:** all work happens in the repo; leave the tickets folder untouched. Work in the worktree given, on the ticket branch given, which was cut from the group branch's tip; create no branch or worktree of your own. Commit locally, with messages that name the ticket number; the branch stays unpushed and no PR is opened.

**How:** implement per your agent definition, with one change: skip its code-review step, since the orchestrator reviews the whole group when its tickets land.

- The acceptance criteria are the definition of done: run the repo's verification (tests, lint, typecheck) and check each criterion.
- A test's expected value comes from the spec or from an independent calculation, never from a run of the code under test. Reach green with every assertion at full strength: never loosen an assertion or turn a throw into a skip.
- A new test file runs in CI: add it where its siblings are wired, such as a hand-written matrix.
- A test fixture is a state the setup path produces.
- An export of a published package keeps its signature (sync stays sync), or the changeset bumps major.
- A quote, estimate, or funding helper gets a test across the entity's whole life: create, re-apply with unchanged input, change one input, each checked against what the chain or backend consumes.
- Read a runtime fact (an account size, a rent value, an IDL field name, a program id) from the artifact or the chain, never from memory.
- A failing check is a defect in your change until its output shows otherwise. Calling it a flake needs the same check failing on the group branch.
- Save every verification command's full output to the scratchpad path given, one file per command.

**Budget:** about 120 tool calls. Past it with criteria still unmet, commit `wip(NN): partial` and return `blocked` with `budget` as the reason and the criteria met and unmet, so the user decides instead of the run drifting.

**Before you return `done`:** other tickets of the PR group land on the group branch while you work. `git merge <group>` into your branch, resolve any conflict (the `mattpocock-skills:resolving-merge-conflicts` skill), rerun verification, and commit. The orchestrator fast-forwards the group branch to your head.

**Before you return `mismatch` or `blocked`:** commit whatever you changed as `wip(NN): partial`, so the worktree is clean.

Return under 250 words in this shape:

- `outcome:` one of:
  - `done` — every criterion verified, or the unverified ones named with why.
  - `mismatch` — the ticket or spec leaves a decision open, conflicts with the code, or contradicts the real system it describes (a dependency graph, an API, an on-chain account). State the question in one sentence, then two or three answers, one marked `recommended:` with a one-line reason drawn from the spec or the code, and stop without implementing any of them.
  - `blocked` — an environmental failure: name the tool, infrastructure, or dependency.
- repo, branch, worktree path, commit hashes, the group sha you merged, the head sha
- per verification command: pass or fail, the output file path, and on failure the failing lines only
- the acceptance criteria as met or unmet
- `unverified:` every claim in the ticket or your commits that no command you ran proves, one line each with the reason. The reviewer closes each one, so a claim missing from this list ships unchecked.

## Reviewer

**Run the review skill.** It lives at `~/.claude/skills/review/`. Read its `SKILL.md` and follow its branch-mode pointers (`triage.md`, `verdicts.md`, `review-rules.md`, `delta-round.md`). The `scout.md`, `brief-*.md`, `domain-*.md`, `return-rule.md`, and `pr-mode.md` files are for its sub-agents and PR mode. Run it once in branch mode, from the worktree (else the repo with the branch checked out), with `<base>` set to the base ref given.

- Write the follow-ups to `<scratchpad>/followups-<round>.md` and name that file to the skill as its follow-ups file. Name the spec files to it.
- **Full round:** the whole of `git diff <base>...HEAD` is in scope.
- **Delta round:** name the previous round's sha, report, and scout manifest to the skill, so it runs a delta round. Leave out a report or manifest path that no longer exists; the round still runs as a delta.
- When the skill's fixer has landed, commit its edits on the branch as `fix: review round <round>`, then run the repo's full lint and test suite on that head, one output file per command in the scratchpad. A suite run before the fix does not count.
- Write the report to `<scratchpad>/review-<branch>-<round>.md`.

Return under 300 words:

1. The sha the skill reviewed, the head sha after that commit, the base sha.
2. The severity table, the confirmed count, and the counts fixed in code, fixed in the claim, and left as is.
3. Each spec question verbatim with its spec line, the confirmed finding in one sentence, its **blast radius** (every job, table, and consumer the finding reaches, beyond the one the spec names), and two answers: `keep` and `change: <the outcome the finding calls for>`, one marked `recommended:` with a one-line reason drawn from the finding's blast radius and the spec. A `change:` answer names the outcome ("a stuck sink pages someone within an hour", "no row keeps a stale stamp") and leaves the mechanism to the implementer. A spec non-goal ("no new alert") backs `keep` only when the spec's authors weighed this blast radius.
4. Each follow-up from the report verbatim, tagged with its severity and one kind: `live-check` (the claim was never run live, in staging, or in production), `pr-note` (a line the PR body or a README owes a reviewer), or `code`. A `code` follow-up of medium or higher severity also carries its blast radius and two answers, `fix: <the outcome the finding calls for>` and `wontfix: <reason>`, one marked `recommended:` with a one-line reason.
5. Open Value suspicions verbatim.
6. The revert check as targets mutated, red, and green.
7. Lint and test pass or fail per command on the post-fix head, with the failing lines when any.
8. The report path.
