# implement-tickets briefs

The sub-agent briefs for `implement-tickets`: scout, explorer, implementer, reviewer, plus the ticket grammar and the tier rubric. The main loop never reads this file: it names the path in each dispatch prompt, and the agent reads it. Everything the main loop acts on lives in `SKILL.md`.

## Return rule

Every agent writes its full output to the scratchpad path it was given and returns a final message in the shape its brief names, under the word cap, with nothing else: no preamble, no coverage narration, no quoted tool output. The main loop works from returns and paths; a return that exceeds its shape is context spent for nothing. The final message is the return itself. The main loop reads that message and nothing else, so never send the return by `SendMessage`, a handback, or another channel, and never end with a pointer such as "I sent the report" or with a placeholder.

## Ticket grammar

A ticket is `NN-slug.md` with a header the scout parses:

- `# NN — title`
- `**Type:**` free text (feature, bug, spec, chore).
- `**Status:**` one of `ready-for-agent`, `in-progress`, `done`, `failed`, `claimed`, `resolved`. A file with no parseable `Status:` is a planning or map ticket.
- `**Blocked by:**` ticket numbers. The list may wrap across lines: read to the end of the sentence. Prose like "must merge before X" is ordering advice, not a blocker.
- `**Tier:**` optional, `standard` or `deep` (legacy `opus` and `fable` mean the same), set by a human; it wins over the rubric.
- `**PR:**` optional, the name of the PR the ticket ships in, set by a human; it wins over the scout's grouping.
- Acceptance criteria as `- [ ]` checkboxes. They are the definition of done.
- Relative links (`../spec.md`, prototypes) resolve against the ticket's own directory.

State classes the scout assigns:

| Status | Class |
| --- | --- |
| none, `resolved` | skipped: not agent-actionable |
| `claimed` | skipped: owned elsewhere |
| `in-progress` | skipped: died mid-run, user decides |
| `done`, `failed` | skipped |
| `ready-for-agent`, every blocker `done`, `resolved`, or a non-actionable ticket whose output exists | **frontier** |
| `ready-for-agent`, any blocker `claimed`, `in-progress`, `failed`, or `ready-for-agent` | **waiting** |

## Tier rubric

The scout proposes one tier per candidate ticket. Standard is the default and needs no reason. Deep is proposed when any of these holds, and the manifest names which:

- **Value** — the change touches funds, authority, keys, an on-chain program or contract, a database migration, or a concurrency or scheduling path.
- **Open design** — an acceptance criterion needs a decision the spec does not make: a data model, a public interface, an algorithm choice.
- **Depth** — the ticket asks for a root-cause fix of a bug with no reproduction, or a performance change with a numeric target.
- **Pinned** — `**Tier:** deep` in the header. `**Tier:** standard` pins the other way and overrides every signal above.

A ticket that is many small mechanical edits, a cut across several modules or repos, a rename, a config change, a test backfill, or a straight port of a described function is standard even when it is long.

## Scout

`general-purpose`, opus. The prompt carries: the issues folder path, the `--only` list, the scratchpad path, and the path of this file.

> Read every `*.md` in the issues folder and parse each by the Ticket grammar in this file. Read the spec the tickets reference (usually `../spec.md`); its section structure names the target repo per ticket. Resolve repo names to absolute paths against, in order: explicit paths in the spec or ticket, the current directory, and the parent of the directory containing `.scratch`. Verify each path exists with `ls`. Classify every ticket by the State classes table, compute the topological order from `Blocked by:`, mark the frontier, and propose a tier per frontier and waiting ticket by the Tier rubric. Assign every candidate ticket a PR group: its `**PR:**` header, else the PR the spec or map puts it in (a "part 1", a PR list, a release split), else one group per repo. Record for each group which group it stacks on, if any. Apply `--only` as a filter on candidates, not on blockers. Write the full plan to `<scratchpad>/plan.md`: the table, the spec section numbers per ticket, the repo paths, and the PR groups. Recommend exploration only when two or more tickets would repeat the same codebase or external research, and name the topic. Return under 300 words: one row per file (number, title, state, repo paths, blockers, tier with a one-phrase reason, PR group, or the skip reason), the spec path, unresolved repo names, and the exploration recommendation.

## Explorer

`general-purpose`, opus. The prompt carries: the research topic, the spec path, the repo paths, the notes folder path, and the path of this file.

> Research the topic against the repos and the spec: the existing code paths, conventions, interfaces, and external facts an implementer would otherwise rediscover. Write markdown notes to the notes folder, one file per topic, each note leading with the file paths and symbols an implementer should open. The notes folder sits outside every repo; write nothing inside a repo. Return the list of note paths written and one line per note saying what it covers.

## Implementer

`implementer` agent for the standard tier, `implementer-deep` for the deep tier. The prompt carries: the ticket path, the spec path with section numbers, the notes folder when exploration ran, the repo path(s), the group branch and its worktree path, the scratchpad path for this ticket, and the path of this file. On an escalation it also carries the standard run's mismatch text.

> Read the ticket at the path given; its relative links resolve against its own directory. Read the spec sections named, the notes folder if given, and the target repo's `CLAUDE.md` before writing code. All work happens in the repo, and the tickets folder stays untouched. Work in the worktree given, on the group branch given; other tickets of the same PR group commit there before and after you, so create no branch or worktree of your own. Implement per your agent definition, with one change: skip its code-review step, since the orchestrator reviews the whole group when its tickets land. Commit locally, with messages that name the ticket number; the branch stays unpushed and no PR is opened. On `mismatch` or `blocked`, commit whatever you changed as `wip(NN): partial` before you return, so the worktree is clean. The acceptance criteria are the definition of done: run the repo's verification (tests, lint, typecheck) and check each criterion. Save every verification command's full output to the scratchpad path given, one file per command. Return under 250 words in this shape: `outcome:` one of `done` (every criterion verified or the unverified ones named with why), `mismatch` (the ticket or spec leaves a decision open, conflicts with the code, or contradicts the real system it describes: a dependency graph, an API, an on-chain account; state the question in one sentence, then two or three answers, one marked `recommended:` with a one-line reason drawn from the spec or the code, and stop without implementing any of them), or `blocked` (an environmental failure: name the tool, infrastructure, or dependency); then repo, branch, worktree path, commit hashes, the head sha, per verification command its pass or fail with the output file path and on failure the failing lines only, the acceptance criteria as met or unmet, and under `unverified:` every claim in the ticket or your commits that no command you ran proves, one line each with the reason. The reviewer closes each one, so a claim missing from that list ships unchecked.

## Reviewer

`general-purpose`, opus. The prompt carries: the repo path, the worktree path when there is one, the branch, the base ref, the follow-ups, the spec files (the group's ticket paths and the spec), the round number, from round 2 the sha the previous round reviewed and that round's report path, the scratchpad path, and the path of this file.

> The review skill lives at `~/.claude/skills/review/`. Read its `SKILL.md` and `review-core.md` and run it once in branch mode, from the worktree (else the repo with the branch checked out), with `<base>` set to the base ref given. Write the follow-ups to `<scratchpad>/followups-<round>.md` and name that file to the skill as its follow-ups file, and name the spec files to it. In round 1 the whole of `git diff <base>...HEAD` is in scope. From round 2, name the previous round's sha and report to the skill, so it runs a delta round: the diff since that sha is the target, and the full diff is context. When the skill's fixer has landed, commit its edits on the branch as `fix: review round <round>`. Write the report to `<scratchpad>/review-<branch>-<round>.md`. Return under 300 words: the sha the skill reviewed, the head sha after that commit, the base sha, the severity table, the confirmed count, counts fixed in code, fixed in the claim, and left as is, each spec question verbatim with its spec line, the confirmed finding in one sentence, and two answers: `keep` and `change: <the behaviour the finding calls for>`, one marked `recommended:` with a one-line reason drawn from the finding and the spec, open Value suspicions verbatim, the revert check as guards flipped, red, and green, whether the fixer changed logic or only formatting, comments, docs, and changesets, lint and test pass or fail per command with the failing lines when any, and the report path.
