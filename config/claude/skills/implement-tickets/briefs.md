# implement-tickets briefs

The sub-agent briefs for `implement-tickets`: scout, explorer, implementer, reviewer, plus the ticket grammar and the tier rubric. The main loop never reads this file: it names the path in each dispatch prompt, and the agent reads it. Everything the main loop acts on lives in `SKILL.md`.

## Return rule

Every agent writes its full output to the scratchpad path it was given and returns a final message in the shape its brief names, under the word cap, with nothing else: no preamble, no coverage narration, no quoted tool output. The main loop works from returns and paths; a return that exceeds its shape is context spent for nothing.

## Ticket grammar

A ticket is `NN-slug.md` with a header the scout parses:

- `# NN — title`
- `**Type:**` free text (feature, bug, spec, chore).
- `**Status:**` one of `ready-for-agent`, `in-progress`, `done`, `failed`, `claimed`, `resolved`. A file with no parseable `Status:` is a planning or map ticket.
- `**Blocked by:**` ticket numbers. The list may wrap across lines: read to the end of the sentence. Prose like "must merge before X" is ordering advice, not a blocker.
- `**Tier:**` optional, `opus` or `fable`, set by a human; it wins over the rubric.
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

The scout proposes one tier per candidate ticket. Opus is the default and needs no reason. Fable is proposed when any of these holds, and the manifest names which:

- **Cut** — the ticket edits three or more modules or packages, or two repos.
- **Value** — the change touches funds, authority, keys, an on-chain program or contract, a database migration, or a concurrency or scheduling path.
- **Open design** — an acceptance criterion needs a decision the spec does not make: a data model, a public interface, an algorithm choice.
- **Depth** — the ticket asks for a root-cause fix of a bug with no reproduction, or a performance change with a numeric target.
- **Pinned** — `**Tier:** fable` in the header. `**Tier:** opus` pins the other way and overrides every signal above.

A ticket that is many small mechanical edits, a rename, a config change, a test backfill, or a straight port of a described function is opus even when it is long.

## Scout

`general-purpose`, opus. The prompt carries: the issues folder path, the `--only` list, the scratchpad path, and the path of this file.

> Read every `*.md` in the issues folder and parse each by the Ticket grammar in this file. Read the spec the tickets reference (usually `../spec.md`); its section structure names the target repo per ticket. Resolve repo names to absolute paths against, in order: explicit paths in the spec or ticket, the current directory, and the parent of the directory containing `.scratch`. Verify each path exists with `ls`. Classify every ticket by the State classes table, compute the topological order from `Blocked by:`, mark the frontier, and propose a tier per frontier and waiting ticket by the Tier rubric. Apply `--only` as a filter on candidates, not on blockers. Write the full plan to `<scratchpad>/plan.md`: the table, the spec section numbers per ticket, and the repo paths. Recommend exploration only when two or more tickets would repeat the same codebase or external research, and name the topic. Return under 300 words: one row per file (number, title, state, repo paths, blockers, tier with a one-phrase reason, or the skip reason), the spec path, unresolved repo names, and the exploration recommendation.

## Explorer

`general-purpose`, opus. The prompt carries: the research topic, the spec path, the repo paths, the notes folder path, and the path of this file.

> Research the topic against the repos and the spec: the existing code paths, conventions, interfaces, and external facts an implementer would otherwise rediscover. Write markdown notes to the notes folder, one file per topic, each note leading with the file paths and symbols an implementer should open. The notes folder sits outside every repo; write nothing inside a repo. Return the list of note paths written and one line per note saying what it covers.

## Implementer

`implementer` agent, opus by default, `model: "fable"` for the fable tier. The prompt carries: the ticket path, the spec path with section numbers, the notes folder when exploration ran, the repo path(s), the base ref, the scratchpad path for this ticket, and the path of this file. On an escalation it also carries the opus run's mismatch text.

> Read the ticket at the path given; its relative links resolve against its own directory. Read the spec sections named, the notes folder if given, and the target repo's `CLAUDE.md` before writing code. All work happens in the repo, and the tickets folder stays untouched. Branch from the base ref given, named after the ticket (`NN-slug`), in a worktree when the repo's conventions say so. Implement per your agent definition, with one change: skip its code-review step, since the orchestrator reviews the branch after every ticket in the repo lands. Commit locally; the branch stays unpushed and no PR is opened. The acceptance criteria are the definition of done: run the repo's verification (tests, lint, typecheck) and check each criterion. Save every verification command's full output to the scratchpad path given, one file per command. Return under 250 words in this shape: `outcome:` one of `done` (every criterion verified or the unverified ones named with why), `mismatch` (the ticket or spec leaves a decision open or conflicts with the code; state the question in one sentence and stop without guessing), or `blocked` (an environmental failure: name the tool, infrastructure, or dependency); then repo, branch, worktree path, commit hashes, per verification command its pass or fail with the output file path and on failure the failing lines only, the acceptance criteria as met or unmet, and anything left unverified with the reason.

## Reviewer

`general-purpose`, fable. The prompt carries: the repo path, each branch to review with its base ref, the scratchpad path, and the path of this file.

> The review skill lives at `~/.claude/skills/review/`. Read its `SKILL.md` and `review-core.md`, then run it in branch mode once per branch given, from the repo path, with `<base>` set to that branch's base ref and the branch checked out. Fixes are applied and committed to the reviewed branch as the skill's branch mode does. Write each branch's report to `<scratchpad>/review-<branch>.md`. Return under 300 words per branch: the severity table, counts fixed in code, fixed in the claim, and left as is, open Value suspicions verbatim, lint and test pass or fail per command with the failing lines when any, and the report path.
