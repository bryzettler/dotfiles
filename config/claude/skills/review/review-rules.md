# Review rules

Rules that `review` and `address-review` both apply. Read the section a step names.

## Revert check

One verifier (`general-purpose`, `model: "opus"`) each time a step calls for it. Its targets, up to 40, are those a unit test file can check in under a minute, in this order:

1. Every guard the diff adds: a `require!`, a `revert`, a thrown error, an early return, a validation branch.
2. Every changed call site whose data the fix depends on: a return value or an argument replaced by `[]`, `null`, or `0`.
3. Every predicate, regex, or filter term the diff adds or narrows (`&& k.isSigner`, a `match` arm, a pattern): drop the term, or widen it to always true. Every row of a lookup table or map the diff adds or changes: mutate it once to a narrower value and once to a wider one.
4. Every exported signature the diff changes: restore the base form (`async` back to sync, a removed export restored, a parameter dropped), then run the tests that call it. A test that stays green under the restore pins nothing about the change.
5. Every test that a fixer item, a report, or a reply in this or a prior round names as the proof of a fix: revert that fix and run the named test. In a delta round, only this round's fixer items count.
6. The revert-check targets the Defects report lists.

One mutant per guard, term, or call site, and a second only when the first stays green; a table row gets both of its mutants. In a delta round, skip a target the previous round's report lists as red, unless the delta changed its line or its test; name the skipped count in the coverage line.

It works in a throwaway worktree at a snapshot of the checkout as it is when the check starts (`python3 -I ~/.claude/skills/review/tools/snapshot.py`, run in the checkout: the head plus every uncommitted edit, a fixer's included), with `node_modules` symlinked from the main checkout for the root and for every workspace package that has one. Per target: apply the mutant, run that one test file, record red or green, restore. With more than 4 targets, split them across up to 6 worktrees of the same snapshot and run the worktrees at the same time, each one working through its own targets in turn. When the whole check would still pass 5 minutes, add worktrees before dropping targets.

It returns one line per target: `file:line`, the mutant, the test file, red or green, and the output path. A target that stays green is a confirmed Pinned finding, with the missing test as its fix. A test file no workflow runs is green for this check, whatever it does locally: name the workflow file and line whose command runs it, or report "no job". Targets past 40 go to the report's coverage line as not mutated.

## Fix re-review

Run the fix, then read it.

1. **Run** — the revert check above, on the fixer's diff: every guard and call site the fix adds or changes. A fix that touches a migration, a lock, a transaction boundary, or a cursor also runs against a disposable instance of the real backend, started with the backend command, with rows in the table or accounts on the chain, on the concurrent or failing path the finding named. When the backend command is "none" or the instance does not start in five minutes, mark the item `unproven` with the backend named. Every report and reply about that item says so.
2. **Read** — the fixer's diff against the six lenses a fix most often trips: **Safe side**, **Failure mode**, **Neighbours**, **Pinned**, **Legit path**, **Fix chain**. Fix chain asks whether this fix patches a hazard an earlier fix on the branch made; when it does, the finding is the earlier design choice, and the resolution is **Redesign**.
3. **Claims** — every text that names a symbol, flag, or behaviour the fix touches: `grep -rn '<symbol>'` across `.changeset/`, doc comments, READMEs, and the PR body. Read each hit sentence against the new code. A sentence the fix made false is a **claim** item for the follow-up `fixer` run (the PR body goes to `<scratchpad>/pr-body.md`).
4. **Remainder** — per finding, compare the fix with the finding's whole scenario, not only its anchor line. Name each part the fix does not close (an export left in place, a sibling left unguarded, a test left unwired). That part goes to the follow-up `fixer` run, or the item is a partial fix and its reply names the part that remains.

Done when every fixer edit is red under its mutants in a test a workflow runs, is proven against the backend or marked `unproven`, passes the six lenses, has its claims swept and its remainder named, or one follow-up `fixer` run has landed the corrections.
