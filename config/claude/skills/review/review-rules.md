# Review rules

Rules that `review` and `address-review` both apply. Read the section a step names.

## Revert check

One verifier (`general-purpose`, `model: "opus"`) each time a step calls for it. Its targets, up to 40, are those a unit test file can check in under a minute, in this order:

1. Every guard the diff adds: a `require!`, a `revert`, a thrown error, an early return, a validation branch.
2. Every changed call site whose data the fix depends on: a return value or an argument replaced by `[]`, `null`, or `0`.
3. The revert-check targets the Defects report lists.

It works in a throwaway worktree at the reviewed head, with `node_modules` symlinked from the main checkout for the root and for every workspace package that has one. In branch mode it applies the uncommitted changes. Per target: apply the mutant, run that one test file, record red or green, restore.

It returns one line per target: `file:line`, the mutant, the test file, red or green, and the output path. A target that stays green is a confirmed Pinned finding, with the missing test as its fix. Targets past 40 go to the report's coverage line as not mutated.

## Fix re-review

Run the fix, then read it.

1. **Run** — the revert check above, on the fixer's diff: every guard and call site the fix adds or changes. A fix that touches a migration, a lock, a transaction boundary, or a cursor also runs against a disposable instance of the real backend, started with the backend command, with rows in the table or accounts on the chain, on the concurrent or failing path the finding named. When the backend command is "none" or the instance does not start in five minutes, mark the item `unproven` with the backend named. Every report and reply about that item says so.
2. **Read** — the fixer's diff against the six lenses a fix most often trips: **Safe side**, **Failure mode**, **Neighbours**, **Pinned**, **Legit path**, **Fix chain**. Fix chain asks whether this fix patches a hazard an earlier fix on the branch made; when it does, the finding is the earlier design choice, and the resolution is **Redesign**.

Done when every fixer edit is red under its mutants, is proven against the backend or marked `unproven`, and passes the six lenses, or one follow-up `fixer` run has landed the corrections.
