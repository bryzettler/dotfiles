# Review: delta round

A delta round reviews the change since `<last>`, not the whole branch or PR. The review diff is `git diff <last> <head>`, with `<head>` from the manifest (branch mode: the scout's snapshot, so uncommitted changes count), and the fixed point for the axes is `<last>`. Name the full diff (`git diff <base>...<head>`, or the full-PR diff) in every prompt as context, never as the target.

## Pin (the scout)

**Branch.** The caller names `<last>` and the previous round's report. When the caller also names the previous round's manifest, start from it: the spec, the standards sources, the test commands, and the domains of unchanged files carry forward. Derive only the delta's line count and file list, the domains of files new to the delta, the tooling on the delta, and `prior.md`. Append the previous round's fixes, open suspicions, and dismissals to `<scratchpad>/prior.md`.

**PR.** Write `<scratchpad>/prior.md`:

- one entry per prior inline comment: path, line, the first line of the body, the author's reply if any, and whether the delta touches that file
- the carried suspicions and dismissals from the state file
- whether the PR body changed since `<last>`, against the body recorded in the state file

The manifest adds `<last>`, the delta line count, the `prior.md` path, and, in PR mode, whether the PR body changed.

## Review (step 2)

Every prompt carries the `prior.md` path and says the target is the delta diff. Name each skipped axis in the report.

**Branch.** Dispatch Defects and Value only: the delta is the previous round's fixes, and Standards and Spec covered the branch in round 1. Defects confirms each prior fix does what the report says, and works the Neighbours lens on it: a sibling call, guard, or path the fix left unchanged is a hit.

**PR.** Defects and Value always run. Standards runs when the delta touches a file outside the test tree. Spec runs when the PR body changed or the delta touches a file outside the test tree. Defects also owns the follow-up check in `prior.md`: for each prior comment the author replied to, confirm the delta does what the reply says. A reply the code does not back is a hit.

## Triage (step 3)

Re-verify a carried suspicion only when the delta touches a file it names. Copy the others forward unchanged.

## Post (PR step 8)

Also drop any entry that restates a prior comment the author's reply resolved, per `prior.md`.
