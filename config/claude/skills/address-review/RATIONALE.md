# address-review: rationale

Why the skill is shaped this way. No step reads this file. Read it before you change a step.

## Models and cost budget

One scout, at most three verifiers, one revert-check verifier for the fix re-review, one `fixer` plus at most one follow-up. Verifiers run as `verifier` (fable, medium): a verdict is a trace of consequences, and the finding already narrows the read. The scout and `fixer` run on `opus`: they execute a fixed procedure. Routing lives in `~/.claude/MODEL_ROUTING.md`.

## Reruns

A rerun needs no section of its own:

- The scout skips a thread whose newest comment carries `<!-- address-review -->`, and a PR comment already quoted by a later `<!-- address-review -->` comment (Scout brief).
- A reviewer reply after that comment reopens the thread: the newest comment is then the reviewer's, so the scout's finding rule collects it.
- A rerun after the user stopped at the gate finds the earlier commit ahead of the remote head. The verifiers check `git log <headRefOid>..HEAD` and return FIXED with its sha, so nothing is fixed twice.
- The new summary covers only the findings this run answered (step 6).
