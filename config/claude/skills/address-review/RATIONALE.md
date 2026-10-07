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

## Misses

A human finding that a `review` round could have seen is the cheapest lens test there is: the code was in front of the round, and it did not fire. The log records the class and the lens, never the instance, because a lens names a class. `review/RATIONALE.md` says where lens edits start. The log lives under `~/.claude/`, beside `review-state/`, because runs in any repo append to it.

## Fix rounds

Each review round on PR #1345 found defects in the previous round's fix commit, and the reviewer could not see our deferred tickets. Hence a blind Delta review on the fix diff before the commit, a Follow-ups section in the PR body for every defer, and a fix reply that names its pinning test and any part that remains.
