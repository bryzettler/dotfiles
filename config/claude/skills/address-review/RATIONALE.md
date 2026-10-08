# address-review: rationale

Why the skill is shaped this way. No step reads this file. Read it before you change a step.

## Models and cost budget

One scout, at most six verifiers, one revert-check verifier for the fix re-review, one `fixer` plus at most one follow-up, and a Delta review of two tracers and its triage. Verifiers run as `verifier` (opus, medium): the finding already narrows the read. The verifier wave is as slow as its largest group, and directory groups were uneven (3.9 minutes on wallet-app #1055, 6.2 on #1345), so groups are sized by count. The scout starts from `tools/collect.py`: the branch check, the fetch, and the thread rules need no judgement, and splitting a review body into items does. `fixer` runs on `opus`: it executes a fixed procedure. The scout runs as `scout` (sonnet, medium), for the reason in `review/RATIONALE.md`. Routing lives in `~/.claude/MODEL_ROUTING.md`.

## Reruns

A rerun is a delta: it collects only what arrived after the earlier run's marks. The rule lives in "Delta" in the Scout brief.

- Review bodies had no skip rule, and a body item lost its covering thread once that thread was answered, so a rerun collected both again. Hence the items line in the summary: the item key is exact, where "a later comment quoting it" had no checkable bound. A summary older than the items line names no keys, so the first rerun after it collects its body items once more.
- The run resolves each thread it replies to, so the PR shows only what is still open. The scout does not count on a reply to reopen a resolved thread: it collects any thread whose newest comment follows our reply. The cost is that a reviewer's "thanks" becomes a finding with an ANSWER verdict.
- A rerun after the user stopped at the gate finds the earlier commit ahead of the remote head. The verifiers check `git log <headRefOid>..HEAD` and return FIXED with its sha, so nothing is fixed twice.
- The new summary covers only the findings this run answered (step 6).

## Misses

A human finding that a `review` round could have seen is the cheapest lens test there is: the code was in front of the round, and it did not fire. The log records the class and the lens, never the instance, because a lens names a class. `review/RATIONALE.md` says where lens edits start. The log lives under `~/.claude/`, beside `review-state/`, because runs in any repo append to it.

## Fix rounds

Each review round on PR #1345 found defects in the previous round's fix commit, and the reviewer could not see our deferred tickets. Hence a blind Delta review on the fix diff before the commit (a delta round through `since`, skipped for a prose-only fix diff: see Fix rounds in `review/RATIONALE.md`), a Follow-ups section in the PR body for every defer, and a fix reply that names its pinning test and any part that remains.
