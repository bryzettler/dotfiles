---
name: ci-fix
description: Watch a PR's CI checks until they settle, fix failures the PR caused, push, and watch again until green. Use when the user says CI or ci/cd is red, failing, or broken on a PR, asks to fix or watch CI, or pastes a failing Actions run link. Also the last step of review (own PR) and address-review after they push.
---

# ci-fix

**Invocation:** `/ci-fix [<n> | #<n> | <PR URL> | <run URL>]`. No argument: the PR open for the current branch. A run URL: the PR whose head that run tested.

Keep the main loop's context for decisions. Logs go to scratchpad files. Read only their tails.

## Steps

1. **Pin** — `gh pr view <n> --json number,headRefName,headRefOid,baseRefName,url`. The checkout must be on `headRefName` with no tracked changes, and `HEAD` must equal `headRefOid`. When it is not, stop and report what differs. Do not switch branches.

2. **Watch** — run `gh pr checks <n> --watch --interval 30` with `run_in_background`. Wait for its completion notification. Never poll with sleep. Then `gh pr checks <n> --json name,state,bucket,link,workflow`.
   - Every check passed or skipped: report and stop.
   - No checks: report "no CI" and stop.

3. **Diagnose** — per failed check, take the run id from its link and save `gh run view <run id> --log-failed` to `<scratchpad>/ci-<check>-<round>.log`. Read the last 80 lines. Classify the failure:

   | Class   | Signs                                                                                                                                                         | Action          |
   | ------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------- |
   | `code`  | lint, type, build, or test errors in files or behaviour this PR changed                                                                                        | step 4          |
   | `infra` | network, package mirror, runner, rate limit, timeout, cancelled, secret missing; or `gh run list --branch <base> --workflow <workflow> -L 3` shows the same failure on the base | rerun once      |
   | `base`  | a real code failure that also fails on the base branch                                                                                                         | report, no fix  |

   For `infra`, run `gh run rerun <run id> --failed` once per check and go to step 2. A second `infra` failure on the same check ends the run: report it with the log tail.

4. **Fix** — reproduce the failing command locally first. Take it from the workflow file (`.github/workflows/`) and the repo's scripts. Then make the smallest edit that fixes the cause. For more than a few lines, or more than one file, send one `fixer` run: the log tail path, the failing command, and the change to make. Rerun the failing command locally until it passes. A fix that needs a design decision, a change outside this PR's scope, or a secret is an **ask**: stop and give the user the options, recommended one first.

5. **Push** — stage with `git add -u` plus the new files the fix made, never `git add -A`. Commit `fix(<scope>): <what the fix does>` per `~/.claude/CODING_STANDARDS.md`. Push with `git push origin HEAD:<headRefName>`, never with force. Go to step 2.

## Stop rules

- At most three fix rounds per run. Then report what still fails.
- The same check failing with the same error after a fix ends the run. Report both log tails. Do not try a third approach without the user.
- Never edit a workflow file to skip, disable, or weaken a check. Fixing a broken workflow step is allowed when the failure is in the workflow itself, and the report says so.

## Report

One row per check: name, final state, class, and the fix commit sha or the rerun. Then the commits pushed, the reruns triggered, and each open failure with the last 10 log lines.
