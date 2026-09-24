---
name: review
description: Four-axis review of a branch against develop (fixes applied) or of a PR (inline suggestions, or an LGTM approval when clean).
disable-model-invocation: true
---

# review

One review, two inputs, two outputs. Read `review-core.md` in this folder first: it holds the domain table, the tooling list, the triage rules, the fix re-review, and the report shape. `briefs.md` holds the four sub-agent briefs and the Defects and Value lenses, and the `domain-*.md` files hold the chain, CI, web, and database lenses; the sub-agents read those by path and the main loop never opens them. Everything below is what differs by mode.

## Mode

Resolve the mode from the argument before anything else:

| Argument                                  | Mode   | Target                                                                                                                                                                                             |
| ----------------------------------------- | ------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| none                                      | branch | `git diff <base>...HEAD` plus uncommitted changes, `<base>` = `origin/develop`, else `develop`, else the base of the open PR for this branch (`gh pr view --json baseRefName`), else `origin/HEAD` |
| a ref (`main`, `origin/release`, a sha)   | branch | as above with `<base>` = the ref                                                                                                                                                                   |
| `pr`                                      | pr     | the PR open for the current branch (`gh pr view --json number`)                                                                                                                                    |
| `pr <n>`, `#<n>`, a bare number, a PR URL | pr     | that PR                                                                                                                                                                                            |

Branch mode's output is edits in the working tree. PR mode's output is a GitHub review on the PR; the checkout is scratch and the PR branch stays as the author left it. In PR mode, read `pr-mode.md` in this folder now: it holds the prior-round and delta-round rules, the PR gate, the steps after the gate, and the public-text and anchoring standards.

## Shared steps

1. **Pin** — one `scout` agent (`general-purpose`, `model: "opus"`), so no tool output, spec text, or PR body enters the main loop. Its prompt carries the mode, the target, the scratchpad path, the domain table from `review-core.md`, and the tooling list from `review-core.md` plus the instruction to read the Tooling section of each matched `domain-*.md` in this folder. It resolves the fixed point, confirms the diff is non-empty, writes the spec to `<scratchpad>/spec.md`, classifies every changed file by the domain table, runs every supported tool with one output file per tool, and returns a manifest under 300 words: fixed point sha, checkout path, spec path, diff line count from `git diff --shortstat`, one line per changed file with its domains, tooling output paths, tools unavailable, the standards-source paths, the project's lint and test commands, and the **execution evidence**: in PR mode the `gh pr checks <n>` result (every check named with pass, fail, or pending, or "no CI"); and, when there is no CI or no check runs the tests, and always in branch mode, the test runs per "Test runs" in `review-core.md`, each as command, working directory, and output path, not yet run. Done when the manifest names all of those and every changed file has a domain.
   - _branch_: `git rev-parse <base>` succeeds. Spec: the `.scratch/<feature>/` PRD and issue files that match the branch when they exist, else the full commit messages from `git log <base>..HEAD`. When the caller names a follow-ups file (claims the author left unverified), the scout copies it to `<scratchpad>/prior.md` and the Defects prompt carries that path. When the caller names spec files (tickets, a PRD), the scout writes those into the spec file instead of searching.
   - _branch, delta round_: when the caller names a last-reviewed sha `<last>` and the previous round's report, the review is a delta round. The review diff is `git diff <last> HEAD` plus uncommitted changes, the fixed point for the axes is `<last>`, and the full `git diff <base>...HEAD` is named in every prompt as context, never as the target. The scout appends the previous round's fixes, open suspicions, and dismissals to `<scratchpad>/prior.md`. Read "Delta round" in `pr-mode.md`; its PR-body condition does not apply in branch mode. Defects confirms each prior fix does what the report says, and works the Neighbours lens on it: a sibling call, guard, or path the fix left unchanged is a hit.
   - _pr_: `gh pr view <n> --json baseRefName,headRefOid,body`. Review from a checkout at the PR head: the current worktree when `HEAD` is that sha, else a throwaway worktree in the scratchpad from `git fetch origin pull/<n>/head`. Fetch the base and take `origin/<base>` as the fixed point. Spec: the PR body, then the full commit messages from `git log origin/<base>..HEAD`. The PR body is the strongest spec a PR has, since every sentence in it is a claim the Spec and Contract lenses can test. Done when the checkout is at `headRefOid`.
   - _pr, prior round_: per "Prior round" in `pr-mode.md`.

2. **Review** — in one message, start the scout's test runs in the background and dispatch the four sub-agents from `briefs.md`: Standards, Spec, Defects, Value. Each prompt carries the diff command and commit list, the spec path, the path of `briefs.md`, the tooling output paths, and a report path under the scratchpad; Standards adds the standards-source paths, Defects and Value add the matched `domain-*.md` paths. No lens, smell, or standards text is pasted. Done when four returns are in hand in the shape of the Return rule in `briefs.md`.
   - _delta round_: per "Delta round" in `pr-mode.md`.

3. **Triage** — per `review-core.md`: fold duplicates, dispatch the verifiers, then resolve. Done when every finding across the four reports is confirmed, dismissed with a reason, or held as an open Value suspicion, and each confirmed finding has a resolution, and the revert check has returned a line per guard.

4. **Gate** — zero confirmed findings and zero open Value suspicions is a clean diff. A clean diff is not yet a proven one: reading is not running. The four axes never execute the code, so the gate also needs the execution evidence from the scout's manifest and the test runs started in step 2 (wait for them here; read only the tail of each output), and a failing or missing test run is a confirmed finding in its own right.
   - _branch_: the test run passed. Write the report and stop. Silence is the deliverable. An open Value suspicion is not silence: the report leads with it. A failing test is not silence either: it is a finding, and the report leads with the failing lines.
   - _pr_: per "Gate" in `pr-mode.md`.

Then continue with the steps for the mode. Both modes dispatch `fixer` once: it starts with no context and cannot ask, so every judgement call is settled before dispatch and the prompt states the change, not the reasoning. Per item: file and line, what is wrong, and the specific minimal edit. Plus the project's lint and test commands and a scratchpad path for their output.

## Branch mode

5. **Fix** — one `fixer` run carrying every code and claim resolution. Done when every item has an edit or a reported mismatch, and lint and tests pass or the failures are shown to pre-exist on `<base>`.

6. **Re-review the fix** — per `review-core.md`. Done when every fixer edit passes the five lenses, or one follow-up `fixer` run has landed the corrections.

## PR mode

Steps 5 to 9 (Draft, Re-review the fix, Redact, Post, Persist) are under "Steps after the gate" in `pr-mode.md`.

## Cost budget

One `scout`, four review sub-agents (more only when the diff is split by size per `review-core.md`, fewer in a delta round), at most six verifiers plus one revert-check verifier, one `fixer`, at most one follow-up `fixer`. A delta round reviews the change since the last round, not the PR, and re-verifies a carried suspicion only when the delta touches the files it names. Every agent runs with `model: "opus"`. The main loop does the resolutions, the +EV bar, the fix re-review, and redaction, and those work from returns and scratchpad paths: the main loop reads a source range or a report file only when a specific verdict needs it. No plugin skill is invoked. `fixer` is the only agent that edits; in PR mode it edits the checkout only, and nothing is pushed. The four axes are the whole review.

## Report

The shape in `review-core.md`. PR mode adds the fields under "Report additions" in `pr-mode.md`.
