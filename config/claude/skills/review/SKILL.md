---
name: review
description: Four-axis review of a branch against develop (fixes applied) or of a PR (inline suggestions, or an LGTM approval when clean).
---

# review

One review, two inputs, two outputs. Four axes are the whole review: **Standards** (repo standards plus a smell baseline), **Spec** (does the diff do what the spec says), **Defects** (logic), and **Value** (loss of funds, authority, or data). Invoke no plugin skill.

Keep the main loop's context for verdicts. Tool output, spec text, lens text, and full reports go to scratchpad files and reach the main loop as paths. Read a source range or a report file only when a specific verdict needs it. Run `git diff` only with `--shortstat` (size) or `--name-only` (classification). The `brief-*.md`, `domain-*.md`, `scout.md`, and `return-rule.md` files in this folder are for the sub-agents: pass their paths, and keep their text out of the main loop.

`fixer` is the only agent that edits. In PR mode it edits the checkout only. Nothing is pushed.

## Mode

Resolve the mode from the argument before anything else:

| Argument                                  | Mode   | Target                                                                                                                                                                                             |
| ----------------------------------------- | ------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| none                                      | branch | `git diff <base>...HEAD` plus uncommitted changes, `<base>` = `origin/develop`, else `develop`, else the base of the open PR for this branch (`gh pr view --json baseRefName`), else `origin/HEAD` |
| a ref (`main`, `origin/release`, a sha)   | branch | as above with `<base>` = the ref                                                                                                                                                                   |
| `pr`                                      | pr     | the PR open for the current branch (`gh pr view --json number`)                                                                                                                                    |
| `pr <n>`, `#<n>`, a bare number, a PR URL | pr     | that PR                                                                                                                                                                                            |

Branch mode's output is edits in the working tree. PR mode's output is a GitHub review on the PR; the checkout is scratch and the PR branch stays as the author left it.

**PR mode:** read `pr-mode.md` in this folder now. It holds the PR gate and steps 5 to 9.

**Delta round** (the caller names a last-reviewed sha `<last>`, or the scout's manifest returns one): read `delta-round.md` in this folder after step 1. It changes steps 2 and 3.

## Steps

1. **Pin** — one scout (`general-purpose`, `model: "opus"`). Its prompt carries the mode, the target, the scratchpad path, and the path of `scout.md` in this folder, plus whatever the caller named: follow-ups file, spec files, `<last>`, the previous report, the previous manifest. Done when the manifest names every field in `scout.md` and every changed file has a domain. A manifest that says "nothing new since `<last>`" ends the run: report that and stop.

2. **Review** — in one message:
   - Start each **test run** from the manifest in the background (`run_in_background`).
   - Dispatch the axes: Standards and Spec (`general-purpose`, `model: "opus"`), Defects and Value (`tracer`).
   - Every prompt carries its return path (`<scratchpad>/return-<axis>[-<slice>].md`), the diff command and commit list, the spec path, the path of its `brief-<axis>.md`, the tooling output paths, and a scratchpad path for proof-run output. Standards adds the **standards sources**. Defects and Value add the matched `domain-*.md` paths. Defects adds the `prior.md` path when there is one. In PR mode, Spec and Defects add the `pr-body.md` path. Paste no lens, smell, or standards text, and name no focus or priority: the prompt carries only these pointers, so each agent covers every path its brief lists.
   - **Standards scope** skip: dispatch no Standards agent. The Spec prompt also names `brief-standards.md` for its End state and Public repo lenses.
   - **Value scope** skip: dispatch no Value agent.
   - Spec path "no spec": dispatch no Spec agent.
   - Diff line count over about 1500: split Defects by package or top-level directory, one agent per slice; each slice's prompt names the files it owns. Split Value by the scout's `flows.md`: one agent per flow group, each prompt naming its flows and every file on them, whatever the package. Changed files on no flow form one more Value slice, by directory.

   Each axis writes its own return file before its final message, per `return-rule.md`. When a file is missing, save the final message to that path verbatim as it arrives; summarise none, and never rewrite a return from memory after a compaction. Triage reads the summary it needs from the file; the misses log needs the coverage lists whole.

   Done when every dispatched axis has a return per "Review hits" in `return-rule.md` in this folder.

3. **Triage** — per `triage.md` in this folder. Done per its done line.

4. **Gate** — zero confirmed findings and zero open Value suspicions is a clean diff. The gate also needs the **execution evidence** from the manifest and the test runs from step 2. Wait for their completion notifications, never with a sleep loop, and read only the tail of each output. A failing or missing test run is a confirmed finding.
   - Branch, zero confirmed findings, test run passed: write the report and stop. When open Value suspicions remain, the report leads with them. A failing test is a confirmed finding, so the run continues to step 5 and the report leads with the failing lines.
   - PR: per "Gate" in `pr-mode.md`.

5. **Fix** — one `fixer` run carrying every code and claim resolution. It starts with no context and cannot ask, so settle every judgement call first and state the change, not the reasoning. Per item: file and line, what is wrong, and the minimal edit. Add the project's **lint and test commands**, the **backend commands**, and a scratchpad path for their output. Done when every item has an edit or a reported mismatch, and lint and tests pass or the failures are shown to pre-exist on `<base>`. PR mode: Draft, per `pr-mode.md`.

6. **Fix re-review** — per "Fix re-review" in `review-rules.md` in this folder, using the manifest's **backend commands**. Done per that section.

## Report

A severity table first: one row per confirmed finding with file, axis or lens, severity. Then seven lists:

1. redesign, with the scenario and the smaller design
2. fixed in code
3. fixed in the claim
4. left as is, with the reasoning
5. follow-ups
6. open suspicions (Value only), each with what was checked, what was not, and whether it was carried from a prior round
7. dismissed, with the reason

Then the execution evidence: CI checks per name as pass, fail, pending, or "no CI"; the test runs as pass or fail, with the failing lines when any; lint and test results per command from the fixer's return, with the failing lines when any; and the entries marked `unproven`.

Then the spec questions from every "Fix the spec" resolution. Then the revert check: targets mutated, red, green.

Then a coverage line: the head sha reviewed and the base, round (full or delta since `<last>`), domains, files read, files not read, axes skipped (Value with the scout's reason), tools run, tools unavailable or skipped.

On the zero path: the open suspicions and dismissed lists, the execution evidence, the revert check, and the coverage line.

PR mode adds the fields under "Report additions" in `pr-mode.md`.
