---
name: review-simplify
description: Review the branch against its merge base on four axes (standards, spec, defects, value), then fix what survives triage. A clean branch gets no edits.
disable-model-invocation: true
---

# review-simplify

Review the branch's changes on four axes, fix confirmed defects and +EV cleanups, and leave a clean branch alone. Read `review-core.md` in this folder first: it holds the Defects and Value briefs and lenses, the tooling step, the triage rules, the fix re-review, and the report shape.

The target is `git diff <base>...HEAD` plus uncommitted changes. `<base>` is the argument; without one, the base of the open PR for this branch (`gh pr view --json baseRefName`), else `origin/HEAD`.

## Steps

1. **Pin** — resolve `<base>` and confirm the diff is non-empty. Write the spec to a scratchpad file: the `.scratch/<feature>/` PRD and issue files that match the branch when they exist, else the full commit messages from `git log <base>..HEAD`. Run the tooling from `review-core.md` and save the output to the scratchpad. Done when `git rev-parse <base>` succeeds, the spec file exists, and the tooling output is saved or each tool is noted as unavailable.

2. **Review** — invoke `mattpocock-skills:code-review` with `<base>` as the fixed point and the spec file as its spec argument. Dispatch the Defects and Value sub-agents from `review-core.md` in the same batch as code-review's two. Done when four reports are in hand.

3. **Triage** — per `review-core.md`. Done when every finding across the four reports is confirmed, dismissed with a reason, or held as an open Value suspicion, and each confirmed finding has a resolution.

4. **Gate** — zero confirmed findings: write the report and stop. Silence is the deliverable for a clean branch. An open Value suspicion is not silence: the report leads with it.

5. **Fix** — one `fixer` run carrying every code and claim resolution. Per item: file and line, what is wrong, and the specific minimal edit. Plus the project's lint and test commands. `fixer` starts with no context and cannot ask, so every judgement call is settled before dispatch and the prompt states the change, not the reasoning. Done when every item has an edit or a reported mismatch, and lint and tests pass or the failures are shown to pre-exist on `<base>`.

6. **Re-review the fix** — per `review-core.md`. Done when every fixer edit passes the five lenses, or one follow-up `fixer` run has landed the corrections.

## Cost budget

Four review sub-agents (more only when the diff is split by size per `review-core.md`), one `fixer`, at most one follow-up `fixer`, the Defects and Value agents dispatched with `model: "fable"` and every other agent with `model: "opus"`, including the two that `code-review` has the main loop spawn. Fable also stays in the main loop for the judgement steps. Triage and the fix re-review stay in the main loop. `fixer` is the only agent that edits. The four axes are the whole review.

## Report

The shape in `review-core.md`.
