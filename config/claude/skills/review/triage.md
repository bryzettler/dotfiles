# Review: triage

Step 3, in the main loop. Every finding ends **confirmed**, **dismissed**, or (Value only) **open**, each with a stated reason. Keep each axis return. The main loop reads no coverage list whole: step 4 audits it with a script, and reads only the lines the script rejects. Read `verdicts.md` in this folder first: it holds the verdicts, their rules, and the +EV bar.

## Steps

1. **Fold** — merge hits that share file, line, and mechanism across axes into one. Keep the higher severity and both axis names.
2. **Contradictions** — find two returns, or a return and a verdict, that state opposite facts about one path ("the timeout bounds the stream" against "the timeout ends at the headers"). Each contradiction goes to a Value verifier as a hit, with both claims quoted. The fact it settles joins the findings.
3. **Tooling hits** — a tooling hit on a changed line (an unused import, a lint error, a type error the diff introduced) is a confirmed finding, resolved as Fix the code, with no verifier.
   - Each `NOAWAIT` line in `signatures.txt` is a confirmed Contract finding: drop the `async`, or await what the body needs.
   - Each `SIG` line on a package that publishes (no `"private": true` in its `package.json`) is a confirmed Contract finding unless a `.changeset/*.md` in the diff bumps that package at the breaking tier (major; minor while the version is `0.x`). Check with `grep -l '<package name>' .changeset/*.md` and read only those files' front matter.
   - Each `ci-<check>.txt` tail is a CI truth item: the Defects return must name it with its own failure signature. A failing check no return names is a confirmed finding with "explain the failure" as its fix.
4. **Coverage audit** — `python3 -I ~/.claude/skills/review/tools/audit-coverage.py <scratchpad>/return-*.md > <scratchpad>/audit.txt`. Read `audit.txt` only. A `WARN` line is a format fault on a line whose fields show nothing: it needs no verifier, and the report counts it. Each `REJECT` line is a clearance the return did not earn: its fields are missing, it cites a forbidden reason, or its own fields show a hit (a changed `sig:`, fewer `examples:` than `arms:`, a `probe:` that admits several types, an `order:` with a fetch or override ahead of a gate, a `parity:` that differs). Group the rejects by return file, at most 25 lines per group, and send each group to one `verifier` with the brief of that axis, the return-rule path, and the rejected lines as quoted: its job is to check only those items and return hits or a clearance with every field filled. Its return goes to `<scratchpad>/return-<axis>[-<slice>]-audit.md`, and its hits join step 5 or 6 by axis. Run the audit once more on that return; a line rejected twice is a confirmed finding of the lens its field names, with the field as its evidence.
5. **Defects** — settle each hit from its packet, in the main loop:
   - Its proof run (a test, script, or repro whose command and output path the hit names) shows the scenario, or its quoted `file:line` settles it where nothing can run: confirmed.
   - Neither, or a scenario the main loop cannot follow from the packet: dismissed as untraced.
   - A guard or call site the Pinned lens lists as reached by no unit test file: a confirmed Pinned finding, with the missing test as its fix.
   - Send no Defects hit to a verifier.
6. **Value** — two waves of `verifier` agents, at most two agents per wave, grouped by top-level directory. The first wave starts when a Value return lands (step 2 of `SKILL.md`) and carries that return's hits and suspicions. The second starts when steps 1 to 4 are done and carries what they produced: the contradictions, every Defects suspicion on an amount, a signer, or a guard, and the Value hits from an audit return. A suspicion goes now unless its answer lives outside the repo (see Settle a suspicion from the repo). Each prompt carries that group's hits with their evidence packets, its suspicions as quoted, the spec path, the manifest's dependency sources, the path of `verdicts.md`, and the path of `brief-value.md`. Read the verdicts, not the code. Send a CONFIRMED verdict you cannot follow back to the same verifier once; then dismiss it. Check every DISMISSED against "Rules for every verdict" in `verdicts.md`: a dismissal whose reason is a spec, README, PR-body, or changeset citation, "pre-existing" on a line the diff touches, or "no funds move" on a traced wrong amount or outcome is not a dismissal. A Defects-class outcome (a wrong amount, a thrown error, a broken caller) is then a confirmed Defects finding; any other goes back to the verifier once, with the rule quoted.
7. **Standards and Spec** — apply "The +EV bar" in `verdicts.md`. They get no verifier.
8. **Revert check** — per "Revert check" in `review-rules.md` in this folder, with the targets the Defects return lists.
9. **Resolve** — give every confirmed finding one resolution, below.

Done when the audit has run and every rejected line has a verifier verdict, every hit has a verdict and a resolution (CONFIRMED with a resolution, DISMISSED with a reason), every Value hit and every suspicion step 6 names has a verifier verdict or a stated OPEN with what was checked and what was not, and the revert check has returned a line per target.

## Rules

**No step is skipped for volume.** A step with more work than its agents can carry gets more agents, split by return file or by directory, never a skip. A step that did not run is a confirmed process finding in the report, and the gate does not pass while one stands.

**Value is never dismissed for lack of proof.** A Value hit or suspicion the main loop cannot confirm goes to the report's open suspicions, with what was checked and what was not. Value findings are defects: the +EV bar does not apply to them.

**Settle a suspicion from the repo.** Before writing a suspicion as open, name the source that would settle it: a release workflow, a deploy script, a manifest, a config, a fixture. When that source is in the repo, a verifier reads it now and returns CONFIRMED, DISMISSED, or OPEN with the `file:line`. Only a suspicion whose answer lives outside the repo (a mainnet simulation, a production measurement, a team's process) stays open, and it goes to the state file so the next round carries it.

## Resolutions

| Verdict | Resolution |
| --- | --- |
| CONFIRMED `defect` or `nit`, a tooling hit, a Pinned miss, a Standards or Spec finding that clears the bar | **Fix the code** — the default. |
| CONFIRMED `claim` | **Fix the claim** — the code is right and the description, schema doc, or comment is wrong. Say which claim. |
| CONFIRMED `design` | **Redesign** — the fixer gets no item. The report leads with the scenario and the smaller design that removes it. PR mode: it goes in the review body per Post in `pr-mode.md`. |
| CONFIRMED `spec` | **Fix the spec** — the code stays, the fixer gets no item, and the report leads with the finding, the spec line, and the one question the user answers to settle it. |
| A bound that holds, a deliberate alignment | **Leave as is** — with the reasoning that closes it, carried in the report. |
| DISMISSED | Dismissed, with its reason in the report. |
| OPEN | An open suspicion in the report. |

A finding left as is that still has a wrong outcome (an accepted gap, a pre-existing bug outside the diff's scope), and a CONFIRMED finding with `scope: out`, is also a **follow-up**: its scenario, severity, and `file:line`, for the caller to file.

A confirmed finding that is a fixed syntactic pattern (a banned API, an unguarded object lookup, a step condition, a test file no workflow runs) is also a follow-up: the lint rule or CI check that would catch it.
