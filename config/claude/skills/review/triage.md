# Review: triage

Step 3, in the main loop. Every finding ends **confirmed**, **dismissed**, or (Value only) **open**, each with a stated reason. Keep each axis return; read its coverage list only when a verdict needs that context. Read `verdicts.md` in this folder first: it holds the verdicts, their rules, and the +EV bar.

## Steps

1. **Fold** — merge hits that share file, line, and mechanism across axes into one. Keep the higher severity and both axis names.
2. **Contradictions** — find two returns, or a return and a verdict, that state opposite facts about one path ("the timeout bounds the stream" against "the timeout ends at the headers"). Each contradiction goes to a Value verifier as a hit, with both claims quoted. The fact it settles joins the findings.
3. **Tooling hits** — a tooling hit on a changed line (an unused import, a lint error, a type error the diff introduced) is a confirmed finding, resolved as Fix the code, with no verifier.
4. **Defects** — settle each hit from its packet, in the main loop:
   - Its proof run (a test, script, or repro whose command and output path the hit names) shows the scenario, or its quoted `file:line` settles it where nothing can run: confirmed.
   - Neither, or a scenario the main loop cannot follow from the packet: dismissed as untraced.
   - A guard or call site the Pinned lens lists as reached by no unit test file: a confirmed Pinned finding, with the missing test as its fix.
   - Send no Defects hit to a verifier.
5. **Value** — group Value hits, contradictions, every Value suspicion, and every Defects suspicion on an amount, a signer, or a guard by top-level directory into at most two `verifier` agents. A suspicion goes now unless its answer lives outside the repo (see Settle a suspicion from the repo). Each prompt carries that group's hits with their evidence packets, its suspicions as quoted, the spec path, the path of `verdicts.md`, and the path of `brief-value.md`. Read the verdicts, not the code. Send a CONFIRMED verdict you cannot follow back to the same verifier once; then dismiss it.
6. **Standards and Spec** — apply "The +EV bar" in `verdicts.md`. They get no verifier.
7. **Revert check** — per "Revert check" in `review-rules.md` in this folder, with the targets the Defects return lists.
8. **Resolve** — give every confirmed finding one resolution, below.

Done when every hit has a verdict and a resolution (CONFIRMED with a resolution, DISMISSED with a reason), every Value hit and every suspicion step 5 names has a verifier verdict or a stated OPEN with what was checked and what was not, and the revert check has returned a line per target.

## Rules

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
