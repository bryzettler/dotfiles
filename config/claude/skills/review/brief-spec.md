# Review brief: Spec

The Spec sub-agent brief for `review`. The main loop never reads this file: it names the path in the dispatch prompt, and the agent reads it. Everything the main loop acts on lives in `review-core.md`.

## Return rule

The agent's final message is the full report: the per-item coverage list, every hit, and the suspicions list, in this shape, and nothing else. The harness refuses report-file writes from sub-agents, so the agent writes no report file. That message is the return itself: never send it by `SendMessage`, a handback, or another channel, and never end with a pointer such as "I sent the report" or with a placeholder.

- Per hit: `file:line`, lens, severity, the scenario in one sentence, the fix in one sentence, then an **evidence packet**: the exact lines traced with ten lines of context, and the caller, callee, constant, or schema line the trace relied on, each quoted with its `file:line`. The packet is what the main loop confirms from; a hit without one is treated as unverified.
- Per suspicion: one line.

No coverage lines, no "checked, nothing" entries, no preamble. Those are in the file.

## Spec sub-agent

Does the diff do what the spec says. The prompt carries: the diff command and commit list, the spec file path, the path of this file, and this brief:

> Read the spec. Report: (a) requirements the spec asked for that are missing or partial; (b) behaviour in the diff that was not asked for (scope creep); (c) requirements that look implemented but where the implementation looks wrong; (d) when the spec fixes a symptom (a hang, a lost row, a crash), every cause of that symptom on the changed path, and each cause the diff leaves open, with the input or state that triggers it; (e) every rollout, backfill, or re-send step in the spec or tickets, checked against the final diff: each table, job, and consumer the diff now writes is covered. Rollout steps are in scope; a step written before a later ticket widened the change is the usual gap. (f) when the prompt names `brief-standards.md`, every End state and Public repo hit from that file. Quote the spec line for each finding. Under 400 words. Deliver per the Return rule in this file, with the spec line and the diff hunk as the evidence packet.

When the spec file says there is no spec, skip this agent and note it in the report.
