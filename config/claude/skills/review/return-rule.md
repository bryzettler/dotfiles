# Return rule

Your final message is your return. The caller reads that message and nothing else.

- Write the final message in the shape your brief names, and nothing else: no preamble, no narration of how you worked, no quoted tool output.
- Deliver it only as the final message. Never send it by `SendMessage`, a handback, or another channel. End on the return itself, never on a pointer ("I sent the report") or a placeholder.

## Review hits

The shape for the four `review` axes. Write no report file: the final message is the full report.

1. The per-item coverage list, when your brief asks for one.
2. Every hit: `file:line`, lens, severity, the scenario in one sentence, the fix in one sentence, then an **evidence packet**: the exact lines traced with ten lines of context, and the caller, callee, constant, or schema line the trace relied on, each quoted with its `file:line`. The main loop confirms from the packet and treats a hit without one as unverified.
3. The suspicions, one line each.
