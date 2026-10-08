# Model Routing

Fable reasons better about 3rd-, 4th-, and 5th-order consequences. Opus executes, orchestrates, and codes better.

## Main loop (Fable, the session default)

- Handle spec planning, debugging, wayfinder, and grilling here.
- Handle trivial, mechanical, and explicit tasks (specific file/line, clear command) here with direct tools.
- Keep tasks that need conversation context or back-and-forth here.
- The main loop skips the advisor: a second Fable rereading the transcript adds cost, not reasoning.

## Subagents

- Implementation of an approved plan: `implementer` (Opus, effort medium). Use `implementer-deep` (Opus, effort high, consults the advisor) only for money, authority, on-chain, migration, concurrency, or open-design work. Implementers always run on Opus.
- Consequence tracing (review Defects, Value): `tracer` (Fable, effort high). The tracer proves Defects hits itself, by a run or a quoted `file:line`, so Defects hits go straight to the report without a verifier.
- Verifying Value hits, contradictions, audit rejects, and address-review findings: `verifier` (Opus, effort medium). The packet narrows the read, and Opus returns each tool call about 1.5 times faster.
- Pinning a review or an address-review run: `scout` (Sonnet, effort medium). A script does the mechanical half, and the rest is a fixed procedure. Set the effort on every Sonnet agent: its default is high.
- Every other subagent runs on Opus, including orchestrating wrappers (the implement-tickets scout, Standards, Spec, fixer, explorer, the implement-tickets reviewer wrapper).

## Advisor

`advisorModel: fable` is set globally. Opus subagents inherit it and get Fable at their decision points without running Fable throughout.

An advisor call resends the caller's whole context to Fable with no cache, about 100k input tokens per call. Keep it where a decision is made:

- On: the main loop (`/advisor off` for a session that does not need it), `implementer-deep`, and `implementer` after a repeated error.
- Off: `tracer`, since it already runs on Fable, `verifier`, since its packet already narrows the decision, and `scout`, since its brief is a procedure. Off for `general-purpose` orchestrators too: each skill's prompt to them ends with "Do not call the advisor."

Agent frontmatter cannot remove the advisor: `disallowedTools` and `tools` filter normal tools but not this server tool (tested on 2.1.291). Only a prompt line turns it off for one agent; `implementer` made 1 call in 14 days under such a line.

For a session that is mostly orchestration (`/implement-tickets`), launch with `claude --model opus`; the Fable advisor carries the hard decisions.
