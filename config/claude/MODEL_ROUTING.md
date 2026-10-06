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
- Verifying Value hits, contradictions, and address-review findings: `verifier` (Fable, effort medium).
- Every other subagent runs on Opus, including orchestrating wrappers (scout, Standards, Spec, fixer, explorer, the implement-tickets reviewer wrapper).

## Advisor

`advisorModel: fable` is set globally. Opus subagents inherit it and get Fable at their decision points without running Fable throughout.

For a session that is mostly orchestration (`/implement-tickets`), launch with `claude --model opus`; the Fable advisor carries the hard decisions.
