# implement-tickets: rationale

Why the skill is shaped this way. No step reads this file. Read it before you change a step.

## Models

The skill runs on opus (`claude --model opus`): the main loop executes a procedure, and the fable advisor carries the hard decisions. Every agent it dispatches runs on opus: the scout, explorers, both implementer tiers, and the reviewer wrapper, because each executes a procedure, and the wrapper is the review main loop, which works from returns and paths, not code. Fable enters through the advisor, which every opus sub-agent inherits: the deep implementer consults it at each open decision, and the wrapper consults it on a resolution or a spec-question recommendation it cannot settle from the returns. The review skill routes its own sub-agents; its consequence tracers (Defects, Value, and the Value verifiers) are the only agents on fable. Routing lives in `~/.claude/MODEL_ROUTING.md`.

## Cost budget

One scout, explorers only on the scout's recommendation, one implementer per ticket plus at most one deep escalation, and a merge rerun only when the group tip's merge conflicts or fails verification. Per PR group: one or two reviewer rounds at its first hardening, then on a rerun one delta round only when a Value ticket landed, four over the group's life. Each reviewer runs the whole review skill, so review is the larger share of a run's agents; that is the price of a branch that lands clean. Each answer that creates or resets a ticket adds one implementer, and a reviewer round for its group only when the ticket's tier reason is Value.

## Execute and Harden

- The group branch only ever holds finished tickets: a ticket lands by fast-forward on `done`.
- No ticket is reviewed on its own: the group review also finds the bugs that sit between tickets. The implementer skips its own code-review step for the same reason.
- Every frontier ticket dispatches at once, whatever its repo: blockers encode the real ordering.
- Before a stacked group's first round, the lower group's final tip merges in, so the lower group's review fixes reach it.
- Ask keeps the return's recommendation because the agent that read the spec and the code made it.

## Asking

- A question is asked when it arises, not after every ticket and round has finished: a batch at the end found the user gone, and the answers then needed a rerun with a flag.
- The agent never answers a spec question for the user. Running to resolution means running around a question while it waits, not guessing it.
- Answers on file apply at the start of every run, so a headless run or a `Decide later` resumes with the same command and no flag.
