# Review brief: Standards

The Standards sub-agent brief for `review`, with the Smell baseline. The main loop never reads this file: it names the path in the dispatch prompt, and the agent reads it. Everything the main loop acts on lives in `review-core.md`.

## Return rule

Every agent writes the full report to the report path they were given: the per-item coverage list, every hit, and the suspicions list. The agent's final message carries only the hits and suspicions, in this shape, and nothing else. That message is the return itself: never send it by `SendMessage`, a handback, or another channel, and never end with a pointer such as "I sent the report" or with a placeholder.

- Per hit: `file:line`, lens, severity, the scenario in one sentence, the fix in one sentence, then an **evidence packet**: the exact lines traced with ten lines of context, and the caller, callee, constant, or schema line the trace relied on, each quoted with its `file:line`. The packet is what the main loop confirms from; a hit without one is treated as unverified.
- Per suspicion: one line.

No coverage lines, no "checked, nothing" entries, no preamble. Those are in the file.

## Standards sub-agent

Repo standards plus a smell baseline. The prompt carries: the diff command and commit list, the standards-source paths from the scout's manifest (`CODING_STANDARDS.md`, `CONTRIBUTING.md`, `AGENTS.md`, `CLAUDE.md`, `docs/agents/*`, whatever the repo documents), the tooling output paths, the path of this file, the report path to write, and this brief:

> Read the standards sources, the Smell baseline, and the End state lens in this file. Report, per file or hunk where relevant: (a) every place the diff violates a documented standard, citing the standard (file plus the rule); (b) any baseline smell you spot, named, with the hunk quoted; and (c) every End state hit, with the commits that produced the shape. Distinguish hard violations from judgement calls: documented-standard breaches can be hard, baseline smells are always judgement calls, and a documented repo standard overrides the baseline. Skip anything tooling enforces and anything already in the tooling output files. For Duplicated Code, look past the diff: a new helper that repeats one already in the repo, or a test fixture that repeats the code under test, is a hit, and the fix names both sites. Under 400 words. Deliver per the Return rule in this file, with the quoted hunk as the evidence packet.

### Smell baseline

A fixed set of Fowler code smells (_Refactoring_, ch.3) that applies even when a repo documents nothing. The repo overrides: where a documented standard endorses something the baseline would flag, suppress the smell. Each smell reads _what it is_ → _how to fix_:

- **Mysterious Name** — a function, variable, or type whose name doesn't reveal what it does or holds. → rename it; if no honest name comes, the design's murky.
- **Duplicated Code** — the same logic shape appears in more than one hunk or file in the change. → extract the shared shape, call it from both.
- **Feature Envy** — a method that reaches into another object's data more than its own. → move the method onto the data it envies.
- **Data Clumps** — the same few fields or params keep travelling together (a type wanting to be born). → bundle them into one type, pass that.
- **Primitive Obsession** — a primitive or string standing in for a domain concept that deserves its own type. → give the concept its own small type.
- **Repeated Switches** — the same `switch`/`if`-cascade on the same type recurs across the change. → replace with polymorphism, or one map both sites share.
- **Shotgun Surgery** — one logical change forces scattered edits across many files in the diff. → gather what changes together into one module.
- **Divergent Change** — one file or module is edited for several unrelated reasons. → split so each module changes for one reason.
- **Speculative Generality** — abstraction, parameters, or hooks added for needs the spec doesn't have. → delete it; inline back until a real need shows.
- **Message Chains** — long `a.b().c().d()` navigation the caller shouldn't depend on. → hide the walk behind one method on the first object.
- **Middle Man** — a class or function that mostly just delegates onward. → cut it, call the real target direct.
- **Refused Bequest** — a subclass or implementer that ignores or overrides most of what it inherits. → drop the inheritance, use composition.

### End state

The diff is read as one change, not as the commits that built it. Several commits, often from separate authors or agents, can leave a shape that no single author would write. This lens covers only lines the diff adds. Code on the base keeps its callers, and the Smell baseline covers it.

- **Branch-internal compatibility** — a shim, flag, fallback, adapter, or old and new path side by side, where both sides were added in the diff. Nothing outside the branch calls the old side. → delete the old side, and call the end-state shape directly.
- **Work-around of an earlier commit** — a later commit wraps, special-cases, or converts around a shape an earlier commit in the diff introduced. → change the earlier shape so the work-around is not needed. The fix names both commits.
- **Historical names** — a name added in the diff that tells how the branch got here (`v2`, `new`, `legacy`, `temp`, a ticket number) and not what the code does. → rename it for the domain.
