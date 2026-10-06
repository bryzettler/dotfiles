# Review brief: Standards

Repo standards plus a smell baseline. Deliver per "Review hits" in `~/.claude/skills/review/return-rule.md`, with the quoted hunk as the evidence packet. Under 400 words.

Read the standards sources your prompt names, the Smell baseline, and the End state lens below. Report, per file or hunk where relevant:

- (a) Every place the diff violates a documented standard, citing the standard (file plus the rule).
- (b) Any baseline smell you spot, named, with the hunk quoted.
- (c) Every End state hit, with the commits that produced the shape.
- (d) Every Public repo hit.

Rules:

- Mark each hit hard or judgement call. A documented-standard breach can be hard; a baseline smell is always a judgement call; a documented repo standard overrides the baseline.
- Skip anything tooling enforces and anything already in the tooling output files.
- A wording change to a comment or doc is a hit only when the text misstates what the code guarantees.
- For Duplicated Code, look past the diff: a new helper that repeats one already in the repo, or a test fixture that repeats the code under test, is a hit, and the fix names both sites.

## Smell baseline

A fixed set of Fowler code smells (_Refactoring_, ch.3) that applies even when a repo documents nothing. Where a documented standard endorses something the baseline would flag, suppress the smell. Each smell reads _what it is_ → _how to fix_:

- **Mysterious Name** — a function, variable, or type whose name hides what it does or holds. → rename it; if no honest name comes, the design is murky.
- **Duplicated Code** — the same logic shape appears in more than one hunk or file in the change. → extract the shared shape, call it from both.
- **Feature Envy** — a method that reaches into another object's data more than its own. → move the method onto the data it envies.
- **Data Clumps** — the same few fields or params keep travelling together (a type wanting to be born). → bundle them into one type, pass that.
- **Primitive Obsession** — a primitive or string standing in for a domain concept that deserves its own type. → give the concept its own small type.
- **Repeated Switches** — the same `switch`/`if`-cascade on the same type recurs across the change. → replace with polymorphism, or one map both sites share.
- **Shotgun Surgery** — one logical change forces scattered edits across many files in the diff. → gather what changes together into one module.
- **Divergent Change** — one file or module is edited for several unrelated reasons. → split so each module changes for one reason.
- **Speculative Generality** — abstraction, parameters, or hooks added for needs the spec lacks. → delete it; inline back until a real need shows.
- **Message Chains** — long `a.b().c().d()` navigation the caller should stay independent of. → hide the walk behind one method on the first object.
- **Middle Man** — a class or function that mostly just delegates onward. → cut it, call the real target direct.
- **Refused Bequest** — a subclass or implementer that ignores or overrides most of what it inherits. → drop the inheritance, use composition.

## End state

Read the diff as one change, not as the commits that built it. Several commits, often from separate authors or agents, can leave a shape that no single author would write. This lens covers only lines the diff adds; code on the base keeps its callers, and the Smell baseline covers it.

- **Branch-internal compatibility** — a shim, flag, fallback, adapter, or old and new path side by side, where both sides were added in the diff, and only the branch calls the old side. → delete the old side, and call the end-state shape directly.
- **Work-around of an earlier commit** — a later commit wraps, special-cases, or converts around a shape an earlier commit in the diff introduced. → change the earlier shape so the work-around goes away. The fix names both commits.
- **Historical names** — a name added in the diff that tells how the branch got here (`v2`, `new`, `legacy`, `temp`, a ticket number) instead of what the code does. → rename it for the domain.

## Public repo

When `gh repo view --json visibility` says public, committed text is public too. A line the diff adds to a doc, comment, commit message, or test name that names an incident, an exploit or its date, a drained account, a person, a customer, or internal infrastructure is a hit. → Keep the rule and drop the context: the rule reads on its own.
