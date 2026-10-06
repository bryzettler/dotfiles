# implement-tickets: apply answers

Run with `--answers` before step 1, and from step 5's Record. Read `<issues-folder>/../spec-questions.md`, the file step 5 writes. Each entry has a `Ticket:`, a `PR group:`, a `Kind:` (`spec`, `mismatch`, or `followup`), the question and its spec line, and an `Answer:` line that the user fills in.

Skip an entry that already has an `Applied:` line, so a rerun applies each answer once. Act on the others by kind and answer:

- `spec`, `keep` → append `Spec answer: keep — <question>` to that ticket's Agent result, so later reviews read the decision as spec.
- `spec`, `change: <behaviour>` → write a new ticket at the next free number, `NN-spec-<slug>.md`: `**Type:** fix`, `**Status:** ready-for-agent`, `**PR:** <group>`, `**Blocked by:**` the ticket at the group's tip, and a body that quotes the question, the spec line, and the answer. Its one acceptance criterion is the behaviour the answer names, pinned by a test.
- `mismatch`, any answer → append `Spec answer: <answer> — <question>` to that ticket's Agent result, and set `Status: failed` → `Status: ready-for-agent`. It runs again at deep, from the group tip, and its `NN-slug-partial-deep` branch stays for reference.
- `followup`, `fix: <outcome>` → write a new ticket at the next free number, `NN-fix-<slug>.md`: `**Type:** bug`, `**Status:** ready-for-agent`, `**PR:** <group>`, `**Blocked by:**` the ticket at the group's tip, and a body that quotes the scenario, severity, `file:line`, report path, and the answer. Its one acceptance criterion is the outcome the answer names, pinned by a test.
- `followup`, `wontfix: <reason>` → one line in `<issues-folder>/../followups-<group>.md` with the scenario, `file:line`, and the reason.
- An empty answer → leave the entry as it is.

Under every entry acted on, add `Applied: <date> → <ticket>`.

Done when every answered entry has an `Applied:` line. The new tickets are on the frontier at step 1, and every other ticket resumes by its `Status:`.
