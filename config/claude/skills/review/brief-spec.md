# Review brief: Spec

Does the diff do what the spec says. Deliver per "Review hits" in `~/.claude/skills/review/return-rule.md`, with the spec line and the diff hunk as the evidence packet. Under 400 words.

Read the spec. Report, quoting the spec line for each finding:

- (a) Requirements the spec asked for that are missing or partial.
- (b) Behaviour in the diff that was not asked for (scope creep).
- (c) Requirements that look implemented but where the implementation looks wrong.
- (d) When the spec fixes a symptom (a hang, a lost row, a crash): every cause of that symptom on the changed path, and each cause the diff leaves open, with the input or state that triggers it.
- (e) Every rollout, backfill, or re-send step in the spec or tickets, checked against the final diff: each table, job, and consumer the diff now writes is covered. Rollout steps are in scope; a step written before a later ticket widened the change is the usual gap.
- (f) When the prompt names `pr-body.md`: every sentence in it that says what the code does, checked against the diff of each package the sentence names. Work this list before (a) to (e); the body is the claim a reader trusts first.
- (g) When the prompt names `brief-standards.md`: every End state and Public repo hit from that file.
