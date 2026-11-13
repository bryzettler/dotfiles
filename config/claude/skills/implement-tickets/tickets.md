# Tickets

The ticket format, its states, and the tier rubric. The `implement-tickets` scout and `address-review` read it.

## Ticket grammar

A ticket is `NN-slug.md`. Its header:

- `# NN — title`
- `**Type:**` free text (feature, bug, spec, chore).
- `**Status:**` one of `ready-for-agent`, `needs-triage`, `in-progress`, `done`, `failed`, `claimed`, `resolved`, `ready-for-human`, `wontfix`. A file with no parseable `Status:` is a planning or map ticket.
- `**Blocked by:**` ticket numbers. The list may wrap across lines: read to the end of the sentence. Prose like "must merge before X" is ordering advice, not a blocker.
- `**Tier:**` optional, `standard` or `deep`, set by a human. It wins over the tier rubric.
- `**PR:**` optional, the name of the PR the ticket ships in, set by a human. It wins over the scout's grouping. The name is also the branch name.

The body:

- Acceptance criteria as `- [ ]` checkboxes. They are the definition of done.
- Relative links (`../spec.md`, prototypes) resolve against the ticket's own directory.

## State classes

| Status                                                                                              | Class                               |
| --------------------------------------------------------------------------------------------------- | ----------------------------------- |
| none, `resolved`, `needs-triage`, `ready-for-human`, `wontfix`                                      | skipped: not agent-actionable       |
| `claimed`                                                                                           | skipped: owned elsewhere            |
| `in-progress`                                                                                       | skipped: died mid-run, user decides |
| `done`, `failed`                                                                                    | skipped                             |
| `ready-for-agent`, every blocker `done`, `resolved`, or a non-actionable ticket whose output exists | **frontier**                        |
| `ready-for-agent`, any blocker `claimed`, `in-progress`, `failed`, or `ready-for-agent`             | **waiting**                         |

## Tier rubric

Propose one tier per candidate ticket. Standard is the default and needs no reason. Propose deep when any of these holds, and name which in the manifest:

- **Value** — the change touches funds, authority, keys, an on-chain program or contract, a database migration, or a concurrency or scheduling path.
- **Open design** — an acceptance criterion needs a decision the spec leaves open: a data model, a public interface, an algorithm choice.
- **Depth** — the ticket asks for a root-cause fix of a bug with no reproduction, or a performance change with a numeric target.
- **Bolt-on** — the code has no clean place for the change: the acceptance criteria fit only as a flag, a special case, or a parallel path beside the existing shape. Settle it per Bolt-on tickets below: deep, or standard behind a preparatory refactor.
- **Pinned** — `**Tier:** deep` in the header. `**Tier:** standard` pins the other way and overrides every signal above.

A ticket that is many small mechanical edits, a cut across several modules or repos, a rename, a config change, a test backfill, or a straight port of a described function is standard even when it is long.

## Bolt-on tickets

The scout settles every bolt-on itself, from the spec and the code, and asks no one. Pick one:

- **Preparatory refactor** — the target shape is clear from the code (extract the seam, replace the flags with a state type, move the logic to its owner), and the refactor changes no behaviour. Propose a ticket `NN-refactor-<slug>.md`, numbered from the next free number in the folder: `**Type:** refactor`, `**Status:** ready-for-agent`, `**PR:**` the bolt-on ticket's group, and a body that names the target shape and why the ticket needs it. Its acceptance criteria: the named shape exists, and the existing tests pass unchanged. The bolt-on ticket gets the new number in `Blocked by:`, and its tier comes from the other signals, since the refactor removes the bolt-on.
- **Deep** — the right shape is a design decision: a new data model, a public interface, or more than one shape with different failure paths. Propose deep with the reason `Bolt-on`. The deep implementer lands the refactor as its own commit before the feature.

Give the choice a one-phrase reason in the manifest. Do not judge Bolt-on again for a ticket whose `Blocked by:` names a `refactor` ticket.
