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
- **Pinned** — `**Tier:** deep` in the header. `**Tier:** standard` pins the other way and overrides every signal above.

A ticket that is many small mechanical edits, a cut across several modules or repos, a rename, a config change, a test backfill, or a straight port of a described function is standard even when it is long.
