# Return rule

Your final message is your return. The caller reads that message and nothing else.

- Write the final message in the shape your brief names, and nothing else: no preamble, no narration of how you worked, no quoted tool output.
- Deliver it only as the final message. Never send it by `SendMessage`, a handback, or another channel. End on the return itself, never on a pointer ("I sent the report") or a placeholder.

## Review hits

The shape for the four `review` axes. Before your final message, write the same text to the return path your prompt names (`<scratchpad>/return-<axis>[-<slice>].md`). The file is the kept copy; the final message is the full report, not a pointer to the file.

1. The per-item coverage list, when your brief asks for one, in the format below.
2. Every hit: `file:line`, lens, severity, the scenario in one sentence, the fix in one sentence, then an **evidence packet**: the exact lines traced with ten lines of context, and the caller, callee, constant, or schema line the trace relied on, each quoted with its `file:line`. The main loop confirms from the packet and treats a hit without one as unverified.
3. The suspicions, one line each.

## Coverage line format

One line per item. Triage audits these lines with `grep`, so keep each field on the item's line, spelled as below.

```
- <file>:<line> <item> — <hit ids | checked, nothing>; <fields>
```

Every item carries `lenses:` and each field its class requires:

| Item class                                               | Field                   | Value                                                                                                              |
| -------------------------------------------------------- | ----------------------- | ------------------------------------------------------------------------------------------------------------------ |
| every item                                               | `lenses:`               | each lens that applies → its result, e.g. `Reorder → env read before signer check, hit H2`                         |
| an exported function, type, or constant                  | `sig:`                  | starts with `unchanged`, `new`, `removed`, or `<old> → <new>` from base to head; a changed one also names the changeset tier. Free text before the form is a REJECT. A changeset, workflow, config, or doc item carries no `sig:` |
| an amount, fee, rent, or funding expression              | `arms:` and `examples:` | the count of conditional arms in the expression, and one worked example with numbers per arm; the counts are equal |
| a truthy or property probe (`if (x.y)`, `x?.y`, `!!x`)   | `probe:`                | `<expr> admits <every type or value that passes>`, the word `admits` included |
| a function that runs two or more checks, reads, or calls | `order:`                | the checks in the order the code runs them, `a > b > c`                                                            |
| an item in a parity table                                | `parity:`               | its row's cells, and `same` or `differs: <cell>` against its siblings                                              |

Example:

```
- src/txVersion.ts:123 resolveTxVersion — H3; sig: new; order: pinned > env override > node detect > signer set; probe: wallet.payer admits NodeWallet, any adapter object with a payer field; lenses: Reorder → env override runs before the signer check, see H3
```

The reason after "checked, nothing" comes from the code. A reason that restates what the code does, or cites the spec, the README, the PR body, a changeset, a comment, the base branch ("same as develop"), a rename, or "pre-existing", is not a reason: triage rejects the line.
