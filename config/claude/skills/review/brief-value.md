# Review brief: Value

The Value sub-agent brief for `review`, with the Universal Value lenses. The main loop never reads this file: it names the path in the dispatch prompt, and the agent reads it. Everything the main loop acts on lives in `review-core.md`.

## Return rule

Every agent writes the full report to the report path they were given: the per-item coverage list, every hit, and the suspicions list. The agent's final message carries only the hits and suspicions, in this shape, and nothing else. That message is the return itself: never send it by `SendMessage`, a handback, or another channel, and never end with a pointer such as "I sent the report" or with a placeholder.

- Per hit: `file:line`, lens, severity, the scenario in one sentence, the fix in one sentence, then an **evidence packet**: the exact lines traced with ten lines of context, and the caller, callee, constant, or schema line the trace relied on, each quoted with its `file:line`. The packet is what the main loop confirms from; a hit without one is treated as unverified.
- Per suspicion: one line.

No coverage lines, no "checked, nothing" entries, no preamble. Those are in the file.

## Value sub-agent

Loss of funds, authority, or data. The prompt carries: the diff command and commit list, the spec file path, the path of this file, the path of each matched `domain-*.md` (read the Universal Value lenses below plus every matched domain file's entry map and Value lenses before starting), the tooling output paths, the report path to write, and this brief:

> Start with the entry map each domain file asks for (Solana: the raw-input map; EVM: the external-call map; web: the trust-boundary map; database: the schema-change map; CI: the secret-scope map and the trigger map). Then a value-flow map: every path where lamports, tokens, ETH, NFTs, rent, fees, credits, or authority move or change hands, and every path where rows are deleted, overwritten, or exposed. For each path: source, destination, amount, and who controls each of the three. Then list every changed instruction, function, handler, endpoint, or job, and for each one enumerate every account, address, or input it takes. Work the entry map and the input constraints before any arithmetic: a math error costs a share, a forwarded authority costs the vault. Check every constraint on those inputs whether or not the constraint line changed; a new caller of an unchanged helper inherits every check the helper lacks. Report every hit as: `file:line`, the lens name, the attacker or failure (who, holding what, sends what), the outcome (what they gain or the protocol loses), and the minimal fix. Read the program, the contract, the IDL or ABI, the schema, and the callers; the diff alone answers no lens here. Report only what you traced to an outcome; list unverified suspicions in one line each at the end, labelled as such. Never omit a suspicion for lack of space. Budget: about 40 tool calls for the trace, then write the report with what is traced and list the rest as suspicions; a verifier reads each hit after you, so a suspicion costs less than a missed path. Rank: critical (funds, authority, or data lost), high (funds stuck, wrong amount, or a table locked for the deploy), medium, low. No length cap on hits; the suspicions list stays under 300 words. Deliver per the Return rule in this file.

### Universal Value lenses

These apply in every domain. The domain files add the lenses for the chain or the web stack.

- **Arithmetic** — every amount, fee, share, or price calculation. → Overflow and underflow on each operation. Decimals and units agree on both sides (lamports against SOL, wei against ether, base units against UI units, token A decimals against token B). Every narrowing cast (`as u32`, `uint128(x)`, `Number(bigint)`) is checked or provably in range. Rounding direction favours the protocol on every division: `floor` or `ceil` by direction, never `round`, and a rounding error that favours the caller compounds when the operation can be repeated with dust. Re-derive each fee and share with a worked example.
- **Destination** — every place funds or authority land. → Who can set the destination account, the close destination, the rent recipient, the new authority, the withdrawal address. A destination taken from an argument or an unchecked input is a hit unless a check binds it to the signer.
- **Replay** — every action with a side effect. → What happens when it runs twice: a retried transaction, a duplicate job, two bot instances, a partial success followed by a resend, a webhook delivered again. A nonce, an idempotency key, or an on-chain state check must make the second run a no-op.
- **Price** — every price, quote, or rate the code consumes. → Its source can be stale (check the timestamp, slot, or round guard) or manipulable within one transaction or block (a pool spot price, an LP token priced from reserves). A swap or transfer built from it carries a non-zero minimum-out or slippage bound that the caller cannot zero.
- **Griefing** — every loop, list, or account the public can grow. → An attacker can make the instruction, function, or job fail, stall, or exceed compute or gas for everyone else by adding entries, dusting accounts, or front-running an init.
- **Secrets** — every key, keypair, token, or seed the code touches. → It is not logged, not in a fixture, not committed, not in an error message, and not sent to any host other than the intended one. New env vars for secrets have no default.
- **Dependencies** — every change to a manifest or lockfile. → Name each new or upgraded package and what it pulls in. Flag install scripts, a package newer than a week, a floor lowered, or a lockfile change with no manifest change.
- **Legit path** — every guard the diff adds. → Name a legitimate caller and confirm the guard admits them. A guard that blocks the protocol's own crank, keeper, migration, or admin flow is a stuck-funds bug.
