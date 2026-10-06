# Review brief: Value

Loss of funds, authority, or data. Before starting, read the Universal Value lenses below, plus the entry map and Value lenses of each domain file your prompt names. Deliver per "Review hits" in `~/.claude/skills/review/return-rule.md`.

## Steps

When your prompt names flows, those flows are your slice, end to end. Read every file on their path, the `context` files too, whatever package owns it, and put the amount each layer computes beside the next layer's for the same operation.


1. **Entry map** — build the map each domain file asks for: Solana, the raw-input map; EVM, the external-call map; web, the trust-boundary map; database, the schema-change map; CI, the secret-scope map and the trigger map.
2. **Value-flow map** — every path where lamports, tokens, ETH, NFTs, rent, fees, credits, or authority move or change hands, and every path where rows are deleted, overwritten, or exposed. For each path: source, destination, amount, and who controls each of the three.
3. **Inputs** — list every changed instruction, function, handler, endpoint, or job, and for each one enumerate every account, address, or input it takes.
4. **Constraints before arithmetic** — work the entry map and the input constraints first. Check every constraint on those inputs whether or not the constraint line changed: a new caller of an unchanged helper inherits every check the helper lacks. Older code is the diff's when the diff calls it, moves or extracts it, widens who reaches it, or makes an outcome depend on it: its loss is a hit.
5. **Report** every hit as `file:line`, the lens name, the attacker or failure (who, holding what, sends what), the outcome (what they gain or the protocol loses), and the minimal fix.
6. **Prove** a critical or high hit with a run. A medium or low hit stands on its quoted `file:line`.
7. **Rank** — critical (funds, authority, or data lost), high (funds stuck, wrong amount, or a table locked for the deploy), medium, low. Hits have no length cap; the suspicions list stays under 300 words. List every suspicion, whatever the space: a verifier reads each hit after you, so a suspicion costs less than a missed path.

## Universal Value lenses

These apply in every domain. The domain files add the lenses for the chain or the web stack.

- **Arithmetic** — every amount, fee, share, or price calculation. → Overflow and underflow on each operation. Decimals and units agree on both sides (lamports against SOL, wei against ether, base units against UI units, token A decimals against token B). Every narrowing cast (`as u32`, `uint128(x)`, `Number(bigint)`) is checked or provably in range. Rounding direction favours the protocol on every division: `floor` or `ceil` by direction, never `round`, and a rounding error that favours the caller compounds when the operation can be repeated with dust. Re-derive each fee and share with a worked example. A lamport or base-unit amount stays an integer end to end: a round trip through a float (`/ LAMPORTS_PER_SOL`, then `*` back) is a hit.
- **Quote** — every entity the diff funds, prices, or quotes: a cron job, an escrow, a rent-paying account, a subscription. → Walk it through create, re-apply with unchanged input, change one input, run N times, and close. At each step put the quote beside what the chain or backend consumes, as a worked example with numbers. A quote sized for a state the entity leaves at the next step (funded for zero claims, then claims added) is a hit. A re-apply with unchanged input that prices or executes as a change (a value resolved fresh, from the clock or a default, compared with the stored one) is a hit. Replay asks whether a second run is a no-op; Quote asks whether the quote still fits the state.
- **Destination** — every place funds or authority land. → Who can set the destination account, the close destination, the rent recipient, the new authority, the withdrawal address. A destination taken from an argument or an unchecked input is a hit unless a check binds it to the signer.
- **Replay** — every action with a side effect. → What happens when it runs twice: a retried transaction, a duplicate job, two bot instances, a partial success followed by a resend, a webhook delivered again. A nonce, an idempotency key, or an on-chain state check must make the second run a no-op.
- **Price** — every price, quote, or rate the code consumes. → Its source can be stale (check the timestamp, slot, or round guard) or manipulable within one transaction or block (a pool spot price, an LP token priced from reserves). A swap or transfer built from it carries a non-zero minimum-out or slippage bound that the caller cannot zero.
- **Griefing** — every loop, list, or account the public can grow. → An attacker can make the instruction, function, or job fail, stall, or exceed compute or gas for everyone else by adding entries, dusting accounts, or front-running an init.
- **Secrets** — every key, keypair, token, or seed the code touches. → It stays out of logs, fixtures, commits, and error messages, and goes to no host other than the intended one. New env vars for secrets have no default.
- **Dependencies** — every change to a manifest or lockfile. → Name each new or upgraded package and what it pulls in. Flag install scripts, a package newer than a week, a floor lowered, or a lockfile change with no manifest change.
- **Legit path** — every guard the diff adds. → Name a legitimate caller and confirm the guard admits them. A guard that blocks the protocol's own crank, keeper, migration, or admin flow is a stuck-funds bug.
