# Review briefs

The four sub-agent briefs for `review`: Standards, Spec, Defects, Value, plus the Defects and Value lenses. The main loop never reads this file: it names the path in each dispatch prompt, and the agent reads it. Everything the main loop acts on lives in `review-core.md`.

## Return rule

Every agent writes the full report to the report path they were given: the per-item coverage list, every hit, and the suspicions list. The agent's final message carries only the hits and suspicions, in this shape, and nothing else. That message is the return itself: never send it by `SendMessage`, a handback, or another channel, and never end with a pointer such as "I sent the report" or with a placeholder.

- Per hit: `file:line`, lens, severity, the scenario in one sentence, the fix in one sentence, then an **evidence packet**: the exact lines traced with ten lines of context, and the caller, callee, constant, or schema line the trace relied on, each quoted with its `file:line`. The packet is what the main loop confirms from; a hit without one is treated as unverified.
- Per suspicion: one line.

No coverage lines, no "checked, nothing" entries, no preamble. Those are in the file.

## Standards sub-agent

Repo standards plus a smell baseline. The prompt carries: the diff command and commit list, the standards-source paths from the scout's manifest (`CODING_STANDARDS.md`, `CONTRIBUTING.md`, `AGENTS.md`, `CLAUDE.md`, `docs/agents/*`, whatever the repo documents), the tooling output paths, the path of this file, the report path to write, and this brief:

> Read the standards sources and the Smell baseline in this file. Report, per file or hunk where relevant: (a) every place the diff violates a documented standard, citing the standard (file plus the rule); and (b) any baseline smell you spot, named, with the hunk quoted. Distinguish hard violations from judgement calls: documented-standard breaches can be hard, baseline smells are always judgement calls, and a documented repo standard overrides the baseline. Skip anything tooling enforces and anything already in the tooling output files. For Duplicated Code, look past the diff: a new helper that repeats one already in the repo, or a test fixture that repeats the code under test, is a hit, and the fix names both sites. Under 400 words. Deliver per the Return rule in this file, with the quoted hunk as the evidence packet.

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

## Spec sub-agent

Does the diff do what the spec says. The prompt carries: the diff command and commit list, the spec file path, the path of this file, the report path to write, and this brief:

> Read the spec. Report: (a) requirements the spec asked for that are missing or partial; (b) behaviour in the diff that was not asked for (scope creep); (c) requirements that look implemented but where the implementation looks wrong. Quote the spec line for each finding. Under 400 words. Deliver per the Return rule in this file, with the spec line and the diff hunk as the evidence packet.

When the spec file says there is no spec, skip this agent and note it in the report.

## Defects sub-agent

The prompt carries: the diff command and commit list, the spec file path, the path of this file, the path of each matched `domain-*.md` (read the Lenses below, then the Defects lenses and Pinned additions of each domain file that has them, before starting), the tooling output paths, the report path to write, and this brief:

> First list every changed file and every changed function, instruction, handler, or endpoint. Every item on that list gets a line in your report: either the hits under it or "checked, nothing". When the prompt names a `prior.md`, add each follow-up in it as an item: a prior comment the author replied to (read the reply and the code), or a claim the author left unverified (read the code, and run the narrowest test that settles it). Report a hit where the code does not back the reply or the claim. For each lens, walk the diff and report every hit as: `file:line`, the lens name, the concrete failure scenario (the input or state, then the wrong output or behaviour), and the minimal fix. Read the surrounding code and whatever the changed code models or depends on: the on-chain program, the contract, the schema, the package manifest, the test fixture. The diff alone answers almost no lens. Show every derivation for a number you re-derive. Report only what you traced to a wrong outcome; list unverified suspicions in one line each at the end, labelled as such. Rank hits high, medium, low by user-visible consequence. No length cap on hits; the suspicions list stays under 200 words. Deliver per the Return rule in this file.

### Lenses

Each lens is a class of defect to look for, then how to check it.

- **Re-derive** — a literal the diff introduces or relies on: byte size, window length, offset, threshold, timeout, version floor. → Compute it from the source it claims to come from (`size_of`, `INIT_SPACE`, a program or contract constant, an on-chain account) and compare. Comments, commit messages and PR bodies are claims, not evidence.
- **Mirror** — off-chain code that reproduces an on-chain or remote rule: a client computing what a program will require, a TS condition copying a Rust or Solidity check. → Put the two conditions side by side, operator by operator (`<` against `<=`, which flags gate which branch). Flag every pair that can disagree, including a pre-existing asymmetry the diff now makes load-bearing.
- **Falsy branch** — an async input that can be absent: provider, connection, wallet, env var, account fetch. → Trace what the code returns in that state and what downstream defaults (`?? 0`) make of it. Two shapes recur: a quote of zero that passes a balance gate while reporting not-loading, and a pending flag that latches and never clears. Trace first load and refetch separately; a guard that covers one often misses the other.
- **Safe side** — a default parameter, a fallback, or a changed direction of error. → Name which side is the safe failure (over-quote beats under-quote; a throw beats a silent fallback to a mainnet constant) and confirm the new behaviour lands there. A callee with no in-repo consumers hands every external caller the new default silently.
- **Failure mode** — a removed panic, `unwrap`, throw, revert, or overflow. → State what the path does now instead (retry, permanent stop, silent convergence) and whether that is the intended response. One hardened operation beside unchecked neighbours (`saturating_sub` next to an unchecked `* 2`) is half a fix. A zero-valued guard (`0 >= 0`) passes when it should not.
- **Early exit** — a `break`, `continue`, `return`, or `?` inside a loop that validates, aggregates, or settles a list. → Name what the remaining iterations were supposed to check or sum, and confirm the exit is reached only once every item is accounted for. A `break` on the first match in a loop that must visit every collateral or obligation skips the checks on the rest.
- **Breadth** — resilience that was accidental and is now gone. → A loop of candidates replaced by one attempt; a retry that drops its parameters on recursion; a lookback window equal to the schedule interval, so consecutive runs have zero overlap.
- **Cost** — a new or widened request path. → Bound the work per request (derivations, RPC calls, entries) in terms of the inputs a caller controls. A rate limiter bounds the rate, not the cost of one call. State the bound or the lack of one.
- **Contract** — a description, schema doc, doc comment, NatSpec, or PR-body sentence that states what the code does. → Check the code does exactly that: caps, batching, "what X would emit". A published package's dependency range must admit only versions the code now runs on; a monorepo override hides a floor that is too low.
- **Reorder** — a read, fetch, or check that moved. → Name the window it widened (a bitmap read now ahead of a long build) and what changed in its replacement source (an env var that threw now falls back to a constant).
- **Neighbours** — two caches, guards, hooks, or helpers side by side doing the same job. → Compare lifetimes and conditions: an hourly TTL beside a process-lifetime memo; one hook guarded for a hazard and its sibling in the same diff not. The diff fixed one; check the other.
- **Pinned** — every test added or changed. → (a) _Revert check_: mentally revert the fix the test is named for and confirm the test goes red; a test green either way pins nothing. Triage runs this check for real on up to ten guards, so spend the lens on the guards and tests that no unit test file reaches. (b) The expectation comes from outside the code under test: a literal, or the artifact read back from the chain or the database, never the same helper the handler calls. (c) Every guard the diff adds (a `require!`, a `revert`, a new error variant, a new early return) has a test that reaches it. (d) An implicit precondition (time of day, epoch boundary, a cron that has not fired yet) is stated in the test or removed by pinning the clock. (e) _Negative test_: every new signer, owner, role, mint, seeds, or amount constraint has a test that reaches it with the wrong signer, owner, role, mint, seeds, or amount and asserts the specific error, not just "it threw". (f) The Pinned additions of each domain file.
- **CI truth** — a change made to fix a failing job. → Reproduce the failure and confirm this change removes it. "The job passed on rerun" and "the change fixed it" are different facts. For an image or build change, run the produced artifact, not only the build.
- **Identity** — any input that names a wallet, user, or principal. → Confirm it is authenticated upstream and that no new path trusts it unverified.

## Value sub-agent

Loss of funds, authority, or data. The prompt carries: the diff command and commit list, the spec file path, the path of this file, the path of each matched `domain-*.md` (read the Universal Value lenses below plus every matched domain file's entry map and Value lenses before starting), the tooling output paths, the report path to write, and this brief:

> Start with the entry map each domain file asks for (Solana: the raw-input map; EVM: the external-call map; web: the trust-boundary map; database: the schema-change map; CI: the secret-scope map and the trigger map). Then a value-flow map: every path where lamports, tokens, ETH, NFTs, rent, fees, credits, or authority move or change hands, and every path where rows are deleted, overwritten, or exposed. For each path: source, destination, amount, and who controls each of the three. Then list every changed instruction, function, handler, endpoint, or job, and for each one enumerate every account, address, or input it takes. Work the entry map and the input constraints before any arithmetic: a math error costs a share, a forwarded authority costs the vault. Check every constraint on those inputs whether or not the constraint line changed; a new caller of an unchanged helper inherits every check the helper lacks. Report every hit as: `file:line`, the lens name, the attacker or failure (who, holding what, sends what), the outcome (what they gain or the protocol loses), and the minimal fix. Read the program, the contract, the IDL or ABI, the schema, and the callers; the diff alone answers no lens here. Report only what you traced to an outcome; list unverified suspicions in one line each at the end, labelled as such. Never omit a suspicion for lack of space. Rank: critical (funds, authority, or data lost), high (funds stuck, wrong amount, or a table locked for the deploy), medium, low. No length cap on hits; the suspicions list stays under 300 words. Deliver per the Return rule in this file.

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
