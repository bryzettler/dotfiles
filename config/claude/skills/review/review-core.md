# Review core

Shared reference for `review`. `SKILL.md` owns the input (a base or a PR) and the output (edits or a GitHub review). Everything between lives here.

## Four axes

`mattpocock-skills:code-review` runs two axes as parallel sub-agents: **Standards** (repo standards plus a smell baseline) and **Spec** (does the diff do what the spec says). Neither hunts defects. Two more axes run as sub-agents dispatched in the same batch as those two: **Defects** (logic, with the brief and lenses below) and **Value** (loss of funds and authority, with its own brief and lenses further down). The two are separate agents so that neither budget crowds the other, and so that a bug can be found twice.

When the diff exceeds about 1500 lines, split the Defects and Value agents by package or top-level directory, one agent per slice, so no agent skims. Each slice's brief names the files it owns.

## Domain

The diff decides which lenses apply. Before dispatch, classify every changed file into one or more domains and read each matching `domain-*.md` in this folder. A diff can hit several domains (a monorepo with a program, its contracts, and a frontend). A file with no chain signal is `web`.

| Domain | Signal | File |
| --- | --- | --- |
| solana | Rust in a crate whose `Cargo.toml` depends on `anchor-lang`, `solana-program`, `pinocchio`, or an `spl-*` crate; an IDL under `target/idl`; TS or Rust that imports `@coral-xyz/anchor`, `@solana/web3.js`, `@solana/kit`, or `solana-client` and builds transactions | `domain-solana.md` |
| evm | a `.sol` file, `foundry.toml`, `hardhat.config.*`; TS or Python that imports `ethers`, `viem`, `wagmi`, or `web3` and sends transactions | `domain-evm.md` |
| web | everything else: backend, frontend, jobs, infra, config | `domain-web.md` |

Each domain file holds its tooling, its entry map (what the Value agent enumerates first), its Value lenses, and its additions to the Pinned lens. The Value brief carries the universal lenses below plus the lenses of every domain the diff hits, pasted in full. Client code that talks to a chain is both its chain domain and `web`: the Mirror lens works the client side, the chain lenses the program side. Name the domains in the report's coverage line.

## Tooling

Before dispatch, run whichever of these the repo supports and attach the output to the Defects and Value briefs: `cargo clippy --all-targets`, `cargo audit`, `npm audit` (or `pnpm audit`), and `gitleaks detect` or `trufflehog filesystem` on the diff. Then the tooling of each domain file. Deterministic and near free. Record in the report which ran and which were unavailable.

`code-review` asks the user for a fixed point or a spec it cannot find, and this run has no user to ask. Hand it both, settled: the fixed point as the argument, and the spec as a file path written to the scratchpad before invoking it.

## Defects sub-agent

The prompt carries: the diff command and commit list, the spec file path, the lens list below pasted in full plus the Pinned additions of each domain file (the agent has no other access to them), and this brief:

> First list every changed file and every changed function, instruction, handler, or endpoint. Every item on that list gets a line in your report: either the hits under it or "checked, nothing". For each lens, walk the diff and report every hit as: `file:line`, the lens name, the concrete failure scenario (the input or state, then the wrong output or behaviour), and the minimal fix. Read the surrounding code and whatever the changed code models or depends on: the on-chain program, the contract, the schema, the package manifest, the test fixture. The diff alone answers almost no lens. Show every derivation for a number you re-derive. Report only what you traced to a wrong outcome; list unverified suspicions in one line each at the end, labelled as such. Rank hits high, medium, low by user-visible consequence. No length cap on hits; the suspicions list stays under 200 words.

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
- **Pinned** — every test added or changed. → (a) _Revert check_: mentally revert the fix the test is named for and confirm the test goes red; a test green either way pins nothing. (b) The expectation comes from outside the code under test: a literal, or the artifact read back from the chain or the database, never the same helper the handler calls. (c) Every guard the diff adds (a `require!`, a `revert`, a new error variant, a new early return) has a test that reaches it. (d) An implicit precondition (time of day, epoch boundary, a cron that has not fired yet) is stated in the test or removed by pinning the clock. (e) _Negative test_: every new signer, owner, role, mint, seeds, or amount constraint has a test that reaches it with the wrong signer, owner, role, mint, seeds, or amount and asserts the specific error, not just "it threw". (f) The Pinned additions of each domain file.
- **CI truth** — a change made to fix a failing job. → Reproduce the failure and confirm this change removes it. "The job passed on rerun" and "the change fixed it" are different facts. For an image or build change, run the produced artifact, not only the build.
- **Identity** — any input that names a wallet, user, or principal. → Confirm it is authenticated upstream and that no new path trusts it unverified.

## Value sub-agent

Loss of funds and loss of authority. The prompt carries: the diff command and commit list, the spec file path, the tooling output, the universal lenses below plus each matching domain file's lenses pasted in full, and this brief:

> Start with the entry map each domain file asks for (Solana: the raw-input map; EVM: the external-call map; web: the trust-boundary map). Then a value-flow map: every path where lamports, tokens, ETH, NFTs, rent, fees, credits, or authority move or change hands. For each path: source, destination, amount, and who controls each of the three. Then list every changed instruction, function, handler, endpoint, or job, and for each one enumerate every account, address, or input it takes. Work the entry map and the input constraints before any arithmetic: a math error costs a share, a forwarded authority costs the vault. Check every constraint on those inputs whether or not the constraint line changed; a new caller of an unchanged helper inherits every check the helper lacks. Report every hit as: `file:line`, the lens name, the attacker or failure (who, holding what, sends what), the outcome (what they gain or the protocol loses), and the minimal fix. Read the program, the contract, the IDL or ABI, the schema, and the callers; the diff alone answers no lens here. Report only what you traced to an outcome; list unverified suspicions in one line each at the end, labelled as such. Never omit a suspicion for lack of space. Rank: critical (funds or authority lost), high (funds stuck or wrong amount), medium, low. No length cap on hits; the suspicions list stays under 300 words.

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

## Triage

Main loop. Every finding from all four axes ends **confirmed**, **dismissed**, or (Value only) **open**, each with a stated reason.

A defect is confirmed when the main loop has read the code and can restate the failure scenario in its own words: the input or state, then the wrong outcome. A hit the main loop cannot restate is dismissed as unverified.

A Value finding is different. Confirmed the same way, but a Value hit or suspicion the main loop cannot confirm is not dismissed: it goes to the report's **open suspicions** section with what was checked and what was not. Silence is never the answer to a possible loss of funds. Value findings are defects; the +EV bar below does not apply to them.

A non-defect finding (Standards smell, Spec scope note, cleanup) is confirmed only when it clears the **+EV bar**: the change makes the logic easier to follow, or makes it measurably more efficient. Stylistic, lateral, or "how I would have written it" changes are below the bar and change nothing.

Every confirmed finding gets one resolution:

- **Fix the code** — the default.
- **Fix the claim** — when the code is right and the description, schema doc, or comment is wrong. Say which claim.
- **Leave as is** — with the reasoning that closes it (a bound that holds, a deliberate alignment). The report carries the reasoning so the decision is visible, not an oversight.

## Fix re-review

After the fixer's edits land, read its diff against the five lenses that a fix most often trips: **Safe side**, **Failure mode**, **Neighbours**, **Pinned**, **Legit path**. A fix that flips a default, adds a guard, or adds a test is where a second round with a human reviewer usually starts. Done when every fixer edit passes those five, or one follow-up `fixer` run has landed the corrections.

## Report shape

A severity table first: one row per confirmed finding with file, axis or lens, severity. Then five lists: fixed in code, fixed in the claim, left as is with the reasoning, open suspicions (Value only, each with what was checked and what was not), dismissed with the reason. Then lint and test results. Then a coverage line: domains, files read, files not read, tools run, tools unavailable. On the zero path, the open suspicions and dismissed lists plus the coverage line.
