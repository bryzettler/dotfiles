# Review core

Shared reference for `review`. `SKILL.md` owns the input (a base or a PR) and the output (edits or a GitHub review). Everything between lives here, except the Defects and Value briefs and lenses, which live in `briefs.md` and are read by the sub-agents only. The main loop's context is the scarce resource: tool output, spec text, lens text, and full reports go to scratchpad files and reach the main loop as paths, and the main loop never runs the bare `git diff` (`--shortstat` for size, `--name-only` for classification).

## Four axes

Four sub-agents dispatched in one batch, each with its brief in `briefs.md`: **Standards** (repo standards plus a smell baseline), **Spec** (does the diff do what the spec says), **Defects** (logic), and **Value** (loss of funds and authority). Standards and Spec are the two axes of `mattpocock-skills:code-review`, carried in-house so the plugin text and its aggregation never enter the main loop. Neither of them hunts defects. Every dispatch prompt names `briefs.md` and the matched domain files by path; nothing from them is pasted. The two are separate agents so that neither budget crowds the other, and so that a bug can be found twice.

When the diff exceeds about 1500 lines, split the Defects and Value agents by package or top-level directory, one agent per slice, so no agent skims. Each slice's brief names the files it owns.

## Domain

The diff decides which lenses apply. Before dispatch, the scout classifies every changed file into one or more domains; the sub-agents read each matching `domain-*.md` in this folder, the main loop does not. A diff can hit several domains (a monorepo with a program, its contracts, and a frontend). A file with no chain signal is `web`.

| Domain | Signal | File |
| --- | --- | --- |
| solana | Rust in a crate whose `Cargo.toml` depends on `anchor-lang`, `solana-program`, `pinocchio`, or an `spl-*` crate; an IDL under `target/idl`; TS or Rust that imports `@coral-xyz/anchor`, `@solana/web3.js`, `@solana/kit`, or `solana-client` and builds transactions | `domain-solana.md` |
| evm | a `.sol` file, `foundry.toml`, `hardhat.config.*`; TS or Python that imports `ethers`, `viem`, `wagmi`, or `web3` and sends transactions | `domain-evm.md` |
| web | everything else: backend, frontend, jobs, infra, config | `domain-web.md` |

Each domain file holds its tooling, its entry map (what the Value agent enumerates first), its Value lenses, and its additions to the Pinned lens. The scout runs the tooling; the Value agent reads the lenses. Client code that talks to a chain is both its chain domain and `web`: the Mirror lens works the client side, the chain lenses the program side. Name the domains in the report's coverage line.

## Tooling

The scout runs whichever of these the repo supports, one output file per tool in the scratchpad: `cargo clippy --all-targets`, `cargo audit`, `npm audit` (or `pnpm audit`), and `gitleaks detect` or `trufflehog filesystem` on the diff. Then the tooling of each domain file. Deterministic and near free. The Defects and Value prompts carry the paths. Record in the report which ran and which were unavailable, from the scout's manifest.

The scout also lists the standards sources for the Standards brief and resolves issue references in the commit messages (`#123`, `Closes #45`) through `docs/agents/issue-tracker.md` when that file exists, folding the issue text into the spec file. No agent asks the user anything; a missing spec is written into the spec file as "no spec".

## Triage

Main loop. First fold hits that share file, line, and mechanism across axes into one, keeping the higher severity and both axis names. Then every finding ends **confirmed**, **dismissed**, or (Value only) **open**, each with a stated reason.

Defects and Value hits are verified by **verifier** agents (`general-purpose`, `model: "fable"`), so the code reading happens outside the main loop. Group the folded hits by file; one verifier per file, and when more than six files have hits, one per top-level directory. Each prompt carries that group's hits with their evidence packets, the spec path, the path of `briefs.md`, and this brief:

> Read the code each hit points at and whatever it depends on; the packet is a starting point, not the evidence. For each hit return one of: CONFIRMED with the failure scenario restated in your own words, the input or state then the wrong outcome, under 80 words; DISMISSED with the specific reason the scenario cannot happen; or, for a Value hit only, OPEN with what you checked and what you could not. Quote the `file:line` that settles each verdict. Nothing else.

The main loop reads the verdicts, not the code. A CONFIRMED verdict whose restatement the main loop cannot follow is sent back in the same verifier's next message once, then dismissed. Standards and Spec findings are not verified this way; they go straight to the +EV bar below. The full reports stay in their scratchpad files and are read only when a verdict needs its coverage context.

A Value finding is different. Confirmed the same way, but a Value hit or suspicion the main loop cannot confirm is not dismissed: it goes to the report's **open suspicions** section with what was checked and what was not. Silence is never the answer to a possible loss of funds. Value findings are defects; the +EV bar below does not apply to them.

A non-defect finding (Standards smell, Spec scope note, cleanup) is confirmed only when it clears the **+EV bar**: the change makes the logic easier to follow, or makes it measurably more efficient. Stylistic, lateral, or "how I would have written it" changes are below the bar and change nothing.

Every confirmed finding gets one resolution, chosen in the main loop:

- **Fix the code** — the default.
- **Fix the claim** — when the code is right and the description, schema doc, or comment is wrong. Say which claim.
- **Leave as is** — with the reasoning that closes it (a bound that holds, a deliberate alignment). The report carries the reasoning so the decision is visible, not an oversight.

## Fix re-review

After the fixer's edits land, read its diff against the five lenses that a fix most often trips: **Safe side**, **Failure mode**, **Neighbours**, **Pinned**, **Legit path**. A fix that flips a default, adds a guard, or adds a test is where a second round with a human reviewer usually starts. Done when every fixer edit passes those five, or one follow-up `fixer` run has landed the corrections.

## Report shape

A severity table first: one row per confirmed finding with file, axis or lens, severity. Then five lists: fixed in code, fixed in the claim, left as is with the reasoning, open suspicions (Value only, each with what was checked and what was not), dismissed with the reason. Then lint and test results, as pass or fail per command from the fixer's return, with the failing lines when any. Then a coverage line: domains, files read, files not read, tools run, tools unavailable. On the zero path, the open suspicions and dismissed lists plus the coverage line.
