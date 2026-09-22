# Review core

Shared reference for `review`. `SKILL.md` owns the input (a base or a PR) and the output (edits or a GitHub review). Everything between lives here, except the Defects and Value briefs and lenses, which live in `briefs.md` and are read by the sub-agents only. The main loop's context is the scarce resource: tool output, spec text, lens text, and full reports go to scratchpad files and reach the main loop as paths, and the main loop never runs the bare `git diff` (`--shortstat` for size, `--name-only` for classification).

## Four axes

Four sub-agents dispatched in one batch, each with its brief in `briefs.md`: **Standards** (repo standards plus a smell baseline), **Spec** (does the diff do what the spec says), **Defects** (logic), and **Value** (loss of funds, authority, or data). Standards and Spec are the two axes of `mattpocock-skills:code-review`, carried in-house so the plugin text and its aggregation never enter the main loop. Neither of them hunts defects. Every dispatch prompt names `briefs.md` and the matched domain files by path; nothing from them is pasted. The two are separate agents so that neither budget crowds the other, and so that a bug can be found twice.

When the diff exceeds about 1500 lines, split the Defects and Value agents by package or top-level directory, one agent per slice, so no agent skims. Each slice's brief names the files it owns.

## Domain

The diff decides which lenses apply. Before dispatch, the scout classifies every changed file into one or more domains; the sub-agents read each matching `domain-*.md` in this folder, the main loop does not. A diff can hit several domains (a monorepo with a program, its contracts, and a frontend). A file with no chain signal is `web`.

| Domain   | Signal                                                                                                                                                                                                                                                                                                                                                                                                                          | File                 |
| -------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------- |
| solana   | Rust in a crate whose `Cargo.toml` depends on `anchor-lang`, `solana-program`, `pinocchio`, or an `spl-*` crate; an IDL under `target/idl`; TS or Rust that imports `@coral-xyz/anchor`, `@solana/web3.js`, `@solana/kit`, or `solana-client` and builds transactions                                                                                                                                                           | `domain-solana.md`   |
| evm      | a `.sol` file, `foundry.toml`, `hardhat.config.*`; TS or Python that imports `ethers`, `viem`, `wagmi`, or `web3` and sends transactions                                                                                                                                                                                                                                                                                        | `domain-evm.md`      |
| web      | everything else: backend, frontend, jobs, infra, config                                                                                                                                                                                                                                                                                                                                                                         | `domain-web.md`      |
| database | additive, on top of `web` or a chain domain: a `.sql` file; a file under a `migrations` directory owned by drizzle, prisma, knex, sequelize-cli, sqlx, or Ecto (`priv/repo/migrations/*.exs`); a `schema.prisma`, `drizzle.config.*`, or drizzle schema file; TS, JS, Rust, or Elixir that imports `drizzle-orm`, `@prisma/client`, `kysely`, `knex`, `sequelize`, `pg`, `postgres`, `sqlx`, or `Ecto.Query` and builds queries | `domain-database.md` |

Each domain file holds its tooling, its entry map (what the Value agent enumerates first), its Value lenses, its Defects lenses when it has any, and its additions to the Pinned lens. The scout runs the tooling; the Value and Defects agents read the lenses. Client code that talks to a chain is both its chain domain and `web`: the Mirror lens works the client side, the chain lenses the program side. Name the domains in the report's coverage line.

## Tooling

The scout runs whichever of these the repo supports, one output file per tool in the scratchpad: `cargo clippy --all-targets`, `cargo audit`, `npm audit` (or `pnpm audit`), `gitleaks detect` or `trufflehog filesystem` on the diff, and `squawk` on changed `.sql` files. Then the tooling of each domain file. Then, in PR mode, `gh pr checks <n>`; and when there is no CI or no check runs the tests, and always in branch mode, one run of the project's test command, output saved like any other tool. That run is the gate's execution evidence, and the only test run the review pays for: the fixer proves its own edits with narrower commands. Everything else here is deterministic and near free. Every review prompt carries the paths. Record in the report which ran and which were unavailable, from the scout's manifest.

A tooling hit on a changed line (an unused import, a lint error, a type error the diff introduced) is a confirmed finding on its own, resolved as "fix the code" and handed to the fixer as an item without a verifier. No agent re-derives what a tool already reported, and the Standards brief skips it.

The scout also lists the standards sources for the Standards brief and resolves issue references in the commit messages (`#123`, `Closes #45`) through `docs/agents/issue-tracker.md` when that file exists, folding the issue text into the spec file. No agent asks the user anything; a missing spec is written into the spec file as "no spec".

## Triage

Main loop. First fold hits that share file, line, and mechanism across axes into one, keeping the higher severity and both axis names. Then every finding ends **confirmed**, **dismissed**, or (Value only) **open**, each with a stated reason.

Defects and Value hits are verified by **verifier** agents (`general-purpose`, `model: "opus"`; `model: "fable"` when the group holds a Value hit), so the code reading happens outside the main loop. Group the folded hits by file; one verifier per file, and when more than six files have hits, one per top-level directory. Each prompt carries that group's hits with their evidence packets, the spec path, the path of `briefs.md`, and this brief:

> Read the code each hit points at and whatever it depends on; the packet is a starting point, not the evidence. For each hit return one of: CONFIRMED with the failure scenario restated in your own words, the input or state then the wrong outcome, under 80 words; DISMISSED with the specific reason the scenario cannot happen; or, for a Value hit only, OPEN with what you checked and what you could not. Quote the `file:line` that settles each verdict. Nothing else.

The main loop reads the verdicts, not the code. A CONFIRMED verdict whose restatement the main loop cannot follow is sent back in the same verifier's next message once, then dismissed. Standards and Spec findings are not verified this way; they go straight to the +EV bar below. The full reports stay in their scratchpad files and are read only when a verdict needs its coverage context.

A Value finding is different. Confirmed the same way, but a Value hit or suspicion the main loop cannot confirm is not dismissed: it goes to the report's **open suspicions** section with what was checked and what was not. Silence is never the answer to a possible loss of funds or data. Value findings are defects; the +EV bar below does not apply to them.

An open suspicion is closed this round when the repo can close it. Before it is written as open, name the source that would settle it: a release workflow, a deploy script, a manifest, a config, a fixture. When that source is in the repo, a verifier reads it now and returns CONFIRMED, DISMISSED, or OPEN with the `file:line`. Only a suspicion whose answer lives outside the repo (a mainnet simulation, a production measurement, a team's process) stays open, and it goes to the state file so the next round carries it instead of re-deriving it. In a delta round a carried suspicion is re-verified only when the delta touches a file it names; otherwise it is copied forward unchanged.

A non-defect finding (Standards smell, Spec scope note, cleanup) is confirmed only when it clears the **+EV bar**: the change makes the logic easier to follow, or makes it measurably more efficient. Stylistic, lateral, or "how I would have written it" changes are below the bar and change nothing.

Every confirmed finding gets one resolution, chosen in the main loop:

- **Fix the code** — the default.
- **Fix the claim** — when the code is right and the description, schema doc, or comment is wrong. Say which claim.
- **Leave as is** — with the reasoning that closes it (a bound that holds, a deliberate alignment). The report carries the reasoning so the decision is visible, not an oversight.

## Fix re-review

After the fixer's edits land, read its diff against the five lenses that a fix most often trips: **Safe side**, **Failure mode**, **Neighbours**, **Pinned**, **Legit path**. A fix that flips a default, adds a guard, or adds a test is where a second round with a human reviewer usually starts. Done when every fixer edit passes those five, or one follow-up `fixer` run has landed the corrections.

## Report shape

A severity table first: one row per confirmed finding with file, axis or lens, severity. Then five lists: fixed in code, fixed in the claim, left as is with the reasoning, open suspicions (Value only, each with what was checked and what was not, and whether it was carried from a prior round), dismissed with the reason. Then the execution evidence: CI checks per name as pass, fail, pending, or "no CI"; the scout's test run as pass or fail with the failing lines when any; then lint and test results per command from the fixer's return, with the failing lines when any, and the entries marked unproven. Then a coverage line: round (full or delta since `<last>`), domains, files read, files not read, axes skipped, tools run, tools unavailable. On the zero path, the open suspicions and dismissed lists, the execution evidence, and the coverage line.
