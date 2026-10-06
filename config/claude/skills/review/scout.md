# Review: scout

You pin the review so the axes start from files, not from a search. Ask the user nothing: write a missing spec into the spec file as "no spec".

## Steps

1. **Fixed point and checkout** — by mode, below. Confirm the diff is non-empty.
2. **Spec** — write it to `<scratchpad>/spec.md`, by mode, below. Resolve issue references in the commit messages (`#123`, `Closes #45`) through `docs/agents/issue-tracker.md` when that file exists, and fold the issue text into the spec file.
3. **Classify** — give every changed file one or more domains from the Domain table.
4. **Tooling** — run the Tooling below, one output file per tool in the scratchpad.
5. **Scopes and commands** — decide the Standards and Value scopes, find the standards sources, the lint and test commands, and the backend commands.
6. **Return** the manifest.

When the prompt names `<last>`, or Prior round below finds one, this is a delta round: also do the Pin section of `delta-round.md` in this folder.

## By mode

**Branch.** `git rev-parse <base>` succeeds. The spec is the `.scratch/<feature>/` `SPEC.md` or `PRD.md` and the issue files that match the branch, else the full commit messages from `git log <base>..HEAD`. When the prompt names spec files (tickets, a PRD), write those into the spec file instead of searching. When the prompt names a follow-ups file (claims the author left unverified), copy it to `<scratchpad>/prior.md`.

**PR.** `gh pr view <n> --json baseRefName,headRefOid,body`. Check out the PR head: the current worktree when `HEAD` is that sha, else a throwaway worktree in the scratchpad from `git fetch origin pull/<n>/head`. Give a throwaway worktree a `node_modules` symlink from the main checkout for the root and for every workspace package that has one, so tests resolve cross-package imports. Fetch the base and take `origin/<base>` as the fixed point. The spec is the PR body, then the full commit messages from `git log origin/<base>..HEAD`. Done when the checkout is at `headRefOid`.

**PR, prior round.** Look for an earlier round on this PR: the newest review by the current user (`gh api user --jq .login`) whose inline comments start with `<!-- review -->`, and the state file `~/.claude/review-state/<owner>__<repo>__<n>.md`.

- Neither exists: this is round one.
- A prior review exists: its `commit_id` is `<last>`. When `<last>` equals `headRefOid` and the PR body is unchanged, return only "nothing new since `<last>`".
- `git fetch origin <last>` (a force-pushed head is still fetchable by sha). When that fails, run a full review and say so in the manifest. Otherwise it is a delta round.

## Domain

A diff can hit several domains. Client code that talks to a chain is both its chain domain and `web`. A file with no chain or CI signal is `web`.

| Domain   | Signal                                                                                                                                                                                                                                                                                                                                                                                                                                                               | File                 |
| -------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------- |
| solana   | Rust in a crate whose `Cargo.toml` depends on `anchor-lang`, `solana-program`, `pinocchio`, or an `spl-*` crate; an IDL under `target/idl`; TS or Rust that imports `@coral-xyz/anchor`, `@solana/web3.js`, `@solana/kit`, or `solana-client` and builds transactions; a script or workflow that decodes a program's accounts or IDL, or builds, verifies, or deploys a program (`anchor build`, `anchor idl`, `solana program`, `solana-verify`, a Squads proposal) | `domain-solana.md`   |
| evm      | a `.sol` file, `foundry.toml`, `hardhat.config.*`; TS or Python that imports `ethers`, `viem`, `wagmi`, or `web3` and sends transactions                                                                                                                                                                                                                                                                                                                             | `domain-evm.md`      |
| ci       | additive, on top of `web` or a chain domain: a file under `.github/workflows/` or `.github/actions/`, and every repo script a workflow step runs                                                                                                                                                                                                                                                                                                                     | `domain-ci.md`       |
| web      | everything else: backend, frontend, jobs, infra, config                                                                                                                                                                                                                                                                                                                                                                                                              | `domain-web.md`      |
| database | additive, on top of `web` or a chain domain: a `.sql` file; a file under a `migrations` directory owned by drizzle, prisma, knex, sequelize-cli, sqlx, or Ecto (`priv/repo/migrations/*.exs`); a `schema.prisma`, `drizzle.config.*`, or drizzle schema file; TS, JS, Rust, or Elixir that imports `drizzle-orm`, `@prisma/client`, `kysely`, `knex`, `sequelize`, `pg`, `postgres`, `sqlx`, or `Ecto.Query` and builds queries                                      | `domain-database.md` |

## Tooling

Run whichever of these the repo supports:

- `cargo clippy --all-targets`.
- `gitleaks detect` or `trufflehog filesystem` on the diff.
- `cargo audit` only when the diff changes a `Cargo.toml` or `Cargo.lock`; `npm audit` (or `pnpm audit`) only when it changes a `package.json` or a JS lockfile.
- The Tooling section of each matched domain file. Save each domain grep's output as its own file: its path goes to the Value agent.
- PR mode: `gh pr checks <n>`.

**Test runs.** When there is no CI or no check runs the tests, and always in branch mode, the gate needs one run of the project's test command, plus the test tooling a domain file names (`anchor test`, `forge test`). Run none of them: return each as command, working directory, and output path. The main loop starts them.

## Scopes and commands

- **Standards scope** — run when a `CODING_STANDARDS.md`, `CONTRIBUTING.md`, `.cursorrules`, or `.cursor/rules/*` covers the language of a changed file; else skip.
- **Standards sources** — `CODING_STANDARDS.md` and every file it points to, `CONTRIBUTING.md`, `AGENTS.md`, `CLAUDE.md`, `.cursorrules`, `.cursor/rules/*`, `docs/agents/*`.
- **Value scope** — run when the diff can reach funds, keys, authority, or stored data: a chain or database domain, web code on a trust-boundary grep hit, or a CI file that reads `secrets.*`, runs on `pull_request_target` or `workflow_run`, or grants a write `permissions:` scope. Else skip, with the one reason.
- **Backend command** — per matched database or chain domain: how to start a disposable instance with the schema or program loaded (a docker Postgres plus the repo's migrate command, a local validator, the repo's bankrun or litesvm script). "none" when it cannot start (`docker info` fails, no validator binary).

## Manifest

Under 300 words:

- fixed point sha, checkout path, spec path ("no spec" when there is none)
- diff line count from `git diff --shortstat`
- one line per changed file with its domains
- tooling output paths, and tools unavailable
- Standards scope and the standards-source paths
- Value scope, run or skip, with the one reason
- the project's lint and test commands, and the backend commands
- execution evidence: in PR mode the `gh pr checks <n>` result, every check named with pass, fail, or pending, or "no CI"; and the test runs, each as command, working directory, and output path
- `prior.md` path when there is one, and the delta-round fields `delta-round.md` adds

Done when the manifest names all of those and every changed file has a domain.
