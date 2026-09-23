# Domain: CI

Loaded for every file under `.github/workflows/` or `.github/actions/`, and for every script a workflow step runs from the repo. A release or deploy workflow is where a key, a token, or an upgrade authority meets code the public can influence, so it gets the same scrutiny as the program it ships.

## Tooling

`actionlint` over the changed workflow files, and `zizmor` over `.github/` when installed. Then the **secret-scope grep** over every workflow file, changed or not, saved by the scout and passed to the Value agent as a path:

```
rg -n 'secrets\.|GITHUB_TOKEN|permissions:|pull_request_target|workflow_run|head_ref|github\.event\.(pull_request|issue|comment|review|head_commit)|uses:|run:|npm (ci|install)|pnpm (install|i\b)|yarn( install)?$|npx |curl .*\| *(ba)?sh|cache|restore-keys|if: *(failure|always|cancelled)\(\)|timeout-minutes|continue-on-error|steps\.[a-z0-9_-]+\.outputs'
```

## Entry map

The **secret-scope map**: every job that can read a secret, a signing key, a deploy token, or a `GITHUB_TOKEN` with write permission, and under each job every step that runs code: each `run:` script and the files it calls, each `uses:` action with its ref, and each package install with its lockfile. Every line of code on that map runs with the secret in reach. Then the **trigger map**: every trigger, and for each one which fields of the event the author of a PR, issue, or comment controls.

## Defects lenses

- **Step outputs** — every `steps.<id>.outputs.<name>` and `needs.<job>.outputs.<name>` read. → The id and the name exist and the step writes them on the path that reaches the read. A wrong id reads as an empty string, and an empty string in an `if:` is false.
- **Cancel path** — every cleanup step: a buffer close, a lock release, a temp-account teardown. → Its condition covers cancel as well as failure (`always()` or `cancelled() || failure()`, not `failure()` alone), and every job that holds state has `timeout-minutes`. A job killed at the 6-hour default leaves the state behind.
- **Cache** — every `actions/cache` or setup-action cache. → The key changes whenever the cached content should, and `restore-keys` is either absent or matches a key some job writes. A restore key that no job writes is dead config; one that restores a stale tree into a build that trusts it is a defect.
- **Loose match** — every regex, glob, or `startsWith` that selects tags, branches, or paths. → Anchor it and name a string it wrongly admits (`v1.2.3-rc` against `^v\d+\.\d+\.\d+`, `programs/foo-bar` against `programs/foo*`).
- **Change detection** — every script that decides which packages, programs, or crates to build or deploy. → Walk the full transitive dependency set, not one hop, and confirm a change to a shared crate or lockfile selects every dependent. Re-derive the output on a fixture where A depends on B depends on C and only C changed.

## Value lenses

- **Secret scope** — every job on the secret-scope map. → The secret is exposed only to the steps that use it (`env:` on the step, not the job), and no step before or beside it runs third-party code: a package install, `npx`, a post-install script, an unpinned action. A build that needs `pnpm install` runs in a job without the key and hands its artifact to the signing job.
- **Pinning** — every `uses:` and every downloaded tool. → Third-party actions are pinned to a full commit sha, not a tag or branch. Downloaded binaries are pinned by version and checked against a hash. One tool pinned in three places is three places to drift: name the single source.
- **Author-controlled input** — every `${{ github.event.* }}`, `github.head_ref`, or branch name used in a `run:` script, an `if:`, or a skip condition. → The PR author chooses it. Inline in `run:` it is shell injection; pass it through `env:` and quote it. As a skip or gate condition it lets the author choose the gate. `pull_request_target` and `workflow_run` run with secrets on the author's code only when the job checks it out: name the checkout ref.
- **Permissions** — every workflow and job. → `permissions:` is set at the narrowest scope the job needs. A missing block inherits the repository default, which may be write-all.
- **Silent skip** — every verification step: a hash check, a signature check, a verify-build step. → When it cannot run it fails the job or prints a warning the run summary shows. A check that skips quietly on a missing tool passes the deploy it was guarding.

## Pinned additions

A change-detection script: a transitive fixture, and a change to a shared file. A tag or path matcher: the near-miss string it must reject. A cleanup step: a test or a documented dry run of the cancel path. A workflow script with unit tests (`node --test`, `pytest`): some workflow runs them on every PR; a test suite no CI job runs pins nothing.
