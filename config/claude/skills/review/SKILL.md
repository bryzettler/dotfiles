---
name: review
description: Four-axis review of a branch against develop (fixes applied) or of a PR (inline suggestions, or an LGTM approval when clean).
disable-model-invocation: true
---

# review

One review, two inputs, two outputs. Read `review-core.md` in this folder first: it holds the domain table, the Defects and Value briefs and universal lenses, the tooling step, the triage rules, the fix re-review, and the report shape. The `domain-*.md` files beside it hold the chain-specific and web-specific lenses, and only the ones the diff hits are read. Everything below is what differs by mode.

## Mode

Resolve the mode from the argument before anything else:

| Argument | Mode | Target |
| --- | --- | --- |
| none | branch | `git diff <base>...HEAD` plus uncommitted changes, `<base>` = `origin/develop`, else `develop`, else the base of the open PR for this branch (`gh pr view --json baseRefName`), else `origin/HEAD` |
| a ref (`main`, `origin/release`, a sha) | branch | as above with `<base>` = the ref |
| `pr` | pr | the PR open for the current branch (`gh pr view --json number`) |
| `pr <n>`, `#<n>`, a bare number, a PR URL | pr | that PR |

Branch mode's output is edits in the working tree. PR mode's output is a GitHub review on the PR; the checkout is scratch and the PR branch stays as the author left it.

## Shared steps

1. **Pin** — resolve the fixed point and confirm the diff is non-empty. Write the spec to a scratchpad file. Classify the changed files by the domain table in `review-core.md` and read each matching `domain-*.md`. Run the tooling from `review-core.md` and from each matched domain file, and save the output to the scratchpad. Done when the fixed point resolves, the spec file exists, every changed file has a domain, and the tooling output is saved or each tool is noted as unavailable.
   - _branch_: `git rev-parse <base>` succeeds. Spec: the `.scratch/<feature>/` PRD and issue files that match the branch when they exist, else the full commit messages from `git log <base>..HEAD`.
   - _pr_: read `gh pr view <n> --json baseRefName,headRefOid,body`. Review from a checkout at the PR head: the current worktree when `HEAD` is that sha, else a throwaway worktree in the scratchpad from `git fetch origin pull/<n>/head`. Fetch the base and take `origin/<base>` as the fixed point. Spec: the PR body, then the full commit messages from `git log origin/<base>..HEAD`. The PR body is the strongest spec a PR has, since every sentence in it is a claim the Spec and Contract lenses can test. Done when the checkout is at `headRefOid`.

2. **Review** — invoke `mattpocock-skills:code-review` with the fixed point as its argument and the spec file as its spec argument. Dispatch the Defects and Value sub-agents from `review-core.md` in the same batch as code-review's two, each brief carrying the universal lenses plus the lenses and Pinned additions of every matched domain file. Done when four reports are in hand.

3. **Triage** — per `review-core.md`. Done when every finding across the four reports is confirmed, dismissed with a reason, or held as an open Value suspicion, and each confirmed finding has a resolution.

4. **Gate** — zero confirmed findings and zero open Value suspicions is a clean diff.
   - _branch_: write the report and stop. Silence is the deliverable. An open Value suspicion is not silence: the report leads with it.
   - _pr_: all four axes agree the change is valid, so approve it: `gh pr review <n> --approve --body "LGTM"`. Write the report and stop. Zero confirmed findings but one or more open Value suspicions: no approval. Submit `gh pr review <n> --comment --body <file>` whose body asks the one question per suspicion that would close it, written to the "Public text" standard, then write the report and stop.

Then continue with the steps for the mode. Both modes dispatch `fixer` once: it starts with no context and cannot ask, so every judgement call is settled before dispatch and the prompt states the change, not the reasoning. Per item: file and line, what is wrong, and the specific minimal edit. Plus the project's lint and test commands.

## Branch mode

5. **Fix** — one `fixer` run carrying every code and claim resolution. Done when every item has an edit or a reported mismatch, and lint and tests pass or the failures are shown to pre-exist on `<base>`.

6. **Re-review the fix** — per `review-core.md`. Done when every fixer edit passes the five lenses, or one follow-up `fixer` run has landed the corrections.

## PR mode

5. **Draft** — one `fixer` run in the checkout. It edits the code as if fixing it, runs the project's lint and tests, then converts each change into a suggestion anchored to the PR diff. Its deliverable is a JSON file in the scratchpad, one entry per comment:

   ````json
   {
     "path": "...",
     "start_line": 12,
     "line": 14,
     "body": "<one-line reason>\n\n```suggestion\n<replacement for lines 12-14>\n```"
   }
   ````

   Every entry starts as a `suggestion` fence. A plain comment is the exception, reached only through the rules under "Anchoring suggestions", and the JSON carries `"plain_reason"` on any entry without a fence so the main loop can check each one before posting. An entry is anchored where the fix lands, not where the symptom shows: if the finding was spotted at one diff line but the edit belongs on another `+` line, the suggestion goes on the latter. A "leave as is" resolution posts nothing; its reasoning lives in the terminal report.

   The prompt also carries the PR head sha, the base branch, the checkout path, the lint and test commands, the JSON path and shape, the anchoring rules, and the "Public text" standard for comment bodies. Done when every item has an entry and lint and tests pass on the edited checkout, or a failure is shown to pre-exist at the PR head.

6. **Re-review the fix** — per `review-core.md`, against the fixer's diff in the checkout. Done when every drafted change passes the five lenses, or one follow-up `fixer` run has corrected the entries.

7. **Redact** — read every `body` in the JSON as a stranger on the internet would, and rewrite it to the "Public text" standard. Done when each body's reason is stated purely in terms of the code visible in the PR diff.

8. **Post** — list existing review comments (`gh api repos/{owner}/{repo}/pulls/<n>/comments`) and drop any entry whose marker, path and line already appear, so a rerun adds only new findings. Submit one review with `gh api repos/{owner}/{repo}/pulls/<n>/reviews --input <file>` where the payload is `{ "event": "COMMENT", "body": "<one-line summary>", "comments": [...] }` and every comment `body` starts with the marker `<!-- review -->`. `COMMENT` here and `APPROVE` only at the gate are the only two events this skill submits. Done when the API returns the review URL.

## Cost budget

Four review sub-agents (more only when the diff is split by size per `review-core.md`), one `fixer`, at most one follow-up `fixer`, the Defects and Value agents dispatched with `model: "fable"` and every other agent with `model: "opus"`, including the two that `code-review` has the main loop spawn. Fable also stays in the main loop for the judgement steps. Triage, the fix re-review, and redaction stay in the main loop. `fixer` is the only agent that edits; in PR mode it edits the checkout only, and nothing is pushed. The four axes are the whole review.

## Public text (PR mode)

The repo may be open source, so every review body and comment body is public the moment it posts. Write each one from the PR diff alone: name the code, the line, and the property of the code that makes the change worth taking. Context that came from anywhere else stays in the terminal report: the reviewer's own memory files, `.scratch/`, `CONTEXT.md`, session history, and anything learned about how the code runs in production. Concretely, the terminal report is the only home for: incident and outage details, log excerpts, replica counts, tick rates, hostnames, RPC endpoints, dashboards, Sentry or ticket links, wallet or program addresses, key names, multisig details, deploy schedules, and the names of people or teams. A reason that needs any of those to make sense is rewritten to lean on the code instead; where the code alone cannot carry it, post the suggestion with a shorter reason rather than the sensitive one.

Disclosure is a separate limit. A Value finding whose exploit path also exists in code that is already deployed (a pre-existing missing check the PR now makes reachable, or a helper the PR reuses) is a live vulnerability, and a PR comment describing it is public disclosure. Post only the fix with a neutral one-line reason ("bind the destination to the signer"), never the attack. The attack, the affected deployed paths, and the urgency go to the terminal report and to the user, who decides how to notify the team.

## Anchoring suggestions (PR mode)

GitHub only accepts inline comments on lines that appear in the PR diff, on `side: "RIGHT"`, and a `suggestion` block replaces exactly `start_line`..`line` (single line: omit `start_line`), both inside one hunk. Fixes that land outside the diff go on the nearest diff line as a plain comment with a fenced code block and no `suggestion` fence, saying where the change belongs.

GitHub also refuses to apply a `suggestion` whose `start_line`..`line` range has `-` lines interleaved with it in the hunk ("Applying suggestions on deleted lines is currently not supported"). Before emitting a `suggestion` fence, check the hunk: if any deleted line sits between the added lines of the range, the range must shrink to a run of pure `+` lines with no deletions between them. Before giving up on a fence, look for a smaller committable fix that fits such a run: the part of the change that stands alone (a hoisted binding, a field assignment on the line that already mutates the account) goes out as a `suggestion`, and the rest of the idea is stated in the body as prose. Only when no pure-`+` run can carry a self-contained edit does the fix go out as a plain fenced code block (with the language tag) headed by the exact line range it replaces. A one-line suggestion on a `+` line is always safe.

Multi-region refactors (a test fixture rewritten to use an existing encoder, helpers replaced by a shared package) are the other legitimate plain comment: the fence would have to replace more than one hunk. Name the helper and the file that already uses it; that is the whole body.

Before posting, count fences: every entry without a `suggestion` fence must have a stated reason from this section, and the report lists them.

## Report

The shape in `review-core.md`. PR mode adds: the review URL and whether it was an approval or a comment review, and per posted comment `file:line`, a one-line summary, and `suggestion` or `plain: <reason>`.
