# review: rationale

Why the skill is shaped this way. No step reads this file. Read it before you change a step.

## Agents and models

- Standards and Spec are the two axes of `mattpocock-skills:code-review`, carried in-house so the plugin text and its aggregation stay out of the main loop. They are separate agents so that neither budget crowds the other, and so that a bug can be found twice.
- Defects and Value run as `tracer` (fable, high): their job is to trace a change to its second- and third-order consequences. A Value verifier runs as `verifier` (fable, medium): the packet narrows the read. The scout, Standards, Spec, the revert-check verifier, and `fixer` run on `opus`: their job is execution against a fixed procedure. Routing lives in `~/.claude/MODEL_ROUTING.md`.
- A Defects hit gets no verifier: the tracer proved it by a run or a quoted `file:line`, and a second fable read of the same lines adds cost, not evidence.
- Splitting Defects and Value above about 1500 lines keeps any one agent from skimming. Defects splits by directory because its lenses read one function at a time. Value splits by flow because a loss of funds lives between layers: on #1345 the hook and the API quoted one cron job two ways, and directory slices handed each side to a different agent, so no Value agent saw both.

- The "leans on" rule for older code sits in `verdicts.md` and again in `brief-defects.md` and `brief-value.md`, on purpose: the tracers do not read `verdicts.md`, and without it they file a moved helper's bug as a pre-existing suspicion.

## Cost budget

One scout, four axes (more when split by size, fewer in a delta round or when an axis is skipped), at most two Value verifiers, one revert-check verifier at triage and one at the fix re-review, one `fixer`, at most one follow-up `fixer`. A delta round reviews the change since the last round, not the PR.

## Execution

- A clean diff is not yet a proven one: reading is not running. The axes run only narrow proofs, never the test suite, so the gate needs execution evidence.
- The main loop starts the test runs in the same message as the axes, so the run overlaps the review instead of delaying it. That run is the only test run the review pays for: the fixer proves its own edits with narrower commands.
- The revert check exists because reading a test is not running it.
- A fix that flips a default, adds a guard, or adds a test is where a second round with a human reviewer usually starts, so the fix re-review runs the fix, not only reads it.
- An audit on an unchanged dependency tree repeats the base's result, and a workspace audit can take minutes: hence audits only on a manifest or lockfile change.
- The scout's tooling is deterministic and near free.
- Branch mode reviewed "HEAD plus uncommitted changes", a state with no sha. `ts-signatures.mjs` reads both sides with `git show`, so it saw none of the uncommitted work: on the Delta review of an `address-review` fix diff it compared HEAD with HEAD. Hence `tools/snapshot.py`: the scout names the working tree as a commit, and the revert check builds its worktree from one in place of applying a diff by hand.

## Spec

The PR body is the strongest spec a PR has: every sentence in it is a claim the Spec and Contract lenses can test.

## Value

Work the entry map and the input constraints before any arithmetic: a math error costs a share, a forwarded authority costs the vault. A medium or low Value hit stands on its quoted `file:line` because Defects runs the failures that lose nothing.

## PR mode

Authors apply fences and skim prose: a finding that leaves Draft without its own code block comes back next round as a new finding, at full price.

## Lenses

Lens edits start from `~/.claude/review-misses.md`, which `address-review` appends to when a human finds what a round missed. Group its entries by `Seen:` first: a miss seen as cleared or unverified is fixed in the brief rule or triage step that let it through. Group the rest by lens; a lens with repeat misses needs sharper wording, and "no lens" entries that share a class need a new lens. The briefs repeat the "leans on" rule from `verdicts.md` on purpose (see Agents and models).

## Clearing, not wording

A blind second round on #1345 ran after the lenses were patched from the human review of the first, and seven of the eleven logged classes recurred with their lens in place. None was a wording miss:

- Agents cleared the item with a reason the briefs forbid in spirit: "rename only", "unchanged from base", "the changeset documents it". Hence the clear-reason rule in both tracer briefs.
- The line reached the axis without the lens: Defects quoted a float round trip, but Arithmetic lived in the Value brief; Value quoted a truthy probe, but Falsy branch lived in the Defects brief. Hence `lenses-shared.md`.
- A slice owned every send path and built no parity table. Hence the parity table in the return.
- One worked example covered one arm of a funding expression; the double count lived on the other. Hence one example per arm.
- Triage step 5 sent hits but no suspicions to the verifiers, so four items sat unread in suspicion lists. Hence suspicions to verifiers.
- The Contract lens said "major", which is wrong under `0.x`; the real miss was a needless break.

The misses log could not show any of this: it named the lens that should have fired, and the returns that showed the lens firing and being cleared were not kept. Hence the kept returns and the `Seen:` field in `address-review`.

## Audited clearances

A third blind round on #1345 (head `1d50617e8`) ran with every rule above in place, and the human reviewer still found eleven classes the round did not. Every one sat in a "checked, nothing" line, cleared with a reason the briefs already forbid ("same as develop", "README documents it") or with a restatement of the code ("per-call > env > node ∩ signer" is the reorder, written as its own clearance). The rule existed; nothing read the lines against it. Triage read coverage lists only "when a verdict needs that context", which is never.

- A rule an agent applies to its own work is a request. A check the main loop runs on the output is a gate. Hence the coverage audit in `triage.md`, run by `tools/audit-coverage.py`, not by reading.
- The audit can only check what the line states. Hence the typed fields in `return-rule.md`: `sig:`, `arms:`/`examples:`, `probe:`, `order:`, `parity:`, `lenses:`. A field forces the check, and a field that shows the hit makes the clearance self-refuting.
- Some classes need no judgement at all. A sync-to-async export and an async body with no await are syntax: `tools/ts-signatures.mjs` finds them in the scout, and triage confirms them as tooling hits.
- A verifier dismissed a traced wrong outcome because the README documented it, which `verdicts.md` already forbids. Hence the dismissal check in triage step 6.
- A parity table had the differing cell ("kit throws at compile" beside fallbacks) and called it clean, because the rule named only empty and doubled cells. Hence differing outcomes in Neighbours.
- The Quote lens named the lifecycle steps, but nothing made the return show each step, so `close` was never walked and an account `create` charges for and `close` never refunds went unseen. Hence one coverage line per lifecycle step in `brief-value.md`.
- The round approved nothing and posted nothing, so Persist never ran and `address-review` would have logged "returns not kept". The main loop also condensed returns after a compaction. Hence persist on every gate path, and each axis writes its own return file.
- CI tails were sampled (3 of 23 failing lanes). Hence one tail per failing check in the scout.

## Fix rounds

- PR #1345, round 5979983: 7 of the reviewer's 10 findings sat on lines our own `fix: address review` commit wrote. The Fix re-review mutated guards and call sites only, so a narrowed predicate (`&& k.isSigner`), a sync-to-async export, and a test no workflow ran all passed. Hence revert-check targets for predicate terms, changed signatures, and cited tests, and "a test no workflow runs is green".
- The same round left a changeset sentence false and replied "fixed" to a partial fix. Hence the Claims and Remainder steps in the Fix re-review, and the Contract rule that a changeset sentence holds at the head, not at the commit that wrote it.
- The audit rejected 160 of 288 lines and the main loop then skipped the audit tracers; a real reject (a cache-lag amount line) went with them. Many rejects were noise: a `.changeset/` path matched the forbidden word "changeset", and agents put free text in `sig:` on changesets and workflows. Hence the audit reads only the reason, a non-code item's `sig:` and a `probe:` with no `admits` are WARN, and "no step is skipped for volume". On that round the audit now gives 128 REJECT and 31 WARN. A missing `lenses:` stays REJECT: the field is the check.

## Domains

- CI: a release or deploy workflow is where a key, a token, or an upgrade authority meets code the public can influence, so it gets the same scrutiny as the program it ships.
- Solana lens sources: the sealevel-attacks catalogue, the Neodyme pitfalls, and the Cashio, Wormhole, Jet, Solend, Candy Machine, and SPL lending post-mortems collected at github.com/sannykim/solsec, and the Token-2022 checklist in the solana-foundation/solana-dev-skill security reference.
