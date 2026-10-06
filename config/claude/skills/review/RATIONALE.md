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

## Spec

The PR body is the strongest spec a PR has: every sentence in it is a claim the Spec and Contract lenses can test.

## Value

Work the entry map and the input constraints before any arithmetic: a math error costs a share, a forwarded authority costs the vault. A medium or low Value hit stands on its quoted `file:line` because Defects runs the failures that lose nothing.

## PR mode

Authors apply fences and skim prose: a finding that leaves Draft without its own code block comes back next round as a new finding, at full price.

## Lenses

Lens edits start from `~/.claude/review-misses.md`, which `address-review` appends to when a human finds what a round missed. Group its entries by lens; a lens with repeat misses needs sharper wording, and "no lens" entries that share a class need a new lens. The briefs repeat the "leans on" rule from `verdicts.md` on purpose (see Agents and models).

## Domains

- CI: a release or deploy workflow is where a key, a token, or an upgrade authority meets code the public can influence, so it gets the same scrutiny as the program it ships.
- Solana lens sources: the sealevel-attacks catalogue, the Neodyme pitfalls, and the Cashio, Wormhole, Jet, Solend, Candy Machine, and SPL lending post-mortems collected at github.com/sannykim/solsec, and the Token-2022 checklist in the solana-foundation/solana-dev-skill security reference.
