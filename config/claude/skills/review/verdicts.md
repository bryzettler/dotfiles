# Review: verdicts

The verifier brief and the verdict vocabulary for `review` and `address-review`. `verifier` agents read the whole file. Each main loop reads it at its triage or decide step.

## Verifier brief

You judge hits against the code. You edit nothing. The prompt carries the hit ids and where to find them (evidence packets, or a findings file), and the spec or PR body path.

Read the code each hit points at and whatever it depends on. The packet or the reviewer's comment is where to start reading, not the evidence. A hit may be right about a different line than the one it names; the code settles it. For an outdated anchor, find where that code lives now at `HEAD`. When the prompt names a head sha, read `git log <head sha>..HEAD`: a local commit may already resolve a hit.

Return one verdict per hit id, with the `file:line` that settles it quoted, and nothing else.

## Verdicts

- **CONFIRMED** `<kind>` — the scenario in your own words, under 80 words: the input or state, then the wrong outcome. Then the minimal edit as file, line, and replacement. When the hit carries a `suggestion` fence, say whether it applies verbatim at `HEAD`, and give the corrected edit when it does not. Add `scope: out` when the fix needs code the diff does not touch, a new design, or a public interface change. Kinds:
  - `defect` — the code is wrong.
  - `claim` — the code is right, and a comment, doc, schema doc, or the PR body says otherwise. Quote the wrong text and give its corrected wording.
  - `spec` — the code does what the spec or PR body says, and the outcome is wrong, or the choice is a product call. Give the spec line and the one question the author answers.
  - `design` — no single edit fixes it: who can reach a key, which component owns a write, a trust boundary in the wrong place. Give the smaller design that removes it.
  - `nit` — a lateral preference (naming, ordering, style). Say whether a repo convention (a lint rule, a style guide, the dominant pattern in sibling files) supports or contradicts it, and give the edit.
- **DISMISSED** — the specific reason the scenario cannot happen, or how the hit misreads the code.
- **OPEN** — only where funds, authority, or data can be lost: what you checked and what you could not.
- **FIXED** — `address-review` only: a local commit or a later line already resolves it. Name the sha.
- **ANSWER** — `address-review` only: the hit asks a question and no change follows. The answer, from the code, in under 60 words.

## Rules for every verdict

- **Two reasons never dismiss a traced wrong outcome:** that the spec asks for the behaviour (that is CONFIRMED `spec`), and that the flaw predates the diff.
- **Pre-existing is a finding when the diff leans on it:** the diff calls it, moves or extracts it, widens who reaches it, or makes an outcome depend on it. When no path in the diff reaches it, quote the `file:line` that shows that. In `review` that is DISMISSED as pre-existing. In `address-review` it is CONFIRMED `defect` with `scope: out`, because a reviewer raised it.
- **The spec is a claim, not a verdict.** A spec that accepts a failure path ("keep the old value and warn", a listed known gap) is checked for its end state: what the next run, retry, or restart does with that state. An end state the spec did not name is a new finding.

## The +EV bar

The main loop applies this bar to every finding that is not a defect: a Standards smell, a Spec scope note, a `nit`, a cleanup. The finding holds only when the change makes the logic easier to follow, or measurably more efficient. A stylistic, lateral, or "how I would have written it" change is below the bar. So is an error arm for a `checked_div` or `checked_rem` by a nonzero constant, which cannot fail. Value findings are defects, and the bar does not apply to them. A pattern that repeats across files is one finding, and its comment lists every site.
