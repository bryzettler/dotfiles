---
name: distill
description: Turn the user's logged rulings into standing rules, so later runs decide the same way without asking.
disable-model-invocation: true
---

# distill

Skills that ask the user a judgement call log each ruling to `~/.claude/decisions.md`. This skill reads the rulings logged since the last run, finds the **lessons** in them, and, with the user's approval, writes each lesson where the next run reads it. A lesson is a rule that would have produced the ruling with no question asked. The goal is fewer questions and fewer corrections per change.

Plan denials are out of scope: `plannotator-compound` covers them. Review misses are out of scope: `address-review` sends them to `~/.claude/review-misses.md`, which tunes the review lenses.

## Decision log

`~/.claude/decisions.md` is the single source of rulings. A skill appends one line per ruling, in this shape:

```
- <YYYY-MM-DD> · <owner/repo, or the repo path> · <skill> · <the question, in one sentence> → <the ruling> (recommended: <the option the skill recommended>): <the user's reason, or "no reason given">
```

A line `--- distilled <YYYY-MM-DD> ---` marks the end of the rulings that a run of this skill has read. The rulings below the last marker are new. Create the file with a `# Decisions` heading when it does not exist.

## Steps

1. **Read** — the rulings below the last marker. With none, say so and stop.
2. **Find lessons** — group the rulings by the rule they share. A lesson needs two or more rulings that point the same way, or one ruling whose reason states a general rule ("we never X here because Y"). An override (a ruling that differs from its recommendation) is the strongest signal: the current rules got it wrong. A confirmation shows the rules already decide that case, and it supports a lesson only beside an override. A ruling that fits only its own instance is not a lesson. Done when every new ruling is in a lesson or named as instance-only.
3. **Check the targets** — for each lesson, read the files that could hold it: `~/.claude/CODING_STANDARDS.md`; the repo's `REVIEW.md`, `CLAUDE.md`, or `AGENTS.md`; the skill file whose step asked the question (for `implement-tickets`, `tickets.md` or `briefs.md`; for `address-review`, its Outcomes table); and the repo's memory folder, `~/.claude/projects/<repo path with / as ->/memory/`. Drop a lesson a rule already states. A lesson that contradicts a rule becomes a proposal to change that rule, with both texts quoted.
4. **Pick a target** — one per lesson:
   - A code rule for every repo → `~/.claude/CODING_STANDARDS.md`.
   - A code rule for one repo → that repo's `REVIEW.md` when it has one, else its `CLAUDE.md`.
   - A rule about how a skill decides (a tier, an outcome, a follow-up kind) → that skill's file.
   - A working preference that is not a code rule → a `feedback` memory in the repo's memory folder, with **Why:** and **How to apply:** lines.
5. **Ask** — put the lessons to the user with `AskUserQuestion`, at most four per call. Per lesson: the header is `Lesson N`; the question gives the rule in one sentence, the rulings behind it (date, repo, ruling), and the target. The options are `Apply to <target>` first, with `(Recommended)` after its label, then the next-best target, then `Skip`. Write the rule in the target file's voice: active, positive, one rule per sentence.
6. **Apply** — write each approved rule into its target, beside the rules on the same subject. Add a pointer line to `MEMORY.md` for a new memory. Then append `--- distilled <today> ---` to the log.
7. **Report** — one line per lesson: applied (with the target path), skipped, or proposed as a change. Name the files under `~/.dotfiles` that changed, so the user can review them in `git diff`. Commit nothing.
