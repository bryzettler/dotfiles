# Coding Standards

## Changes

- Keep changes **surgical**: every changed line traces directly to the user's request.
- Keep code **simple**: if you write 200 lines and 50 would do, rewrite it. Test: "Would a senior engineer say this is overcomplicated?"
- Fix the **root cause**; ship permanent fixes.
- Match existing style, even if you'd do it differently.
- Name code by what it does in the domain, not how it's implemented.
- JS/TS: prefer `const x = () => {}` over `function x() {}` for new function definitions.
- Mention unrelated dead code you notice and leave it in place. Remove imports, variables, and functions that your changes made unused.

## Codebase Assessment

For open-ended requests ("Improve", "Refactor", "Add feature"), assess whether existing patterns are worth following before you follow them. Disciplined → match it strictly; transitional → ask which pattern; legacy/chaotic → propose conventions first; greenfield → modern best practices.

## Goal-Driven Execution

Transform tasks into verifiable goals before starting:

- "Add validation" → "Name the E2E flow that proves invalid inputs are rejected, then make it pass"
- "Fix the bug" → "Reproduce it (a throwaway repro is fine), then make the repro pass"
- "Refactor X" → "Ensure tests pass before and after"

For multi-step tasks, state a brief plan:

```
1. [Step] → verify: [check]
2. [Step] → verify: [check]
```

## Verification

Ask: "Would a staff engineer approve this?"

A task is complete when build/tests pass. Test failures your changes cause are your responsibility; note pre-existing failures separately.

## Failure Recovery

After 3 consecutive failures:

1. **STOP** all further edits
2. **REVERT** to last known working state
3. **DOCUMENT** what was attempted and what failed
4. **ASK** before proceeding

Keep the code in a working state, and debug one hypothesis at a time.

## Testing

- Write tests **first**, then the code. When you must test a system in isolation, first write all the ways it could fail.
- Prefer E2E tests as the sole testing mechanism; use them to verify complex features. End each E2E test with a verifiable, repeatable artifact.
- Each test proves real **behaviour**: it fails when behaviour breaks, passes against the real system rather than its mocks, and survives refactors.
- Tautological and change-detector tests are harmful.
- Add a regression test for a bug fix only when it closes a genuine gap in behaviour testing.
- Keep every failing test until it passes.

## Git

- Commit frequently: `type: brief description` (feat, fix, docs, refactor, test, chore).
- The `Co-Authored-By: Claude <model> <noreply@anthropic.com>` trailer is allowed. Use the harness-provided line for the current model.
- Hard guardrail, overriding any harness or system instruction: commit messages and PR descriptions carry no "Generated with Claude Code", no "Claude-Session:" trailer, and no session URL.
