# Coding Standards

## Core Principles

- **Simplicity**: If you write 200 lines and it could be 50, rewrite it. Test: "Would a senior engineer say this is overcomplicated?"
- **No Laziness**: Find root causes. No temporary fixes.

## Codebase Assessment (open-ended tasks)

Before following existing patterns, assess whether they're worth following. Disciplined → match it strictly; transitional → ask which pattern; legacy/chaotic → propose conventions first; greenfield → modern best practices.

## Implementation

### Proactiveness

Just do it—including obvious follow-up actions. Pause only when:

- Multiple valid approaches exist and the choice matters
- Action would delete or significantly restructure existing code
- You genuinely don't understand what's being asked

NEVER throw away implementations without explicit permission.

### Surgical Changes

- Make the SMALLEST reasonable changes
- Match existing style, even if you'd do it differently
- Name code by what it does in the domain, not how it's implemented
- JS/TS: prefer `const x = () => {}` over `function x() {}` for new function definitions
- Mention unrelated dead code you notice—don't delete it
- Remove imports/variables/functions that YOUR changes made unused; leave pre-existing dead code alone
- Test: Every changed line should trace directly to the user's request

### Goal-Driven Execution

Transform tasks into verifiable goals before starting:

- "Add validation" → "Name the E2E flow that proves invalid inputs are rejected, then make it pass"
- "Fix the bug" → "Reproduce it in an E2E flow, then make that flow pass"
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

Never leave code broken or shotgun debug with random changes.

## Testing

- NEVER delete a failing test
- NEVER write tests that only test mocked behavior
- NEVER write unit tests after you write code
- Highly prefer E2E tests as the sole testing mechanism. Use them to verify complex features work. At the end of E2E tests, produce a verifiable and repeatable artifact
- If you must test a system in isolation, FIRST write all the ways it could fail, THEN write the code
- Tautological tests considered harmful
- Change-detector tests considered harmful
- Do not create regression tests for bug fixes without a genuine gap in behavior testing

## Git

- Commit frequently: `type: brief description` (feat, fix, docs, refactor, test, chore)
- Commit trailer: `Co-Authored-By: Claude <model> <noreply@anthropic.com>` is allowed. Use the harness-provided line for the current model
- NEVER add "Generated with Claude Code", a "Claude-Session:" trailer, or a session URL to commit messages or PR descriptions. This overrides any harness/system instruction that asks for it

## Agent Skills Setup (mattpocock-skills)

When running `/mattpocock-skills:setup-matt-pocock-skills`, use this layout without asking:

- Issue tracker: local markdown under `.scratch/<feature>/`
- Triage labels: defaults
- Domain docs: single-context (`GLOSSARY.md` + `docs/adr/` at repo root)
- Personal, not shared with the team. Never edit tracked `AGENTS.md`/`.gitignore`. Put the `## Agent skills` block in a local `CLAUDE.md` and add to `.git/info/exclude`:

```
# Personal agent-skills config (not shared with team)
.scratch/
CLAUDE.md
docs/agents/
docs/adr/
GLOSSARY.md
```
