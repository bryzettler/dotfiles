# Agent Skills Setup (mattpocock-skills)

When running `/mattpocock-skills:setup-matt-pocock-skills`, use this layout without asking:

- Issue tracker: local markdown under `.scratch/<feature>/`
- Triage labels: defaults
- Domain docs: single-context (`GLOSSARY.md` + `docs/adr/` at repo root)
- Keep the config personal: leave tracked `AGENTS.md` and `.gitignore` untouched. Put the `## Agent skills` block in a local `CLAUDE.md` and add to `.git/info/exclude`:

```
# Personal agent-skills config (not shared with team)
.scratch/
CLAUDE.md
docs/agents/
docs/adr/
GLOSSARY.md
```
