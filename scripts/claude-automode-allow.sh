#!/usr/bin/env bash
# Add an autoMode.allow rule to the dotfiles Claude settings (which ~/.claude/settings.json symlinks to).
# Idempotent: keeps "$defaults" first and skips a rule that is already present.
#
# Usage:
#   scripts/claude-automode-allow.sh                 # adds the built-in PR review rule
#   scripts/claude-automode-allow.sh "Custom rule…"  # adds the given prose rule instead
set -euo pipefail

SETTINGS="$(cd "$(dirname "$0")/.." && pwd)/config/claude/settings.json"

DEFAULT_RULE='PR review submission: submitting a GitHub pull request review with `gh pr review` (approve or comment) or `gh api repos/<owner>/<repo>/pulls/<n>/reviews` (APPROVE or COMMENT event) is allowed. It is the output of the user'"'"'s /review skill, publishes only review text and code suggestions to a repo the user already has push access to, and changes no branch or file.'
RULE="${1:-$DEFAULT_RULE}"

RULE="$RULE" python3 - "$SETTINGS" <<'PY'
import json, os, sys
path = sys.argv[1]
rule = os.environ["RULE"]
with open(path) as f:
    d = json.load(f)
allow = d.setdefault("autoMode", {}).setdefault("allow", [])
if "$defaults" not in allow:
    allow.insert(0, "$defaults")
if rule in allow:
    print(f"already present: {path}")
else:
    allow.append(rule)
    with open(path, "w") as f:
        json.dump(d, f, indent=2, ensure_ascii=False)
        f.write("\n")
    print(f"added to {path}")
PY

echo "verify with: claude auto-mode config | grep -c 'PR review submission'"
