#!/bin/bash
VAULT_ROOT="/Users/bry/Documents/Personal/Vaults/2026/03 - Commonplace/Projects"
WORK_ROOT="/Users/bry/Documents/Work"

CWD="$PWD"

# Only activate for Work project directories
[[ "$CWD" != "$WORK_ROOT"/* ]] && exit 0

# Extract project name (first component after Work/)
PROJECT=$(echo "$CWD" | sed "s|$WORK_ROOT/||" | cut -d'/' -f1)
VAULT_DIR="$VAULT_ROOT/$PROJECT"

[[ ! -d "$VAULT_DIR" ]] && exit 0

# Read all markdown files and output as context
for file in "$VAULT_DIR"/*.md; do
  [ -f "$file" ] || continue
  echo "--- $(basename "$file") ---"
  cat "$file"
  echo ""
done
