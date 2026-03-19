#!/bin/bash
INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command')

if [[ "$COMMAND" == *"git commit"* ]]; then
  if ! yarn lint 2>&1; then
    echo "Lint failed. Fix errors before committing." >&2
    exit 2
  fi
fi

exit 0
