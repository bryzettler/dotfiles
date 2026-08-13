#!/bin/bash
INPUT=$(cat)
COMMAND=$(echo "$INPUT" | jq -r '.tool_input.command')

block() {
  echo "BLOCKED: '$COMMAND' $1. The user has prevented you from doing this." >&2
  exit 2
}

DANGEROUS_PATTERNS=(
  "git reset --hard"
  "git clean -fd"
  "git clean -f"
  "git branch -D"
  "git checkout \."
  "git restore \."
  "reset --hard"
)

for pattern in "${DANGEROUS_PATTERNS[@]}"; do
  if echo "$COMMAND" | grep -qE "$pattern"; then
    block "matches dangerous pattern '$pattern'"
  fi
done

# Protected branch names: exact match, or term followed by a separator (release/1.2, prod-eu)
is_protected() {
  local b="${1#refs/heads/}"
  echo "$b" | grep -qiE '^(main|master|develop|development|dev|trunk|prod|production|live|stable|release|releases|staging|stage|stg|preprod|pre-prod|uat|qa|sdlc|edge|canary|beta)([/_.-].*)?$'
}

if echo "$COMMAND" | grep -qE '(^|[;&| ])git +push'; then
  # Isolate the push invocation from compound commands (up to &&, ||, ;, |)
  PUSH_SEG=$(echo "$COMMAND" | sed -E 's/.*git +push//' | sed -E 's/(&&|\|\||;|\|).*//')

  ARGS=()
  for tok in $PUSH_SEG; do
    case "$tok" in
      -f|--force|--force-with-lease|--force-with-lease=*|--force-if-includes)
        block "is a force push" ;;
      -*) ;; # other flags: ignore
      *) ARGS+=("$tok") ;;
    esac
  done

  if [ "${#ARGS[@]}" -le 1 ]; then
    # Bare push (or remote only): target is the current branch
    BRANCH=$(git branch --show-current 2>/dev/null)
    if [ -z "$BRANCH" ]; then
      block "pushes from an unknown/detached branch"
    fi
    if is_protected "$BRANCH"; then
      block "pushes protected branch '$BRANCH'"
    fi
  else
    # Explicit refspec(s): check the destination side of each
    for ref in "${ARGS[@]:1}"; do
      dst="${ref#*:}"
      if [ "$dst" = "HEAD" ] || [ "$ref" = "HEAD" ]; then
        dst=$(git branch --show-current 2>/dev/null)
        [ -z "$dst" ] && block "pushes from an unknown/detached branch"
      fi
      if is_protected "$dst"; then
        block "targets protected branch '$dst'"
      fi
    done
  fi
fi

exit 0
