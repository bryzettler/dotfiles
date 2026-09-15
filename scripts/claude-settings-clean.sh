#!/bin/sh
# git clean filter for config/claude/settings.json.
# ~/.claude/settings.json is a symlink into this repo, and Claude Code writes
# an autoMode block into it describing whatever repo it last ran in. That block
# leaks private work infra (hosts, repo names, secret file paths) into this
# public repo, so strip it from the committed blob while leaving the live file intact.
exec jq '
  if .autoMode then
    .autoMode |= (
      del(.environment)
      | if .soft_deny then .soft_deny |= map(select(test("Documents/Work") | not)) else . end
    )
  else . end
'
