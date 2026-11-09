#!/bin/sh
# Agent fleet summary for herdr's tab_bar_right, e.g. "2 working · 1 blocked · 1 done".
# Prints nothing when no agents run, which hides the entry.
"${HERDR_BIN_PATH:-herdr}" pane list 2>/dev/null | jq -r '
  [.result.panes[] | select(.agent) | .agent_status] as $s
  | ["blocked", "working", "done"]
  | map(. as $k | ($s | map(select(. == $k)) | length) as $n
        | select($n > 0) | "\($n) \($k)")
  | join(" · ")'
