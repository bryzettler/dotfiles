#!/bin/bash
# Format an edited file with the repo's own prettier. Skip repos without prettier.
file=$(jq -r '.tool_input.file_path // empty')
[ -f "$file" ] || exit 0

dir=$(dirname "$file")
while [ "$dir" != "/" ]; do
  if [ -x "$dir/node_modules/.bin/prettier" ]; then
    "$dir/node_modules/.bin/prettier" --write --ignore-unknown "$file" >/dev/null 2>&1
    exit 0
  fi
  dir=$(dirname "$dir")
done
exit 0
