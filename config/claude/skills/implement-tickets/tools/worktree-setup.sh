#!/bin/sh
# Give a worktree a node_modules that matches its own dependencies.
#
# Usage: worktree-setup.sh <worktree> <main-checkout>
#
# When package.json, the lockfile, and patches/ match the main checkout, it
# links the main checkout's node_modules. Otherwise it installs from the
# worktree's own lockfile and records the inputs' hash in
# node_modules/.worktree-setup. A rerun re-checks and does nothing when the
# tree still matches. Prints "linked", "installed", or "up to date".
set -eu

wt=$(cd "$1" && pwd)
main=$(cd "$2" && pwd)

deps_hash() {
  (
    cd "$1"
    for f in package.json yarn.lock package-lock.json pnpm-lock.yaml; do
      if [ -f "$f" ]; then echo "$f"; cat "$f"; fi
    done
    if [ -d patches ]; then
      find patches -type f | sort | while read -r p; do echo "$p"; cat "$p"; done
    fi
  ) | shasum | cut -d' ' -f1
}

want=$(deps_hash "$wt")
same_as_main=no
if [ -d "$main/node_modules" ] && [ "$want" = "$(deps_hash "$main")" ]; then
  same_as_main=yes
fi

nm="$wt/node_modules"
if [ -L "$nm" ]; then
  if [ "$same_as_main" = yes ]; then echo "up to date (linked)"; exit 0; fi
  # The link points at a tree built for other dependencies: drop the link only.
  rm "$nm"
elif [ -f "$nm/.worktree-setup" ] && [ "$(cat "$nm/.worktree-setup")" = "$want" ]; then
  echo "up to date (installed)"
  exit 0
fi

if [ "$same_as_main" = yes ] && [ ! -e "$nm" ]; then
  ln -s "$main/node_modules" "$nm"
  echo linked
  exit 0
fi

cd "$wt"
if [ -f yarn.lock ] && [ -f .yarnrc.yml ]; then
  yarn install --immutable
elif [ -f yarn.lock ]; then
  yarn install --frozen-lockfile
elif [ -f pnpm-lock.yaml ]; then
  pnpm install --frozen-lockfile
else
  npm ci
fi
echo "$want" > "$nm/.worktree-setup"
echo installed
