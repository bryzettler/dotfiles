#!/usr/bin/env python3
"""Resolve a session alias to live herdr agent panes.

Usage: resolve.py <alias|all|self>
Prints one tab-separated line per match: pane_id, status, alias, cwd.
Exit 0 on a single match (or any matches for `all`), 2 on no match, 3 when ambiguous.
"""
import json
import os
import subprocess
import sys

# alias -> repo directory basename. A worktree such as wallet-app-215-preview
# matches its repo by prefix.
ALIASES = {
    "hpl": "helium-program-library",
    "wallet": "wallet-app",
    "world": "world-explorer",
    "vote": "helium-vote",
    "k8s": "helium-foundation-k8s",
    "tuktuk": "tuktuk",
    "hotspot": "hotspot-app",
    "dotfiles": "dotfiles",
}


def alias_of(agent):
    title = (agent.get("terminal_title_stripped") or "").strip().lower()
    if title in ALIASES:
        return title
    base = os.path.basename(agent.get("foreground_cwd") or agent.get("cwd") or "")
    # Longest repo name first, so helium-program-library never matches a shorter prefix.
    for alias, repo in sorted(ALIASES.items(), key=lambda kv: -len(kv[1])):
        if base == repo or base.startswith(repo + "-"):
            return alias
    return title or base


def main():
    if len(sys.argv) != 2:
        print(__doc__.strip(), file=sys.stderr)
        return 2
    want = sys.argv[1].lower()
    out = subprocess.run(["herdr", "agent", "list"], capture_output=True, text=True)
    if out.returncode != 0:
        print(out.stderr.strip(), file=sys.stderr)
        return 2
    agents = json.loads(out.stdout)["result"]["agents"]
    me = os.environ.get("HERDR_PANE_ID")
    rows = [(a["pane_id"], a.get("agent_status", "unknown"), alias_of(a), a.get("cwd", ""))
            for a in agents if a.get("agent") == "claude"]

    if want == "self":
        hits = [r for r in rows if r[0] == me]
    elif want == "all":
        hits = [r for r in rows if r[0] != me]
    else:
        hits = [r for r in rows if r[2] == want and r[0] != me]

    for r in hits:
        print("\t".join(r))
    if not hits:
        print(f"no live claude session matches '{want}'", file=sys.stderr)
        for r in rows:
            print("\t".join(r), file=sys.stderr)
        return 2
    if want not in ("all",) and len(hits) > 1:
        print("ambiguous: name the pane id", file=sys.stderr)
        return 3
    return 0


if __name__ == "__main__":
    sys.exit(main())
