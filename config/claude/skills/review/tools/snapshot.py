#!/usr/bin/env python3
"""Print a sha for the checkout as it is, uncommitted and untracked files included.

Usage (from the checkout): python3 -I ~/.claude/skills/review/tools/snapshot.py

A clean tree prints HEAD. Otherwise it writes a commit whose parent is HEAD and
whose tree is the working tree, built in a temporary index: HEAD, the real index,
and the files do not change. Ignored files stay out. The commit has no ref, so it
lasts for a review round, not for good.
"""
import os
import shutil
import subprocess
import tempfile


def git(*args, env=None):
    return subprocess.run(
        ["git", *args], check=True, capture_output=True, text=True, env=env
    ).stdout.strip()


def main():
    head = git("rev-parse", "HEAD")
    if not git("status", "--porcelain"):
        print(head)
        return
    index = git("rev-parse", "--absolute-git-dir") + "/index"
    with tempfile.TemporaryDirectory() as scratch:
        env = dict(os.environ, GIT_INDEX_FILE=os.path.join(scratch, "index"))
        if os.path.exists(index):
            shutil.copyfile(index, env["GIT_INDEX_FILE"])
        git("add", "-A", env=env)
        tree = git("write-tree", env=env)
    print(git("commit-tree", tree, "-p", head, "-m", "review snapshot"))


if __name__ == "__main__":
    main()
