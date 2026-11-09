#!/usr/bin/env python3
"""Compare Defects tracer speed by model, from subagent transcripts.

Scans ~/.claude/projects/*/*/subagents/ for `tracer` agents whose
description names Defects. Per model it prints the run count, the median
tool calls per run, the median and p90 seconds per model turn (time from
the previous tool result to the next assistant message), and the median
wall time per run.

Usage: defects-timing.py [--since YYYY-MM-DD] [--lens Defects]
"""
import argparse
import json
import statistics
from datetime import datetime
from pathlib import Path

ROOT = Path.home() / ".claude" / "projects"


def ts(s):
    return datetime.fromisoformat(s.replace("Z", "+00:00"))


def p90(xs):
    xs = sorted(xs)
    return xs[min(len(xs) - 1, int(len(xs) * 0.9))]


def run_stats(jsonl):
    model, turns, tool_calls = None, [], 0
    first = last = prev = None
    seen = set()
    for line in jsonl.open():
        try:
            o = json.loads(line)
        except json.JSONDecodeError:
            continue
        t = o.get("timestamp")
        if not t or o.get("type") not in ("user", "assistant"):
            continue
        t = ts(t)
        first = first or t
        last = t
        msg = o.get("message") or {}
        if o["type"] == "assistant":
            mid = msg.get("id")
            content = msg.get("content") or []
            tool_calls += sum(1 for c in content if isinstance(c, dict) and c.get("type") == "tool_use")
            if mid in seen:
                continue
            seen.add(mid)
            model = msg.get("model") or model
            if prev is not None:
                turns.append((t - prev).total_seconds())
            prev = None
        else:
            prev = t
    if not model or not first:
        return None
    return model, turns, tool_calls, (last - first).total_seconds(), first


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--since", help="only runs started on or after this date")
    ap.add_argument("--lens", default="Defects")
    a = ap.parse_args()
    since = datetime.fromisoformat(a.since).date() if a.since else None

    by_model = {}
    for meta in ROOT.glob("*/*/subagents/*.meta.json"):
        try:
            m = json.loads(meta.read_text())
        except (json.JSONDecodeError, OSError):
            continue
        if m.get("agentType") != "tracer" or a.lens.lower() not in m.get("description", "").lower():
            continue
        jsonl = meta.with_name(meta.name.replace(".meta.json", ".jsonl"))
        if not jsonl.exists():
            continue
        r = run_stats(jsonl)
        if not r or (since and r[4].date() < since):
            continue
        by_model.setdefault(r[0], []).append(r)

    print(f"{'model':<22}{'runs':>5}{'calls/run':>11}{'s/turn med':>12}{'s/turn p90':>12}{'wall min':>10}")
    for model, runs in sorted(by_model.items()):
        turns = [x for r in runs for x in r[1]]
        if not turns:
            continue
        print(
            f"{model:<22}{len(runs):>5}"
            f"{statistics.median(r[2] for r in runs):>11.0f}"
            f"{statistics.median(turns):>12.1f}{p90(turns):>12.1f}"
            f"{statistics.median(r[3] for r in runs) / 60:>10.1f}"
        )


if __name__ == "__main__":
    main()
