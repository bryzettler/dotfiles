#!/usr/bin/env python3
"""Audit the "checked, nothing" lines of review axis returns.

Usage: python3 -I ~/.claude/skills/review/tools/audit-coverage.py <scratchpad>/return-*.md

Audits every coverage line: a cleared line for missing fields and forbidden
reasons, and every line (a hit line too) for fields that show another hit.
Prints one line per rejected line:
  REJECT <return file>:<line> <reason> | <the coverage line>
and a last line with the counts. Exit status 0 always; the caller reads the output.
See "Coverage line format" in return-rule.md for the fields.
"""
import re
import sys

CLEARED = re.compile(r"checked,\s*nothing", re.I)
BAD_REASON = re.compile(
    r"\b(spec|readme|pr body|changeset|documented|documents|same as (develop|base|master|main)"
    r"|unchanged from base|pre-?existing|rename|renamed|moved|no regression|no funds move)\b",
    re.I,
)
SIG = re.compile(r"\bsig:\s*([^;|]+)", re.I)
ARMS = re.compile(r"\barms:\s*(\d+)", re.I)
EXAMPLES = re.compile(r"\bexamples:\s*(\d+)", re.I)
PROBE = re.compile(r"\bprobe:\s*([^;|]+)", re.I)
ORDER = re.compile(r"\border:\s*([^;|]+)", re.I)
PARITY = re.compile(r"\bparity:\s*([^;|]+)", re.I)
AMOUNT = re.compile(r"(lamport|fee|rent|fund|amount|price|quote|cost|balance|reward)", re.I)
EARLY = re.compile(r"(env|override|rpc|fetch|get\w*Info|detect|fallback|default)", re.I)
GATE = re.compile(r"(signer|capabilit|auth|owner|permission|guard)", re.I)


def audit(path, n, line, cleared):
    reasons = []
    if cleared:
        if not re.search(r"\blenses:", line, re.I):
            reasons.append("no lenses: field")
        m = BAD_REASON.search(line.split("lenses:")[0])
        if m:
            reasons.append(f"forbidden reason '{m.group(0)}'")
    m = SIG.search(line)
    if m:
        v = m.group(1).strip()
        low = v.lower()
        if re.search(r"→|->", v):
            if cleared and not re.search(r"\b(major|breaking tier|minor \(0\.x\))\b", line, re.I):
                reasons.append(f"sig changed with no breaking-tier changeset named ({v})")
        elif not (low.startswith("unchanged") or low.startswith("new") or low.startswith("removed")):
            reasons.append(f"sig not in 'unchanged | new | removed | old → new' form ({v})")
    arms, examples = ARMS.search(line), EXAMPLES.search(line)
    if cleared and AMOUNT.search(line.split("—")[0]) and not (arms and examples):
        reasons.append("amount item without arms:/examples:")
    if arms and examples and int(examples.group(1)) < int(arms.group(1)):
        reasons.append(f"examples {examples.group(1)} < arms {arms.group(1)}")
    m = PROBE.search(line)
    if m:
        if "admits" not in m.group(1):
            reasons.append(f"probe names no admitted types ({m.group(1).strip()})")
        else:
            admitted = m.group(1).split("admits", 1)[1]
            if len(re.split(r",|\bor\b|/|\|", admitted)) > 1:
                reasons.append(f"probe admits several types ({admitted.strip()})")
    m = ORDER.search(line)
    if m:
        steps = [s.strip() for s in re.split(r">|->|→", m.group(1))]
        gates = [i for i, s in enumerate(steps) if GATE.search(s)]
        early = [i for i, s in enumerate(steps) if EARLY.search(s)]
        if gates and early and min(early) < min(gates):
            reasons.append(f"'{steps[min(early)]}' runs before '{steps[min(gates)]}'")
    m = PARITY.search(line)
    if m and "differs" in m.group(1).lower():
        reasons.append(f"parity {m.group(1).strip()}")
    return reasons


def main(paths):
    total = rejected = 0
    for path in paths:
        try:
            lines = open(path, encoding="utf8").read().splitlines()
        except OSError as e:
            print(f"UNREADABLE {path}: {e}")
            continue
        for n, line in enumerate(lines, 1):
            cleared = bool(CLEARED.search(line))
            # every coverage line: cleared, or a hit line that carries fields
            if not cleared and not re.search(r"\b(lenses|sig|probe|order|parity):", line, re.I):
                continue
            total += 1
            reasons = audit(path, n, line, cleared)
            if reasons:
                rejected += 1
                print(f"REJECT {path}:{n} {'; '.join(reasons)} | {line.strip()}")
    print(f"AUDIT {total} coverage lines, {rejected} rejected")


if __name__ == "__main__":
    main(sys.argv[1:])
