#!/usr/bin/env python3
"""Audit the "checked, nothing" lines of review axis returns.

Usage: python3 -I ~/.claude/skills/review/tools/audit-coverage.py <scratchpad>/return-*.md

Audits every coverage line: a cleared line for missing fields and forbidden
reasons, and every line (a hit line too) for fields that show another hit.
Prints one line per rejected line, and one per line with only format faults:
  REJECT <return file>:<line> <reason> | <the coverage line>
  WARN <return file>:<line> <reason> | <the coverage line>
and a last line with the counts. A REJECT is a clearance the line did not earn
or a field that shows a hit; a WARN is a format fault on a line whose fields
show nothing, and needs no tracer. Exit status 0 always; the caller reads the output.
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
# a field runs to the next ";"; "|" ends it too, but "||" in a code span does not
FIELD = r"((?:\|\||[^;|])+)"
SIG = re.compile(r"\bsig:\s*" + FIELD, re.I)
ARMS = re.compile(r"\barms:\s*(\d+)", re.I)
EXAMPLES = re.compile(r"\bexamples:\s*(\d+)", re.I)
PROBE = re.compile(r"\bprobe:\s*" + FIELD, re.I)
ORDER = re.compile(r"\border:\s*" + FIELD, re.I)
# parity cells are "|"-separated, so the field runs to the next ";"
PARITY = re.compile(r"\bparity:\s*([^;]+)", re.I)
TIER = re.compile(r"\b(major|breaking tier|minor)\b", re.I)
AMOUNT = re.compile(r"(lamport|fee|rent|fund|amount|price|quote|cost|balance|reward)", re.I)
EARLY = re.compile(r"(env|override|rpc|fetch|get\w*Info|detect|fallback|default)", re.I)
GATE = re.compile(r"(signer|capabilit|auth|owner|permission|guard)", re.I)
# items with no exported signature: docs, changesets, config, lockfiles, workflows
NON_CODE = re.compile(r"^\s*-\s*`?\S*\.(md|ya?ml|json|toml|lock)\b", re.I)


def audit(path, n, line, cleared):
    reasons, warns = [], []
    if cleared:
        if not re.search(r"\blenses:", line, re.I):
            reasons.append("no lenses: field")
        # the reason follows the item name, which may itself be a changeset path;
        # the sig: field may name the changeset that admits a break
        reason = line.split("—", 1)[-1].split("lenses:")[0]
        m = BAD_REASON.search(SIG.sub("", reason))
        if m:
            reasons.append(f"forbidden reason '{m.group(0)}'")
    m = SIG.search(line)
    if m:
        v = m.group(1).strip()
        low = v.lower()
        if low.startswith("unchanged") or low.startswith("new"):
            pass
        elif low.startswith("removed") or re.search(r"→|->", v):
            if cleared and not TIER.search(v):
                reasons.append(f"sig changed with no breaking-tier changeset named ({v})")
        else:
            # an unexported item has no signature to change; any other free text hides one
            no_sig = NON_CODE.search(line) or re.search(r"\b(internal|not exported)\b", low)
            bucket = warns if no_sig else reasons
            bucket.append(f"sig not in 'unchanged | new | removed | old → new' form ({v})")
    arms, examples = ARMS.search(line), EXAMPLES.search(line)
    item = re.sub(r"^\s*-\s*\S+:\d+\s*", "", line.split("—")[0])
    if cleared and AMOUNT.search(item) and not (arms and examples):
        reasons.append("amount item without arms:/examples:")
    if arms and examples and int(examples.group(1)) < int(arms.group(1)):
        reasons.append(f"examples {examples.group(1)} < arms {arms.group(1)}")
    for m in PROBE.finditer(line):
        if "admits" not in m.group(1):
            warns.append(f"probe names no admitted types ({m.group(1).strip()})")
        else:
            # a parenthetical explains a type; it does not add one
            admitted = re.sub(r"\([^()]*\)", "", m.group(1).split("admits", 1)[1])
            if len(re.split(r",|\band\b|\bor\b|/", admitted)) > 1:
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
    elif m:
        # a sibling lacks a guard another runs: a differing cell (lenses-shared.md, Neighbours)
        nones = [c.strip() for c in m.group(1).split("|") if re.search(r"\bnone\b", c, re.I)]
        if nones:
            reasons.append(f"parity cell none ({' | '.join(nones)})")
    return reasons, warns


def main(paths):
    total = rejected = warned = 0
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
            reasons, warns = audit(path, n, line, cleared)
            if reasons:
                rejected += 1
                print(f"REJECT {path}:{n} {'; '.join(reasons + warns)} | {line.strip()}")
            elif warns:
                warned += 1
                print(f"WARN {path}:{n} {'; '.join(warns)} | {line.strip()}")
    print(f"AUDIT {total} coverage lines, {rejected} rejected, {warned} format warnings")


if __name__ == "__main__":
    main(sys.argv[1:])
