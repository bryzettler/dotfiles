#!/usr/bin/env node
// Deterministic signature scan for the review scout.
// Usage (from the repo root, so `typescript` resolves from the repo):
//   node ~/.claude/skills/review/tools/ts-signatures.mjs <base-ref> [head-ref]
// Prints one line per finding:
//   SIG     <file>:<line> <name> <old signature> -> <new signature>   (exported, changed)
//   NOAWAIT <file>:<line> <name>   (async function, new or made async by the diff, body awaits nothing)
import { execFileSync } from "node:child_process";
import { createRequire } from "node:module";

const require = createRequire(process.cwd() + "/");
const ts = require("typescript");

const [base, head = "HEAD"] = process.argv.slice(2);
if (!base) {
  console.error("usage: ts-signatures.mjs <base-ref> [head-ref]");
  process.exit(2);
}

const git = (...args) =>
  execFileSync("git", args, { encoding: "utf8", maxBuffer: 1 << 28 });
const show = (ref, path) => {
  try {
    return execFileSync("git", ["show", `${ref}:${path}`], {
      encoding: "utf8",
      maxBuffer: 1 << 28,
      stdio: ["ignore", "pipe", "ignore"],
    });
  } catch {
    return null;
  }
};

const files = git(
  "diff",
  "--name-only",
  "--diff-filter=AMR",
  `${base}...${head}`,
)
  .split("\n")
  .filter((f) => /\.(ts|tsx|mts|cts)$/.test(f) && !f.endsWith(".d.ts"));

const isAsync = (node) =>
  !!node.modifiers?.some((m) => m.kind === ts.SyntaxKind.AsyncKeyword);

const isExported = (node) =>
  !!node.modifiers?.some((m) => m.kind === ts.SyntaxKind.ExportKeyword);

// await or for-await in this function's own body, not in nested functions
const awaits = (body) => {
  let found = false;
  const visit = (n) => {
    if (found) return;
    if (
      n !== body &&
      (ts.isFunctionDeclaration(n) ||
        ts.isFunctionExpression(n) ||
        ts.isArrowFunction(n) ||
        ts.isMethodDeclaration(n))
    )
      return;
    if (ts.isAwaitExpression(n)) found = true;
    if (ts.isForOfStatement(n) && n.awaitModifier) found = true;
    ts.forEachChild(n, visit);
  };
  if (body) visit(body);
  return found;
};

const collect = (path, text) => {
  const out = new Map();
  if (text == null) return out;
  const sf = ts.createSourceFile(path, text, ts.ScriptTarget.Latest, true);
  const params = (fn) =>
    fn.parameters
      .map((p) =>
        ts.isObjectBindingPattern(p.name)
          ? `{${p.name.elements.map((e) => e.name.getText(sf)).join(",")}}`
          : p.name.getText(sf),
      )
      .join(", ");
  const sig = (fn) => ({
    async: isAsync(fn),
    params: params(fn),
    returns: fn.type
      ? fn.type
          .getText(sf)
          .replace(/\/\/[^\n]*|\/\*[\s\S]*?\*\//g, "")
          .replace(/\s+/g, " ")
      : "",
  });
  const add = (name, fn, exported) =>
    out.set(name, {
      name,
      exported,
      async: isAsync(fn),
      awaits: awaits(fn.body),
      sig: sig(fn),
      line: sf.getLineAndCharacterOfPosition(fn.getStart(sf)).line + 1,
    });
  const visit = (n, cls) => {
    if (ts.isFunctionDeclaration(n) && n.name)
      add(n.name.text, n, isExported(n));
    else if (ts.isVariableStatement(n)) {
      for (const d of n.declarationList.declarations) {
        const init = d.initializer;
        if (
          ts.isIdentifier(d.name) &&
          init &&
          (ts.isArrowFunction(init) || ts.isFunctionExpression(init))
        )
          add(d.name.text, init, isExported(n));
      }
    } else if (ts.isMethodDeclaration(n) && n.name && cls)
      add(`${cls}.${n.name.getText(sf)}`, n, true);
    const nextCls =
      ts.isClassDeclaration(n) && n.name && isExported(n) ? n.name.text : cls;
    ts.forEachChild(n, (c) => visit(c, nextCls));
  };
  visit(sf, null);
  return out;
};

for (const file of files) {
  const before = collect(file, show(base, file));
  const after = collect(file, show(head, file));
  for (const [name, fn] of after) {
    const old = before.get(name);
    if (fn.exported && old) {
      const changes = [];
      if (old.sig.async !== fn.sig.async)
        changes.push(old.sig.async ? "async -> sync" : "sync -> async");
      if (old.sig.params !== fn.sig.params)
        changes.push(`params (${old.sig.params}) -> (${fn.sig.params})`);
      if (old.sig.returns !== fn.sig.returns)
        changes.push(`returns ${old.sig.returns || "?"} -> ${fn.sig.returns || "?"}`);
      if (changes.length)
        console.log(`SIG     ${file}:${fn.line} ${name}: ${changes.join("; ")}`);
    }
    if (fn.async && !fn.awaits && (!old || !old.async))
      console.log(`NOAWAIT ${file}:${fn.line} ${name}`);
  }
}
