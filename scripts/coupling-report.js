// coupling-report.js -- measure subsystem coupling, separating hard from soft.
//
//   node scripts/coupling-report.js
//
// A back-edge is a reference from a lower subsystem up into a higher one, and
// they are not all equal:
//
//   HARD  (foo ...)            direct call. The caller links against the
//                              callee; it cannot load or be reasoned about
//                              without it.
//   SOFT  'foo  #'foo          a quoted symbol, almost always reached through
//                              (when (fboundp 'foo) (funcall 'foo ...)). The
//                              caller names the callee but does not depend on
//                              it, and degrades when it is absent.
//
// An earlier version of this counted both the same way, which made a real
// change -- converting two direct calls into one late-bound port -- look like
// no change at all. Distinguishing them is the difference between measuring
// coupling and measuring mentions.
//
// Soft edges are still coupling: undeclared, unenforced, invisible to any
// test. But they are cheap to formalise, whereas hard edges have to be
// inverted. Track the two separately or the cheap work hides the real work.

const fs = require('fs');
const path = require('path');

const SUBSYSTEMS = {
  cognitive: ['mind/publication', 'mind/context', 'mind/ticks', 'mind/reflection',
              'kernel/agent_loop.lisp', 'pending-split'],
  memory:    ['mind/memory', 'mind/conversation'],
  authority: ['kernel/runtime-truth.lisp', 'kernel/event-log.lisp',
              'mind/publication/first-person-evidence.lisp',
              'kernel/runtime-observer-registry.lisp',
              'kernel/runtime-observer-audit.lisp', 'kernel/replay-capsules.lisp']
};

// Layering, lowest first. An edge pointing left-to-right is a back-edge.
const ORDER = ['authority', 'memory', 'cognitive'];

const files = [];
(function walk(dir) {
  for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
    const p = path.join(dir, e.name);
    if (e.isDirectory()) walk(p);
    else if (p.endsWith('.lisp')) files.push(p);
  }
})('src');

const rel = p => p.split(path.sep).slice(1).join('/');
const subsystemOf = p => {
  const r = rel(p);
  for (const [name, globs] of Object.entries(SUBSYSTEMS))
    if (globs.some(g => r === g || r.startsWith(g + '/'))) return name;
  return null;
};

// A symbol may be DEFUN'd in several files: that is the wrap idiom, where a
// later layer redefines an earlier layer's function. Such a symbol has no
// single owning subsystem, so attributing it to whichever definition happened
// to be read last invents edges that are not there. Collect every definer and
// treat multiply-defined symbols as their own category.
const definers = new Map();
for (const f of files) {
  const text = fs.readFileSync(f, 'utf8');
  for (const m of text.matchAll(/^\((?:defun|defmacro|define-seam)\s+([%A-Za-z0-9*+/<>=-]+)/gm)) {
    const sym = m[1].toLowerCase();
    if (!definers.has(sym)) definers.set(sym, new Set());
    definers.get(sym).add(subsystemOf(f));
  }
}
const wrapped = new Set();
const definedIn = new Map();
for (const [sym, subs] of definers) {
  const real = [...subs].filter(Boolean);
  if (new Set(real).size > 1) wrapped.add(sym);
  else definedIn.set(sym, real[0] ?? null);
}

const isBackEdge = (from, to) => ORDER.indexOf(from) < ORDER.indexOf(to);

const edges = new Map();   // "from -> to" : { hard:Set, soft:Set }
for (const f of files) {
  const from = subsystemOf(f);
  if (!from) continue;
  const text = fs.readFileSync(f, 'utf8');
  // Head position = a direct call. Quoted or sharp-quoted = late-bound.
  const scan = (re, kind) => {
    for (const m of text.matchAll(re)) {
      const sym = m[1].toLowerCase();
      const to = definedIn.get(sym);
      if (!to || to === from || !isBackEdge(from, to)) continue;
      const key = `${from} -> ${to}`;
      if (!edges.has(key)) edges.set(key, { hard: new Set(), soft: new Set() });
      edges.get(key)[kind].add(sym);
    }
  };
  scan(/\(([%A-Za-z0-9*+/<>=-]{3,})[\s)]/g, 'hard');
  scan(/#?'([%A-Za-z0-9*+/<>=-]{3,})/g, 'soft');
}

let hardTotal = 0, softTotal = 0;
const allSoft = new Set();
console.log('Back-edges (lower subsystem referencing a higher one)\n');
for (const [key, { hard, soft }] of [...edges.entries()].sort()) {
  // A symbol reached both ways is hard: the direct call is what binds.
  for (const s of hard) soft.delete(s);
  hardTotal += hard.size;
  softTotal += soft.size;
  for (const s of soft) allSoft.add(s);
  console.log(`  ${key}`);
  console.log(`     HARD ${hard.size}: ${[...hard].sort().join(', ') || '-'}`);
  console.log(`     soft ${soft.size}: ${[...soft].sort().join(', ') || '-'}`);
}
console.log(`\n  TOTAL   hard ${hardTotal}   soft ${softTotal}`);
console.log('\nHard edges must be inverted. Soft edges need declaring, not moving.');

// Completeness: every soft edge above must be named in
// src/kernel/soft-edge-port-registry.lisp, as a PORT or as ACCEPTED with a
// reason -- the same discipline wrap-chain-completeness-tests.lisp applies
// to the wrap idiom. Parsed as source text, not loaded: this script has no
// Lisp reader, and the registry's own header explains why a trivial
// one-string-per-entry shape keeps that safe.
const registryPath = path.join('src', 'kernel', 'soft-edge-port-registry.lisp');
let exitCode = 0;
if (fs.existsSync(registryPath)) {
  const registryText = fs.readFileSync(registryPath, 'utf8');
  const declared = new Set(
    [...registryText.matchAll(/^\s*"([%A-Za-z0-9*+/<>=-]+)"\s*$/gm)].map(m => m[1].toLowerCase())
  );
  const undeclared = [...allSoft].filter(s => !declared.has(s)).sort();
  console.log(`\nSoft-edge registry: ${declared.size} declared in ${registryPath}`);
  if (undeclared.length) {
    console.log(`UNDECLARED soft edge(s): ${undeclared.join(', ')}`);
    console.log('Add each to *soft-edge-ports* as a PORT or ACCEPTED entry with a reason.');
    exitCode = 1;
  } else {
    console.log('All current soft edges are declared.');
  }
} else {
  console.log(`\nNo soft-edge registry found at ${registryPath} -- skipping completeness check.`);
}

const wrapList = [...wrapped].sort();
console.log(`\nWrap-chain symbols (defined in more than one subsystem): ${wrapList.length}`);
console.log('Excluded from the counts above: they have no owning layer, so any');
console.log('edge attributed to them would be an artefact of which definition');
console.log('was read last. Each is a place where load order decides behaviour.\n');
for (const s of wrapList)
  console.log(`  ${s}  <- ${[...definers.get(s)].filter(Boolean).sort().join(', ')}`);

process.exitCode = exitCode;
