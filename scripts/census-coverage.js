// census-coverage.js -- every event type a producer writes must be classified.
//
//   node scripts/census-coverage.js
//
// The census is authoritative for the admission table and the generated
// document, but it had no relationship to the code that actually WRITES
// events. Adding a new `log-event "something-new"` anywhere in the tree
// classified nothing and failed nothing: the new type silently defaulted to
// journal-only.
//
// Defaulting to exclusion is the right default -- an unclassified type must
// never reach cognition by accident. But defaulting SILENTLY is not: nobody
// decided that the new event should not wake the agent, and nobody was asked.
// The default protects the runtime; this check protects the decision.
//
// So: scan producers, compare against the manifest, and fail on any type that
// is written but unclassified. The fix for a failure is to add a manifest
// entry with a reason -- including, quite often, "journal-only because ...",
// which is exactly the decision that was being skipped.

const fs = require('fs');
const path = require('path');

const MANIFEST = path.join('src', 'mind', 'conscious', 'event-type-census.sexp');

// Types written by the conscious runtime itself, or consumed from producers
// that do not yet exist. Each needs a reason, same rule as a manifest entry.
const NOT_YET_PRODUCED = {
  'pulse-committed': 'concern outcome event; produced from Q3',
  'concern-deferred': 'concern outcome event; produced from Q3',
  'concern-presented': 'concern outcome event; produced from Q3',
};

function stripComments(text) {
  return text
    .split('\n')
    .map((line) => {
      let inString = false;
      for (let i = 0; i < line.length; i += 1) {
        const c = line[i];
        if (c === '"' && line[i - 1] !== '\\') inString = !inString;
        else if (c === ';' && !inString) return line.slice(0, i);
      }
      return line;
    })
    .join('\n');
}

function manifestTypes() {
  const text = stripComments(fs.readFileSync(MANIFEST, 'utf8'));
  const types = new Set();
  for (const m of text.matchAll(/\(:type\s+"([^"]+)"/g)) types.add(m[1]);
  return types;
}

function producedTypes() {
  const found = new Map();
  (function walk(dir) {
    for (const e of fs.readdirSync(dir, { withFileTypes: true })) {
      const p = path.join(dir, e.name);
      if (e.isDirectory()) walk(p);
      else if (p.endsWith('.lisp')) {
        const text = fs.readFileSync(p, 'utf8');
        // Literal type names at any *-log call site. Generalised from
        // log-event alone after the first run reported near-term-intention
        // events as "not produced" — they are written through a helper
        // (%near-term-intention-log) that the narrower pattern missed.
        const patterns = [
          /\(log-event\s+"([a-z0-9-]+)"/g,
          /funcall\s+'log-event\s+"([a-z0-9-]+)"/g,
          /\(%[a-z0-9-]*log[a-z0-9-]*\s*\n?\s*"([a-z0-9-]+)"/g,
        ];
        for (const re of patterns) {
          for (const m of text.matchAll(re)) {
            if (!found.has(m[1])) found.set(m[1], []);
            const where = found.get(m[1]);
            const rel = p.split(path.sep).join('/');
            if (!where.includes(rel)) where.push(rel);
          }
        }
      }
    }
  })('src');
  return found;
}

const declared = manifestTypes();
const produced = producedTypes();

const unclassified = [];
for (const [type, files] of produced) {
  if (!declared.has(type)) unclassified.push({ type, files });
}

// A manifest entry for a type nothing produces is not an error -- it may be a
// future producer or a payload-discriminated pseudo-entry -- but it is worth
// reporting, since a stale classification is a decision about nothing.
const unproduced = [];
for (const type of declared) {
  if (!produced.has(type) && !NOT_YET_PRODUCED[type] && !type.includes('/')) {
    unproduced.push(type);
  }
}

// HONEST LIMIT: this finds LITERAL type names at call sites. A producer that
// computes its type -- heap-health does, choosing "heap-pressure" or
// "heap-health" from a threshold -- cannot be found by scanning, so absence
// from this list is not proof a type is unproduced. That is why an
// unproduced classification is reported and not failed: the scan is
// authoritative about what it FINDS, never about what it does not.
console.log(`producers write ${produced.size} distinct literal event types`);
console.log(`manifest classifies ${declared.size}`);

if (unproduced.length) {
  console.log(`\nclassified but not currently produced (${unproduced.length}):`);
  for (const t of unproduced.sort()) console.log(`  ${t}`);
  console.log('  (not a failure -- future producers and pseudo-entries live here)');
}

if (unclassified.length) {
  console.error(`\nUNCLASSIFIED event types written by producers (${unclassified.length}):`);
  for (const { type, files } of unclassified.sort((a, b) => a.type.localeCompare(b.type))) {
    console.error(`  ${type}`);
    for (const f of files) console.error(`      ${f}`);
  }
  console.error(`
Each needs an entry in ${MANIFEST} with a :class and a :reason.
Defaulting to journal-only is usually right -- but it should be a decision
someone made and wrote down, not one that happened because nobody looked.`);
  process.exitCode = 1;
} else {
  console.log('\nevery produced event type is classified');
}
