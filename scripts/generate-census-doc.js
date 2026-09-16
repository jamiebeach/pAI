// generate-census-doc.js -- render docs/event-type-census.md from the manifest.
//
//   node scripts/generate-census-doc.js          # write the doc
//   node scripts/generate-census-doc.js --check  # fail if it is out of date
//
// The census previously existed twice: a markdown document and a Lisp table,
// maintained by hand, with a fixture that compared a Lisp constant to a Lisp
// hash table while being described as comparing document to code. It checked
// nothing of the kind, and the two had already drifted.
//
// Now src/mind/conscious/event-type-census.sexp is the single source: the
// admission table is built from it at load, and this renders the readable
// view. --check is what CI runs, so an edited manifest with a stale document
// fails rather than diverging quietly.
//
// Deliberately a tiny hand-rolled reader rather than a general s-expression
// parser: the manifest is a fixed, flat shape, and a parser able to read
// arbitrary Lisp would be more code and more failure modes than the job needs.

const fs = require('fs');
const path = require('path');

const MANIFEST = path.join('src', 'mind', 'conscious', 'event-type-census.sexp');
const DOC = path.join('docs', 'event-type-census.md');

function stripComments(text) {
  // Lisp's reader skips ; comments; a naive split does not. The manifest
  // header documents the entry format with a literal (:type "event-type" ...)
  // example inside a comment block, which this parser happily read as a real
  // entry -- reporting 90 types and 20 admitted where the loader saw 89 and
  // 19. Caught only by cross-checking the two counts, which is the argument
  // for having both rather than trusting either.
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

function parseEntries(text) {
  const entries = [];
  const blocks = stripComments(text).split(/\(:type\s+/).slice(1);
  for (const raw of blocks) {
    const typeMatch = raw.match(/^"([^"]+)"/);
    if (!typeMatch) continue;
    const body = raw.slice(typeMatch[0].length);
    const field = (name) => {
      const m = body.match(new RegExp(`:${name}\\s+("([^"]*)"|[^\\s)]+)`));
      if (!m) return null;
      return m[2] !== undefined ? m[2] : m[1];
    };
    entries.push({
      type: typeMatch[1],
      class: field('class'),
      kind: field('kind'),
      source: field('source'),
      urgency: field('urgency'),
      barrier: field('barrier') === 't',
      discriminator: field('discriminator'),
      group: field('group'),
      reason: field('reason'),
    });
  }
  return entries;
}

function render(entries, version) {
  const admitted = entries.filter((e) => e.class === ':stimulus');
  const journal = entries.filter((e) => e.class === ':journal');

  const groups = new Map();
  for (const e of journal) {
    const g = e.group || 'ungrouped';
    if (!groups.has(g)) groups.set(g, []);
    groups.get(g).push(e);
  }

  const out = [];
  out.push('# Event-type census');
  out.push('');
  out.push('**GENERATED — do not edit.**');
  out.push('');
  out.push('Source: `src/mind/conscious/event-type-census.sexp`.');
  out.push('Regenerate: `node scripts/generate-census-doc.js`.');
  out.push('Verify: `node scripts/generate-census-doc.js --check`.');
  out.push('');
  out.push(`Census version ${version}. ${entries.length} event types classified: `
         + `${admitted.length} admitted as stimuli, ${journal.length} journal-only.`);
  out.push('');
  out.push('The admission table in `src/mind/conscious/policy.lisp` is built from');
  out.push('the same manifest at load time, so this document and the running');
  out.push('policy cannot disagree — there is nothing left to disagree.');
  out.push('');
  out.push('## The classification rule');
  out.push('');
  out.push('An event is a **stimulus** if a reasonable agent could need to *wake*');
  out.push('for it, or if ignoring it would leave something permanently unresolved.');
  out.push('Everything else is **journal-only**: written so the past is');
  out.push('reconstructible, not because anything should act on it.');
  out.push('');
  out.push('The default is exclusion. A type absent from the manifest is never');
  out.push('admitted, so adding a stimulus is a deliberate decision — the reverse');
  out.push('default would flood attention with backup notifications.');
  out.push('');
  out.push('## Admitted');
  out.push('');
  out.push('| event type | kind | urgency | barrier | why it can wake the agent |');
  out.push('| --- | --- | --- | --- | --- |');
  for (const e of admitted) {
    out.push(`| \`${e.type}\` | \`${e.kind}\` | ${e.urgency} | ${e.barrier ? 'yes' : 'no'} | ${e.reason} |`);
  }
  out.push('');

  const disc = admitted.filter((e) => e.discriminator);
  if (disc.length) {
    out.push('### Payload-discriminated');
    out.push('');
    out.push('These map to more than one classification depending on payload. A');
    out.push('discriminator may also reclassify a particular payload as');
    out.push('journal-only — a delivered scheduler notification being the case');
    out.push('that forced it.');
    out.push('');
    out.push('| event type | discriminator |');
    out.push('| --- | --- |');
    for (const e of disc) out.push(`| \`${e.type}\` | \`${e.discriminator}\` |`);
    out.push('');
  }

  out.push('## Journal-only, with reasons');
  out.push('');
  out.push('The exclusions are the part worth reviewing: each is a claim that the');
  out.push('agent is not missing something.');
  out.push('');
  for (const [group, items] of groups) {
    out.push(`**${group}**`);
    out.push('');
    for (const e of items) out.push(`- \`${e.type}\` — ${e.reason}`);
    out.push('');
  }
  return out.join('\n').trimEnd() + '\n';
}

const text = fs.readFileSync(MANIFEST, 'utf8');
const version = (text.match(/:census-version\s+(\d+)/) || [])[1] || '?';
const rendered = render(parseEntries(text), version);

if (process.argv.includes('--check')) {
  const current = fs.existsSync(DOC) ? fs.readFileSync(DOC, 'utf8') : '';
  if (current !== rendered) {
    console.error('docs/event-type-census.md is out of date with the manifest.');
    console.error('Run: node scripts/generate-census-doc.js');
    process.exitCode = 1;
  } else {
    console.log('census doc is current');
  }
} else {
  fs.writeFileSync(DOC, rendered);
  console.log(`wrote ${DOC}`);
}
