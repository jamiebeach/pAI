# Empty-state qualification

The public instance path must work without a restored private database. This
gate uses a synthetic identity, no credentials, no provider connection, and a
container with networking disabled. All behavioral settings come from a copy of
`config/instance.example.json`; the launcher receives only its config pathname.

## Acceptance contract

1. An absent event database with `initialize_if_empty` enabled creates one
   durable `instance-genesis` event and one sealed empty-memory baseline.
2. A second start opens the same ledger without adding another genesis or
   baseline.
3. Removing only the derived SQLite database allows startup to rebuild every
   projection from the unchanged event ledger.
4. A derived database without its event database is rejected.
5. Local provider configuration cannot select a paid-provider profile, and the
   instance config cannot contain credential-like keys.

## Candidate r26 evidence — 2026-09-15

The isolated run used `agent:synthetic-first-run`, fixture model and embedding
revision names, and `local-providerless-v1`. The first start completed all five
startup phases, reported `initialized-empty`, reached the prompt, displayed the
configured runtime settings, and exited with status 0. The ledger contained five
events in authority order:

1. `instance-genesis`
2. `memory-baseline-started`
3. `memory-baseline-committed`
4. `runtime-settings-initialized`
5. `conscious-runtime-plan-registered`

The memory receipt contained zero nodes and zero edges. Its node, vector, and
edge digests were the canonical SHA-256 of empty input,
`e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855`.
The second start reported `opened`, five events, maximum event ID 5, and reached
the prompt without appending duplicate initialization events.

After a WAL checkpoint, the authoritative event database SHA-256 was
`7600acf9a33bda888f541676303e27aa92043117add582f370c24e9dfa34809b`.
Only `derived.sqlite3` and its WAL/SHM companions were removed. The next start
reported `projection-rebuilt-from-ledger`, five events through ID 5, and reached
the prompt. The event database retained the exact same SHA-256.

The gate exposed and rejected three incomplete behaviors before passing:

- the supported entry point still required a private clone label and fixed
  development agent ID;
- a durable local provider label was interpreted as a paid-provider profile;
- an absent derived database was treated as an interrupted migration instead of
  a rebuildable projection.

The fixes retain fail-closed handling for an existing checkpoint-free database,
which can still represent an interrupted migration. The focused SQLite authority
suite passes 29/29, including missing-projection rebuild, migration interruption,
composition drift, and event-database binding. The config/startup/provider Python
set passes 21/21 in the network-disabled Linux image.

The immutable r26 image also passed the live web adapter security suite 30/30.
The suite started a real loopback acceptor and observed anonymous HTTP 401,
authenticated HTTP 200, unauthenticated terminal redirection, and HTTP 403 for an
authenticated mutation without its same-origin marker. It also verifies that a
non-loopback listener is refused without configured credentials.

The authenticated file API suite passed 21/21 in the same network-disabled
image. It covers listing, UTF-8 reads, binary download, traversal rejection,
read-only defaults, explicit authenticated write authority, CSRF rejection,
upload persistence, and download-only previews for large or binary files.
