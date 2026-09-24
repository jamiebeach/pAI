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
3. Removing only the derived SQLite database allows the public config launcher
   to rebuild from the unchanged ledger through position 10,000. Larger ledgers
   and ordinary live CLI instances require explicit stopped-instance maintenance.
   Empty reviewed graphs must also publish a source-bound checkpoint.
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

## Reconciliation candidate check — 2026-09-23

This check used a new synthetic agent, a disposable Docker volume, no
credentials, and networking disabled. Its first start completed in 135.9 s
(including cold Lisp dependency compilation). A second start reopened the
same five-event ledger in 4.7 s without adding another genesis or baseline.

Acceptance item 3 above does **not** currently pass: after moving only the
derived database and its WAL/SHM files aside in the disposable volume,
ordinary startup refused with “derived database is absent; explicit offline
rebuild is required.” That fail-closed behavior is intentional for the first
checkpoint-dependent private-instance canary, but it is a change from this
public empty-state contract. The offline maintenance command rebuilt the
conscious and recursive projections at position 5, then failed before a
reviewed-graph completion receipt. The five logical event rows were unchanged.
This is an unqualified first-run/rebuild profile, not a passing release gate;
the old r26 evidence above does not qualify the current candidate.

## Recovery policy and requalification — 2026-09-24

The full 243-suite isolated run and both serial performance profiles passed on
the pre-fix source candidate. Its clean-image first-run/restart check reproduced
the missing-derived refusal, and a separate maintenance probe reproduced the
absent empty-graph checkpoint. Those results do not qualify the recovery changes.

The bounded public-launcher path grants only missing-derived recovery; it cannot
repair composition drift or an existing checkpoint-free database. Ordinary live
CLI startup explicitly disables that permission, including when an inherited
environment variable attempts to enable it. Graph synchronization now publishes
the initial checkpoint even without graph journals, while warm reads and valid
cold restores avoid another write. Offline maintenance honors its documented
default derived path under the selected state directory.

For explicit recovery, stop the owning instance and preserve an independent
state backup. Set `PAI_ROOT` to the source root, `PAI_STATE_ROOT` to that instance's
state directory, and `PAI_AGENT_ID` / `PAI_PERSONA_ID` to its existing identifiers.
Optionally set `PAI_DERIVED_DATABASE`; otherwise it defaults to
`derived.sqlite3` under `PAI_STATE_ROOT`. With Quicklisp loaded, run:

```sh
sbcl --dynamic-space-size 3072 --non-interactive \
  --load /opt/quicklisp/setup.lisp \
  --load "$PAI_ROOT/scripts/rebuild-recursive-thread-checkpoint.lisp"
```

Require exit zero and completion receipts for conscious, recursive and reviewed
graph projections, then restart normally so memory projections restore from the
ledger. A ready prompt after a **failed** maintenance command is only partial
recovery, not qualification. Verify unchanged authoritative event rows before
and after. Final release qualification must repeat this flow in a disposable,
network-disabled image built from the exact candidate tree, including automatic
small-ledger recovery and explicit offline recovery with the default path.
