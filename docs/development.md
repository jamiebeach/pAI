# Development envelope

This candidate supports source qualification with SBCL, Quicklisp, Python 3.10+
and Docker. Image construction downloads operating-system and Lisp dependencies;
it requires network access. Dependencies are not yet fully version-locked; see
[the dependency inventory](dependency-provenance.md).

From this directory:

```sh
docker compose build pai-dev
docker compose up -d pai-dev
docker compose exec pai-dev python3 scripts/pai_cli.py --help
```

The development service has a read-only source mount, disposable `/agent/state`,
and no runtime network. It contains no credentials, preserved agent state, or
automatically launched cognition. Stop it with `docker compose down`.

Run the offline source load:

```sh
docker compose exec pai-dev sbcl --dynamic-space-size 2048 --non-interactive \
  --load /opt/quicklisp/setup.lisp --eval '(require :asdf)' \
  --eval '(push #p"/workspace/" asdf:*central-registry*)' \
  --eval '(asdf:load-system "pai")'
```

Run the qualification commands in [qualification.md](qualification.md).
For Docker commands entered through Git Bash on Windows, set
`MSYS_NO_PATHCONV=1` so the shell does not rewrite container paths.

## New instance

Copy `config/instance.example.json` to the ignored
`config/instance.json`. Replace every `replace-with-...` value, keeping runtime,
storage, provider, persona, web and memory settings in that file. Credentials are
forbidden in instance configuration; supply them through the documented ignored
secret files or environment variables.

Start the instance with:

```sh
python3 scripts/pai_instance.py --config config/instance.json
```

When the configured event database is absent and `initialize_if_empty` is true,
this appends a durable genesis record and a sealed empty memory baseline. Later
starts detect the existing event authority and omit initialization. Defaults keep
paid and autonomous activity disabled. Existing-instance backups are never read
by this path.

### A second, persistent, Docker-based instance

The path above is the local-provider-only quick start. For a persistent
instance running in Docker (`pai-runtime` in `compose.yaml`, an OpenRouter
provider profile, a web terminal) -- including a *second* instance cloned
into a sibling directory on the same host -- see
`scripts/pai_bootstrap.py new` and `docs/windows-clone-bootstrap-notes.md`.
That doc records real problems hit bootstrapping a second instance this way
on Windows (line-ending corruption on clone, a native-pathname bug, SQLite
over a Windows bind mount, an image-tag collision between sibling
instances, an unconfigurable storage-partition constant) and what changed
in this repo as a result -- read it before assuming a fresh clone "just
works" on Windows.

To migrate an existing (non-pAI) agent's event history and memory into a
freshly cloned instance rather than starting empty, see
`scripts/pai_bootstrap.py clone-legacy-agent`.

The release qualification performs this flow with synthetic identity and model
values, no provider credential, and networking disabled. It then restarts from
the same event database and deletes only the derived database to prove that the
projection is rebuildable from the ledger.

The public config launcher permits missing-derived recovery only through ledger
position 10,000, matching the recursive and reviewed-graph replay bounds. This
permission does not repair an existing incomplete or stale checkpoint. The
ordinary `pai_cli.py` live entry point still requires explicit offline rebuild
for a missing derived database. For a larger ledger or an ordinary live instance,
stop the instance and use the maintenance procedure in
[first-run-qualification.md](first-run-qualification.md).

The `clone-*.sh` scripts are retained as legacy development tools. They require
explicit `PAI_REPO`, `PAI_CLONE_STATE`, and `PAI_IMAGE` values and an isolated clone
database/network. They do not define the supported public setup.

Paid laboratory launchers require a new, explicit operator approval seal. Historical
approval seals are intentionally absent; a missing seal is not authorization.
