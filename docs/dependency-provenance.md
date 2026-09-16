# Dependency and provenance inventory

This inventory describes candidate r27's successfully built development image.
It is evidence for review, not a legal conclusion. Public dependency notices are
in `THIRD_PARTY_NOTICES.md`.

## Base and host packages

The Dockerfile pins Debian bookworm-slim by image digest
`sha256:7b140f374b289a7c2befc338f42ebe6441b7ea838a042bbd5acbfca6ec875818`.
The built image reported:

| Package | Installed version |
| --- | --- |
| bash | 5.2.15-2+b13 |
| ca-certificates | 20250419~deb12u1 |
| curl | 7.88.1-10+deb12u15 |
| git | 1:2.39.5-0+deb12u3 |
| libpq5 | 15.19-0+deb12u1 |
| libsqlite3-0 | 3.40.1-2+deb12u2 |
| libssl3 | 3.0.20-1~deb12u2 |
| python3 | 3.11.2-1+b1 |
| SBCL | 2:2.2.9-1 |

These package versions are observations from one built image. `apt-get install`
does not pin them in the Dockerfile, so a later cold build can select newer
bookworm packages even though the base image digest is fixed.

## Direct Common Lisp dependencies

The Dockerfile asks Quicklisp for ten direct systems. ASDF metadata from the
built image reported:

| System | Version | Declared license |
| --- | --- | --- |
| dexador | 0.9.15 | MIT |
| shasht | 0.1 | MIT |
| hunchentoot | 1.3.1 | BSD-2-Clause |
| cl-base64 | 3.4 | BSD-style |
| postmodern | 1.33.11 | zlib |
| local-time | 1.0.6 | BSD |
| ironclad | not declared by ASDF | not declared by ASDF |
| cffi | not declared by ASDF | MIT |
| babel | not declared by ASDF | MIT |
| bordeaux-threads | 0.9.4 | MIT |

The 40-release transitive closure is recorded in
`docker/quicklisp-releases.lock`. The Docker build verifies every complete lock
row against the pinned release index, then verifies that the installed directory
set exactly equals that closure. Licenses were reviewed from the archived notice,
source-header, README, and ASDF text; the result is recorded in
`THIRD_PARTY_NOTICES.md`. In particular, quri's bundled public-suffix data is
MPL-2.0 even though quri itself is BSD-3-Clause.

The image contains Quicklisp client `2021-02-13` and distribution `2026-01-01`.
`docker/quicklisp.lock` pins those versions and the SHA-256 hashes of the
Quicklisp bootstrap, distribution descriptor, release index, and system index.
The Docker build fails before resolving project systems when any reviewed byte
changes. Quicklisp's dated release index supplies the archive identities for the
resolved Lisp release set.

## Reproducibility policy

Application dependencies use a content-locked Quicklisp index and an exact
transitive closure. Debian package versions intentionally follow the repositories
available to the pinned bookworm base image at build time so security updates are
not frozen indefinitely. Therefore a release records its built image digest and
installed Debian versions; the project does not promise bit-identical images from
later rebuilds. A changed image must repeat the cold build and qualification.

Repository-source provenance is recorded separately in
`docs/source-provenance.md`. The review found no vendored binary assets or source
with a distinct embedded third-party license.

## Cold build evidence — 2026-09-15

Candidate r27 built with `docker build --no-cache --pull` into
`pai-public-cold-r27:20260915`. The build downloaded and verified the pinned
Quicklisp client and `2026-01-01` distribution from an empty build layer. The
resulting image identity was
`sha256:fb2b4b5c9262d38dc1c7e3474a0ba0ab2e2f818acbf825a140cd8f8aded05ca6`.
Its `/workspace` contained exactly the 645 candidate files with no missing,
extra, or changed bytes. With networking disabled and disposable `/agent/state`,
the documented Quicklisp-aware ASDF load completed successfully.

The first locked build attempt passed the bootstrap hash but used Quicklisp's
unscoped dated distribution path, which returned HTTP 403. A second attempt
proved that the pinned 2021 bootstrap cannot parse HTTPS URLs. Neither attempt
produced a candidate image. The accepted build names the canonical dated
`dist/quicklisp/2026-01-01/` URL explicitly and verifies the downloaded
descriptor, releases index, and systems index before resolving any project
system. The old client uses HTTP transport; the reviewed SHA-256 values are the
integrity boundary for those indexes.
