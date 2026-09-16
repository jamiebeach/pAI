# Third-party notices

pAI does not vendor its Common Lisp dependencies in this source repository.
The development image resolves the exact 40-release closure recorded in
[`docker/quicklisp-releases.lock`](docker/quicklisp-releases.lock). That lock
records each Quicklisp archive URL, byte count, archive MD5, unpacked-content
SHA-1, directory prefix, and supplied ASDF files. The image retains the upstream
license and copyright files under `/opt/quicklisp/dists/quicklisp/software/`.

The following inventory was reviewed from those exact archives. “Public domain”
is the upstream project's description rather than an SPDX legal conclusion.

| Release | License or dedication |
| --- | --- |
| alexandria | Public-domain dedication |
| babel | MIT |
| bordeaux-threads | MIT |
| cffi | MIT |
| chipz | BSD-2-Clause |
| chunga | BSD-2-Clause |
| cl+ssl | MIT |
| cl-base64 | BSD-3-Clause |
| cl-cookie | BSD-2-Clause |
| cl-fad | BSD-2-Clause |
| cl-ppcre | BSD-2-Clause |
| cl-utilities | Public-domain statement; its bundled historical `split-sequence.lisp` notes uncertainty |
| closer-mop | MIT |
| dexador | MIT |
| fast-http | MIT |
| fast-io | MIT |
| flexi-streams | BSD-2-Clause |
| global-vars | MIT |
| hunchentoot | BSD-2-Clause |
| idna | MIT |
| ironclad | BSD-3-Clause |
| local-time | MIT |
| md5 | Public-domain dedication and CC0-1.0 waiver |
| postmodern | Zlib |
| proc-parse | BSD-2-Clause |
| quri | BSD-3-Clause; bundled public-suffix data is MPL-2.0 |
| rfc2388 | BSD-3-Clause; bundled RFC text retains the Internet Society notice |
| shasht | MIT |
| smart-buffer | BSD-3-Clause |
| split-sequence | MIT |
| static-vectors | MIT |
| trivial-backtrace | MIT |
| trivial-do | MIT |
| trivial-features | MIT |
| trivial-garbage | Public-domain statement |
| trivial-gray-streams | MIT |
| trivial-mimes | Zlib |
| uax-15 | MIT |
| usocket | MIT |
| xsubseq | BSD-2-Clause |

The container base is Debian bookworm-slim. Debian package copyright files are
retained in built images under `/usr/share/doc/*/copyright`. This repository does
not publish a prebuilt image; anyone distributing an image should preserve those
files and the Quicklisp source archives' notices.
