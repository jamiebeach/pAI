FROM debian:bookworm-slim@sha256:7b140f374b289a7c2befc338f42ebe6441b7ea838a042bbd5acbfca6ec875818

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        bash \
        ca-certificates \
        curl \
        git \
        libpq5 \
        libsqlite3-0 \
        libssl3 \
        python3 \
        sbcl \
    && rm -rf /var/lib/apt/lists/*

COPY docker/quicklisp.lock /tmp/quicklisp.lock
COPY docker/quicklisp-releases.lock /tmp/quicklisp-releases.lock

RUN set -eu; \
    . /tmp/quicklisp.lock; \
    curl -fsSL https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp; \
    echo "$QUICKLISP_BOOTSTRAP_SHA256  /tmp/quicklisp.lisp" | sha256sum -c -; \
    sbcl --noinform --non-interactive \
        --load /tmp/quicklisp.lisp \
        --eval "(quicklisp-quickstart:install :path \"/opt/quicklisp/\" :client-url \"$QUICKLISP_CLIENT_URL\" :dist-url \"$QUICKLISP_DIST_URL\")"; \
    echo "$QUICKLISP_DISTINFO_SHA256  /opt/quicklisp/dists/quicklisp/distinfo.txt" | sha256sum -c -; \
    echo "$QUICKLISP_RELEASES_SHA256  /opt/quicklisp/dists/quicklisp/releases.txt" | sha256sum -c -; \
    echo "$QUICKLISP_SYSTEMS_SHA256  /opt/quicklisp/dists/quicklisp/systems.txt" | sha256sum -c -; \
    grep -v '^#' /tmp/quicklisp-releases.lock | grep -v '^$' > /tmp/expected-releases.txt; \
    while IFS= read -r release; do \
        grep -Fqx "$release" /opt/quicklisp/dists/quicklisp/releases.txt; \
    done < /tmp/expected-releases.txt; \
    sbcl --noinform --non-interactive \
        --load /opt/quicklisp/setup.lisp \
        --eval '(ql:quickload (quote (:dexador :shasht :hunchentoot :cl-base64 :postmodern :local-time :ironclad :cffi :babel :bordeaux-threads)))'; \
    awk '{print $6}' /tmp/expected-releases.txt | sort > /tmp/expected-prefixes.txt; \
    find /opt/quicklisp/dists/quicklisp/software -mindepth 1 -maxdepth 1 -type d -printf '%f\n' | sort > /tmp/installed-prefixes.txt; \
    diff -u /tmp/expected-prefixes.txt /tmp/installed-prefixes.txt; \
    rm -f /tmp/quicklisp.lisp /tmp/quicklisp.lock /tmp/quicklisp-releases.lock \
        /tmp/expected-releases.txt /tmp/expected-prefixes.txt /tmp/installed-prefixes.txt

# Docker Desktop bind mounts do not share Linux uid ownership. Trust only the
# one workspace path that Compose mounts; this lets the CLI verify .gitignore
# without weakening Git's ownership check globally.
RUN git config --system --add safe.directory /workspace

RUN useradd --create-home --uid 1000 --shell /bin/bash pai \
    && mkdir -p /home/pai/.cache /var/lib/pai \
    && chown -R pai:pai /home/pai /var/lib/pai

# The bind-mounted development service masks this tree. An empty named source
# volume is seeded from it once and is never overwritten on later image builds.
COPY --chown=pai:pai . /workspace

ENV PAI_QUICKLISP_SETUP=/opt/quicklisp/setup.lisp \
    PAI_CONTAINER_PROFILE=workspace-development-v1 \
    PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

WORKDIR /workspace
USER pai

CMD ["sleep", "infinity"]
