#!/bin/sh
# clone-boot.sh -- run pAI against a restored clone in a container.
#
# No PowerShell: this has to work the same on any host that can run Docker.
#
# The repository mounts read-only. State mounts read-write, at /agent/state,
# because 118 source references still hardcode that path (see
# docs/progress.md). Mounting a copy there is what lets the restore path be
# tested before that refactor.
#
# The clone database is on its own Docker network and is never the production
# container. clone-boot.lisp refuses to run if PAI_PG_HOST does not look like
# a clone.
#
# Usage:
#   scripts/clone-boot.sh [phases]
# e.g.
#   scripts/clone-boot.sh "configure install restore"

set -u

PHASES="${1:-configure install restore}"

REPO="${PAI_REPO:?Set PAI_REPO to the absolute source directory}"
STATE="${PAI_CLONE_STATE:?Set PAI_CLONE_STATE to disposable restored state}"
IMAGE="${PAI_IMAGE:?Set PAI_IMAGE to the prepared development image}"
NET="${PAI_CLONE_NET:-pai-clone-net}"

MSYS_NO_PATHCONV=1 docker run --rm \
  --network "$NET" \
  -v "$REPO:/pai:ro" \
  -v "$STATE:/agent/state" \
  -e PAI_PG_BACKUP=off \
  -e PAI_PG_HOST=pai-clone-postgres \
  -e PAI_PG_PORT=5432 \
  -e PAI_PG_DATABASE=pai_memory \
  -e PAI_PG_USER=pai \
  -e PAI_PG_PASSWORD=pai_local_dev_only \
  -e PAI_CLONE_PHASES="$PHASES" \
  --entrypoint sh \
  "$IMAGE" \
  -c "sbcl --dynamic-space-size 4096 --non-interactive --load /pai/scripts/clone-boot.lisp"
