#!/bin/sh
# clone-run.sh -- run a pAI script against the restored clone.
#
#   scripts/clone-run.sh <script-name> [prompt]
# e.g.
#   scripts/clone-run.sh clone-turn.lisp "what do you remember about me?"
#
# Deliberately sets NO credentials. Without TELEGRAM_BOT_TOKEN or
# OPENROUTER_API_KEY in the environment there is no path out of this container
# to a transport or a paid provider, whatever any code path attempts.

set -u

SCRIPT="${1:-clone-turn.lisp}"
PROMPT="${2:-Briefly, what do you remember about me?}"

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
  -e PAI_COGNITION_RUNTIME=auto \
  -e PAI_OLLAMA_ENDPOINT=http://pai-clone-ollama:11434/api/embeddings \
  -e PAI_MODEL_ENDPOINT="${PAI_MODEL_ENDPOINT:-http://pai-clone-ollama:11434/v1/chat/completions}" \
  -e PAI_MODEL="${PAI_MODEL:-qwen2.5:1.5b}" \
  -e PAI_TURN_PROMPT="$PROMPT" \
  --entrypoint sh \
  "$IMAGE" \
  -c "sbcl --dynamic-space-size 4096 --non-interactive --load /pai/scripts/$SCRIPT"
