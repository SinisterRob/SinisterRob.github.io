#!/usr/bin/env bash
# Enable repo-tracked .githooks on this checkout, or across all Tailscale mesh nodes.
#
# Usage:
#   ./scripts/install-hooks.sh         # Configure hooks locally
#   ./scripts/install-hooks.sh --mesh  # Configure hooks on local + robbit-theliomira + robbit-risha-rig

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MESH=false

if [[ "${1:-}" == "--mesh" ]]; then
  MESH=true
fi

configure_local() {
  chmod +x "$REPO_ROOT"/.githooks/* "$REPO_ROOT"/scripts/*.sh "$REPO_ROOT"/scripts/*.py 2>/dev/null || true
  git -C "$REPO_ROOT" config core.hooksPath .githooks
  echo "[local] core.hooksPath -> $(git -C "$REPO_ROOT" config --get core.hooksPath)"
}

configure_local

if [[ "$MESH" == true ]]; then
  for spec in \
    "robbit-theliomira:~/Projects/SinisterRob.github.io" \
    "robbit-risha-rig:~/Projects/SinisterRob.github.io"; do
    HOST="${spec%%:*}"
    REMOTE_DIR="${spec#*:}"
    if ssh -o BatchMode=yes -o ConnectTimeout=5 "$HOST" \
      "bash -lc 'if [ -d ${REMOTE_DIR}/.git ]; then git -C ${REMOTE_DIR} config core.hooksPath .githooks && echo \"[${HOST}] core.hooksPath -> \$(git -C ${REMOTE_DIR} config --get core.hooksPath)\"; else echo \"[${HOST}] repo not found at ${REMOTE_DIR}\"; fi'"; then
      :
    else
      echo "[${HOST}] unreachable (skipped)" >&2
    fi
  done
fi
