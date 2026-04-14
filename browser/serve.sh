#!/usr/bin/env bash
set -euo pipefail

# Build the WASM inspector and serve the browser app locally.
# Usage: ./browser/serve.sh [PORT]

PORT="${1:-8080}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_DIR"

echo "Building inspect-address for WASM..."
nix shell 'gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org#all_9_12' \
  --command bash -c "wasm32-wasi-cabal --project-file=cabal-wasm.project build inspect-address"

WASM=$(find dist-newstyle -name "inspect-address.wasm" -type f | head -1)
if [ -z "$WASM" ]; then
  echo "Error: inspect-address.wasm not found"
  exit 1
fi

echo "Copying artifacts..."
cp "$WASM" "$SCRIPT_DIR/inspect-address.wasm"

echo ""
echo "Serving at http://localhost:$PORT"
echo "Open in browser and paste a Cardano address."
echo ""
cd "$SCRIPT_DIR"
python3 -m http.server "$PORT"
