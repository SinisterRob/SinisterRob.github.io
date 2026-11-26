#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

echo "==> Running Rust unit tests (edition 2021)..."
TEST_BIN="$(mktemp /tmp/excess_layers_test.XXXXXX)"
trap 'rm -f "$TEST_BIN"' EXIT
rustc --edition=2021 --test src/excess_layers.rs -o "$TEST_BIN"
"$TEST_BIN"

echo "==> Building optimized WebAssembly kernel..."
rustc --edition=2021 \
  --crate-type=cdylib \
  --target=wasm32-unknown-unknown \
  -C opt-level=z \
  -C lto=yes \
  -C codegen-units=1 \
  -C panic=abort \
  -C strip=symbols \
  -C link-arg=--gc-sections \
  src/excess_layers.rs \
  -o excess_layers.wasm
chmod 644 excess_layers.wasm

WASM_BYTES="$(wc -c < excess_layers.wasm)"
WASM_GZIP="$(gzip -c excess_layers.wasm | wc -c)"
echo "    excess_layers.wasm: ${WASM_BYTES} bytes (${WASM_GZIP} bytes gzipped)"

echo "==> Verifying WebAssembly exports..."
node -e '
const fs = require("fs");
WebAssembly.instantiate(fs.readFileSync("excess_layers.wasm")).then(({ instance }) => {
  const e = instance.exports;
  for (const fn of ["expected_layer", "simulated_layer", "exceedance_prob", "severity_pdf"]) {
    if (typeof e[fn] !== "function") throw new Error("Missing export: " + fn);
  }
  const primary = e.expected_layer(1, 350000, 0);
  const excess = e.expected_layer(1, 350000, 1);
  if (Math.abs(primary - 61707.51) > 0.05 || Math.abs(excess - 29650.34) > 0.05) {
    throw new Error(`Unexpected default values: Primary=${primary}, Excess=${excess}`);
  }
  console.log(`    Verified default Moderate @ $350k: Primary=$${primary.toFixed(2)}, Excess=$${excess.toFixed(2)}, ILF=${(1 + excess / primary).toFixed(2)}x`);
});
'

if command -v rsvg-convert >/dev/null 2>&1 && [ -f og-image.svg ]; then
  echo "==> Rendering og-image.png from og-image.svg..."
  rsvg-convert -w 1200 -h 630 og-image.svg -o og-image.png
fi

echo "==> Syncing published site files to dist/..."
rm -rf dist
mkdir -p dist
SITE_FILES=(
  .nojekyll
  index.html
  style.css
  excess-layers.js
  excess_layers.wasm
  favicon.svg
  llms.txt
  og-image.png
  og-image.svg
  robots.txt
  sitemap.xml
)
for f in "${SITE_FILES[@]}"; do
  if [ -f "$f" ]; then
    cp -f "$f" "dist/$f"
  fi
done

echo "==> Build & sync complete."
