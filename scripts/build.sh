#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

echo "==> Running Rust unit tests (edition 2024)..."
TEST_BIN="$(mktemp /tmp/excess_layers_test.XXXXXX)"
trap 'rm -f "$TEST_BIN"' EXIT
rustc --edition=2024 --test src/excess_layers.rs -o "$TEST_BIN"
"$TEST_BIN"

echo "==> Building optimized WebAssembly kernel..."
rustc --edition=2024 \
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
node <<'JS'
const fs = require("fs");
WebAssembly.instantiate(fs.readFileSync("excess_layers.wasm")).then(({ instance }) => {
  const e = instance.exports;
  for (const fn of ["limited_expected_value", "expected_layer", "simulated_layer", "exceedance_prob", "severity_pdf"]) {
    if (typeof e[fn] !== "function") throw new Error("Missing export: " + fn);
  }
  const primary = e.expected_layer(1, 100000, 350000, 0);
  const excess = e.expected_layer(1, 100000, 350000, 1);
  if (Math.abs(primary - 61707.51) > 0.05 || Math.abs(excess - 29650.34) > 0.05) {
    throw new Error(`Unexpected default values: Primary=${primary}, Excess=${excess}`);
  }
  const pAtt = e.exceedance_prob(1, 100000);
  const avgHit = excess / pAtt;
  const layerRate = (excess / 250000) * 100;
  console.log(`    Verified Moderate $250k xs $100k: Primary=$${primary.toFixed(2)}, Excess=$${excess.toFixed(2)}, ILF=${(1 + excess / primary).toFixed(2)}x, LayerRate=${layerRate.toFixed(2)}%, AvgHit=$${avgHit.toFixed(2)}`);
});
JS

if command -v rsvg-convert >/dev/null 2>&1 && [ -f og-image.svg ]; then
  echo "==> Rendering og-image.png from og-image.svg..."
  rsvg-convert -w 1200 -h 630 og-image.svg -o og-image.png
fi

echo "==> Build complete."
