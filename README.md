# Robert Jurgens, FCAS

Live site: **[https://sinisterrob.github.io](https://sinisterrob.github.io)**

Personal portfolio and illustrative excess-layering / ILF workbench for **Robert Jurgens, FCAS** (Senior Actuarial Consultant, Edgewater Actuarial Insights).

## Architecture

- **`src/excess_layers.rs`** — Zero-dependency deterministic Rust kernel compiled to `wasm32-unknown-unknown` (`excess_layers.wasm`, ~9.5 KB). Evaluates synthetic lognormal severity distributions, closed-form limited expected layer losses, attachment probabilities, Increased Limit Factors (ILFs), and a fixed-seed 65,536-draw SplitMix64 Box–Muller Monte Carlo verification check.
- **`index.html`, `style.css`, `excess-layers.js`** — Static HTML/CSS/SVG front end with pre-rendered baseline values and live WebAssembly hydration.
- **`scripts/build.sh`** — Runs Rust unit tests, builds the optimized WebAssembly module, verifies exports, renders `og-image.png`, and syncs published assets to `dist/`.

## Related Repository

- **[`reserving-kernels`](https://github.com/SinisterRob/reserving-kernels)** — Pure-Rust P&C loss-reserving, Mack standard errors, ODP bootstrap, Iman–Conover dependence, and parametric severity/ILF kernels.

## Build & Test

```bash
./scripts/build.sh
```
