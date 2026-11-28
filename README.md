<p align="center">
  <img src="https://raw.githubusercontent.com/SinisterRob/SinisterRob.github.io/main/assets/banner.png" alt="sinisterrob.github.io — Actuarial portfolio and Rust/WASM labs" width="100%" />
</p>

<p align="center">
  <strong>Personal actuarial portfolio and dual Rust/WebAssembly interactive workbenches for Robert Jurgens, FCAS.</strong>
</p>

<p align="center">
  <a href="https://sinisterrob.github.io"><img src="https://img.shields.io/badge/live-sinisterrob.github.io-f97316?style=flat" alt="Live Site" /></a>
  <a href="https://github.com/SinisterRob/SinisterRob.github.io/actions/workflows/ci.yml"><img src="https://img.shields.io/github/actions/workflow/status/SinisterRob/SinisterRob.github.io/ci.yml?branch=main&label=CI&style=flat" alt="CI" /></a>
  <a href="src/excess_layers.rs"><img src="https://img.shields.io/badge/Rust-Edition%202024-f97316?style=flat" alt="Rust Edition 2024" /></a>
  <a href="scripts/build.sh"><img src="https://img.shields.io/badge/target-wasm32-30363d?style=flat" alt="WebAssembly target" /></a>
  <a href="index.html"><img src="https://img.shields.io/badge/frontend-zero--framework%20static-30363d?style=flat" alt="Zero-framework static site" /></a>
  <a href="scripts/verify-site.py"><img src="https://img.shields.io/badge/tests-Playwright%20%2B%20static-30363d?style=flat" alt="Playwright and static verification" /></a>
</p>

<p align="center">
  <a href="https://sinisterrob.github.io">Live Site</a> &nbsp;·&nbsp;
  <a href="https://sinisterrob.github.io/reserving/">Reserving Lab</a> &nbsp;·&nbsp;
  <a href="#interactive-exhibits">Exhibits</a> &nbsp;·&nbsp;
  <a href="#architecture">Architecture</a> &nbsp;·&nbsp;
  <a href="#verification-and-ci">Verification &amp; CI</a> &nbsp;·&nbsp;
  <a href="#build-and-test">Build &amp; Test</a> &nbsp;·&nbsp;
  <a href="https://github.com/SinisterRob/pricing-kernels">pricing-kernels</a> &nbsp;·&nbsp;
  <a href="https://github.com/SinisterRob/reserving-kernels">reserving-kernels</a>
</p>

---

`SinisterRob.github.io` is the personal portfolio and interactive actuarial lab for **Robert Jurgens, FCAS** (Senior Actuarial Consultant, Edgewater Actuarial Insights). It pairs a zero-framework static HTML/CSS/SVG front end with two client-side Rust/WebAssembly compute kernels — an **Excess Layer & Increased Limits Factor (ILF) Simulator** at [`/`](https://sinisterrob.github.io/) and a **Loss Reserving & Mack Variability Simulator** at [`/reserving/`](https://sinisterrob.github.io/reserving/) powered by [`reserving-kernels`](https://github.com/SinisterRob/reserving-kernels).

## Interactive Exhibits

| Exhibit | Route | WASM Kernel | Models & Capabilities |
| --- | --- | --- | --- |
| **Excess Layer & ILF Simulator** | [`/`](https://sinisterrob.github.io/) | `excess_layers.wasm` (`15.1 KB` / `6.8 KB` gz) | Lognormal & spliced Gamma–Pareto severity tails ($100k ground-up mean), closed-form Limited Expected Values (LEV), attachment probability, ILFs, layer rate, direct-scrubbing ILF/PDF SVG charts, and a fixed-seed 65,536-draw SplitMix64 Box–Muller Monte Carlo parity check |
| **Loss Reserving & Mack Variability Simulator** | [`/reserving/`](https://sinisterrob.github.io/reserving/) | `reserving_kernels.wasm` (`55.2 KB` / `19.6 KB` gz) | Chain Ladder, Bornhuetter–Ferguson, and Cape Cod ultimates, Mack (1993) process & parameter standard errors, 6/12/18-month projected cash-flow runoff, and development-pattern views across three published benchmark triangles (`RAA`, `GenIns`, `US Auto`) |

<details>
<summary><strong>WASM exports &amp; embedded benchmark triangles</strong></summary>

### `src/excess_layers.rs` C-ABI Exports

| Export | Purpose |
| --- | --- |
| `limited_expected_value(tail, limit)` | Closed-form Lognormal limited expected value $E[\min(X, L)]$ |
| `expected_layer(tail, attach, limit, layer)` | Exact primary (`layer = 0`) or excess (`layer = 1`) expected layer loss |
| `simulated_layer(tail, attach, limit, layer)` | Deterministic 65,536-draw SplitMix64 Box–Muller Monte Carlo layer check (`SEED = 2026`) |
| `exceedance_prob(tail, threshold)` | Lognormal survival / attachment probability $P(X > x)$ |
| `severity_pdf(tail, x)` | Lognormal severity density $f(x)$ for interactive chart rendering |
| `spliced_threshold(body, tail)` | Continuity splice point for composite Gamma body + Pareto tail |
| `spliced_limited_expected_value(body, tail, limit)` | Closed-form spliced Gamma–Pareto LEV |
| `spliced_expected_layer(body, tail, attach, limit, layer)` | Exact spliced primary or excess expected layer loss |
| `spliced_simulated_layer(body, tail, attach, limit, layer)` | Deterministic 65,536-draw spliced Monte Carlo verification |
| `spliced_exceedance_prob(body, tail, threshold)` | Spliced survival / attachment probability |
| `spliced_severity_pdf(body, tail, x)` | Spliced severity density $f(x)$ |

### Embedded Reserving Benchmark Triangles (`reserving-lab.js`)

| Dataset | Source | Size | Basis |
| --- | --- | :---: | --- |
| **RAA** | Mack (1993) GL Reinsurance | 10×10 | Incurred ($000s) |
| **GenIns** | Taylor & Ashe / England & Verrall | 10×10 | Paid ($) |
| **US Auto** | Friedland (2010) US Industry Auto | 10×10 | Paid ($000s) |

</details>

## Why Static HTML + Rust/WASM?

- **Zero runtime dependencies:** No React, bundler, CDN scripts, or backend server. Both compute kernels compile directly from pure Rust to compact `wasm32-unknown-unknown` binaries (`~26.4 KB` gzipped combined).
- **Instant first paint with live hydration:** Both routes ship with pre-rendered baseline numbers and SVG charts, then hydrate seamlessly once WebAssembly instantiates.
- **Deterministic numerical parity:** Fixed-seed SplitMix64 Box–Muller draws (`SEED = 2026`) and closed-form actuarial formulas produce identical results across native Rust tests and browser WASM execution.
- **Dual-route SEO & print polish:** `scripts/generate-reserving-page.py` keeps `reserving/index.html` synchronized with `index.html` while providing dedicated canonical URLs, Open Graph metadata, JSON-LD, `llms.txt`, and a 2-page print/PDF layout.

## Architecture

| Path | Role |
| --- | --- |
| [`src/excess_layers.rs`](src/excess_layers.rs) | Zero-dependency Rust Edition 2024 severity & excess-layer kernel with unit tests |
| `excess_layers.wasm` | Size-optimized (`-C opt-level=z`, LTO, stripped) WASM build of `src/excess_layers.rs` |
| `reserving_kernels.wasm` | Pre-built C-ABI `wasm32` module from [`reserving-kernels`](https://github.com/SinisterRob/reserving-kernels) |
| [`index.html`](index.html), [`reserving/index.html`](reserving/index.html) | Static portfolio and dual-simulator entry points (root for ILF lab, `/reserving/` for Mack lab) |
| [`style.css`](style.css) | Editorial light/dark design system, responsive 1440/1024/375 layouts, and 2-page print stylesheet |
| [`excess-layers.js`](excess-layers.js), [`reserving-lab.js`](reserving-lab.js) | Vanilla JS WebAssembly bridges, interactive SVG chart scrubbing, and carousel navigation |
| [`og-image.svg`](og-image.svg), [`og-image.png`](og-image.png) | Vector source and `1200×630` rendered Open Graph social preview card |
| [`llms.txt`](llms.txt), [`sitemap.xml`](sitemap.xml), [`robots.txt`](robots.txt) | Structured crawler metadata and LLM-readable portfolio summary |
| [`scripts/build.sh`](scripts/build.sh) | Runs Rust unit tests, builds `excess_layers.wasm`, verifies exports, renders `og-image.png`, and syncs `reserving/index.html` |
| [`scripts/generate-reserving-page.py`](scripts/generate-reserving-page.py) | Deterministic generator and `--check` parity verifier for `reserving/index.html` |
| [`scripts/verify-site.py`](scripts/verify-site.py) | Static HTML/CSS/JS/Rust/WASM lint + headless Chromium Playwright E2E verification |

## Verification and CI

Every push and pull request runs a 3-job GitHub Actions workflow ([`.github/workflows/ci.yml`](.github/workflows/ci.yml)):

1. **Static Lint, Rust & WASM (`static-lint`)** — Enforces `rustfmt --edition 2024 --check src/excess_layers.rs` and runs `python3 scripts/verify-site.py --static-only` (duplicate HTML IDs, broken anchors, missing local assets, `reserving/index.html` sync, CSS brace/selector checks, JS reference checks, and Rust/WASM unit & export checks).
2. **Browser Appearance & WASM E2E (`verify-site`)** — Serves the site locally in headless Chromium via Playwright, verifying:
   - Both WASM modules hydrate with zero console/page/HTTP errors.
   - Baseline parity (`1.48×` ILF on `$250k xs $100k` Moderate Lognormal; `$52.1M` IBNR / `$26.9M` Mack SE and `$8.8M` / `$17.5M` / `$24.0M` 6/12/18m runoff on RAA).
   - Interactive controls (Gamma–Pareto splice toggle, carousel navigation, dataset/method/view switches, dark mode toggle, and `/reserving/` route H1/scroll position).
   - Zero horizontal overflow and non-overlapping geometry across desktop (`1440×900`), tablet (`1024×768`), and mobile (`375×812`) viewports.
3. **CI Pass Gate (`ci-pass`)** — Aggregates required status checks for branch protection.

## Build and Test

```bash
# Run Rust tests, rebuild excess_layers.wasm, verify exports, render og-image.png, and sync reserving/index.html
./scripts/build.sh

# Run fast static HTML/CSS/JS/Rust/WASM checks (no browser required)
python3 scripts/verify-site.py --static-only

# Run full static + headless Chromium Playwright verification suite
python3 scripts/verify-site.py

# Check that reserving/index.html is in sync with index.html
python3 scripts/generate-reserving-page.py --check

# Install local pre-commit and pre-push verification hooks
./scripts/install-hooks.sh

# Preview locally
python3 -m http.server 8000
```

## Related Repositories

| Repository | Description |
| --- | --- |
| [`pricing-kernels`](https://github.com/SinisterRob/pricing-kernels) | Pure-Rust P&C commercial ratemaking, IRLS GLMs, credibility, NCCI/ISO experience & retro rating, ILFs, MBBEFD reinsurance, and Panjer/FFT underwriting-risk capital kernels |
| [`reserving-kernels`](https://github.com/SinisterRob/reserving-kernels) | Pure-Rust P&C loss-reserving, Mack standard errors, Merz–Wüthrich 1-year CDR, ODP bootstrap, Iman–Conover dependence, and Euler/Myers–Read capital allocation kernels |
