#!/usr/bin/env python3
"""Pre-commit / pre-push / CI static lint, dedupe, Rust/WASM, and browser verification for SinisterRob.github.io."""

from __future__ import annotations

import argparse
from collections import Counter, defaultdict
import functools
import http.server
from pathlib import Path
import re
import shutil
import socketserver
import subprocess
import sys
import tempfile
import threading

ROOT = Path(__file__).resolve().parent.parent


class WasmHandler(http.server.SimpleHTTPRequestHandler):
    extensions_map = {
        **http.server.SimpleHTTPRequestHandler.extensions_map,
        ".wasm": "application/wasm",
        ".js": "application/javascript",
        ".svg": "image/svg+xml",
    }

    def log_message(self, *args):
        pass


def check_html(failures: list[str]) -> set[str]:
    html = (ROOT / "index.html").read_text(encoding="utf-8")
    ids = re.findall(r'\bid="([^"]+)"', html)
    dup_ids = {k: v for k, v in Counter(ids).items() if v > 1}
    if dup_ids:
        failures.append(f"Duplicate HTML id attributes in index.html: {dup_ids}")

    id_set = set(ids)
    broken_anchors = [h for h in re.findall(r'\bhref="#([^"]+)"', html) if h not in id_set]
    if broken_anchors:
        failures.append(f"Broken internal #anchor links in index.html: {sorted(set(broken_anchors))}")

    local_refs = re.findall(r'\b(?:href|src)="([^"#][^"]*)"', html)
    missing_files = sorted(
        {
            ref
            for ref in local_refs
            if not ref.startswith(("http://", "https://", "mailto:", "tel:", "data:", "//"))
            and not (ROOT / ref.split("?", 1)[0]).exists()
        }
    )
    if missing_files:
        failures.append(f"Missing local asset references in index.html: {missing_files}")

    return id_set


def check_css(failures: list[str]) -> None:
    css = (ROOT / "style.css").read_text(encoding="utf-8")
    stripped = re.sub(r"/\*.*?\*/", "", css, flags=re.S)

    # 1. Balanced braces and duplicate top-level selectors
    depth = 0
    buf: list[str] = []
    line = 1
    sel_start = 1
    top_selectors: dict[str, list[int]] = defaultdict(list)
    for ch in stripped:
        if ch == "\n":
            line += 1
        if ch == "{":
            if depth == 0:
                sel = re.sub(r"\s+", " ", "".join(buf).strip())
                if sel and not sel.startswith("@"):
                    top_selectors[sel].append(sel_start)
            depth += 1
            buf = []
        elif ch == "}":
            depth -= 1
            if depth < 0:
                failures.append(f"Unmatched '}}' in style.css near line {line}")
                depth = 0
            buf = []
            sel_start = line
        elif depth == 0:
            if not buf and ch.strip():
                sel_start = line
            buf.append(ch)
    if depth != 0:
        failures.append(f"Unclosed '{{' in style.css (remaining depth={depth})")

    dup_top = {sel: lns for sel, lns in top_selectors.items() if len(lns) > 1}
    if dup_top:
        failures.append(f"Duplicate top-level CSS selectors in style.css: {dup_top}")

    # 2. Duplicate properties within the same declaration block
    dup_props: list[str] = []
    for match in re.finditer(r"([^{}@]+)\{([^{}]+)\}", stripped):
        sel = re.sub(r"\s+", " ", match.group(1).strip())
        seen: set[str] = set()
        for decl in match.group(2).split(";"):
            decl = decl.strip()
            if ":" in decl:
                prop = decl.split(":", 1)[0].strip()
                if prop in seen:
                    dup_props.append(f"{sel} -> {prop}")
                seen.add(prop)
    if dup_props:
        failures.append(f"Duplicate CSS properties in same block: {dup_props}")


def check_js(id_set: set[str], failures: list[str]) -> None:
    node = shutil.which("node")
    for js_file in ("excess-layers.js", "reserving-lab.js"):
        path = ROOT / js_file
        if not path.exists():
            failures.append(f"Missing expected JS file: {js_file}")
            continue
        if node:
            res = subprocess.run([node, "--check", str(path)], capture_output=True, text=True)
            if res.returncode != 0:
                failures.append(f"JS syntax error in {js_file}: {res.stderr.strip()}")
        src = path.read_text(encoding="utf-8")
        if re.search(r"\bdebugger\b", src):
            failures.append(f"Stray debugger statement in {js_file}")
        missing_ids = sorted(
            {
                dom_id
                for dom_id in re.findall(r'getElementById\(["\']([^"\']+)["\']\)', src)
                if dom_id not in id_set
            }
        )
        if missing_ids:
            failures.append(f"{js_file} references unknown DOM IDs: {missing_ids}")


def check_shell_and_rust(failures: list[str]) -> None:
    if shutil.which("shellcheck"):
        sh_files = [
            str(p)
            for p in list((ROOT / ".githooks").glob("*")) + list((ROOT / "scripts").glob("*.sh"))
            if p.is_file()
        ]
        if sh_files:
            res = subprocess.run(["shellcheck", *sh_files], capture_output=True, text=True)
            if res.returncode != 0:
                failures.append(f"shellcheck failed:\n{res.stdout}{res.stderr}".strip())

    rs_src = ROOT / "src" / "excess_layers.rs"
    if rs_src.exists() and shutil.which("rustc"):
        with tempfile.TemporaryDirectory(prefix="excess_layers_test_") as tmp:
            test_bin = Path(tmp) / "excess_layers_test"
            comp = subprocess.run(
                ["rustc", "--edition=2024", "--test", "-D", "warnings", str(rs_src), "-o", str(test_bin)],
                capture_output=True,
                text=True,
            )
            if comp.returncode != 0:
                failures.append(f"rustc test compile failed for src/excess_layers.rs:\n{comp.stderr.strip()}")
            else:
                run = subprocess.run([str(test_bin)], capture_output=True, text=True)
                if run.returncode != 0:
                    failures.append(f"Rust unit tests failed:\n{run.stdout}{run.stderr}".strip())

    if shutil.which("node"):
        smoke_js = """
const assert = require("assert");
const fs = require("fs");
(async () => {
  const ex = (await WebAssembly.instantiate(fs.readFileSync("excess_layers.wasm"))).instance.exports;
  for (const fn of ["limited_expected_value", "expected_layer", "simulated_layer", "exceedance_prob", "severity_pdf"]) {
    assert.strictEqual(typeof ex[fn], "function", `excess_layers.wasm missing ${fn}`);
  }
  const primary = ex.expected_layer(1, 100000, 350000, 0);
  const excess = ex.expected_layer(1, 100000, 350000, 1);
  assert.ok(Math.abs(primary - 61707.51) < 0.05, `primary=${primary}`);
  assert.ok(Math.abs(excess - 29650.34) < 0.05, `excess=${excess}`);

  const rk = (await WebAssembly.instantiate(fs.readFileSync("reserving_kernels.wasm"))).instance.exports;
  for (const fn of ["memory", "rk_n_max", "rk_tri_ptr", "rk_chain_ladder", "rk_mack", "rk_cape_cod"]) {
    assert.ok(fn in rk, `reserving_kernels.wasm missing ${fn}`);
  }
})().catch((err) => { console.error(err); process.exit(1); });
"""
        res = subprocess.run(["node", "-e", smoke_js], cwd=str(ROOT), capture_output=True, text=True)
        if res.returncode != 0:
            failures.append(f"WASM export smoke test failed:\n{res.stderr.strip()}")


def rects_overlap(a: dict, b: dict) -> bool:
    return not (
        a["right"] <= b["left"] + 1
        or a["left"] >= b["right"] - 1
        or a["bottom"] <= b["top"] + 1
        or a["top"] >= b["bottom"] - 1
    )


def run_browser_checks(failures: list[str]) -> None:
    from playwright.sync_api import sync_playwright

    handler = functools.partial(WasmHandler, directory=str(ROOT))
    httpd = socketserver.TCPServer(("127.0.0.1", 0), handler)
    port = httpd.server_address[1]
    threading.Thread(target=httpd.serve_forever, daemon=True).start()
    base_url = f"http://127.0.0.1:{port}"

    executable = shutil.which("chromium") or shutil.which("chromium-browser") or shutil.which("google-chrome")
    launch_kwargs = {"executable_path": executable} if executable else {}

    try:
        with sync_playwright() as p:
            browser = p.chromium.launch(**launch_kwargs)
            context = browser.new_context(viewport={"width": 1440, "height": 900})
            page = context.new_page()

            page_errors: list[str] = []
            failed_requests: list[str] = []

            page.on("pageerror", lambda e: page_errors.append(f"pageerror: {e}"))
            page.on(
                "console",
                lambda m: page_errors.append(f"console.error: {m.text}")
                if m.type == "error" and "umami" not in m.text.lower()
                else None,
            )
            page.on(
                "response",
                lambda r: failed_requests.append(f"{r.status} {r.url}")
                if r.url.startswith(base_url) and r.status >= 400
                else None,
            )

            # 1. Load home & verify both WASM simulators hydrate
            page.goto(f"{base_url}/", wait_until="domcontentloaded")
            page.wait_for_selector("#excess-layers[data-ready]", timeout=8000)
            page.wait_for_selector("#reserving-lab[data-ready]", timeout=8000)

            ilf_val = page.locator("#ilf").text_content()
            sim_delta = page.locator("#sim-delta").text_content()
            if ilf_val != "1.48×":
                failures.append(f"Unexpected baseline ILF: {ilf_val!r}")
            if "MC error vs. closed-form" not in (sim_delta or ""):
                failures.append(f"Missing MC error label: {sim_delta!r}")

            # 1b. Switch Distribution to Gamma–Pareto, pick body k = 6.0, and verify labels & ILF update
            if page.locator("#body-severity-row").is_visible():
                failures.append("Gamma body row should be hidden when Lognormal is active")
            page.click('[data-severity-model="spliced"]')
            if not page.locator("#body-severity-row").is_visible():
                failures.append("Gamma body row did not appear after selecting Gamma–Pareto")
            if page.locator("#tail-label").text_content() != "Pareto tail (α)":
                failures.append(f"Expected 'Pareto tail (α)' label, got {page.locator('#tail-label').text_content()!r}")
            page.click('[data-body="2"]')
            tail_first = page.locator('[data-tail="0"]').text_content()
            if tail_first != "4.0":
                failures.append(f"Expected Pareto tail label '4.0' after Gamma–Pareto click, got {tail_first!r}")
            spliced_ilf = page.locator("#ilf").text_content()
            if not spliced_ilf or not spliced_ilf.endswith("×") or spliced_ilf == "NaN×":
                failures.append(f"Invalid Gamma + Pareto ILF: {spliced_ilf!r}")
            page.click('[data-severity-model="lognormal"]')
            if page.locator("#body-severity-row").is_visible():
                failures.append("Gamma body row did not hide after returning to Lognormal")

            # 2. Test side-arrow navigation -> Reserving lab & interactions
            page.click("#lab-next")
            page.wait_for_function(
                "Math.abs(document.getElementById('lab-track').scrollLeft - document.getElementById('lab-track').clientWidth) < 3",
                timeout=4000,
            )
            res_ibnr = page.locator("#res-ibnr").text_content()
            res_se = page.locator("#res-se").text_content()
            if res_ibnr != "$52.1M" or res_se != "$26.9M":
                failures.append(f"Unexpected RAA Mack baseline: IBNR={res_ibnr!r}, SE={res_se!r}")
            cf_6m = page.locator("#res-cf-6m-inc").text_content()
            cf_12m = page.locator("#res-cf-12m-cum").text_content()
            cf_18m = page.locator("#res-cf-18m-cum").text_content()
            if cf_6m != "$8.8M" or cf_12m != "$17.5M" or cf_18m != "$24.0M":
                failures.append(
                    f"Unexpected RAA cash flow projection: 6m={cf_6m!r}, 12m={cf_12m!r}, 18m={cf_18m!r}"
                )

            page.click('[data-res-dataset="auto"]')
            page.click('[data-res-method="cc"]')
            page.click('[data-res-view="pattern"]')
            if "US Auto" not in (page.locator("#res-chart-label").text_content() or ""):
                failures.append("Reserving chart label did not update to US Auto")

            # Reset to RAA / CL / Ultimate and return via left arrow
            page.click('[data-res-dataset="raa"]')
            page.click('[data-res-method="cl"]')
            page.click('[data-res-view="ultimate"]')
            page.click("#lab-prev")
            page.wait_for_function("document.getElementById('lab-track').scrollLeft < 3", timeout=4000)

            # 3. Check layout geometry across Desktop (1440), Tablet (1024), Mobile (375)
            for width, height, label in [(1440, 900, "desktop"), (1024, 768, "tablet"), (375, 812, "mobile")]:
                page.set_viewport_size({"width": width, "height": height})
                page.wait_for_timeout(120)
                geom = page.evaluate(
                    """() => {
                        const r = el => {
                            const b = el.getBoundingClientRect();
                            return { left: b.left, right: b.right, top: b.top, bottom: b.bottom, width: b.width, height: b.height };
                        };
                        return {
                            overflow: document.documentElement.scrollWidth > document.documentElement.clientWidth,
                            scrollWidth: document.documentElement.scrollWidth,
                            clientWidth: document.documentElement.clientWidth,
                            switcher: r(document.querySelector('.lab-switcher')),
                            prev: r(document.getElementById('lab-prev')),
                            next: r(document.getElementById('lab-next')),
                            title: r(document.getElementById('lab-title')),
                            workCols: getComputedStyle(document.querySelector('#work .editorial-columns')).display,
                        };
                    }"""
                )
                if geom["overflow"]:
                    failures.append(
                        f"[{label} {width}px] horizontal page overflow: {geom['scrollWidth']} > {geom['clientWidth']}"
                    )
                if rects_overlap(geom["switcher"], geom["prev"]) or rects_overlap(geom["switcher"], geom["next"]):
                    failures.append(f"[{label} {width}px] side arrows overlap lab switcher tabs")
                if rects_overlap(geom["title"], geom["prev"]) or rects_overlap(geom["title"], geom["next"]):
                    failures.append(f"[{label} {width}px] side arrows overlap lab title")
                if geom["workCols"] != "grid":
                    failures.append(f"[{label} {width}px] #work .editorial-columns display={geom['workCols']!r}")

            # 4. Verify dark theme toggle & #work anchor visibility
            page.set_viewport_size({"width": 1440, "height": 900})
            page.click("#theme-toggle")
            if page.evaluate("document.documentElement.dataset.theme") != "dark":
                failures.append("Theme toggle did not switch to dark mode")
            page.goto(f"{base_url}/#work", wait_until="domcontentloaded")
            page.wait_for_function(
                "parseFloat(getComputedStyle(document.getElementById('work')).opacity) >= 0.99",
                timeout=2000,
            )

            if page_errors:
                failures.extend(page_errors)
            if failed_requests:
                failures.extend([f"HTTP error: {r}" for r in failed_requests])

            browser.close()
    finally:
        httpd.shutdown()


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--static-only",
        action="store_true",
        help="Run static HTML/CSS/JS/Rust/WASM/shell checks without launching Chromium",
    )
    args = parser.parse_args()

    failures: list[str] = []
    id_set = check_html(failures)
    check_css(failures)
    check_js(id_set, failures)
    check_shell_and_rust(failures)

    if not args.static_only:
        run_browser_checks(failures)

    if failures:
        print("verify-site FAILED:", file=sys.stderr)
        for item in failures:
            print(f"  - {item}", file=sys.stderr)
        return 1

    mode = "static" if args.static_only else "static + browser"
    print(
        f"verify-site OK ({mode}: HTML/CSS/JS dedupe, Rust+WASM x2, "
        "carousel arrows, 1440/1024/375 layout, dark mode, #work, 0 errors)"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
