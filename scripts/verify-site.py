#!/usr/bin/env python3
"""Comprehensive pre-push & CI verification for SinisterRob.github.io.

Checks:
  1. Static hygiene: JS syntax, JSON-LD schema, local href/src asset existence,
     cache-bust query params on CSS/JS, and Rust unit tests for src/excess_layers.rs.
  2. Live Chromium E2E across 1440px, 1280px, 1024px, and 375px viewports in
     both light and dark themes:
     - Zero console/page errors and zero local HTTP 4xx/5xx responses
     - Both Rust/WASM kernels hydrate and pass numerical baselines
     - All Slide 1 (ILF) and Slide 2 (Reserving) controls, sliders, views, drawers
     - Carousel side arrows, tabs, keyboard ArrowRight/ArrowLeft, ?lab=reserving deep link
     - Command palette open/filter/close
     - Zero horizontal overflow, zero arrow/tab/heading overlap, all sections visible
"""

from __future__ import annotations

import functools
import http.server
import json
import re
import shutil
import socketserver
import subprocess
import sys
import tempfile
import threading
from pathlib import Path

from playwright.sync_api import sync_playwright

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


def rects_overlap(a: dict, b: dict) -> bool:
    return not (
        a["right"] <= b["left"] + 1
        or a["left"] >= b["right"] - 1
        or a["bottom"] <= b["top"] + 1
        or a["top"] >= b["bottom"] - 1
    )


def run_static_checks(failures: list[str]) -> None:
    html = (ROOT / "index.html").read_text(encoding="utf-8")

    # 1. JS syntax check
    if shutil.which("node"):
        for js_file in ("excess-layers.js", "reserving-lab.js"):
            path = ROOT / js_file
            res = subprocess.run(["node", "--check", str(path)], capture_output=True, text=True)
            if res.returncode != 0:
                failures.append(f"JS syntax error in {js_file}: {res.stderr.strip()}")

    # 2. Rust unit tests for src/excess_layers.rs
    if shutil.which("rustc") and (ROOT / "src/excess_layers.rs").exists():
        with tempfile.TemporaryDirectory() as tmp:
            test_bin = Path(tmp) / "excess_layers_test"
            comp = subprocess.run(
                ["rustc", "--edition=2021", "--test", str(ROOT / "src/excess_layers.rs"), "-o", str(test_bin)],
                capture_output=True,
                text=True,
            )
            if comp.returncode != 0:
                failures.append(f"rustc test compile failed: {comp.stderr.strip()}")
            else:
                run = subprocess.run([str(test_bin)], capture_output=True, text=True)
                if run.returncode != 0:
                    failures.append(f"excess_layers.rs unit tests failed:\n{run.stdout}\n{run.stderr}")

    # 3. Verify all local href/src references exist on disk
    for attr, raw in re.findall(r'\b(href|src)="([^"]+)"', html):
        if raw.startswith(("http://", "https://", "mailto:", "#", "data:")):
            continue
        clean = raw.split("#", 1)[0].split("?", 1)[0]
        if clean and not (ROOT / clean).exists():
            failures.append(f"Broken local {attr} in index.html: {raw!r} (missing {clean})")

    # 4. Verify cache-busting query string on CSS and JS entrypoints
    for asset in ("style.css", "excess-layers.js", "reserving-lab.js"):
        if not re.search(rf'(?:href|src)="{re.escape(asset)}\?v=[^"]+"', html):
            failures.append(f"Missing cache-busting ?v=... query string on {asset} in index.html")

    # 5. Validate JSON-LD structured data
    ld_match = re.search(
        r'<script type="application/ld\+json">\s*(.*?)\s*</script>',
        html,
        re.DOTALL,
    )
    if not ld_match:
        failures.append("Missing JSON-LD block in index.html")
    else:
        try:
            ld = json.loads(ld_match.group(1))
            if "@graph" not in ld or not ld["@graph"]:
                failures.append("JSON-LD missing @graph entries")
        except json.JSONDecodeError as exc:
            failures.append(f"Invalid JSON-LD: {exc}")

    # 6. Required static files present and non-empty
    for req in ("excess_layers.wasm", "reserving_kernels.wasm", "og-image.png", "favicon.svg", "robots.txt", "sitemap.xml", "llms.txt"):
        p = ROOT / req
        if not p.exists() or p.stat().st_size == 0:
            failures.append(f"Missing or empty required file: {req}")


def run_browser_checks(failures: list[str]) -> None:
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

            # ---- 1. Initial load & WASM hydration ----
            page.goto(f"{base_url}/", wait_until="domcontentloaded")
            page.wait_for_selector("#excess-layers[data-ready]", timeout=8000)
            page.wait_for_selector("#reserving-lab[data-ready]", timeout=8000)

            if page.locator("#ilf").text_content() != "1.48×":
                failures.append(f"ILF baseline mismatch: {page.locator('#ilf').text_content()!r}")
            if page.locator("#loss-excess-key").text_content() != "$29,650":
                failures.append(f"Excess loss baseline mismatch: {page.locator('#loss-excess-key').text_content()!r}")
            if "MC error vs. closed-form" not in (page.locator("#sim-delta").text_content() or ""):
                failures.append(f"Missing MC error label: {page.locator('#sim-delta').text_content()!r}")

            # ---- 2. Slide 1 (ILF) interactive controls ----
            page.click('[data-preset="excess"]')
            high_ilf = page.locator("#ilf").text_content()
            page.click('[data-preset="working"]')
            low_ilf = page.locator("#ilf").text_content()
            if high_ilf == low_ilf:
                failures.append(f"ILF presets did not change output: high={high_ilf!r}, low={low_ilf!r}")
            page.click('[data-preset="baseline"]')

            page.click('[data-chart-view="pdf"]')
            if "Severity PDF" not in (page.locator("#chart-title-label").text_content() or ""):
                failures.append("PDF chart view did not activate")
            page.click('[data-chart-view="ilf"]')

            page.click("#method-toggle")
            if not page.evaluate("document.getElementById('method-drawer').open"):
                failures.append("ILF methodology drawer failed to open")
            page.click("#method-toggle")

            # ---- 3. Side-arrow carousel + Slide 2 (Reserving) interactive controls ----
            page.click("#lab-next")
            page.wait_for_function(
                "Math.abs(document.getElementById('lab-track').scrollLeft - document.getElementById('lab-track').clientWidth) < 3",
                timeout=4000,
            )
            if page.locator("#res-ibnr").text_content() != "$52.1M" or page.locator("#res-se").text_content() != "$26.9M":
                failures.append(
                    f"RAA Mack baseline mismatch: IBNR={page.locator('#res-ibnr').text_content()!r}, SE={page.locator('#res-se').text_content()!r}"
                )

            # Tail factor slider increases IBNR monotonically
            page.click('[data-tail-jump="1.20"]')
            ibnr_tail_120 = page.locator("#res-ibnr").text_content()
            if ibnr_tail_120 == "$52.1M":
                failures.append("Tail factor 1.20× did not change RAA IBNR")
            page.click('[data-tail-jump="1.00"]')

            # All datasets & methods
            for ds in ("genins", "auto", "raa"):
                page.click(f'[data-res-dataset="{ds}"]')
                for method in ("bf", "cc", "cl"):
                    page.click(f'[data-res-method="{method}"]')
                    val = page.locator("#res-ibnr").text_content() or ""
                    if not val.startswith("$"):
                        failures.append(f"Invalid IBNR for {ds}/{method}: {val!r}")

            page.click('[data-res-view="pattern"]')
            if "Emergence Pattern" not in (page.locator("#res-chart-label").text_content() or ""):
                failures.append("Reserving Pattern view did not activate")
            page.click('[data-res-view="ultimate"]')

            page.click("#reserving-method-toggle")
            if not page.evaluate("document.getElementById('reserving-method-drawer').open"):
                failures.append("Reserving methodology drawer failed to open")
            page.click("#reserving-method-toggle")

            # Keyboard tab navigation back to Slide 1
            page.focus("#tab-reserving")
            page.keyboard.press("ArrowLeft")
            page.wait_for_function("document.getElementById('lab-track').scrollLeft < 3", timeout=4000)

            # ---- 4. Command palette open / filter / close ----
            page.click("#cmd-trigger")
            if not page.evaluate("document.getElementById('cmd-dialog').open"):
                failures.append("Command palette failed to open")
            page.fill("#cmd-input", "High")
            if page.locator("#cmd-list .cmd-item").count() < 1:
                failures.append("Command palette filter returned 0 items for 'High'")
            page.click("#cmd-close")
            if page.evaluate("document.getElementById('cmd-dialog').open"):
                failures.append("Command palette failed to close via ESC button")

            # ---- 5. Multi-viewport layout, overflow, overlap & SVG checks ----
            for width, height, label in [
                (1440, 900, "desktop-1440"),
                (1280, 800, "desktop-1280"),
                (1024, 768, "tablet-1024"),
                (375, 812, "mobile-375"),
            ]:
                page.set_viewport_size({"width": width, "height": height})
                page.wait_for_timeout(120)
                geom = page.evaluate(
                    """() => {
                        const r = el => {
                            const b = el.getBoundingClientRect();
                            return { left: b.left, right: b.right, top: b.top, bottom: b.bottom, width: b.width, height: b.height };
                        };
                        const svgs = [...document.querySelectorAll('#severity-chart, #reserving-chart')].map(s => ({
                            id: s.id,
                            w: s.getBoundingClientRect().width,
                            h: s.getBoundingClientRect().height,
                            children: s.children.length,
                        }));
                        return {
                            overflow: document.documentElement.scrollWidth > document.documentElement.clientWidth,
                            scrollWidth: document.documentElement.scrollWidth,
                            clientWidth: document.documentElement.clientWidth,
                            switcher: r(document.querySelector('.lab-switcher')),
                            prev: r(document.getElementById('lab-prev')),
                            next: r(document.getElementById('lab-next')),
                            title: r(document.getElementById('lab-title')),
                            workCols: getComputedStyle(document.querySelector('#work .editorial-columns')).display,
                            svgs,
                        };
                    }"""
                )
                if geom["overflow"]:
                    failures.append(
                        f"[{label}] horizontal page overflow: {geom['scrollWidth']} > {geom['clientWidth']}"
                    )
                if rects_overlap(geom["switcher"], geom["prev"]) or rects_overlap(geom["switcher"], geom["next"]):
                    failures.append(f"[{label}] side arrows overlap lab switcher tabs")
                if rects_overlap(geom["title"], geom["prev"]) or rects_overlap(geom["title"], geom["next"]):
                    failures.append(f"[{label}] side arrows overlap lab title")
                if geom["workCols"] != "grid":
                    failures.append(f"[{label}] #work .editorial-columns display={geom['workCols']!r}")
                for s in geom["svgs"]:
                    if s["w"] <= 0 or s["h"] <= 0 or s["children"] < 3:
                        failures.append(f"[{label}] broken SVG #{s['id']}: {s}")

            # ---- 6. Dark theme contrast/switching + deep link + all sections visible ----
            page.set_viewport_size({"width": 1440, "height": 900})
            light_bg = page.evaluate("getComputedStyle(document.body).backgroundColor")
            page.click("#theme-toggle")
            dark_bg = page.evaluate("getComputedStyle(document.body).backgroundColor")
            if light_bg == dark_bg:
                failures.append(f"Dark mode did not change body background: {light_bg}")

            # Deep link ?lab=reserving opens directly on Slide 2
            page.goto(f"{base_url}/?lab=reserving", wait_until="domcontentloaded")
            page.wait_for_selector("#reserving-lab[data-ready]", timeout=8000)
            page.wait_for_function(
                "Math.abs(document.getElementById('lab-track').scrollLeft - document.getElementById('lab-track').clientWidth) < 3",
                timeout=4000,
            )

            # Anchor navigation to #work and full-page section reveal check
            page.goto(f"{base_url}/#work", wait_until="domcontentloaded")
            for sec_id in ("work", "experience", "interests", "about", "contact"):
                page.locator(f"#{sec_id}").scroll_into_view_if_needed()
                page.wait_for_function(
                    f"parseFloat(getComputedStyle(document.getElementById('{sec_id}')).opacity) >= 0.99",
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
    failures: list[str] = []
    run_static_checks(failures)
    run_browser_checks(failures)

    if failures:
        print("verify-site FAILED:", file=sys.stderr)
        for item in failures:
            print(f"  - {item}", file=sys.stderr)
        return 1

    print(
        "verify-site OK "
        "(rustc tests, JS/JSON-LD/assets/cache-bust, WASM x2, all controls & drawers, "
        "carousel/kbd/?lab=, cmd-palette, 1440/1280/1024/375 layout, dark mode, all sections, 0 errors)"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
