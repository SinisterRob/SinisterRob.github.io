#!/usr/bin/env python3
"""Fast pre-push / CI appearance, JS, CSS, and WASM verification for SinisterRob.github.io."""

from __future__ import annotations

import functools
import http.server
import os
import shutil
import socketserver
import subprocess
import sys
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


def check_js_syntax() -> None:
    if not shutil.which("node"):
        return
    for js_file in ("excess-layers.js", "reserving-lab.js"):
        path = ROOT / js_file
        if path.exists():
            subprocess.run(["node", "--check", str(path)], check=True)


def rects_overlap(a: dict, b: dict) -> bool:
    return not (
        a["right"] <= b["left"] + 1
        or a["left"] >= b["right"] - 1
        or a["bottom"] <= b["top"] + 1
        or a["top"] >= b["bottom"] - 1
    )


def main() -> int:
    check_js_syntax()

    handler = functools.partial(WasmHandler, directory=str(ROOT))
    httpd = socketserver.TCPServer(("127.0.0.1", 0), handler)
    port = httpd.server_address[1]
    threading.Thread(target=httpd.serve_forever, daemon=True).start()
    base_url = f"http://127.0.0.1:{port}"

    failures: list[str] = []
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

    if failures:
        print("verify-site FAILED:", file=sys.stderr)
        for item in failures:
            print(f"  - {item}", file=sys.stderr)
        return 1

    print("verify-site OK (WASM x2, carousel arrows, 1440/1024/375 layout, dark mode, #work, 0 errors)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
