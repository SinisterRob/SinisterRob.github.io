#!/usr/bin/env python3
"""Generate reserving/index.html from index.html with Reserving-specific SEO metadata.

Keeps visual layout 100% identical to index.html while giving the Loss Reserving
& Mack Variability Simulator its own indexable URL, canonical, title, description,
H1, and JSON-LD mainEntity.
"""

from __future__ import annotations

import argparse
from pathlib import Path
import re
import sys

ROOT = Path(__file__).resolve().parent.parent
SRC = ROOT / "index.html"
DEST = ROOT / "reserving" / "index.html"

RES_TITLE = "Loss Reserving &amp; Mack Variability Simulator | Robert Jurgens, FCAS"
RES_TITLE_PLAIN = "Loss Reserving & Mack Variability Simulator | Robert Jurgens, FCAS"
RES_DESC = (
    "Interactive Rust/WebAssembly P&amp;C loss reserving simulator for Chain-Ladder ultimates, "
    "Mack (1993) standard errors, Cape Cod estimates, and cash-flow projections. By Robert Jurgens, FCAS."
)
RES_URL = "https://sinisterrob.github.io/reserving/"


def build_reserving_html(src_html: str) -> str:
    html = src_html

    # 1. Head title, description, canonical, Open Graph, Twitter tags
    html, n = re.subn(r"<title>[^<]*</title>", f"<title>{RES_TITLE}</title>", html, count=1)
    assert n == 1, "Failed to replace <title>"

    html, n = re.subn(
        r'<meta name="description" content="[^"]*">',
        f'<meta name="description" content="{RES_DESC}">',
        html,
        count=1,
    )
    assert n == 1, "Failed to replace meta description"

    html, n = re.subn(
        r'<link rel="canonical" href="[^"]*">',
        f'<link rel="canonical" href="{RES_URL}">',
        html,
        count=1,
    )
    assert n == 1, "Failed to replace canonical"

    html, n = re.subn(
        r'<meta property="og:url" content="[^"]*">',
        f'<meta property="og:url" content="{RES_URL}">',
        html,
        count=1,
    )
    assert n == 1, "Failed to replace og:url"

    html, n = re.subn(
        r'<meta property="og:title" content="[^"]*">',
        f'<meta property="og:title" content="{RES_TITLE}">',
        html,
        count=1,
    )
    assert n == 1, "Failed to replace og:title"

    html, n = re.subn(
        r'<meta property="og:description" content="[^"]*">',
        f'<meta property="og:description" content="{RES_DESC}">',
        html,
        count=1,
    )
    assert n == 1, "Failed to replace og:description"

    html, n = re.subn(
        r'<meta name="twitter:title" content="[^"]*">',
        f'<meta name="twitter:title" content="{RES_TITLE}">',
        html,
        count=1,
    )
    assert n == 1, "Failed to replace twitter:title"

    html, n = re.subn(
        r'<meta name="twitter:description" content="[^"]*">',
        f'<meta name="twitter:description" content="{RES_DESC}">',
        html,
        count=1,
    )
    assert n == 1, "Failed to replace twitter:description"

    # 2. Rewrite relative asset paths to parent directory
    for attr in ("href", "src"):
        html = re.sub(
            rf'({attr}=")(?!(?:https?:|mailto:|tel:|data:|#|//|\.\./))([^"]+)"',
            r'\1../\2"',
            html,
        )

    # 3. Tailor WebPage JSON-LD node to /reserving/
    html = html.replace(
        '"@id": "https://sinisterrob.github.io/#webpage",\n        "url": "https://sinisterrob.github.io/",',
        f'"@id": "{RES_URL}#webpage",\n        "url": "{RES_URL}",',
        1,
    )
    html = re.sub(
        r'("@id": "https://sinisterrob\.github\.io/reserving/#webpage",\s*"url": "[^"]+",\s*"name": ")[^"]+(")',
        rf'\1{RES_TITLE_PLAIN}\2',
        html,
        count=1,
    )
    html = re.sub(
        r'"mainEntity": \[\s*\{ "@id": "https://sinisterrob\.github\.io/#ilf-lab" \},\s*\{ "@id": "https://sinisterrob\.github\.io/#reserving-lab" \}\s*\]',
        '"mainEntity": { "@id": "https://sinisterrob.github.io/#reserving-lab" }',
        html,
        count=1,
    )

    # 4. Promote reserving-title to H1 and demote lab-title to H2 (pixel-identical via .lab-title-display)
    html = html.replace(
        'aria-labelledby="lab-title"',
        'aria-labelledby="reserving-title"',
        1,
    )
    html = html.replace(
        '<h1 id="lab-title">Excess Layer &amp; Increased Limits Factor (ILF) Simulator</h1>',
        '<h2 id="lab-title" class="lab-title-display">Excess Layer &amp; Increased Limits Factor (ILF) Simulator</h2>',
        1,
    )
    html = html.replace(
        '<h2 id="reserving-title" class="lab-title-display">Loss Reserving &amp; Mack Variability Simulator</h2>',
        '<h1 id="reserving-title" class="lab-title-display">Loss Reserving &amp; Mack Variability Simulator</h1>',
        1,
    )

    return html


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--check",
        action="store_true",
        help="Verify reserving/index.html is up to date without modifying it",
    )
    args = parser.parse_args()

    expected = build_reserving_html(SRC.read_text(encoding="utf-8"))

    if args.check:
        if not DEST.exists():
            print("reserving/index.html is missing; run scripts/generate-reserving-page.py", file=sys.stderr)
            return 1
        actual = DEST.read_text(encoding="utf-8")
        if actual != expected:
            print("reserving/index.html is out of sync with index.html; run scripts/generate-reserving-page.py", file=sys.stderr)
            return 1
        return 0

    DEST.parent.mkdir(parents=True, exist_ok=True)
    DEST.write_text(expected, encoding="utf-8")
    print(f"Generated {DEST.relative_to(ROOT)} ({len(expected)} bytes)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
