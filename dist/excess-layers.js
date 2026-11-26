const GROUND_UP_MEAN = 100_000;
const ATTACHMENT = 100_000;
const MIN_LIMIT = 150_000;
const MAX_LIMIT = 750_000;
const LIMIT_STEP = 25_000;
const PDF_MAX = 800_000;
const TAILS = ['Lighter', 'Moderate', 'Heavier'];
const TAIL_SIGMAS = ['σ = 0.55', 'σ = 1.00', 'σ = 1.55'];

const dollars = new Intl.NumberFormat('en-US', {
  style: 'currency',
  currency: 'USD',
  maximumFractionDigits: 0,
});

const percent = new Intl.NumberFormat('en-US', {
  style: 'percent',
  minimumFractionDigits: 1,
  maximumFractionDigits: 1,
});

const compactDollars = value => `$${Math.round(value / 1000)}k`;
const ratio = value => `${value.toFixed(2)}×`;
const byId = id => document.getElementById(id);

const debugAnalytics = new URLSearchParams(window.location.search).has('debug_analytics');
const pageStart = performance.now();
const pendingAnalytics = [];
let labEngaged = false;
let labController = null;

const OUTBOUND_MAP = {
  'linkedin-hero': { target: 'linkedin', location: 'hero' },
  'github-hero': { target: 'github', location: 'hero' },
  'rust-source': { target: 'rust_source', location: 'lab' },
  'cas-actuarial-review': { target: 'actuarial_review', location: 'lab' },
  'linkedin-experience': { target: 'linkedin', location: 'experience' },
  'github-interests': { target: 'github', location: 'interests' },
  'reserving-kernels': { target: 'reserving_kernels', location: 'interests' },
  'ut-scholarship': { target: 'ut_scholarship', location: 'background' },
  'ut-case-comp': { target: 'ut_case_comp', location: 'background' },
  'linkedin-footer': { target: 'linkedin', location: 'footer' },
  'github-footer': { target: 'github', location: 'footer' },
};

function flushAnalytics() {
  if (!window.umami) return;
  while (pendingAnalytics.length) {
    const item = pendingAnalytics.shift();
    try {
      if (item.kind === 'identify' && typeof window.umami.identify === 'function') {
        window.umami.identify(item.payload);
      } else if (item.kind === 'track' && typeof window.umami.track === 'function') {
        window.umami.track(item.name, item.payload);
      } else if (item.kind === 'error' && typeof window.umami.captureException === 'function') {
        window.umami.captureException(item.error, item.payload);
      }
    } catch {
      // Analytics must never interrupt UX.
    }
  }
}

function track(eventName, payload = {}) {
  if (debugAnalytics) {
    console.info('[analytics:event]', eventName, payload);
  }
  try {
    if (window.umami && typeof window.umami.track === 'function') {
      window.umami.track(eventName, payload);
    } else if (pendingAnalytics.length < 32) {
      pendingAnalytics.push({ kind: 'track', name: eventName, payload });
    }
  } catch {
    // Analytics must never interrupt UX.
  }
}

function identifySession(traits) {
  if (debugAnalytics) {
    console.info('[analytics:identify]', traits);
  }
  try {
    if (window.umami && typeof window.umami.identify === 'function') {
      window.umami.identify(traits);
    } else if (pendingAnalytics.length < 32) {
      pendingAnalytics.push({ kind: 'identify', payload: traits });
    }
  } catch {
    // Analytics must never interrupt UX.
  }
}

function captureError(error, tags = {}) {
  if (debugAnalytics) {
    console.info('[analytics:error]', error, tags);
  }
  try {
    if (window.umami && typeof window.umami.captureException === 'function') {
      window.umami.captureException(error, { tags });
    } else if (pendingAnalytics.length < 32) {
      pendingAnalytics.push({ kind: 'error', error, payload: { tags } });
    }
  } catch {
    // Analytics must never interrupt UX.
  }
}

function timeBucket(ms) {
  const sec = ms / 1000;
  if (sec < 5) return '<5s';
  if (sec < 15) return '5-15s';
  if (sec < 60) return '15-60s';
  return '60s+';
}

function deriveChannel() {
  const params = new URLSearchParams(window.location.search);
  const utm = params.get('utm_source')?.trim().toLowerCase();
  if (utm) return utm.slice(0, 32);
  if (!document.referrer) return 'direct';
  try {
    const host = new URL(document.referrer).hostname.replace(/^www\./, '').toLowerCase();
    if (host.includes('linkedin')) return 'linkedin';
    if (host.includes('github')) return 'github';
    if (host.includes('google')) return 'google';
    if (host.includes('bing')) return 'bing';
    if (host.includes('duckduckgo')) return 'duckduckgo';
    if (host.includes('casact')) return 'cas';
    if (host.includes('utexas')) return 'utexas';
    if (host === window.location.hostname) return 'internal';
    return 'referral';
  } catch {
    return 'referral';
  }
}

function getViewportTier() {
  const w = window.innerWidth;
  if (w < 640) return 'mobile';
  if (w < 1024) return 'tablet';
  return 'desktop';
}

function trackLabEngagement(trigger) {
  if (labEngaged) return;
  labEngaged = true;
  track('lab_engaged', {
    trigger,
    time_bucket: timeBucket(performance.now() - pageStart),
  });
}

function initAnalytics() {
  const umamiScript = document.querySelector('script[data-website-id]');
  if (umamiScript) {
    umamiScript.addEventListener('load', flushAnalytics, { once: true });
  }
  window.addEventListener('load', flushAnalytics, { once: true });

  const seenSections = new Set();
  let engagedFired = false;

  function fireEngagedReader(trigger) {
    if (engagedFired) return;
    engagedFired = true;
    track('engaged_reader', {
      trigger,
      sections: seenSections.size,
      lab_engaged: labEngaged ? 'yes' : 'no',
    });
  }

  document.addEventListener('click', event => {
    const tracked = event.target.closest('[data-track]');
    if (tracked) {
      const mapped = OUTBOUND_MAP[tracked.dataset.track] || {
        target: tracked.dataset.track,
        location: 'page',
      };
      track('outbound_click', mapped);
      return;
    }

    const nav = event.target.closest('[data-nav]');
    if (nav) {
      const raw = nav.dataset.nav;
      let source = 'desktop_nav';
      let target = raw;
      if (nav.closest('.mobile-nav')) {
        source = 'mobile_nav';
      } else if (raw.startsWith('hero-')) {
        source = 'hero_cta';
        target = raw.slice(5);
      } else if (raw.startsWith('interests-')) {
        source = 'interests_card';
        target = raw.slice(10);
      } else if (raw.startsWith('footer-')) {
        source = 'footer';
        target = raw.slice(7);
      }
      track('nav_click', { target, source });
      return;
    }

    const contact = event.target.closest('[data-contact]');
    if (contact) {
      track('contact_click', { method: contact.dataset.contact });
    }
  });

  // One-shot section funnel tracking
  if ('IntersectionObserver' in window) {
    const sectionObserver = new IntersectionObserver(
      entries => {
        entries.forEach(entry => {
          const id = entry.target.id;
          if (entry.isIntersecting && id && !seenSections.has(id)) {
            seenSections.add(id);
            track('section_view', { section: id });
            sectionObserver.unobserve(entry.target);
            if ((id === 'about' || id === 'contact') && performance.now() - pageStart >= 10_000) {
              fireEngagedReader('deep_read');
            }
          }
        });
      },
      { threshold: 0.35 },
    );
    document.querySelectorAll('main section[id], footer[id]').forEach(el => sectionObserver.observe(el));
  }

  // Active visible-tab 30s dwell milestone
  let visibleMs = 0;
  let visibleSince = document.visibilityState === 'visible' ? performance.now() : null;
  let dwellTimer;

  function scheduleDwell() {
    if (engagedFired || visibleSince === null) return;
    const remaining = Math.max(0, 30_000 - visibleMs);
    clearTimeout(dwellTimer);
    dwellTimer = setTimeout(() => {
      if (!engagedFired && document.visibilityState === 'visible') {
        fireEngagedReader('dwell_30s');
      }
    }, remaining);
  }

  document.addEventListener('visibilitychange', () => {
    if (engagedFired) return;
    if (document.visibilityState === 'hidden' && visibleSince !== null) {
      visibleMs += performance.now() - visibleSince;
      visibleSince = null;
      clearTimeout(dwellTimer);
    } else if (document.visibilityState === 'visible' && visibleSince === null) {
      visibleSince = performance.now();
      scheduleDwell();
    }
  });
  scheduleDwell();

  // High-intent executive / recruiter signals: print/PDF save & manual text copy (section ID only, zero PII)
  window.addEventListener('beforeprint', () => {
    track('print_page', { theme: document.documentElement.dataset.theme || 'light' });
  });

  document.addEventListener('copy', () => {
    const sel = window.getSelection();
    if (!sel || !sel.toString().trim()) return;
    const anchor = sel.anchorNode;
    const el = anchor && anchor.nodeType === 1 ? anchor : anchor?.parentElement;
    const section = el?.closest('section[id], footer[id]')?.id || (el?.closest('.hero') ? 'hero' : 'other');
    track('text_copy', { section });
  });
}

let toggleThemeFn = () => {};

function initTheme() {
  const root = document.documentElement;
  const button = byId('theme-toggle');
  const themeColor = document.querySelector('meta[name="theme-color"]');

  function apply(theme) {
    root.dataset.theme = theme;
    const isDark = theme === 'dark';
    const next = isDark ? 'light' : 'dark';
    button.setAttribute('aria-pressed', String(isDark));
    button.setAttribute('aria-label', `Switch to ${next} theme`);
    button.title = `Switch to ${next} theme`;
    if (themeColor) {
      themeColor.content = isDark ? '#0e1013' : '#f7f4ee';
    }
  }

  let initial = root.dataset.theme === 'dark' ? 'dark' : 'light';
  try {
    const saved = localStorage.getItem('rj-theme');
    if (saved === 'dark' || saved === 'light') {
      initial = saved;
    } else if (window.matchMedia('(prefers-color-scheme: dark)').matches) {
      initial = 'dark';
    }
  } catch {
    // Keep pre-paint theme when storage is unavailable.
  }

  apply(initial);
  toggleThemeFn = () => {
    const next = root.dataset.theme === 'dark' ? 'light' : 'dark';
    apply(next);
    track('theme_toggle', { to: next });
    try {
      localStorage.setItem('rj-theme', next);
    } catch {
      // Theme toggle still works for the current session without storage.
    }
  };
  button.addEventListener('click', toggleThemeFn);
}

let copyEmailFn = async () => false;

function initCopyEmail() {
  const button = byId('copy-email');
  const status = byId('copy-status');
  const label = button?.querySelector('.copy-label');
  if (!button) return;

  let resetTimer;
  copyEmailFn = async () => {
    const email = button.dataset.email || 'robertdjurgens@gmail.com';
    let copied = false;

    try {
      if (navigator.clipboard && window.isSecureContext) {
        await navigator.clipboard.writeText(email);
        copied = true;
      }
    } catch {
      copied = false;
    }

    if (!copied) {
      try {
        const input = document.createElement('textarea');
        input.value = email;
        input.setAttribute('readonly', '');
        input.style.position = 'fixed';
        input.style.opacity = '0';
        document.body.appendChild(input);
        input.select();
        copied = document.execCommand('copy');
        document.body.removeChild(input);
      } catch {
        copied = false;
      }
    }

    if (copied) {
      track('contact_click', { method: 'copy_email' });
      if (label) label.textContent = 'Copied';
      else button.textContent = 'Copied';
      button.classList.add('is-copied');
      if (status) status.textContent = `Copied ${email} to clipboard.`;
      clearTimeout(resetTimer);
      resetTimer = setTimeout(() => {
        if (label) label.textContent = 'Copy email';
        else button.textContent = 'Copy email';
        button.classList.remove('is-copied');
      }, 2000);
    }
    return copied;
  };

  button.addEventListener('click', copyEmailFn);
}

function initScrollOrientation() {
  const bar = byId('scroll-progress-bar');
  const targets = [...document.querySelectorAll('main section[id], footer[id]')];
  const navLinks = [...document.querySelectorAll('.site-nav a[data-nav], .mobile-nav a[data-nav]')];
  let activeId = null;
  let ticking = false;

  function setActive(id) {
    if (id === activeId) return;
    activeId = id;
    navLinks.forEach(link => {
      const match = link.dataset.nav === id;
      link.classList.toggle('is-active', match);
      if (match) link.setAttribute('aria-current', 'location');
      else link.removeAttribute('aria-current');
    });
  }

  function update() {
    ticking = false;
    const scrollTop = window.scrollY || document.documentElement.scrollTop || 0;
    const maxScroll = Math.max(1, document.documentElement.scrollHeight - window.innerHeight);
    const pct = Math.min(1, Math.max(0, scrollTop / maxScroll));
    if (bar) {
      bar.style.transform = `scaleX(${pct.toFixed(4)})`;
    }

    // Near top (hero): clear active section highlight
    if (scrollTop < 140) {
      setActive(null);
      return;
    }

    // Near bottom: highlight Contact
    if (scrollTop + window.innerHeight >= document.documentElement.scrollHeight - 40) {
      setActive('contact');
      return;
    }

    const probe = window.innerHeight * 0.34;
    let current = targets[0]?.id || null;
    for (const el of targets) {
      const rect = el.getBoundingClientRect();
      if (rect.top <= probe) {
        current = el.id;
      } else {
        break;
      }
    }
    setActive(current);
  }

  function requestUpdate() {
    if (!ticking) {
      ticking = true;
      window.requestAnimationFrame(update);
    }
  }

  window.addEventListener('scroll', requestUpdate, { passive: true });
  window.addEventListener('resize', requestUpdate, { passive: true });
  update();
}

function initReveal() {
  const sections = [...document.querySelectorAll('[data-reveal]')];
  if (!sections.length) return;
  if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) return;

  function sweepVisible() {
    const threshold = window.innerHeight * 0.96;
    sections.forEach(el => {
      if (!el.classList.contains('is-pending')) return;
      const rect = el.getBoundingClientRect();
      // Reveal if entering viewport OR already scrolled past (prevents stuck hidden sections on fast scroll / anchor jumps)
      if (rect.top <= threshold) {
        el.classList.remove('is-pending');
      }
    });
  }

  const fold = window.innerHeight * 0.96;
  sections.forEach(el => {
    const rect = el.getBoundingClientRect();
    if (rect.top > fold) {
      el.classList.add('is-pending');
    }
  });

  if ('IntersectionObserver' in window) {
    const observer = new IntersectionObserver(
      entries => {
        entries.forEach(entry => {
          if (entry.isIntersecting || entry.boundingClientRect.top <= window.innerHeight) {
            entry.target.classList.remove('is-pending');
            observer.unobserve(entry.target);
          }
        });
      },
      { rootMargin: '0px 0px 40px 0px', threshold: 0.01 },
    );
    sections.forEach(el => {
      if (el.classList.contains('is-pending')) observer.observe(el);
    });
  }

  window.addEventListener('scroll', sweepVisible, { passive: true });
  window.addEventListener('hashchange', sweepVisible);
  window.addEventListener('beforeprint', () => {
    sections.forEach(el => el.classList.remove('is-pending'));
  });
}

function initMethodologyDrawer() {
  const toggle = byId('method-toggle');
  const drawer = byId('method-drawer');
  if (!toggle || !drawer) return;

  toggle.addEventListener('click', () => {
    const open = drawer.hidden;
    drawer.hidden = !open;
    toggle.setAttribute('aria-expanded', String(open));
    toggle.textContent = open ? 'Hide notes ▴' : 'Model notes ▾';
    track('lab_interact', {
      control: 'methodology',
      value: open ? 'open' : 'close',
    });
  });
}

function initPrintButton() {
  const btn = byId('print-brief');
  if (!btn) return;
  btn.addEventListener('click', () => window.print());
}

function initCommandPalette() {
  const dialog = byId('cmd-dialog');
  const trigger = byId('cmd-trigger');
  const input = byId('cmd-input');
  const list = byId('cmd-list');
  const closeBtn = byId('cmd-close');
  const kbd = byId('cmd-kbd');
  if (!dialog || !trigger || !input || !list) return;

  const isMac = /Mac|iPhone|iPad|iPod/i.test(navigator.platform || navigator.userAgent);
  if (kbd) kbd.textContent = isMac ? '⌘K' : 'Ctrl K';

  const items = [
    { group: 'Navigate', label: 'Excess Layer & ILF Lab', sub: '#lab', keywords: 'wasm rust simulator chart', run: () => jumpTo('#lab', 'lab') },
    { group: 'Navigate', label: 'Core Capabilities', sub: '#work', keywords: 'pricing xll excel monte carlo capital', run: () => jumpTo('#work', 'work') },
    { group: 'Navigate', label: 'Career Experience', sub: '#experience', keywords: 'edgewater oliver wyman tokio marine', run: () => jumpTo('#experience', 'experience') },
    { group: 'Navigate', label: 'Independent Research', sub: '#interests', keywords: 'reserving quant bayesian', run: () => jumpTo('#interests', 'interests') },
    { group: 'Navigate', label: 'Credentials & Background', sub: '#about', keywords: 'fcas acas ut austin cas mentor', run: () => jumpTo('#about', 'about') },
    { group: 'Navigate', label: 'Contact', sub: '#contact', keywords: 'email linkedin github connect', run: () => jumpTo('#contact', 'contact') },
    { group: 'Lab Presets', label: 'Load Severe Tail Scenario', sub: '$650k · σ 1.55', keywords: 'heavy tail preset lab', run: () => runLabPreset(650_000, 2, 'excess') },
    { group: 'Lab Presets', label: 'Load Working Layer Scenario', sub: '$250k · σ 0.55', keywords: 'light tail preset lab', run: () => runLabPreset(250_000, 0, 'working') },
    { group: 'Lab Presets', label: 'Toggle ILF Curve / Severity PDF', sub: 'Chart view', keywords: 'pdf ilf chart view', run: () => toggleLabView() },
    { group: 'Actions', label: 'Copy Email Address', sub: 'robertdjurgens@gmail.com', keywords: 'mail clipboard contact', run: () => copyEmailFn() },
    { group: 'Actions', label: 'Switch Light / Dark Theme', sub: 'Appearance', keywords: 'dark light mode theme', run: () => toggleThemeFn() },
    { group: 'Actions', label: 'Print / Save Executive Brief (PDF)', sub: 'Print', keywords: 'resume cv pdf print', run: () => setTimeout(() => window.print(), 120) },
    { group: 'External', label: 'Open LinkedIn Profile', sub: 'linkedin.com ↗', keywords: 'social profile', run: () => openExternal('https://www.linkedin.com/in/robert-jurgens-fcas-352017143/', 'linkedin') },
    { group: 'External', label: 'Open GitHub (@SinisterRob)', sub: 'github.com ↗', keywords: 'code repos rust', run: () => openExternal('https://github.com/SinisterRob', 'github') },
    { group: 'External', label: 'Open reserving-kernels Crate', sub: 'reserving-kernels ↗', keywords: 'rust crate reserving mack odp', run: () => openExternal('https://github.com/SinisterRob/reserving-kernels', 'reserving_kernels') },
    { group: 'External', label: 'Inspect Rust WASM Kernel Source', sub: 'excess_layers.rs ↗', keywords: 'rust wasm source', run: () => openExternal('https://github.com/SinisterRob/SinisterRob.github.io/blob/main/src/excess_layers.rs', 'rust_source') },
  ];

  function jumpTo(hash, target) {
    track('nav_click', { target, source: 'command_palette' });
    const el = document.querySelector(hash);
    if (el) el.scrollIntoView({ behavior: 'smooth' });
    else window.location.hash = hash;
  }

  function runLabPreset(limit, tailIndex, name) {
    if (labController) {
      labController.applyPreset(limit, tailIndex, name, 'command_palette');
    }
    byId('lab')?.scrollIntoView({ behavior: 'smooth' });
  }

  function toggleLabView() {
    if (labController) {
      labController.toggleView();
    }
    byId('lab')?.scrollIntoView({ behavior: 'smooth' });
  }

  function openExternal(url, target) {
    track('outbound_click', { target, location: 'command_palette' });
    window.open(url, '_blank', 'noopener,noreferrer');
  }

  let filtered = items;
  let selectedIndex = 0;

  function renderList(query = '') {
    const q = query.trim().toLowerCase();
    filtered = q
      ? items.filter(item =>
          `${item.label} ${item.sub} ${item.group} ${item.keywords}`.toLowerCase().includes(q),
        )
      : items;
    selectedIndex = 0;

    if (!filtered.length) {
      list.innerHTML = '<p class="cmd-empty">No matching sections or actions.</p>';
      return;
    }

    let lastGroup = '';
    const html = [];
    filtered.forEach((item, idx) => {
      if (item.group !== lastGroup) {
        lastGroup = item.group;
        html.push(`<div class="cmd-group-label">${item.group}</div>`);
      }
      html.push(
        `<button type="button" class="cmd-item ${idx === selectedIndex ? 'is-selected' : ''}" role="option" aria-selected="${idx === selectedIndex}" data-cmd-idx="${idx}"><span>${item.label}</span><span class="cmd-item-sub">${item.sub}</span></button>`,
      );
    });
    list.innerHTML = html.join('');
  }

  function updateSelection(next) {
    if (!filtered.length) return;
    selectedIndex = (next + filtered.length) % filtered.length;
    const buttons = [...list.querySelectorAll('.cmd-item')];
    buttons.forEach((btn, idx) => {
      const sel = idx === selectedIndex;
      btn.classList.toggle('is-selected', sel);
      btn.setAttribute('aria-selected', String(sel));
      if (sel) btn.scrollIntoView({ block: 'nearest' });
    });
  }

  function execute(idx) {
    const item = filtered[idx];
    if (!item) return;
    dialog.close();
    item.run();
  }

  function openPalette() {
    if (dialog.open) return;
    input.value = '';
    renderList('');
    dialog.showModal();
    input.focus();
  }

  trigger.addEventListener('click', openPalette);
  closeBtn?.addEventListener('click', () => dialog.close());
  dialog.addEventListener('click', e => {
    if (e.target === dialog) dialog.close();
  });

  input.addEventListener('input', () => renderList(input.value));
  input.addEventListener('keydown', e => {
    if (e.key === 'ArrowDown') {
      e.preventDefault();
      updateSelection(selectedIndex + 1);
    } else if (e.key === 'ArrowUp') {
      e.preventDefault();
      updateSelection(selectedIndex - 1);
    } else if (e.key === 'Enter') {
      e.preventDefault();
      execute(selectedIndex);
    }
  });

  list.addEventListener('click', e => {
    const btn = e.target.closest('[data-cmd-idx]');
    if (btn) execute(Number(btn.dataset.cmdIdx));
  });

  document.addEventListener('keydown', e => {
    if ((e.metaKey || e.ctrlKey) && e.key.toLowerCase() === 'k') {
      e.preventDefault();
      if (dialog.open) dialog.close();
      else openPalette();
    }
  });
}

async function loadModel() {
  const response = await fetch(new URL('./excess_layers.wasm', import.meta.url));
  if (!response.ok) throw new Error(`Model request returned ${response.status}`);
  const { instance } = await WebAssembly.instantiate(await response.arrayBuffer());
  return instance.exports;
}

function getPlot(svg, plotWidth, { start, end, left, right, low, high }) {
  const width = Math.max(250, Math.round(plotWidth || 540));
  const height = width < 380 ? 152 : 172;
  const top = 22;
  const bottom = height - 26;
  svg.setAttribute('viewBox', `0 0 ${width} ${height}`);
  return {
    width,
    height,
    top,
    bottom,
    left,
    right: width - right,
    start,
    end,
    x: value => left + ((value - start) / (end - start)) * (width - left - right),
    y: value => bottom - ((value - low) / (high - low)) * (bottom - top),
  };
}

function axes(plot, xTicks, yTicks = []) {
  const { x, y, left, right, bottom } = plot;
  const grid = yTicks
    .map(
      value => `
      <line class="chart-grid" x1="${left}" y1="${y(value).toFixed(1)}" x2="${right}" y2="${y(value).toFixed(1)}"/>
      <text class="chart-text" x="${left - 6}" y="${(y(value) + 4).toFixed(1)}" text-anchor="end">${ratio(value)}</text>`,
    )
    .join('');

  const labels = xTicks
    .map((value, index) => {
      const anchor = index === 0 ? 'start' : index === xTicks.length - 1 ? 'end' : 'middle';
      return `<text class="chart-text" x="${x(value).toFixed(1)}" y="${bottom + 18}" text-anchor="${anchor}">${value === 0 ? '$0' : compactDollars(value)}</text>`;
    })
    .join('');

  return `${grid}<line class="chart-baseline" x1="${left}" y1="${bottom}" x2="${right}" y2="${bottom}"/>${labels}`;
}

function path(points, x, y) {
  return points
    .map(([a, b], index) => `${index ? 'L' : 'M'} ${x(a).toFixed(1)} ${y(b).toFixed(1)}`)
    .join(' ');
}

function chartLabels(title, description) {
  return `<title id="chart-title">${title}</title><desc id="chart-description">${description}</desc>`;
}

function renderIlf(svg, plotWidth, curves, tail, limit, ilf) {
  const narrow = (plotWidth || 540) < 420;
  const plot = getPlot(svg, plotWidth, {
    start: ATTACHMENT,
    end: MAX_LIMIT,
    left: 44,
    right: narrow ? 12 : 66,
    low: 1,
    high: 2.05,
  });
  const { x, y, top, bottom, right } = plot;
  const ticks = narrow
    ? [100_000, 350_000, 750_000]
    : [100_000, 250_000, 400_000, 550_000, 750_000];

  const comparison = curves
    .map((points, index) => (index === tail ? '' : `<path d="${path(points, x, y)}" class="chart-curve"/>`))
    .join('');

  const labels = !narrow
    ? curves
        .map((points, index) => {
          const final = points.at(-1)[1];
          return `<text class="chart-text ${index === tail ? 'is-active' : ''}" x="${right + 8}" y="${(y(final) + 4).toFixed(1)}">${TAILS[index]}</text>`;
        })
        .join('')
    : '';

  const marker = x(limit);
  const markerY = y(ilf);
  const markerLabel = `${compactDollars(limit)} · ${ratio(ilf)}`;
  const halfW = (markerLabel.length * 6.5) / 2;
  const labelX = Math.max(plot.left + halfW, Math.min(marker, right - halfW));

  svg.innerHTML = `
    ${chartLabels('Increased limits factor by coverage limit', `The selected ${TAILS[tail].toLowerCase()} tail has an ILF of ${ilf.toFixed(2)} at a ${dollars.format(limit)} limit. All three curves use a ${dollars.format(ATTACHMENT)} base limit.`)}
    ${axes(plot, ticks, [1, 1.25, 1.5, 1.75, 2])}
    <line class="chart-marker" x1="${marker.toFixed(1)}" y1="${top}" x2="${marker.toFixed(1)}" y2="${bottom}"/>
    ${comparison}
    <path d="${path(curves[tail], x, y)}" class="chart-curve is-active"/>
    ${labels}
    <circle class="chart-dot-ring" cx="${marker.toFixed(1)}" cy="${markerY.toFixed(1)}" r="8.5"/>
    <circle class="chart-dot" cx="${marker.toFixed(1)}" cy="${markerY.toFixed(1)}" r="4.4"/>
    <text class="chart-text is-active" x="${labelX.toFixed(1)}" y="14" text-anchor="middle">${markerLabel}</text>`;
  return plot;
}

function renderSeverity(svg, plotWidth, distributions, tailPeaks, density, tail, limit) {
  const narrow = (plotWidth || 540) < 420;
  const plot = getPlot(svg, plotWidth, {
    start: 0,
    end: PDF_MAX,
    left: 10,
    right: 12,
    low: 0,
    high: tailPeaks[tail] * 1.08,
  });
  const { x, y, top, bottom, left, right } = plot;
  const ticks = narrow
    ? [0, 400_000, PDF_MAX]
    : [0, 200_000, 400_000, 600_000, PDF_MAX];
  const points = distributions[tail];

  function area(from, to) {
    const interior = points.filter(([value]) => value > from && value < to);
    const shape = [
      [from, 0],
      [from, density(tail, from)],
      ...interior,
      [to, density(tail, to)],
      [to, 0],
    ];
    return `${path(shape, x, y)} Z`;
  }

  const xAtt = x(ATTACHMENT);
  const xLim = x(limit);
  const topLabel = `Top ${compactDollars(limit)}`;
  const placeTopRight = xLim < left + (right - left) * 0.68;
  const topLabelX = placeTopRight ? Math.min(xLim + 6, right) : Math.max(xLim - 6, left);
  const topLabelAnchor = placeTopRight ? 'start' : 'end';

  const attLabel =
    !narrow && xLim - xAtt > 78
      ? `<text class="chart-text is-attachment" x="${(xAtt + 5).toFixed(1)}" y="14" text-anchor="start">$100k</text>`
      : '';

  svg.innerHTML = `
    ${chartLabels('Severity distribution', `The ${TAILS[tail].toLowerCase()} tail has a fixed $100,000 mean. Shaded regions show Primary ($0 to $100,000) and Excess Layer ($100,000 to ${dollars.format(limit)}).`)}
    <line class="chart-grid" x1="${left}" y1="${(top + (bottom - top) * 0.35).toFixed(1)}" x2="${right}" y2="${(top + (bottom - top) * 0.35).toFixed(1)}"/>
    <line class="chart-grid" x1="${left}" y1="${(top + (bottom - top) * 0.7).toFixed(1)}" x2="${right}" y2="${(top + (bottom - top) * 0.7).toFixed(1)}"/>
    <path d="${area(0, ATTACHMENT)}" class="area-primary"/>
    <path d="${area(ATTACHMENT, limit)}" class="area-excess"/>
    ${axes(plot, ticks)}
    <path d="${path(points, x, y)}" class="chart-curve is-active"/>
    <line class="chart-marker attachment" x1="${xAtt.toFixed(1)}" y1="${top}" x2="${xAtt.toFixed(1)}" y2="${bottom}"/>
    <line class="chart-marker" x1="${xLim.toFixed(1)}" y1="${top}" x2="${xLim.toFixed(1)}" y2="${bottom}"/>
    ${attLabel}
    <text class="chart-text is-active" x="${topLabelX.toFixed(1)}" y="14" text-anchor="${topLabelAnchor}">${topLabel}</text>`;
  return plot;
}

function showModel(exports) {
  const {
    expected_layer: expectedLayer,
    simulated_layer: simulatedLayer,
    exceedance_prob: exceedanceProb,
    severity_pdf: density,
  } = exports;

  const shell = byId('excess-layers');
  const slider = byId('layer-top');
  const svg = byId('severity-chart');
  const chartTitleLabel = byId('chart-title-label');
  const takeawayEl = byId('takeaway-text');
  const tailButtons = [...document.querySelectorAll('[data-tail]')];
  const viewButtons = [...document.querySelectorAll('[data-chart-view]')];
  const presetButtons = [...document.querySelectorAll('[data-preset]')];
  const tickButtons = [...document.querySelectorAll('[data-limit-jump]')];
  const layerRows = [...document.querySelectorAll('[data-layer-row]')];

  const limits = Array.from({ length: 27 }, (_, index) => ATTACHMENT + index * LIMIT_STEP);
  const curves = TAILS.map((_, tailIndex) => {
    const base = expectedLayer(tailIndex, ATTACHMENT, 0);
    return limits.map(limit => [limit, 1 + expectedLayer(tailIndex, limit, 1) / base]);
  });

  let distributions = null;
  let tailPeaks = null;
  function ensurePdfData() {
    if (distributions) return;
    const pdfGrid = [];
    for (let loss = 0; loss <= 60_000; loss += 2_500) pdfGrid.push(loss);
    for (let loss = 65_000; loss <= PDF_MAX; loss += 5_000) pdfGrid.push(loss);
    distributions = TAILS.map((_, tailIndex) =>
      pdfGrid.map(loss => [loss, density(tailIndex, loss)]),
    );
    tailPeaks = distributions.map(points =>
      Math.max(...points.map(([, value]) => value)),
    );
  }

  let tail = 1;
  let view = 'ilf';
  let ilf = 1 + expectedLayer(1, 350_000, 1) / expectedLayer(1, 350_000, 0);
  let plotWidth = 540;
  let lastPlot = { width: 540, left: 44, right: 474, start: ATTACHMENT, end: MAX_LIMIT };
  let announcement;
  let sliderTrackTimer;

  function syncSliderVisual(limit) {
    const pct = ((limit - MIN_LIMIT) / (MAX_LIMIT - MIN_LIMIT)) * 100;
    slider.style.setProperty('--slider-pct', `${pct.toFixed(2)}%`);
  }

  function syncPresetChips(limit) {
    presetButtons.forEach(btn => {
      const match =
        Number(btn.dataset.presetLimit) === limit &&
        Number(btn.dataset.presetTail) === tail;
      btn.setAttribute('aria-pressed', String(match));
    });
  }

  function draw() {
    const limit = slider.valueAsNumber;
    if (view === 'ilf') {
      lastPlot = renderIlf(svg, plotWidth, curves, tail, limit, ilf);
      chartTitleLabel.textContent = 'ILF Curve · $100k Base';
    } else {
      ensurePdfData();
      lastPlot = renderSeverity(svg, plotWidth, distributions, tailPeaks, density, tail, limit);
      chartTitleLabel.textContent = `Severity PDF · ${TAILS[tail]} Tail`;
    }
  }

  function update(announce = false) {
    const limit = slider.valueAsNumber;
    syncSliderVisual(limit);
    syncPresetChips(limit);

    const primary = expectedLayer(tail, limit, 0);
    const excess = expectedLayer(tail, limit, 1);
    const above = Math.max(0, GROUND_UP_MEAN - primary - excess);
    ilf = 1 + excess / primary;

    const pAtt = exceedanceProb(tail, ATTACHMENT);
    const pTop = exceedanceProb(tail, limit);

    byId('top-value').textContent = dollars.format(limit);
    slider.setAttribute('aria-valuetext', dollars.format(limit));
    byId('excess-range').textContent = `$100k–${compactDollars(limit)}`;
    byId('above-range').textContent = `>${compactDollars(limit)}`;
    byId('tail-sigma').textContent = TAIL_SIGMAS[tail];

    byId('loss-primary').textContent = dollars.format(primary);
    byId('loss-excess').textContent = dollars.format(excess);
    byId('loss-above').textContent = dollars.format(above);
    byId('relativity').textContent = percent.format(excess / primary);
    byId('ilf').textContent = ratio(ilf);
    byId('pierce-att').textContent = percent.format(pAtt);
    byId('pierce-top').textContent = percent.format(pTop);

    const pctPrimary = (primary / GROUND_UP_MEAN) * 100;
    const pctExcess = (excess / GROUND_UP_MEAN) * 100;
    const pctAbove = Math.max(0, 100 - pctPrimary - pctExcess);
    byId('bar-primary').style.width = `${pctPrimary.toFixed(2)}%`;
    byId('bar-excess').style.width = `${pctExcess.toFixed(2)}%`;
    byId('bar-above').style.width = `${pctAbove.toFixed(2)}%`;
    byId('pct-primary').textContent = `${pctPrimary.toFixed(1)}%`;
    byId('pct-excess').textContent = `${pctExcess.toFixed(1)}%`;
    byId('pct-above').textContent = `${pctAbove.toFixed(1)}%`;

    if (takeawayEl) {
      takeawayEl.innerHTML = `Under a <strong>${TAILS[tail].toLowerCase()} tail</strong> (${TAIL_SIGMAS[tail]}) at a <strong>${compactDollars(limit)}</strong> limit, <strong>${pctExcess.toFixed(1)}%</strong> of expected loss sits in the excess layer and <strong>${percent.format(pTop)}</strong> of claims pierce above—giving an ILF of <strong>${ratio(ilf)}</strong>.`;
    }

    if (typeof simulatedLayer === 'function') {
      const simPrimary = simulatedLayer(tail, limit, 0);
      const simExcess = simulatedLayer(tail, limit, 1);
      const simIlf = 1 + simExcess / simPrimary;
      const diffExcess = simExcess - excess;
      const sign = diffExcess >= 0 ? '+' : '−';
      const simCell = byId('sim-ilf');
      simCell.innerHTML = `${ratio(simIlf)} <small id="sim-diff">${sign}${dollars.format(Math.abs(diffExcess))}</small>`;
      simCell.title = `65,536-draw fixed-seed Rust MC: Primary ${dollars.format(simPrimary)}, Excess ${dollars.format(simExcess)} (${sign}${dollars.format(Math.abs(diffExcess))} vs. closed-form)`;
    }

    draw();

    clearTimeout(announcement);
    if (announce) {
      announcement = setTimeout(() => {
        byId('results-summary').textContent =
          `${TAILS[tail]} tail at ${dollars.format(limit)}. Primary ${dollars.format(primary)}, Excess ${dollars.format(excess)}, above limit ${dollars.format(above)}, relativity ${percent.format(excess / primary)}, increased limits factor ${ratio(ilf)}.`;
      }, 220);
    }
  }

  function applyLimit(nextLimit, control) {
    const snapped = Math.min(
      MAX_LIMIT,
      Math.max(MIN_LIMIT, Math.round(nextLimit / LIMIT_STEP) * LIMIT_STEP),
    );
    if (snapped === slider.valueAsNumber) return;
    slider.value = String(snapped);
    update(true);
    trackLabEngagement(control);
    clearTimeout(sliderTrackTimer);
    sliderTrackTimer = setTimeout(() => {
      track('lab_interact', {
        control,
        value: compactDollars(snapped),
        tail: TAILS[tail].toLowerCase(),
        limit: compactDollars(snapped),
      });
    }, 600);
  }

  function applyTail(nextTail) {
    tail = nextTail;
    tailButtons.forEach(option => option.setAttribute('aria-pressed', String(Number(option.dataset.tail) === tail)));
  }

  slider.addEventListener('input', () => {
    update(true);
    trackLabEngagement('slider');
    clearTimeout(sliderTrackTimer);
    sliderTrackTimer = setTimeout(() => {
      track('lab_interact', {
        control: 'slider',
        value: compactDollars(slider.valueAsNumber),
        tail: TAILS[tail].toLowerCase(),
        limit: compactDollars(slider.valueAsNumber),
      });
    }, 600);
  });

  tickButtons.forEach(btn => {
    btn.addEventListener('click', () => {
      applyLimit(Number(btn.dataset.limitJump), 'slider');
    });
  });

  tailButtons.forEach(button =>
    button.addEventListener('click', () => {
      applyTail(Number(button.dataset.tail));
      trackLabEngagement('tail');
      track('lab_interact', {
        control: 'tail',
        value: TAILS[tail].toLowerCase(),
        tail: TAILS[tail].toLowerCase(),
        limit: compactDollars(slider.valueAsNumber),
      });
      update(true);
    }),
  );

  presetButtons.forEach(btn => {
    btn.addEventListener('click', () => {
      const limit = Number(btn.dataset.presetLimit);
      const nextTail = Number(btn.dataset.presetTail);
      const name = btn.dataset.preset;
      applyTail(nextTail);
      slider.value = String(limit);
      trackLabEngagement('preset');
      track('lab_interact', {
        control: 'preset',
        value: name,
        tail: TAILS[tail].toLowerCase(),
        limit: compactDollars(limit),
      });
      update(true);
    });
  });

  function setView(nextView, trackEvent = true) {
    view = nextView;
    viewButtons.forEach(option => option.setAttribute('aria-pressed', String(option.dataset.chartView === view)));
    if (trackEvent) {
      trackLabEngagement('view');
      track('lab_interact', {
        control: 'view',
        value: view,
        tail: TAILS[tail].toLowerCase(),
        limit: compactDollars(slider.valueAsNumber),
      });
    }
    draw();
  }

  viewButtons.forEach(button =>
    button.addEventListener('click', () => setView(button.dataset.chartView, true)),
  );

  // Direct pointer scrubbing on the SVG plot (Bartosz Ciechanowski / Distill style)
  let dragging = false;
  function limitFromPointer(event) {
    const rect = svg.getBoundingClientRect();
    if (!rect.width) return null;
    const svgX = ((event.clientX - rect.left) / rect.width) * lastPlot.width;
    const span = Math.max(1, lastPlot.right - lastPlot.left);
    const frac = Math.min(1, Math.max(0, (svgX - lastPlot.left) / span));
    return lastPlot.start + frac * (lastPlot.end - lastPlot.start);
  }

  svg.addEventListener('pointerdown', event => {
    if (event.button !== 0 && event.pointerType === 'mouse') return;
    dragging = true;
    try {
      svg.setPointerCapture(event.pointerId);
    } catch {}
    const next = limitFromPointer(event);
    if (next !== null) applyLimit(next, 'chart_drag');
  });

  svg.addEventListener('pointermove', event => {
    if (!dragging) return;
    const next = limitFromPointer(event);
    if (next !== null) applyLimit(next, 'chart_drag');
  });

  const endDrag = () => {
    dragging = false;
  };
  svg.addEventListener('pointerup', endDrag);
  svg.addEventListener('pointercancel', endDrag);

  // Cross-hover highlighting between Ledger rows, proportion bar, and PDF shading
  layerRows.forEach(row => {
    const layer = row.dataset.layerRow;
    row.addEventListener('mouseenter', () => {
      if (shell) shell.dataset.hoverLayer = layer;
    });
    row.addEventListener('mouseleave', () => {
      if (shell) delete shell.dataset.hoverLayer;
    });
  });

  labController = {
    applyPreset(limit, tailIndex, name, control = 'preset') {
      applyTail(tailIndex);
      slider.value = String(limit);
      trackLabEngagement(control);
      track('lab_interact', {
        control,
        value: name,
        tail: TAILS[tail].toLowerCase(),
        limit: compactDollars(limit),
      });
      update(true);
    },
    toggleView() {
      setView(view === 'ilf' ? 'pdf' : 'ilf', true);
    },
  };

  // Default $350k Moderate state is pre-rendered in HTML; avoid 131k-draw MC work on initial paint.
  syncSliderVisual(slider.valueAsNumber);
  let lastWidth = 0;
  new ResizeObserver(entries => {
    const nextWidth = Math.round(entries[0]?.contentRect.width || 0);
    if (nextWidth > 0 && nextWidth !== lastWidth) {
      const initialDesktop = lastWidth === 0 && nextWidth >= 480;
      lastWidth = nextWidth;
      plotWidth = nextWidth;
      if (!initialDesktop) draw();
    }
  }).observe(svg);
}

initTheme();
initAnalytics();
initCopyEmail();
initScrollOrientation();
initReveal();
initMethodologyDrawer();
initPrintButton();
initCommandPalette();

const entryHash = window.location.hash.replace(/^#/, '').slice(0, 24) || 'none';
let wasmReady = 'no';

try {
  showModel(await loadModel());
  wasmReady = 'yes';
} catch (error) {
  console.error('Excess layer model failed to load', error);
  captureError(error, { component: 'wasm-lab' });
  const status = byId('model-status');
  status.textContent = 'Interactive WebAssembly controls could not load; baseline $350k moderate-tail values are displayed below.';
  status.hidden = false;
}

identifySession({
  theme: document.documentElement.dataset.theme || 'light',
  viewport: getViewportTier(),
  entry_hash: entryHash,
  channel: deriveChannel(),
  wasm_ready: wasmReady,
});
