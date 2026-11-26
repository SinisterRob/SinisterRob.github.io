const GROUND_UP_MEAN = 100_000;
const MIN_VALUE = 25_000;
const MAX_LIMIT = 1_000_000;
const LIMIT_STEP = 25_000;
const PDF_MAX = 1_000_000;
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

const compactDollars = value => {
  if (value >= 1_000_000) {
    const m = value / 1_000_000;
    return `$${Number.isInteger(m) ? m : m.toFixed(2).replace(/0$/, '')}M`;
  }
  return `$${Math.round(value / 1000)}k`;
};

const compactDollarsPrecise = value => {
  if (value >= 1_000_000) return `$${(value / 1_000_000).toFixed(2).replace(/\.?0+$/, '')}M`;
  if (value >= 1000) return `$${(value / 1000).toFixed(1).replace(/\.0$/, '')}k`;
  return dollars.format(value);
};
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
      if (label) label.textContent = 'Copied ✓';
      else button.textContent = 'Copied ✓';
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

function initNavInert() {
  const primary = byId('primary-nav');
  const mobile = byId('mobile-nav');
  if (!primary || !mobile) return;
  const mq = window.matchMedia('(max-width: 48rem)');
  function sync() {
    const isMobile = mq.matches;
    primary.inert = isMobile;
    primary.setAttribute('aria-hidden', String(isMobile));
    mobile.inert = !isMobile;
    mobile.setAttribute('aria-hidden', String(!isMobile));
  }
  if (typeof mq.addEventListener === 'function') mq.addEventListener('change', sync);
  else if (typeof mq.addListener === 'function') mq.addListener(sync);
  sync();
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
    { group: 'Lab Presets', label: 'Load High Severity Scenario', sub: '$750k xs $250k · σ 1.55', keywords: 'heavy tail high severity excess preset lab 1m', run: () => runLabPreset(250_000, 1_000_000, 2, 'excess') },
    { group: 'Lab Presets', label: 'Load Low Severity Scenario', sub: '$200k xs $50k · σ 0.55', keywords: 'light tail low severity working layer preset lab', run: () => runLabPreset(50_000, 250_000, 0, 'working') },
    { group: 'Lab Presets', label: 'Toggle ILF Curve / Severity PDF', sub: 'Chart view', keywords: 'pdf ilf chart view', run: () => toggleLabView() },
    { group: 'Actions', label: 'Copy Email Address', sub: 'robertdjurgens@gmail.com', keywords: 'mail clipboard contact', run: () => copyEmailFn() },
    { group: 'Actions', label: 'Switch Light / Dark Theme', sub: 'Appearance', keywords: 'dark light mode theme', run: () => toggleThemeFn() },
    { group: 'Actions', label: 'Print / Save Executive Brief (PDF)', sub: 'Print', keywords: 'resume cv pdf print', run: () => setTimeout(() => window.print(), 120) },
    { group: 'External', label: 'Open LinkedIn Profile', sub: 'linkedin.com ↗', keywords: 'social profile', run: () => openExternal('https://www.linkedin.com/in/robert-jurgens-fcas-352017143/', 'linkedin') },
    { group: 'External', label: 'Open GitHub (@SinisterRob)', sub: 'github.com ↗', keywords: 'code repos rust', run: () => openExternal('https://github.com/SinisterRob', 'github') },
    { group: 'External', label: 'Open reserving-kernels Crate', sub: 'reserving-kernels ↗', keywords: 'rust crate reserving mack odp', run: () => openExternal('https://github.com/SinisterRob/reserving-kernels', 'reserving_kernels') },
    { group: 'External', label: 'Inspect Rust WASM Kernel Source', sub: 'excess_layers.rs ↗', keywords: 'rust wasm source', run: () => openExternal('https://github.com/SinisterRob/SinisterRob.github.io/blob/main/src/excess_layers.rs', 'rust_source') },
  ];

  function scrollBehavior() {
    return window.matchMedia('(prefers-reduced-motion: reduce)').matches ? 'auto' : 'smooth';
  }

  function jumpTo(hash, target) {
    track('nav_click', { target, source: 'command_palette' });
    const el = document.querySelector(hash);
    if (el) el.scrollIntoView({ behavior: scrollBehavior() });
    else window.location.hash = hash;
  }

  function runLabPreset(attach, limit, tailIndex, name) {
    if (labController) {
      labController.applyPreset(attach, limit, tailIndex, name, 'command_palette');
    }
    byId('lab')?.scrollIntoView({ behavior: scrollBehavior() });
  }

  function toggleLabView() {
    if (labController) {
      labController.toggleView();
    }
    byId('lab')?.scrollIntoView({ behavior: scrollBehavior() });
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
      list.innerHTML = '<p class="cmd-empty" role="presentation">No matching sections or actions.</p>';
      input.removeAttribute('aria-activedescendant');
      return;
    }

    let lastGroup = '';
    const html = [];
    filtered.forEach((item, idx) => {
      if (item.group !== lastGroup) {
        lastGroup = item.group;
        html.push(`<div class="cmd-group-label" role="presentation">${item.group}</div>`);
      }
      html.push(
        `<button type="button" id="cmd-opt-${idx}" class="cmd-item ${idx === selectedIndex ? 'is-selected' : ''}" role="option" aria-selected="${idx === selectedIndex}" data-cmd-idx="${idx}"><span>${item.label}</span><span class="cmd-item-sub">${item.sub}</span></button>`,
      );
    });
    list.innerHTML = html.join('');
    input.setAttribute('aria-activedescendant', `cmd-opt-${selectedIndex}`);
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
    input.setAttribute('aria-activedescendant', `cmd-opt-${selectedIndex}`);
  }

  let restoreFocusEl = null;
  let skipRestoreFocus = false;

  function execute(idx) {
    const item = filtered[idx];
    if (!item) return;
    skipRestoreFocus = true;
    dialog.close();
    item.run();
  }

  function openPalette() {
    if (dialog.open) return;
    restoreFocusEl = document.activeElement instanceof HTMLElement ? document.activeElement : trigger;
    skipRestoreFocus = false;
    input.value = '';
    renderList('');
    dialog.showModal();
    input.focus();
  }

  dialog.addEventListener('close', () => {
    if (!skipRestoreFocus) {
      (restoreFocusEl || trigger).focus();
    }
    skipRestoreFocus = false;
  });

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
  const url = new URL('./excess_layers.wasm', import.meta.url);
  if (typeof WebAssembly.instantiateStreaming === 'function') {
    try {
      const { instance } = await WebAssembly.instantiateStreaming(fetch(url));
      return instance.exports;
    } catch {
      // Fallback when server lacks application/wasm MIME type.
    }
  }
  const response = await fetch(url);
  if (!response.ok) throw new Error(`Model request returned ${response.status}`);
  const { instance } = await WebAssembly.instantiate(await response.arrayBuffer());
  return instance.exports;
}

function getPlot(svg, plotWidth, { start, end, left, right, low, high }) {
  const width = Math.max(250, Math.round(plotWidth || 540));
  const height = width < 380 ? 176 : 236;
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

function axes(plot, xTicks, yTicks = [], pivotValue = null) {
  const { x, y, left, right, bottom } = plot;
  const grid = yTicks
    .map(value => {
      const isPivot = pivotValue !== null && Math.abs(value - pivotValue) < 1e-6;
      return `
      <line class="${isPivot ? 'chart-pivot-line' : 'chart-grid'}" x1="${left}" y1="${y(value).toFixed(1)}" x2="${right}" y2="${y(value).toFixed(1)}"/>
      <text class="chart-text${isPivot ? ' is-attachment' : ''}" x="${left - 6}" y="${(y(value) + 4).toFixed(1)}" text-anchor="end">${ratio(value)}</text>`;
    })
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

function ilfAxisScale(maxIlf) {
  if (maxIlf <= 1.52) return { high: 1.6, ticks: [0, 0.5, 1, 1.5] };
  if (maxIlf <= 2.04) return { high: 2.12, ticks: [0, 0.5, 1, 1.5, 2] };
  if (maxIlf <= 3.05) return { high: 3.16, ticks: [0, 1, 2, 3] };
  const topTick = Math.ceil(maxIlf);
  const step = topTick <= 4 ? 1 : Math.ceil(topTick / 4);
  const ticks = [0, 1];
  for (let v = step > 1 ? step : 2; v <= topTick; v += step) {
    if (v > 1) ticks.push(v);
  }
  return { high: Math.max(maxIlf * 1.06, topTick + 0.12), ticks };
}

const ILF_GRID = (() => {
  const g = [0, 5_000, 10_000, 15_000, 25_000, 40_000, 50_000, 75_000];
  for (let l = 100_000; l <= MAX_LIMIT; l += LIMIT_STEP) g.push(l);
  return g;
})();

function renderIlf(svg, plotWidth, lev, tail, attach, limit, ilf) {
  const narrow = (plotWidth || 540) < 420;
  const curves = TAILS.map((_, tailIndex) => {
    const base = Math.max(1e-6, lev(tailIndex, attach));
    return ILF_GRID.map(l => [l, lev(tailIndex, l) / base]);
  });
  const maxIlf = Math.max(...curves.map(pts => pts.at(-1)[1]), ilf);
  const { high, ticks: yTicks } = ilfAxisScale(maxIlf);

  const plot = getPlot(svg, plotWidth, {
    start: 0,
    end: MAX_LIMIT,
    left: 44,
    right: narrow ? 12 : 66,
    low: 0,
    high,
  });
  const { x, y, top, bottom, left, right } = plot;
  const xTicks = narrow
    ? [0, 500_000, MAX_LIMIT]
    : [0, 250_000, 500_000, 750_000, MAX_LIMIT];

  const comparison = curves
    .map((points, index) => (index === tail ? '' : `<path d="${path(points, x, y)}" class="chart-curve"/>`))
    .join('');

  const activeFull = curves[tail];
  const activePrimary = activeFull.filter(([v]) => v <= attach);
  const activeExcess = activeFull.filter(([v]) => v >= attach);

  const labels = !narrow
    ? (() => {
        const items = curves.map((points, index) => ({
          index,
          yPos: y(points.at(-1)[1]) + 4,
        }));
        items.sort((a, b) => a.yPos - b.yPos);
        for (let i = 1; i < items.length; i++) {
          if (items[i].yPos - items[i - 1].yPos < 13) {
            items[i].yPos = items[i - 1].yPos + 13;
          }
        }
        return items
          .map(
            item =>
              `<text class="chart-text ${item.index === tail ? 'is-active' : ''}" x="${right + 8}" y="${item.yPos.toFixed(1)}">${TAILS[item.index]}</text>`,
          )
          .join('');
      })()
    : '';

  const xAtt = x(attach);
  const xLim = x(limit);
  const yPivot = y(1);
  const markerY = y(ilf);
  const markerLabel = `${compactDollars(limit)} · ${ratio(ilf)}`;
  const halfW = (markerLabel.length * 6.5) / 2;
  const labelX = Math.max(left + halfW, Math.min(xLim, right - halfW));

  const attText = compactDollars(attach);
  const attHalfW = (attText.length * 6.5) / 2;
  const attLabelX = Math.max(left + attHalfW, Math.min(xAtt, right - attHalfW));
  const showAttLabel = Math.abs(labelX - attLabelX) > halfW + attHalfW + 8;

  svg.innerHTML = `
    ${chartLabels('Increased limits factor by coverage limit', `The selected ${TAILS[tail].toLowerCase()} tail has an ILF of ${ilf.toFixed(2)} for ${compactDollars(limit - attach)} excess of ${compactDollars(attach)} across a $0 to $1M limit range.`)}
    ${axes(plot, xTicks, yTicks, 1)}
    <rect class="chart-primary-band" x="${left.toFixed(1)}" y="${top}" width="${Math.max(0, xAtt - left).toFixed(1)}" height="${(bottom - top).toFixed(1)}"/>
    <rect class="chart-layer-band" x="${xAtt.toFixed(1)}" y="${top}" width="${Math.max(0, xLim - xAtt).toFixed(1)}" height="${(bottom - top).toFixed(1)}"/>
    <line class="chart-marker attachment" x1="${xAtt.toFixed(1)}" y1="${top}" x2="${xAtt.toFixed(1)}" y2="${bottom}"/>
    <line class="chart-marker" x1="${xLim.toFixed(1)}" y1="${top}" x2="${xLim.toFixed(1)}" y2="${bottom}"/>
    ${comparison}
    <path d="${path(activePrimary, x, y)}" class="chart-curve is-primary"/>
    <path d="${path(activeExcess, x, y)}" class="chart-curve is-active"/>
    ${labels}
    <circle class="chart-dot-attach" cx="${xAtt.toFixed(1)}" cy="${yPivot.toFixed(1)}" r="3.8" aria-hidden="true"/>
    <circle class="chart-dot-ring" cx="${xLim.toFixed(1)}" cy="${markerY.toFixed(1)}" r="8.5" aria-hidden="true"/>
    <circle class="chart-dot" cx="${xLim.toFixed(1)}" cy="${markerY.toFixed(1)}" r="4.4" aria-hidden="true"/>
    <circle class="chart-handle-hit" data-handle="attach" cx="${xAtt.toFixed(1)}" cy="${yPivot.toFixed(1)}" r="22" aria-hidden="true"/>
    <circle class="chart-handle-hit" data-handle="limit" cx="${xLim.toFixed(1)}" cy="${markerY.toFixed(1)}" r="22" aria-hidden="true"/>
    ${showAttLabel ? `<text class="chart-text is-attachment" x="${attLabelX.toFixed(1)}" y="14" text-anchor="middle">${attText}</text>` : ''}
    <text class="chart-text is-active" x="${labelX.toFixed(1)}" y="14" text-anchor="middle">${markerLabel}</text>`;
  return plot;
}

function renderSeverity(svg, plotWidth, distributions, tailPeaks, density, tail, attach, limit) {
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
    ? [0, 500_000, PDF_MAX]
    : [0, 250_000, 500_000, 750_000, PDF_MAX];
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

  const xAtt = x(attach);
  const xLim = x(limit);
  const yAtt = y(density(tail, attach));
  const yLim = y(density(tail, limit));
  const topLabel = `Cap ${compactDollars(limit)}`;
  const placeTopRight = xLim < left + (right - left) * 0.68;
  const topLabelX = placeTopRight ? Math.min(xLim + 6, right) : Math.max(xLim - 6, left);
  const topLabelAnchor = placeTopRight ? 'start' : 'end';

  const attLabel =
    !narrow && xLim - xAtt > 72
      ? `<text class="chart-text is-attachment" x="${(xAtt + 5).toFixed(1)}" y="14" text-anchor="start">${compactDollars(attach)}</text>`
      : '';

  svg.innerHTML = `
    ${chartLabels('Severity distribution', `The ${TAILS[tail].toLowerCase()} tail has a fixed $100,000 mean. Shaded regions show Primary ($0 to ${dollars.format(attach)}) and Excess Layer (${dollars.format(attach)} to ${dollars.format(limit)}).`)}
    <line class="chart-grid" x1="${left}" y1="${(top + (bottom - top) * 0.35).toFixed(1)}" x2="${right}" y2="${(top + (bottom - top) * 0.35).toFixed(1)}"/>
    <line class="chart-grid" x1="${left}" y1="${(top + (bottom - top) * 0.7).toFixed(1)}" x2="${right}" y2="${(top + (bottom - top) * 0.7).toFixed(1)}"/>
    <path d="${area(0, attach)}" class="area-primary"/>
    <path d="${area(attach, limit)}" class="area-excess"/>
    ${axes(plot, ticks)}
    <path d="${path(points, x, y)}" class="chart-curve is-active"/>
    <line class="chart-marker attachment" x1="${xAtt.toFixed(1)}" y1="${top}" x2="${xAtt.toFixed(1)}" y2="${bottom}"/>
    <line class="chart-marker" x1="${xLim.toFixed(1)}" y1="${top}" x2="${xLim.toFixed(1)}" y2="${bottom}"/>
    <circle class="chart-dot-attach" cx="${xAtt.toFixed(1)}" cy="${yAtt.toFixed(1)}" r="3.8" aria-hidden="true"/>
    <circle class="chart-dot-ring" cx="${xLim.toFixed(1)}" cy="${yLim.toFixed(1)}" r="8" aria-hidden="true"/>
    <circle class="chart-dot" cx="${xLim.toFixed(1)}" cy="${yLim.toFixed(1)}" r="4.2" aria-hidden="true"/>
    <circle class="chart-handle-hit" data-handle="attach" cx="${xAtt.toFixed(1)}" cy="${yAtt.toFixed(1)}" r="22" aria-hidden="true"/>
    <circle class="chart-handle-hit" data-handle="limit" cx="${xLim.toFixed(1)}" cy="${yLim.toFixed(1)}" r="22" aria-hidden="true"/>
    ${attLabel}
    <text class="chart-text is-active" x="${topLabelX.toFixed(1)}" y="14" text-anchor="${topLabelAnchor}">${topLabel}</text>`;
  return plot;
}

function showModel(exports) {
  const {
    limited_expected_value: lev,
    expected_layer: expectedLayer,
    simulated_layer: simulatedLayer,
    exceedance_prob: exceedanceProb,
    severity_pdf: density,
  } = exports;

  const shell = byId('excess-layers');
  const dualRange = byId('dual-range');
  const attachSlider = byId('layer-attach');
  const limitSlider = byId('layer-top');
  const svg = byId('severity-chart');
  const chartTitleLabel = byId('chart-title-label');
  const tailButtons = [...document.querySelectorAll('[data-tail]')];
  const viewButtons = [...document.querySelectorAll('[data-chart-view]')];
  const presetButtons = [...document.querySelectorAll('[data-preset]')];
  const tickButtons = [...document.querySelectorAll('[data-limit-jump]')];
  const layerRows = [...document.querySelectorAll('[data-layer-row]')];

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
  let ilf = 1 + expectedLayer(1, 100_000, 350_000, 1) / expectedLayer(1, 100_000, 350_000, 0);
  let plotWidth = 540;
  let lastPlot = { width: 540, left: 44, right: 474, start: 0, end: MAX_LIMIT };
  const simPrimaryCache = new Map();
  const simExcessCache = new Map();
  let mcTimer = null;
  let rafId = null;
  let sliderTrackTimer;
  let latestSummary = '';

  function syncSliderVisual(attach, limit) {
    const span = MAX_LIMIT - MIN_VALUE;
    const attachPct = ((attach - MIN_VALUE) / span) * 100;
    const limitPct = ((limit - MIN_VALUE) / span) * 100;
    if (dualRange) {
      dualRange.style.setProperty('--attach-pct', `${attachPct.toFixed(2)}%`);
      dualRange.style.setProperty('--limit-pct', `${limitPct.toFixed(2)}%`);
    }
    // Keep the attachment thumb on top if it approaches the right end so it stays grabbable
    attachSlider.classList.toggle('is-top-thumb', attach >= MAX_LIMIT - LIMIT_STEP * 3);
  }

  function syncPresetChips(attach, limit) {
    presetButtons.forEach(btn => {
      const match =
        Number(btn.dataset.presetAttach) === attach &&
        Number(btn.dataset.presetLimit) === limit &&
        Number(btn.dataset.presetTail) === tail;
      btn.setAttribute('aria-pressed', String(match));
    });
  }

  function draw() {
    const attach = attachSlider.valueAsNumber;
    const limit = limitSlider.valueAsNumber;
    if (view === 'ilf') {
      lastPlot = renderIlf(svg, plotWidth, lev, tail, attach, limit, ilf);
      chartTitleLabel.textContent = `ILF Curve · ${compactDollars(attach)} Base`;
    } else {
      ensurePdfData();
      lastPlot = renderSeverity(svg, plotWidth, distributions, tailPeaks, density, tail, attach, limit);
      chartTitleLabel.textContent = `Severity PDF · ${TAILS[tail]} Tail`;
    }
  }

  function updateMc(attach, limit, excess) {
    if (typeof simulatedLayer !== 'function') return;
    const priKey = `${tail}:${attach}`;
    let simPrimary = simPrimaryCache.get(priKey);
    if (simPrimary === undefined) {
      simPrimary = simulatedLayer(tail, attach, limit, 0);
      simPrimaryCache.set(priKey, simPrimary);
    }
    const exKey = `${tail}:${attach}:${limit}`;
    let simExcess = simExcessCache.get(exKey);
    if (simExcess === undefined) {
      simExcess = simulatedLayer(tail, attach, limit, 1);
      simExcessCache.set(exKey, simExcess);
    }
    const diffExcess = simExcess - excess;
    const sign = diffExcess >= 0 ? '+' : '−';
    const diffPct = excess > 1 ? Math.abs((diffExcess / excess) * 100) : 0;
    const simCheck = byId('sim-check');
    const simDelta = byId('sim-delta');
    const mcVerify = byId('mc-verify');
    if (simCheck) simCheck.textContent = dollars.format(simExcess);
    if (simDelta) simDelta.textContent = `${sign}${diffPct.toFixed(1)}%`;
    if (mcVerify) {
      mcVerify.title = `65,536-draw fixed-seed Rust MC: Primary ${dollars.format(simPrimary)}, Excess ${dollars.format(simExcess)} (${sign}${dollars.format(Math.abs(diffExcess))} vs. closed-form)`;
    }
  }

  function announceResults() {
    const summaryEl = byId('results-summary');
    if (summaryEl && latestSummary) {
      summaryEl.textContent = latestSummary;
    }
  }

  function update({ announce = false, immediateMc = true } = {}) {
    const attach = attachSlider.valueAsNumber;
    const limit = limitSlider.valueAsNumber;
    const width = Math.max(LIMIT_STEP, limit - attach);
    syncSliderVisual(attach, limit);
    syncPresetChips(attach, limit);

    const primary = expectedLayer(tail, attach, limit, 0);
    const excess = expectedLayer(tail, attach, limit, 1);
    const above = Math.max(0, GROUND_UP_MEAN - primary - excess);
    ilf = 1 + excess / primary;

    const pAtt = exceedanceProb(tail, attach);
    const pTop = exceedanceProb(tail, limit);
    const layerRate = excess / width;
    const avgHit = pAtt > 1e-9 ? excess / pAtt : 0;
    const shorthand = `${compactDollars(width)} xs ${compactDollars(attach)}`;

    byId('layer-shorthand').textContent = shorthand;
    byId('attach-value').textContent = compactDollars(attach);
    byId('top-value').textContent = compactDollars(limit);
    attachSlider.setAttribute('aria-valuetext', dollars.format(attach));
    limitSlider.setAttribute('aria-valuetext', dollars.format(limit));

    byId('primary-range').textContent = `$0–${compactDollars(attach)}`;
    byId('excess-range').textContent = `${compactDollars(attach)}–${compactDollars(limit)}`;
    byId('above-range').textContent = `>${compactDollars(limit)}`;
    byId('tail-sigma').textContent = TAILS[tail];

    byId('loss-primary').textContent = dollars.format(primary);
    byId('loss-excess').textContent = dollars.format(excess);
    byId('loss-above').textContent = dollars.format(above);
    byId('pierce-att').textContent = percent.format(pAtt);
    byId('pierce-top').textContent = percent.format(pTop);

    byId('ilf').textContent = ratio(ilf);
    byId('ilf-sub').textContent = `${compactDollars(limit)} / ${compactDollars(attach)}`;
    byId('layer-rate').textContent = percent.format(layerRate);
    byId('burn-sub').textContent = `of ${compactDollars(width)}`;
    byId('avg-hit').textContent = compactDollarsPrecise(avgHit);
    byId('avg-hit-sub').textContent = `X>${compactDollars(attach)}`;

    const pctPrimary = (primary / GROUND_UP_MEAN) * 100;
    const pctExcess = (excess / GROUND_UP_MEAN) * 100;
    const pctAbove = Math.max(0, 100 - pctPrimary - pctExcess);
    byId('bar-primary').style.width = `${pctPrimary.toFixed(2)}%`;
    byId('bar-excess').style.width = `${pctExcess.toFixed(2)}%`;
    byId('bar-above').style.width = `${pctAbove.toFixed(2)}%`;
    byId('pct-primary').textContent = `${pctPrimary.toFixed(1)}%`;
    byId('pct-excess').textContent = `${pctExcess.toFixed(1)}%`;
    byId('pct-above').textContent = `${pctAbove.toFixed(1)}%`;

    clearTimeout(mcTimer);
    if (immediateMc) {
      updateMc(attach, limit, excess);
    } else {
      mcTimer = setTimeout(() => updateMc(attach, limit, excess), 75);
    }

    draw();

    latestSummary = `${TAILS[tail]} tail (${TAIL_SIGMAS[tail]}) for ${shorthand}. Primary ${dollars.format(primary)}, Excess ${dollars.format(excess)}, above limit ${dollars.format(above)}, ILF ${ratio(ilf)}, layer burn ${percent.format(layerRate)}, average attached claim ${dollars.format(avgHit)}.`;
    if (announce) {
      announceResults();
    }
  }

  function scheduleDragUpdate() {
    if (rafId !== null) return;
    rafId = window.requestAnimationFrame(() => {
      rafId = null;
      update({ announce: false, immediateMc: false });
    });
  }

  function flushCommit() {
    if (rafId !== null) {
      window.cancelAnimationFrame(rafId);
      rafId = null;
    }
    update({ announce: true, immediateMc: true });
  }

  function scheduleSliderTrack(control) {
    const attach = attachSlider.valueAsNumber;
    const limit = limitSlider.valueAsNumber;
    clearTimeout(sliderTrackTimer);
    sliderTrackTimer = setTimeout(() => {
      track('lab_interact', {
        control,
        value: `${compactDollars(limit - attach)} xs ${compactDollars(attach)}`,
        tail: TAILS[tail].toLowerCase(),
        limit: compactDollars(limit),
      });
    }, 600);
  }

  function setHandle(which, rawValue, control, commit = false) {
    const snapped = Math.min(
      MAX_LIMIT,
      Math.max(MIN_VALUE, Math.round(rawValue / LIMIT_STEP) * LIMIT_STEP),
    );
    if (which === 'attach') {
      const clamped = Math.min(snapped, limitSlider.valueAsNumber - LIMIT_STEP);
      if (clamped === attachSlider.valueAsNumber) return;
      attachSlider.value = String(clamped);
    } else {
      const clamped = Math.max(snapped, attachSlider.valueAsNumber + LIMIT_STEP);
      if (clamped === limitSlider.valueAsNumber) return;
      limitSlider.value = String(clamped);
    }
    if (commit) flushCommit();
    else scheduleDragUpdate();
    trackLabEngagement(control);
    scheduleSliderTrack(control);
  }

  function applyTail(nextTail) {
    tail = nextTail;
    tailButtons.forEach(option => option.setAttribute('aria-pressed', String(Number(option.dataset.tail) === tail)));
  }

  attachSlider.addEventListener('input', () => {
    const maxAttach = limitSlider.valueAsNumber - LIMIT_STEP;
    if (attachSlider.valueAsNumber > maxAttach) {
      attachSlider.value = String(maxAttach);
    }
    scheduleDragUpdate();
    trackLabEngagement('slider_attach');
    scheduleSliderTrack('slider_attach');
  });
  attachSlider.addEventListener('change', flushCommit);

  limitSlider.addEventListener('input', () => {
    const minLimit = attachSlider.valueAsNumber + LIMIT_STEP;
    if (limitSlider.valueAsNumber < minLimit) {
      limitSlider.value = String(minLimit);
    }
    scheduleDragUpdate();
    trackLabEngagement('slider_limit');
    scheduleSliderTrack('slider_limit');
  });
  limitSlider.addEventListener('change', flushCommit);

  // Click anywhere on dual-range track to snap the nearer thumb
  dualRange?.addEventListener('pointerdown', event => {
    if (event.target === attachSlider || event.target === limitSlider) return;
    const rect = dualRange.getBoundingClientRect();
    if (!rect.width) return;
    const frac = Math.min(1, Math.max(0, (event.clientX - rect.left) / rect.width));
    const val = MIN_VALUE + frac * (MAX_LIMIT - MIN_VALUE);
    const dAttach = Math.abs(val - attachSlider.valueAsNumber);
    const dLimit = Math.abs(val - limitSlider.valueAsNumber);
    setHandle(dAttach <= dLimit ? 'attach' : 'limit', val, 'slider_track', true);
  });

  tickButtons.forEach(btn => {
    btn.addEventListener('click', () => {
      const val = Number(btn.dataset.limitJump);
      const dAttach = Math.abs(val - attachSlider.valueAsNumber);
      const dLimit = Math.abs(val - limitSlider.valueAsNumber);
      const target = val === MIN_VALUE ? 'attach' : val === MAX_LIMIT ? 'limit' : dAttach <= dLimit ? 'attach' : 'limit';
      setHandle(target, val, 'slider_tick', true);
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
        limit: compactDollars(limitSlider.valueAsNumber),
      });
      flushCommit();
    }),
  );

  presetButtons.forEach(btn => {
    btn.addEventListener('click', () => {
      const attach = Number(btn.dataset.presetAttach);
      const limit = Number(btn.dataset.presetLimit);
      const nextTail = Number(btn.dataset.presetTail);
      const name = btn.dataset.preset;
      applyTail(nextTail);
      attachSlider.value = String(attach);
      limitSlider.value = String(limit);
      trackLabEngagement('preset');
      track('lab_interact', {
        control: 'preset',
        value: name,
        tail: TAILS[tail].toLowerCase(),
        limit: compactDollars(limit),
      });
      flushCommit();
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
        limit: compactDollars(limitSlider.valueAsNumber),
      });
    }
    draw();
  }

  viewButtons.forEach(button =>
    button.addEventListener('click', () => setView(button.dataset.chartView, true)),
  );

  // Direct pointer scrubbing on the SVG plot for BOTH Attachment & Limit handles
  let dragState = null;
  function valueFromPointer(event) {
    const rect = svg.getBoundingClientRect();
    if (!rect.width) return null;
    const svgX = ((event.clientX - rect.left) / rect.width) * lastPlot.width;
    const span = Math.max(1, lastPlot.right - lastPlot.left);
    const frac = Math.min(1, Math.max(0, (svgX - lastPlot.left) / span));
    return lastPlot.start + frac * (lastPlot.end - lastPlot.start);
  }

  svg.addEventListener('pointerdown', event => {
    if (event.button !== 0 && event.pointerType === 'mouse') return;
    const val = valueFromPointer(event);
    if (val === null) return;
    const hitTarget = event.target.closest?.('.chart-handle-hit');
    const explicitHandle = hitTarget?.dataset.handle;
    const dAttach = Math.abs(val - attachSlider.valueAsNumber);
    const dLimit = Math.abs(val - limitSlider.valueAsNumber);
    const handle = explicitHandle || (dAttach <= dLimit ? 'attach' : 'limit');
    const isTouch = event.pointerType === 'touch';
    const immediate = !isTouch || Boolean(explicitHandle);
    dragState = {
      pointerId: event.pointerId,
      handle,
      startX: event.clientX,
      startY: event.clientY,
      active: immediate,
      moved: false,
    };
    if (immediate) {
      try {
        svg.setPointerCapture(event.pointerId);
      } catch {}
      setHandle(handle, val, 'chart_drag', false);
      dragState.moved = true;
    }
  });

  svg.addEventListener('pointermove', event => {
    if (!dragState || event.pointerId !== dragState.pointerId) return;
    if (!dragState.active) {
      const dx = Math.abs(event.clientX - dragState.startX);
      const dy = Math.abs(event.clientY - dragState.startY);
      if (dx < 6 && dy < 6) return;
      if (dy >= dx) {
        dragState = null;
        return;
      }
      dragState.active = true;
      try {
        svg.setPointerCapture(event.pointerId);
      } catch {}
    }
    const next = valueFromPointer(event);
    if (next !== null) {
      setHandle(dragState.handle, next, 'chart_drag', false);
      dragState.moved = true;
    }
  });

  const endDrag = event => {
    if (dragState && event.pointerId === dragState.pointerId) {
      const shouldCommit = dragState.moved;
      dragState = null;
      if (shouldCommit) flushCommit();
    }
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
    applyPreset(attach, limit, tailIndex, name, control = 'preset') {
      applyTail(tailIndex);
      attachSlider.value = String(attach);
      limitSlider.value = String(limit);
      trackLabEngagement(control);
      track('lab_interact', {
        control,
        value: name,
        tail: TAILS[tail].toLowerCase(),
        limit: compactDollars(limit),
      });
      flushCommit();
    },
    toggleView() {
      setView(view === 'ilf' ? 'pdf' : 'ilf', true);
    },
  };

  // Default $250k xs $100k Moderate state is pre-rendered in HTML; avoid 131k-draw MC work on initial paint.
  syncSliderVisual(attachSlider.valueAsNumber, limitSlider.valueAsNumber);
  let lastWidth = 0;
  new ResizeObserver(entries => {
    const nextWidth = Math.round(entries[0]?.contentRect.width || 0);
    if (nextWidth > 0 && nextWidth !== lastWidth) {
      lastWidth = nextWidth;
      plotWidth = nextWidth;
      draw();
    }
  }).observe(svg);
}

initTheme();
initAnalytics();
initCopyEmail();
initNavInert();
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
