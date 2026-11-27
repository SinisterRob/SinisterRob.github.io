// Interactive Loss Reserving & Mack Variability Simulator + Lab Carousel
// Powered by the zero-dependency reserving_kernels.wasm C-ABI build.

const byId = id => document.getElementById(id);

const DATASETS = {
  raa: {
    name: 'RAA',
    fullName: 'Mack (1993) RAA 10×10 GL reinsurance',
    basisLabel: 'Incurred emergence · linear 6m',
    unitScale: 1000, // published values in $000s
    aPrioriElr: 0.72,
    triangle: [
      [5012, 8269, 10907, 11805, 13539, 16181, 18009, 18608, 18662, 18834],
      [106, 4285, 5396, 10666, 13782, 15599, 15496, 16169, 16704],
      [3410, 8992, 13873, 16141, 18735, 22214, 22863, 23466],
      [5655, 11555, 15766, 21266, 23425, 26083, 27067],
      [1092, 9565, 15836, 22169, 25955, 26180],
      [1513, 6445, 11702, 12935, 15852],
      [557, 4020, 10946, 12314],
      [1351, 6947, 13112],
      [3133, 5395],
      [2063],
    ],
    // Illustrative earned premium scaled to mature accident-year volume ($000s)
    premium: [26000, 27500, 29000, 30500, 32000, 33000, 34000, 35000, 36000, 37000],
  },
  genins: {
    name: 'GenIns',
    fullName: 'Taylor & Ashe / England & Verrall 10×10 GenIns',
    basisLabel: 'Paid runoff · linear 6m',
    unitScale: 1, // published values in dollars
    aPrioriElr: 0.70,
    triangle: [
      [357848, 1124788, 1735330, 2218270, 2745596, 3319994, 3466336, 3606286, 3833515, 3901463],
      [352118, 1236139, 2170033, 3353322, 3799067, 4120063, 4647867, 4914039, 5339085],
      [290507, 1292306, 2218525, 3235179, 3985995, 4132918, 4628910, 4909315],
      [310608, 1418858, 2195047, 3757447, 4029929, 4381982, 4588268],
      [443160, 1136350, 2128333, 2897821, 3402672, 3873311],
      [396132, 1333217, 2180715, 2985752, 3691712],
      [440832, 1288463, 2419861, 3483130],
      [359480, 1421128, 2864498],
      [376686, 1363294],
      [344014],
    ],
    premium: [5800000, 6200000, 6500000, 6800000, 7000000, 7200000, 7400000, 7600000, 7800000, 8000000],
  },
  auto: {
    name: 'US Auto',
    fullName: 'Friedland (2010) US Industry Auto 10×10 Paid',
    basisLabel: 'Paid runoff · linear 6m',
    unitScale: 1000, // published values in $000s
    aPrioriElr: 0.68,
    triangle: [
      [18539254, 33231039, 40062008, 43892039, 45896535, 46765422, 47221322, 47446877, 47555456, 47644187],
      [20410193, 36090684, 43259402, 47159241, 49208532, 50162043, 50625757, 50878808, 51000534],
      [22120843, 38976014, 46389282, 50562385, 52735280, 53740101, 54284334, 54533225],
      [22992259, 40096198, 47767835, 52093916, 54363436, 55378801, 55878421],
      [24092782, 41795313, 49903803, 54352884, 56754376, 57807215],
      [24084451, 41399612, 49070332, 53584201, 55930654],
      [24369770, 41489863, 49236678, 53774672],
      [25100697, 42702229, 50644994],
      [25608776, 43606497],
      [27229969],
    ],
    premium: [68574209, 68544981, 68907977, 72544955, 79228887, 86643542, 91763523, 94115312, 95272279, 95176240],
  },
};

const METHOD_NAMES = {
  cl: 'Chain Ladder',
  bf: 'Bornhuetter–Ferguson',
  cc: 'Cape Cod',
};

const METHOD_SHORT = {
  cl: 'CL',
  bf: 'BF',
  cc: 'Cape Cod',
};

function fmtCompactCurrency(rawValue, unitScale) {
  const value = rawValue * unitScale;
  const abs = Math.abs(value);
  const sign = value < 0 ? '−' : '';
  if (abs >= 1e9) return `${sign}$${(abs / 1e9).toFixed(1)}B`;
  if (abs >= 1e6) return `${sign}$${(abs / 1e6).toFixed(1)}M`;
  if (abs >= 1e3) return `${sign}$${(abs / 1e3).toFixed(0)}k`;
  return `${sign}$${Math.round(abs)}`;
}

function fmtPct(v) {
  return `${(v * 100).toFixed(1)}%`;
}

function fmtFactor(v) {
  return `${v.toFixed(2)}×`;
}

// ---- Lab Carousel Controller (Side Arrows + Native Scroll-Snap + Tabs) ----

function initLabCarousel() {
  const track = byId('lab-track');
  const prevBtn = byId('lab-prev');
  const nextBtn = byId('lab-next');
  const tabs = [...document.querySelectorAll('[data-lab-slide]')];
  if (!track || !prevBtn || !nextBtn || tabs.length < 2) return;

  const slides = [...track.querySelectorAll('.lab-slide')];
  const slideCount = Math.min(tabs.length, slides.length);
  let activeIndex = 0;
  let programmaticUntil = 0;
  let scrollEndTimer = null;

  function reducedMotion() {
    return window.matchMedia('(prefers-reduced-motion: reduce)').matches;
  }

  function syncUi(idx) {
    activeIndex = Math.max(0, Math.min(slideCount - 1, idx));
    prevBtn.disabled = activeIndex === 0;
    nextBtn.disabled = activeIndex === slideCount - 1;
    tabs.forEach((tab, i) => {
      tab.setAttribute('aria-selected', String(i === activeIndex));
    });
  }

  function goToSlide(idx, { smooth = true } = {}) {
    const clamped = Math.max(0, Math.min(slideCount - 1, idx));
    syncUi(clamped);
    const useSmooth = smooth && !reducedMotion();
    if (useSmooth) programmaticUntil = performance.now() + 650;
    const targetLeft = slides[clamped]?.offsetLeft ?? clamped * track.clientWidth;
    track.scrollTo({
      left: targetLeft,
      behavior: useSmooth ? 'smooth' : 'instant',
    });
    // Sync lightweight URL query parameter without polluting history
    try {
      const url = new URL(window.location.href);
      if (clamped === 1) url.searchParams.set('lab', 'reserving');
      else url.searchParams.delete('lab');
      window.history.replaceState(null, '', url.pathname + url.search + url.hash);
    } catch {
      // Ignore restricted history contexts.
    }
  }

  prevBtn.addEventListener('click', () => goToSlide(activeIndex - 1));
  nextBtn.addEventListener('click', () => goToSlide(activeIndex + 1));

  tabs.forEach(tab => {
    tab.addEventListener('click', () => goToSlide(Number(tab.dataset.labSlide)));
    tab.addEventListener('keydown', event => {
      if (event.key === 'ArrowRight') {
        event.preventDefault();
        const next = Math.min(slideCount - 1, activeIndex + 1);
        goToSlide(next);
        tabs[next]?.focus();
      } else if (event.key === 'ArrowLeft') {
        event.preventDefault();
        const prev = Math.max(0, activeIndex - 1);
        goToSlide(prev);
        tabs[prev]?.focus();
      }
    });
  });

  // Sync arrows & tabs when the user natively scrolls or swipes horizontally
  function settleScroll() {
    if (!track.clientWidth) return;
    const idx = Math.round(track.scrollLeft / track.clientWidth);
    if (idx !== activeIndex) syncUi(idx);
  }

  track.addEventListener(
    'scroll',
    () => {
      if (performance.now() < programmaticUntil) return;
      clearTimeout(scrollEndTimer);
      scrollEndTimer = setTimeout(settleScroll, 120);
    },
    { passive: true },
  );
  track.addEventListener('scrollend', () => {
    programmaticUntil = 0;
    settleScroll();
  });

  // Keep slide aligned on viewport resize
  window.addEventListener(
    'resize',
    () => {
      const targetLeft = slides[activeIndex]?.offsetLeft ?? activeIndex * track.clientWidth;
      track.scrollTo({ left: targetLeft, behavior: 'instant' });
    },
    { passive: true },
  );

  // Honor ?lab=reserving deep link on initial load
  try {
    const labParam = new URLSearchParams(window.location.search).get('lab');
    if (labParam === 'reserving') {
      window.requestAnimationFrame(() => goToSlide(1, { smooth: false }));
    } else {
      syncUi(0);
    }
  } catch {
    syncUi(0);
  }
}

// ---- Reserving WASM Loader & Interactive Charts ----

async function loadReservingWasm() {
  const url = new URL('./reserving_kernels.wasm', import.meta.url);
  if (typeof WebAssembly.instantiateStreaming === 'function') {
    try {
      const { instance } = await WebAssembly.instantiateStreaming(fetch(url));
      return instance.exports;
    } catch {
      // Fallback when server lacks application/wasm MIME type.
    }
  }
  const response = await fetch(url);
  if (!response.ok) throw new Error(`Reserving WASM request returned ${response.status}`);
  const { instance } = await WebAssembly.instantiate(await response.arrayBuffer());
  return instance.exports;
}

function computeCashFlows(n, resArr, cdfArr) {
  const pct = cdfArr.map(c => (c > 0 ? Math.min(1, 1 / c) : 1));
  let cy1 = 0;
  let cy2 = 0;
  for (let i = 0; i < n; i++) {
    const k = n - 1 - i;
    const unrep = 1 - pct[k];
    if (unrep <= 1e-12 || !Number.isFinite(resArr[i]) || resArr[i] === 0) continue;
    const p1 = k + 1 < n ? pct[k + 1] : pct[n - 1];
    const p2 = k + 2 < n ? pct[k + 2] : pct[n - 1];
    const w1 = Math.min(1, Math.max(0, p1 - pct[k]) / unrep);
    const w2 = Math.min(1 - w1, Math.max(0, p2 - p1) / unrep);
    cy1 += resArr[i] * w1;
    cy2 += resArr[i] * w2;
  }
  const total = resArr.reduce((a, b) => a + b, 0);
  const m6Inc = 0.5 * cy1;
  const m12Inc = 0.5 * cy1;
  const m18Inc = 0.5 * cy2;
  const m18Cum = cy1 + m18Inc;
  const remInc = total - m18Cum;
  return {
    total,
    rows: [
      { id: '6m', bar: 'res-bar-m6', inc: m6Inc, cum: m6Inc },
      { id: '12m', bar: 'res-bar-m12', inc: m12Inc, cum: cy1 },
      { id: '18m', bar: 'res-bar-m18', inc: m18Inc, cum: m18Cum },
      { id: 'rem', bar: 'res-bar-m18p', inc: remInc, cum: total },
    ],
  };
}

function renderUltimateChart(svg, plotWidth, { reported, reserves, se, unitScale, datasetName }) {
  const width = Math.max(260, Math.round(plotWidth || 540));
  const height = width < 380 ? 176 : 236;
  const top = 22;
  const bottom = height - 26;
  const left = 52;
  const right = width - 14;
  svg.setAttribute('viewBox', `0 0 ${width} ${height}`);

  const n = reported.length;
  const maxVal = Math.max(
    ...reported.map((rep, i) => rep + Math.max(0, reserves[i]) + (Number.isFinite(se[i]) ? se[i] : 0)),
    1,
  );
  // Nice rounded ceiling for clean actuarial axis ticks
  const rawStep = (maxVal * 1.08) / 4;
  const mag = 10 ** Math.floor(Math.log10(Math.max(1e-6, rawStep)));
  const norm = rawStep / mag;
  const niceStep = (norm <= 1 ? 1 : norm <= 2 ? 2 : norm <= 2.5 ? 2.5 : norm <= 5 ? 5 : 10) * mag;
  const yMax = niceStep * 4;
  const y = val => bottom - (val / yMax) * (bottom - top);

  const ticks = [0, 1, 2, 3, 4].map(k => niceStep * k);
  const grid = ticks
    .map(
      t => `
      <line class="chart-grid" x1="${left}" y1="${y(t).toFixed(1)}" x2="${right}" y2="${y(t).toFixed(1)}"/>
      <text class="chart-text" x="${left - 6}" y="${(y(t) + 3.5).toFixed(1)}" text-anchor="end">${t === 0 ? '$0' : fmtCompactCurrency(t, unitScale)}</text>`,
    )
    .join('');

  const plotW = right - left;
  const slot = plotW / n;
  const barW = Math.max(10, Math.min(28, slot * 0.62));

  const bars = reported
    .map((rep, i) => {
      const ibnr = Math.max(0, reserves[i]);
      const ult = rep + ibnr;
      const seVal = Number.isFinite(se[i]) ? se[i] : 0;
      const cx = left + slot * (i + 0.5);
      const bx = cx - barW / 2;
      const yRep = y(rep);
      const yUlt = y(ult);
      const hRep = Math.max(0, bottom - yRep);
      const hIbnr = Math.max(0, yRep - yUlt);

      let whisker = '';
      if (seVal > 0) {
        const yHi = y(Math.min(yMax, ult + seVal));
        const yLo = y(Math.max(rep, ult - seVal));
        whisker = `
          <line class="chart-whisker" x1="${cx.toFixed(1)}" y1="${yHi.toFixed(1)}" x2="${cx.toFixed(1)}" y2="${yLo.toFixed(1)}"/>
          <line class="chart-whisker" x1="${(cx - 4).toFixed(1)}" y1="${yHi.toFixed(1)}" x2="${(cx + 4).toFixed(1)}" y2="${yHi.toFixed(1)}"/>
          <line class="chart-whisker" x1="${(cx - 4).toFixed(1)}" y1="${yLo.toFixed(1)}" x2="${(cx + 4).toFixed(1)}" y2="${yLo.toFixed(1)}"/>`;
      }

      const showLabel = width >= 360 || i % 2 === 0 || i === n - 1;
      const xLabel = showLabel
        ? `<text class="chart-text" x="${cx.toFixed(1)}" y="${bottom + 17}" text-anchor="middle">Y${i + 1}</text>`
        : '';

      return `
        <g class="chart-bar-group">
          <title>AY ${i + 1}: Reported ${fmtCompactCurrency(rep, unitScale)} · IBNR ${fmtCompactCurrency(ibnr, unitScale)} · Ultimate ${fmtCompactCurrency(ult, unitScale)}${seVal > 0 ? ` · Mack SE ±${fmtCompactCurrency(seVal, unitScale)}` : ''}</title>
          <rect class="chart-bar-reported" x="${bx.toFixed(1)}" y="${yRep.toFixed(1)}" width="${barW.toFixed(1)}" height="${hRep.toFixed(1)}" rx="1.5"/>
          ${hIbnr > 0.3 ? `<rect class="chart-bar-ibnr" x="${bx.toFixed(1)}" y="${yUlt.toFixed(1)}" width="${barW.toFixed(1)}" height="${hIbnr.toFixed(1)}" rx="1.5"/>` : ''}
          ${whisker}
        </g>
        ${xLabel}`;
    })
    .join('');

  svg.innerHTML = `
    <title id="res-chart-title">Estimated ultimate loss and Mack standard error by accident year</title>
    <desc id="res-chart-desc">Stacked bars for ${datasetName} showing reported-to-date loss and estimated IBNR with Mack ±1 SE whiskers across ${n} accident years.</desc>
    ${grid}
    <line class="chart-baseline" x1="${left}" y1="${bottom}" x2="${right}" y2="${bottom}"/>
    ${bars}
    <text class="chart-text is-attachment" x="${left}" y="13" text-anchor="start">■ Reported</text>
    <text class="chart-text is-active" x="${left + 72}" y="13" text-anchor="start">■ IBNR</text>
    <text class="chart-text" x="${right}" y="13" text-anchor="end">I Mack ±1 SE</text>`;
}

function renderPatternChart(svg, plotWidth, { cdfs, tail, datasetName }) {
  const width = Math.max(260, Math.round(plotWidth || 540));
  const height = width < 380 ? 176 : 236;
  const top = 22;
  const bottom = height - 26;
  const left = 44;
  const right = width - 18;
  svg.setAttribute('viewBox', `0 0 ${width} ${height}`);

  const n = cdfs.length;
  // Emergence % reported at each development age (1..n), plus Ultimate = 100%
  const pctReported = cdfs.map(c => (c > 0 ? Math.min(1, 1 / c) : 1));
  const x = idx => left + (idx / (n - 1)) * (right - left);
  const y = pct => bottom - pct * (bottom - top);

  const yTicks = [0, 0.25, 0.5, 0.75, 1];
  const grid = yTicks
    .map(
      t => `
      <line class="${t === 1 ? 'chart-pivot-line' : 'chart-grid'}" x1="${left}" y1="${y(t).toFixed(1)}" x2="${right}" y2="${y(t).toFixed(1)}"/>
      <text class="chart-text" x="${left - 6}" y="${(y(t) + 3.5).toFixed(1)}" text-anchor="end">${Math.round(t * 100)}%</text>`,
    )
    .join('');

  const pts = pctReported.map((p, i) => [x(i), y(p)]);
  const linePath = pts.map(([px, py], i) => `${i ? 'L' : 'M'} ${px.toFixed(1)} ${py.toFixed(1)}`).join(' ');
  const areaPath = `M ${left.toFixed(1)} ${bottom.toFixed(1)} ${pts.map(([px, py]) => `L ${px.toFixed(1)} ${py.toFixed(1)}`).join(' ')} L ${right.toFixed(1)} ${bottom.toFixed(1)} Z`;

  const xLabels = [0, 2, 4, 6, 9]
    .filter(i => i < n)
    .map((i, idx, arr) => {
      const anchor = idx === 0 ? 'start' : idx === arr.length - 1 ? 'end' : 'middle';
      return `<text class="chart-text" x="${x(i).toFixed(1)}" y="${bottom + 17}" text-anchor="${anchor}">${(i + 1) * 12}m</text>`;
    })
    .join('');

  const dots = pts
    .map(
      ([px, py], i) =>
        `<circle class="${i === n - 1 ? 'chart-dot' : 'chart-dot-attach'}" cx="${px.toFixed(1)}" cy="${py.toFixed(1)}" r="${i === n - 1 ? 4.4 : 3.2}"><title>${(i + 1) * 12}m: ${fmtPct(pctReported[i])} reported (CDF ${cdfs[i].toFixed(3)}×)</title></circle>`,
    )
    .join('');

  const lastPct = pctReported[n - 1];
  svg.innerHTML = `
    <title id="res-chart-title">Cumulative loss emergence pattern</title>
    <desc id="res-chart-desc">Percent reported by development age for ${datasetName} with ${fmtFactor(tail)} tail factor.</desc>
    ${grid}
    <path d="${areaPath}" class="area-primary"/>
    <line class="chart-baseline" x1="${left}" y1="${bottom}" x2="${right}" y2="${bottom}"/>
    ${xLabels}
    <path d="${linePath}" class="chart-curve is-active"/>
    <circle class="chart-dot-ring" cx="${x(n - 1).toFixed(1)}" cy="${y(lastPct).toFixed(1)}" r="8.5" aria-hidden="true"/>
    ${dots}
    <text class="chart-text is-attachment" x="${left}" y="13" text-anchor="start">12m: ${fmtPct(pctReported[0])} emerged</text>
    <text class="chart-text is-active" x="${right}" y="13" text-anchor="end">120m: ${fmtPct(lastPct)} (Tail ${fmtFactor(tail)})</text>`;
}

function initReservingLab(exports) {
  const shell = byId('reserving-lab');
  const svg = byId('reserving-chart');
  const tailSlider = byId('res-tail-slider');
  const tailRange = byId('res-tail-range');
  const datasetButtons = [...document.querySelectorAll('[data-res-dataset]')];
  const methodButtons = [...document.querySelectorAll('[data-res-method]')];
  const viewButtons = [...document.querySelectorAll('[data-res-view]')];
  const tailJumpButtons = [...document.querySelectorAll('[data-tail-jump]')];
  const horizonRows = [...document.querySelectorAll('[data-res-horizon]')];
  const horizonSegs = [...document.querySelectorAll('[data-res-seg]')];

  if (!shell || !svg || !tailSlider) return;

  const nMax = exports.rk_n_max();
  const stride = exports.rk_tri_stride();

  let datasetKey = 'raa';
  let method = 'cl';
  let chartView = 'ultimate';
  let plotWidth = 540;

  function runKernels() {
    const ds = DATASETS[datasetKey];
    const n = ds.triangle.length;
    const tail = tailSlider.valueAsNumber;

    // Write triangle & premium into WASM static buffers
    const triBuf = new Float64Array(exports.memory.buffer, exports.rk_tri_ptr(), nMax * nMax);
    triBuf.fill(NaN);
    const reported = [];
    for (let i = 0; i < n; i++) {
      const row = ds.triangle[i];
      for (let j = 0; j < row.length; j++) {
        triBuf[i * stride + j] = row[j];
      }
      reported.push(row[row.length - 1]);
    }
    const premBuf = new Float64Array(exports.memory.buffer, exports.rk_prem_ptr(), nMax);
    premBuf.fill(0);
    for (let i = 0; i < n; i++) premBuf[i] = ds.premium[i];

    // Always run Mack first to obtain volume-weighted LDFs, CDFs, and distribution-free SEs
    const totalSe = exports.rk_mack(n, tail);
    const seArr = [...new Float64Array(exports.memory.buffer, exports.rk_se_ptr(), n)];
    const cdfArr = [...new Float64Array(exports.memory.buffer, exports.rk_cdf_ptr(), n)];

    // Compute Cape Cod implied ELR for the footer check (and for CC method)
    const ccElr = exports.rk_cape_cod_elr(n, tail);

    // Run selected reserving method
    if (method === 'bf') {
      exports.rk_bf_expected_from_premium(n, ds.aPrioriElr);
      exports.rk_bf(n, tail);
    } else if (method === 'cc') {
      exports.rk_cape_cod(n, tail);
    } else {
      exports.rk_chain_ladder(n, tail);
    }

    const ultArr = [...new Float64Array(exports.memory.buffer, exports.rk_ult_ptr(), n)];
    const resArr = [...new Float64Array(exports.memory.buffer, exports.rk_res_ptr(), n)];

    return { ds, n, tail, reported, ultArr, resArr, seArr, cdfArr, totalSe, ccElr };
  }

  function update() {
    const { ds, n, tail, reported, ultArr, resArr, seArr, cdfArr, totalSe, ccElr } = runKernels();

    const pctSlider = ((tail - 1.0) / 0.2) * 100;
    if (tailRange) tailRange.style.setProperty('--tail-pct', `${pctSlider.toFixed(1)}%`);

    const totalRes = resArr.reduce((a, b) => a + b, 0);
    const totalUlt = ultArr.reduce((a, b) => a + b, 0);
    const cv = totalRes > 0 ? totalSe / totalRes : 0;
    const unrepShare = totalUlt > 0 ? totalRes / totalUlt : 0;

    byId('res-tail-value').textContent = fmtFactor(tail);
    tailSlider.setAttribute('aria-valuetext', `${fmtFactor(tail)} tail factor`);
    byId('res-cdf-readout').textContent = fmtFactor(cdfArr[0]);
    byId('res-method-sub').textContent =
      method === 'bf' ? `BF (${Math.round(ds.aPrioriElr * 100)}% ELR)` : METHOD_NAMES[method];

    byId('res-ibnr').textContent = fmtCompactCurrency(totalRes, ds.unitScale);
    byId('res-ibnr-sub').textContent = `${METHOD_SHORT[method]} · Tail ${fmtFactor(tail)}`;
    byId('res-ultimate').textContent = fmtCompactCurrency(totalUlt, ds.unitScale);
    byId('res-se').textContent = fmtCompactCurrency(totalSe, ds.unitScale);
    byId('res-se-sub').textContent = `CV ${fmtPct(cv)}`;
    byId('res-unreported').textContent = fmtPct(unrepShare);

    // Projected 6 / 12 / 18m cash flow runoff
    const cf = computeCashFlows(n, resArr, cdfArr);
    const basisEl = byId('res-cf-basis');
    if (basisEl) basisEl.textContent = ds.basisLabel;
    const absCfTotal = Math.max(
      1e-9,
      cf.rows.reduce((acc, row) => acc + Math.max(0, row.inc), 0),
    );
    cf.rows.forEach(row => {
      const cumPct = cf.total > 0 ? row.cum / cf.total : 0;
      byId(`res-cf-${row.id}-inc`).textContent = fmtCompactCurrency(row.inc, ds.unitScale);
      byId(`res-cf-${row.id}-cum`).textContent = fmtCompactCurrency(row.cum, ds.unitScale);
      byId(`res-cf-${row.id}-pct`).textContent = cf.total > 0 ? fmtPct(cumPct) : '—';
      const barEl = byId(row.bar);
      if (barEl) barEl.style.width = `${((Math.max(0, row.inc) / absCfTotal) * 100).toFixed(1)}%`;
    });

    byId('res-cc-elr').textContent = Number.isFinite(ccElr) ? fmtPct(ccElr) : '—';

    const labelEl = byId('res-chart-label');
    const hintEl = byId('res-chart-hint');
    if (chartView === 'ultimate') {
      if (labelEl) labelEl.textContent = `Ultimate by AY · ${ds.name}`;
      if (hintEl) hintEl.textContent = 'Mack ±1 SE whiskers';
      renderUltimateChart(svg, plotWidth, {
        reported,
        reserves: resArr,
        se: seArr,
        unitScale: ds.unitScale,
        datasetName: ds.name,
      });
    } else {
      if (labelEl) labelEl.textContent = `Emergence Pattern · ${ds.name}`;
      if (hintEl) hintEl.textContent = `CDF 12m→Ult ${fmtFactor(cdfArr[0])}`;
      renderPatternChart(svg, plotWidth, {
        cdfs: cdfArr,
        tail,
        datasetName: ds.name,
      });
    }

    const summaryEl = byId('res-results-summary');
    if (summaryEl) {
      summaryEl.textContent = `${ds.fullName}, ${METHOD_NAMES[method]}, tail ${fmtFactor(tail)}. Total IBNR ${fmtCompactCurrency(totalRes, ds.unitScale)}, ultimate ${fmtCompactCurrency(totalUlt, ds.unitScale)}, Mack SE ${fmtCompactCurrency(totalSe, ds.unitScale)}.`;
    }
  }

  datasetButtons.forEach(btn => {
    btn.addEventListener('click', () => {
      datasetKey = btn.dataset.resDataset;
      datasetButtons.forEach(b => b.setAttribute('aria-pressed', String(b === btn)));
      update();
    });
  });

  methodButtons.forEach(btn => {
    btn.addEventListener('click', () => {
      method = btn.dataset.resMethod;
      methodButtons.forEach(b => b.setAttribute('aria-pressed', String(b === btn)));
      update();
    });
  });

  viewButtons.forEach(btn => {
    btn.addEventListener('click', () => {
      chartView = btn.dataset.resView;
      viewButtons.forEach(b => b.setAttribute('aria-pressed', String(b === btn)));
      update();
    });
  });

  tailSlider.addEventListener('input', update);

  tailJumpButtons.forEach(btn => {
    btn.addEventListener('click', () => {
      tailSlider.value = btn.dataset.tailJump;
      update();
    });
  });

  [...horizonRows, ...horizonSegs].forEach(el => {
    const horizon = el.dataset.resHorizon || el.dataset.resSeg;
    el.addEventListener('mouseenter', () => {
      shell.dataset.hoverHorizon = horizon;
    });
    el.addEventListener('mouseleave', () => {
      delete shell.dataset.hoverHorizon;
    });
  });

  plotWidth = Math.round(svg.getBoundingClientRect().width) || 540;
  update();

  new ResizeObserver(entries => {
    const nextWidth = Math.round(entries[0]?.contentRect.width || 0);
    if (nextWidth > 0 && nextWidth !== plotWidth) {
      plotWidth = nextWidth;
      update();
    }
  }).observe(svg);

  shell.querySelectorAll('button:disabled, input:disabled').forEach(control => {
    control.disabled = false;
  });
  shell.dataset.ready = '';
}

initLabCarousel();

try {
  initReservingLab(await loadReservingWasm());
} catch (error) {
  console.error('Reserving WASM kernel failed to load', error);
  const status = byId('reserving-status');
  if (status) {
    status.textContent = 'The reserving simulator could not load. Showing Mack (1993) RAA baseline values.';
    status.hidden = false;
  }
}
