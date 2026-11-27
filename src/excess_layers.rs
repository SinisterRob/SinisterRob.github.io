use std::f64::consts::TAU;

const GROUND_UP_MEAN: f64 = 100_000.0;
const SAMPLE_SIZE: usize = 65_536;
const SEED: u64 = 2026;
const INV_SQRT_TAU: f64 = 0.3989422804014327;

static mut STANDARD_NORMALS: [f64; SAMPLE_SIZE] = [0.0; SAMPLE_SIZE];

fn populate_normals(buf: &mut [f64; SAMPLE_SIZE]) {
    let mut state = SEED;
    for slot in buf.iter_mut() {
        let radius = (-2.0 * uniform(&mut state).ln()).sqrt();
        let angle = TAU * uniform(&mut state);
        *slot = radius * angle.cos();
    }
}

#[cfg(target_arch = "wasm32")]
fn standard_normals() -> &'static [f64; SAMPLE_SIZE] {
    static mut READY: bool = false;
    // SAFETY: wasm32-unknown-unknown is single-threaded; initialized once before shared reads.
    unsafe {
        if !READY {
            populate_normals(&mut *std::ptr::addr_of_mut!(STANDARD_NORMALS));
            READY = true;
        }
        &*std::ptr::addr_of!(STANDARD_NORMALS)
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn standard_normals() -> &'static [f64; SAMPLE_SIZE] {
    use std::sync::Once;
    static INIT: Once = Once::new();
    INIT.call_once(|| unsafe {
        populate_normals(&mut *std::ptr::addr_of_mut!(STANDARD_NORMALS));
    });
    // SAFETY: STANDARD_NORMALS is initialized exactly once and never mutated afterward.
    unsafe { &*std::ptr::addr_of!(STANDARD_NORMALS) }
}

#[no_mangle]
pub extern "C" fn limited_expected_value(tail: u32, limit: f64) -> f64 {
    limited_mean(limit, log_sd(tail))
}

#[no_mangle]
pub extern "C" fn expected_layer(tail: u32, attach: f64, limit: f64, layer: u32) -> f64 {
    let sigma = log_sd(tail);
    let lower = limited_mean(attach, sigma);
    if layer == 0 {
        lower
    } else {
        (limited_mean(limit, sigma) - lower).max(0.0)
    }
}

#[no_mangle]
pub extern "C" fn simulated_layer(tail: u32, attach: f64, limit: f64, layer: u32) -> f64 {
    let sigma = log_sd(tail);
    let log_mean = GROUND_UP_MEAN.ln() - sigma * sigma / 2.0;
    let mut total = 0.0;
    let upper_cap = limit.max(attach);
    for &z in standard_normals().iter() {
        let severity = (log_mean + sigma * z).exp();
        let lower = severity.min(attach);
        // Reuse the same fixed-seed severity draws across layers, attachments, tails, and limits.
        total += if layer == 0 {
            lower
        } else {
            severity.min(upper_cap) - lower
        };
    }
    total / (SAMPLE_SIZE as f64)
}

#[no_mangle]
pub extern "C" fn exceedance_prob(tail: u32, threshold: f64) -> f64 {
    if threshold <= 0.0 {
        return 1.0;
    }
    let sigma = log_sd(tail);
    let log_mean = GROUND_UP_MEAN.ln() - sigma * sigma / 2.0;
    let z = (threshold.ln() - log_mean) / sigma;
    normal_cdf(-z)
}

#[no_mangle]
pub extern "C" fn severity_pdf(tail: u32, x: f64) -> f64 {
    if x <= 0.0 {
        return 0.0;
    }
    let sigma = log_sd(tail);
    let log_mean = GROUND_UP_MEAN.ln() - sigma * sigma / 2.0;
    let z = (x.ln() - log_mean) / sigma;
    (INV_SQRT_TAU / (x * sigma)) * (-0.5 * z * z).exp()
}

// ===========================================================================
// Composite Spliced Gamma Body + Pareto Tail (Exact $100k Ground-Up Mean)
// ===========================================================================

fn gamma_body_shape(body: u32) -> f64 {
    match body {
        0 => 1.2,
        2 => 6.0,
        _ => 2.5,
    }
}

fn pareto_tail_alpha(tail: u32) -> f64 {
    match tail {
        0 => 4.0,
        2 => 1.7,
        _ => 2.5,
    }
}

fn ln_gamma(z: f64) -> f64 {
    const COEFFS: [f64; 9] = [
        0.999_999_999_999_809_9,
        676.520_368_121_885_1,
        -1_259.139_216_722_402_8,
        771.323_428_777_653_1,
        -176.615_029_162_140_6,
        12.507_343_278_686_905,
        -0.138_571_095_265_720_12,
        9.984_369_578_019_572e-6,
        1.505_632_735_149_311_6e-7,
    ];
    let z = z - 1.0;
    let mut x = COEFFS[0];
    for (i, &c) in COEFFS.iter().enumerate().skip(1) {
        x += c / (z + i as f64);
    }
    let t = z + 7.5;
    0.5 * TAU.ln() + (z + 0.5) * t.ln() - t + x.ln()
}

fn gamma_p(a: f64, x: f64) -> f64 {
    if !(x > 0.0) {
        return 0.0;
    }
    if x < a + 1.0 {
        let mut ap = a;
        let mut term = 1.0 / a;
        let mut sum = term;
        for _ in 0..200 {
            ap += 1.0;
            term *= x / ap;
            sum += term;
            if term.abs() <= sum.abs() * 1e-15 {
                break;
            }
        }
        (sum * (-x + a * x.ln() - ln_gamma(a)).exp()).clamp(0.0, 1.0)
    } else {
        (1.0 - gamma_q_cf(a, x)).clamp(0.0, 1.0)
    }
}

fn gamma_q_cf(a: f64, x: f64) -> f64 {
    const TINY: f64 = 1e-300;
    let mut b = x + 1.0 - a;
    let mut c = 1.0 / TINY;
    let mut d = 1.0 / b;
    let mut h = d;
    for i in 1..=200 {
        let fi = f64::from(i);
        let an = -fi * (fi - a);
        b += 2.0;
        d = an * d + b;
        if d.abs() < TINY {
            d = TINY;
        }
        c = b + an / c;
        if c.abs() < TINY {
            c = TINY;
        }
        d = 1.0 / d;
        let del = d * c;
        h *= del;
        if (del - 1.0).abs() <= 1e-15 {
            break;
        }
    }
    ((-x + a * x.ln() - ln_gamma(a)).exp() * h).clamp(0.0, 1.0)
}

fn gamma_pdf_unit(shape: f64, z: f64) -> f64 {
    if !(z > 0.0) {
        return 0.0;
    }
    ((shape - 1.0) * z.ln() - z - ln_gamma(shape)).exp()
}

struct SplicedParams {
    shape: f64,
    scale: f64,
    splice: f64,
    tail_alpha: f64,
    w: f64,
    f_u: f64,
}

fn spliced_params(body: u32, tail: u32) -> SplicedParams {
    let shape = gamma_body_shape(body);
    let tail_alpha = pareto_tail_alpha(tail);
    let z0 = 1.4 * shape;
    let f_u = gamma_p(shape, z0);
    let pdf_u = gamma_pdf_unit(shape, z0);
    let w = (tail_alpha * f_u / (tail_alpha * f_u + z0 * pdf_u)).clamp(0.0, 1.0);
    let body_unit = (w / f_u) * shape * gamma_p(shape + 1.0, z0);
    let tail_unit = (1.0 - w) * tail_alpha * z0 / (tail_alpha - 1.0);
    let scale = GROUND_UP_MEAN / (body_unit + tail_unit);
    let splice = z0 * scale;
    SplicedParams {
        shape,
        scale,
        splice,
        tail_alpha,
        w,
        f_u,
    }
}

fn spliced_lev_inner(p: &SplicedParams, limit: f64) -> f64 {
    if limit <= 0.0 {
        return 0.0;
    }
    if limit <= p.splice {
        let z = limit / p.scale;
        (p.w / p.f_u) * p.shape * p.scale * gamma_p(p.shape + 1.0, z)
            + limit * (1.0 - p.w * gamma_p(p.shape, z) / p.f_u)
    } else {
        let z0 = p.splice / p.scale;
        let lev_u =
            (p.w / p.f_u) * p.shape * p.scale * gamma_p(p.shape + 1.0, z0) + (1.0 - p.w) * p.splice;
        let tail_excess = (1.0 - p.w)
            * (p.splice / (p.tail_alpha - 1.0))
            * (1.0 - (p.splice / limit).powf(p.tail_alpha - 1.0));
        lev_u + tail_excess
    }
}

fn sample_gamma_ge1(shape: f64, state: &mut u64) -> f64 {
    let d = shape - (1.0 / 3.0);
    let c = (1.0 / 3.0) / d.sqrt();
    loop {
        let radius = (-2.0 * uniform(state).ln()).sqrt();
        let angle = TAU * uniform(state);
        let z = radius * angle.cos();
        let v = 1.0 + c * z;
        if v <= 0.0 {
            continue;
        }
        let v3 = v * v * v;
        let u = uniform(state);
        if u < 1.0 - 0.0331 * z * z * z * z {
            return d * v3;
        }
        if u.ln() < 0.5 * z * z + d * (1.0 - v3 + v3.ln()) {
            return d * v3;
        }
    }
}

#[no_mangle]
pub extern "C" fn spliced_threshold(body: u32, tail: u32) -> f64 {
    spliced_params(body, tail).splice
}

#[no_mangle]
pub extern "C" fn spliced_limited_expected_value(body: u32, tail: u32, limit: f64) -> f64 {
    spliced_lev_inner(&spliced_params(body, tail), limit)
}

#[no_mangle]
pub extern "C" fn spliced_expected_layer(
    body: u32,
    tail: u32,
    attach: f64,
    limit: f64,
    layer: u32,
) -> f64 {
    let p = spliced_params(body, tail);
    let lower = spliced_lev_inner(&p, attach);
    if layer == 0 {
        lower
    } else {
        (spliced_lev_inner(&p, limit) - lower).max(0.0)
    }
}

#[no_mangle]
pub extern "C" fn spliced_simulated_layer(
    body: u32,
    tail: u32,
    attach: f64,
    limit: f64,
    layer: u32,
) -> f64 {
    let p = spliced_params(body, tail);
    let z0 = p.splice / p.scale;
    let upper_cap = limit.max(attach);
    let mut state = SEED;
    let mut total = 0.0;
    for _ in 0..SAMPLE_SIZE {
        let u = uniform(&mut state);
        let severity = if u < p.w {
            loop {
                let g = sample_gamma_ge1(p.shape, &mut state);
                if g <= z0 {
                    break g * p.scale;
                }
            }
        } else {
            let tail_u = ((1.0 - u) / (1.0 - p.w)).max(1e-15);
            p.splice * tail_u.powf(-1.0 / p.tail_alpha)
        };
        let lower = severity.min(attach);
        total += if layer == 0 {
            lower
        } else {
            severity.min(upper_cap) - lower
        };
    }
    total / (SAMPLE_SIZE as f64)
}

#[no_mangle]
pub extern "C" fn spliced_exceedance_prob(body: u32, tail: u32, threshold: f64) -> f64 {
    if threshold <= 0.0 {
        return 1.0;
    }
    let p = spliced_params(body, tail);
    if threshold <= p.splice {
        (1.0 - p.w * gamma_p(p.shape, threshold / p.scale) / p.f_u).clamp(0.0, 1.0)
    } else {
        ((1.0 - p.w) * (p.splice / threshold).powf(p.tail_alpha)).clamp(0.0, 1.0)
    }
}

#[no_mangle]
pub extern "C" fn spliced_severity_pdf(body: u32, tail: u32, x: f64) -> f64 {
    if x <= 0.0 {
        return 0.0;
    }
    let p = spliced_params(body, tail);
    if x <= p.splice {
        p.w * gamma_pdf_unit(p.shape, x / p.scale) / (p.scale * p.f_u)
    } else {
        (1.0 - p.w) * p.tail_alpha * p.splice.powf(p.tail_alpha) / x.powf(p.tail_alpha + 1.0)
    }
}

fn log_sd(tail: u32) -> f64 {
    match tail {
        0 => 0.55,
        2 => 1.55,
        _ => 1.0,
    }
}

fn limited_mean(limit: f64, sigma: f64) -> f64 {
    if limit <= 0.0 {
        return 0.0;
    }
    let log_mean = GROUND_UP_MEAN.ln() - sigma * sigma / 2.0;
    let z = (limit.ln() - log_mean) / sigma;
    GROUND_UP_MEAN * normal_cdf(z - sigma) + limit * normal_cdf(-z)
}

fn normal_cdf(z: f64) -> f64 {
    // Abramowitz and Stegun 26.2.17, absolute CDF error below 7.5e-8.
    let x = z.abs();
    let t = 1.0 / (1.0 + 0.2316419 * x);
    let polynomial = t
        * (0.319381530
            + t * (-0.356563782 + t * (1.781477937 + t * (-1.821255978 + t * 1.330274429))));
    let upper = INV_SQRT_TAU * (-0.5 * x * x).exp() * polynomial;
    if z < 0.0 {
        upper
    } else {
        1.0 - upper
    }
}

fn uniform(state: &mut u64) -> f64 {
    // SplitMix64 with 52 random bits, mapped to the open interval (0, 1).
    *state = state.wrapping_add(0x9e3779b97f4a7c15);
    let mut bits = *state;
    bits = (bits ^ (bits >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
    bits = (bits ^ (bits >> 27)).wrapping_mul(0x94d049bb133111eb);
    bits ^= bits >> 31;
    ((bits >> 12) as f64 + 0.5) / 4_503_599_627_370_496.0
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn primary_layer_matches_reference_values() {
        for (tail, reference) in [78_331.62, 61_707.51, 43_833.97].into_iter().enumerate() {
            assert!(
                (expected_layer(tail as u32, 100_000.0, 350_000.0, 0) - reference).abs() < 0.03
            );
            assert!((limited_expected_value(tail as u32, 100_000.0) - reference).abs() < 0.03);
        }
    }

    #[test]
    fn variable_attachment_and_limit_change_relativities() {
        for attach in [50_000.0, 100_000.0, 250_000.0] {
            let mut previous_ilf = 1.0;
            for tail in 0..=2 {
                let lower = expected_layer(tail, attach, 350_000.0, 0);
                assert_eq!(expected_layer(tail, attach, attach, 1), 0.0);
                let mut previous_upper = 0.0;
                let start = (attach as u32) + 25_000;
                for top in (start..=1_000_000).step_by(25_000) {
                    let upper = expected_layer(tail, attach, f64::from(top), 1);
                    assert!(upper > previous_upper);
                    assert!(lower + upper <= GROUND_UP_MEAN + 1e-6);
                    previous_upper = upper;
                }
                let ilf = 1.0 + expected_layer(tail, attach, 500_000.0, 1) / lower;
                assert!(ilf > previous_ilf);
                previous_ilf = ilf;
            }
        }
    }

    #[test]
    fn fixed_seed_simulation_agrees_with_limited_expectations() {
        for tail in 0..=2 {
            for attach in [50_000, 100_000, 250_000] {
                for top in (attach + 50_000..=1_000_000).step_by(125_000) {
                    for layer in 0..=1 {
                        let exact = expected_layer(tail, f64::from(attach), f64::from(top), layer);
                        let simulated =
                            simulated_layer(tail, f64::from(attach), f64::from(top), layer);
                        assert!(
                            (exact - simulated).abs() < 1_000.0,
                            "tail {tail}, attach {attach}, top {top}, layer {layer}: {exact} vs. {simulated}"
                        );
                    }
                }
            }
        }
        assert_eq!(
            simulated_layer(1, 100_000.0, 350_000.0, 1),
            simulated_layer(1, 100_000.0, 350_000.0, 1)
        );
    }

    #[test]
    fn exceedance_and_pdf_are_consistent() {
        for tail in 0..=2 {
            let p_att = exceedance_prob(tail, 100_000.0);
            let p_top = exceedance_prob(tail, 350_000.0);
            assert!(p_att > 0.0 && p_att < 1.0);
            assert!(p_top > 0.0 && p_top < p_att);
            assert_eq!(exceedance_prob(tail, 0.0), 1.0);
            assert_eq!(severity_pdf(tail, 0.0), 0.0);
            assert!(severity_pdf(tail, 50_000.0) > 0.0);
            let dx = 1_000.0;
            let mut area = 0.0;
            for i in 0..2_000 {
                let x = (i as f64 + 0.5) * dx;
                area += severity_pdf(tail, x) * dx;
            }
            assert!(area > 0.90 && area <= 1.001, "tail {tail} area={area}");
        }
    }

    #[test]
    fn spliced_gamma_pareto_matches_mean_continuity_and_integral() {
        for body in 0..=2 {
            for tail in 0..=2 {
                let u = spliced_threshold(body, tail);
                assert!(u > 0.0 && u < 500_000.0);
                // Continuous PDF across splice threshold u.
                let left = spliced_severity_pdf(body, tail, u * (1.0 - 1e-6));
                let right = spliced_severity_pdf(body, tail, u * (1.0 + 1e-6));
                assert!(
                    (left - right).abs() / left < 1e-4,
                    "body={body} tail={tail} discontinuity: {left} vs {right}"
                );
                // Lee survival integral matches closed-form LEV.
                let limit = 350_000.0;
                let exact = spliced_limited_expected_value(body, tail, limit);
                let steps = 4_000;
                let dx = limit / (steps as f64);
                let mut integral = 0.0;
                for i in 0..steps {
                    let x = (i as f64 + 0.5) * dx;
                    integral += spliced_exceedance_prob(body, tail, x) * dx;
                }
                assert!(
                    (exact - integral).abs() < 50.0,
                    "body={body} tail={tail} lev={exact} vs integral={integral}"
                );
            }
        }
        // Fixed-seed MC check on Moderate body + Moderate tail.
        let exact = spliced_expected_layer(1, 1, 100_000.0, 350_000.0, 1);
        let sim = spliced_simulated_layer(1, 1, 100_000.0, 350_000.0, 1);
        assert!(
            (exact - sim).abs() < 1_000.0,
            "spliced exact={exact} vs sim={sim}"
        );
    }
}
