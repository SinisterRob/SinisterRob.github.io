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
            assert!((expected_layer(tail as u32, 100_000.0, 350_000.0, 0) - reference).abs() < 0.03);
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
}
