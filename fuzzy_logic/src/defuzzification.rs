pub fn gravity_center(
    membership_functions: &impl Fn(f64) -> f64,
    lower_bound: f64,
    upper_bound: f64,
    step: f64
) -> f64 {
    let mut weighted_sum = 0.0;
    let mut membership_sum = 0.0;
    let mut x = lower_bound;

    while x <= upper_bound {
        let mf_value = membership_functions(x);
        weighted_sum += x * mf_value;
        membership_sum += mf_value;
        x += step;
    }

    if membership_sum == 0.0 {
        return 0.0;
    }

    weighted_sum / membership_sum
}