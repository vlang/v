module math

// log_gamma is the wrapper around log_gamma_sign that drops the sign, so the first
// return value must be returned unchanged for every input.
fn test_log_gamma_drops_the_sign_of_log_gamma_sign() {
	for x in [f64(0.5), 1.0, 2.0, 5.0, 10.0, 0.25, 3.5, 100.0, 1e-5, 1e-30, -0.5, -1.5, -2.5, -3.5,
		-100.5] {
		lg, _ := log_gamma_sign(x)
		assert log_gamma(x) == lg
	}
}

// For positive arguments where Gamma(x) > 0, log(Gamma(x)) is exactly the log-gamma.
fn test_log_gamma_equals_log_of_gamma() {
	for x in [f64(0.5), 1.0, 2.0, 5.0, 0.25, 3.5, 1e-5, 1e-30] {
		assert tolerance(log_gamma(x), log(gamma(x)), 1e-15)
	}
	assert log_gamma(1.0) == 0.0
	assert log_gamma(2.0) == 0.0
	// gamma(5) = 4! = 120 and gamma(10) = 9! = 362880.
	assert tolerance(log_gamma(5.0), log(24.0), 1e-15)
	assert tolerance(log_gamma(10.0), log(362880.0), 1e-15)
	assert tolerance(log_gamma(0.5), log(sqrt(pi)), 1e-15)
}

// exp(log_gamma(x)) recovers |Gamma(x)|, and the sign is what log_gamma_sign carries.
fn test_exp_of_log_gamma_is_the_absolute_gamma() {
	for x in [f64(0.5), 1.5, 2.5, 5.0, 10.0, 100.0] {
		_, sgn := log_gamma_sign(x)
		recovered := exp(log_gamma(x))
		assert tolerance(recovered, abs(gamma(x)), 1e-13)
		// Gamma(x) > 0 for all of these, so the sign must be +1.
		assert sgn == 1
	}
	// Gamma(-0.5) is negative, so the sign carries the information exp() cannot.
	lg, sgn := log_gamma_sign(-0.5)
	assert sgn == -1
	assert tolerance(exp(lg), abs(gamma(-0.5)), 1e-13)
	assert log_gamma(-0.5) == lg
}

// The negative arguments are where the two functions differ most from log(gamma(x)),
// because gamma itself is negative there.
fn test_log_gamma_on_negative_arguments() {
	assert log_gamma(-2.5) == -0.05624371649767412
	assert log_gamma(-1.5) == 0.8600470153764809
	assert log_gamma(-20.5) == -42.70719597482576
	assert log_gamma(-50.5) == -149.29649894115255
	assert log_gamma(-100.5) == -364.90096830942736
}

fn test_log_gamma_special_cases() {
	// log_gamma(+inf) = +inf
	assert log_gamma(inf(1)) == inf(1)
	// log_gamma(0) = +inf
	assert log_gamma(0.0) == inf(1)
	// log_gamma(-integer) = +inf
	assert log_gamma(-1.0) == inf(1)
	assert log_gamma(-2.0) == inf(1)
	assert log_gamma(-10.0) == inf(1)
	// log_gamma(nan) = nan
	assert is_nan(log_gamma(nan()))
}

// NOTE: the doc comment above log_gamma claims log_gamma(-inf) = -inf, but both the
// wrapper and log_gamma_sign return +inf for -inf: log_gamma_sign hits the
// "x >= exp2(52), must be -integer" branch for negative infinities. The documented
// value and the implementation disagree; the implementation's value is asserted here.
fn test_log_gamma_negative_infinity_contradicts_its_doc_comment() {
	assert log_gamma(inf(-1)) == inf(1)
	assert is_inf(log_gamma(inf(-1)), 1)
	lg, sgn := log_gamma_sign(inf(-1))
	assert lg == inf(1)
	assert sgn == 1
}
