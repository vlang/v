module math

// q_rsqrt is the Quake fast inverse square root, an approximation with a bounded
// relative error, so the check is |q_rsqrt(x) - 1/sqrt(x)| / (1/sqrt(x)) being small.
fn test_q_rsqrt_approximates_the_inverse_square_root() {
	for x in [f64(1.0), 2.0, 4.0, 0.25, 100.0, 0.5, 3.0, 1024.0, 1e10, 1e-10] {
		got := q_rsqrt(x)
		want := 1.0 / sqrt(x)
		relative_error := abs(got - want) / want
		assert relative_error < 1e-4
	}
	// Measured at roughly 4.3e-06 for these inputs; the assertion above is the contract.
	assert tolerance(q_rsqrt(4.0), 0.5, 1e-5)
	assert tolerance(q_rsqrt(0.25), 2.0, 1e-4)
	assert tolerance(q_rsqrt(1.0), 1.0, 1e-4)
}

// Newton refinement keeps the result on the same side of the true value and does not
// diverge, so the approximation must be a small underestimate, never an overestimate.
fn test_q_rsqrt_stays_close_over_a_wide_range() {
	mut x := 1.0
	for _ in 0 .. 200 {
		got := q_rsqrt(x)
		want := 1.0 / sqrt(x)
		assert got <= want
		assert want - got < want * 1e-4
		x *= 1.05
		if x > 1e12 {
			x = 1.0
		}
	}
	assert tolerance(q_rsqrt(1e100), 1.0e-50, 1e-6)
	assert tolerance(q_rsqrt(1e-100), 1.0e50, 1e-4)
}

// 1/sqrt(x) is strictly decreasing, so the approximation must be too.
fn test_q_rsqrt_is_monotonically_decreasing() {
	mut x := 1.0
	mut prev := q_rsqrt(x)
	for _ in 0 .. 200 {
		x *= 1.05
		got := q_rsqrt(x)
		assert got < prev
		prev = got
	}
}

// The function reads the bits of its argument, so zero does not produce a NaN and the
// result is a large finite number rather than a division by zero.
// NOTE: q_rsqrt(0.0) returns 2.1606767556858245e+154, not inf or nan. That is what the
// bit trick produces for a zero input; it is current behaviour, not a documented case.
fn test_q_rsqrt_zero_is_not_a_documented_case() {
	got := q_rsqrt(0.0)
	assert !is_nan(got)
	assert is_finite(got)
	assert got > 0.0
}
