module easing

import math

const eps = 1e-9

fn approx(a f64, b f64) bool {
	return math.abs(a - b) < eps
}

fn test_linear_is_the_identity() {
	assert linear(0.0) == 0.0
	assert linear(0.25) == 0.25
	assert linear(1.0) == 1.0
}

fn test_sine_endpoints_are_pinned() {
	assert in_sine(0.0) == 0.0
	assert approx(in_sine(1.0), 1.0)
	assert approx(out_sine(0.0), 0.0)
	assert approx(out_sine(1.0), 1.0)
	assert in_out_sine(0.0) == 0.0
	assert approx(in_out_sine(1.0), 1.0)
	assert approx(in_out_sine(0.5), 0.5)
}

fn test_quad_follows_the_closed_form() {
	assert in_quad(0.25) == 0.0625
	assert in_quad(0.5) == 0.25
	assert in_quad(0.0) == 0.0
	assert in_quad(1.0) == 1.0
	assert out_quad(0.5) == 0.75
	assert out_quad(0.0) == 0.0
	assert out_quad(1.0) == 1.0
	assert in_out_quad(0.25) == 0.125
	assert in_out_quad(0.75) == 0.875
	assert in_out_quad(0.5) == 0.5
}

fn test_cubic_follows_the_closed_form() {
	assert in_cubic(0.5) == 0.125
	assert in_cubic(0.0) == 0.0
	assert in_cubic(1.0) == 1.0
	assert out_cubic(0.5) == 0.875
	assert out_cubic(0.0) == 0.0
	assert out_cubic(1.0) == 1.0
	assert in_out_cubic(0.25) == 0.0625
	assert in_out_cubic(0.75) == 0.9375
	assert in_out_cubic(0.5) == 0.5
}

fn test_quart_follows_the_closed_form() {
	assert in_quart(0.5) == 0.0625
	assert in_quart(0.0) == 0.0
	assert in_quart(1.0) == 1.0
	assert out_quart(0.5) == 0.9375
	assert out_quart(0.0) == 0.0
	assert out_quart(1.0) == 1.0
	assert in_out_quart(0.25) == 0.03125
	assert in_out_quart(0.75) == 0.96875
	assert in_out_quart(0.5) == 0.5
}

fn test_quint_follows_the_closed_form() {
	assert in_quint(0.5) == 0.03125
	assert in_quint(0.0) == 0.0
	assert in_quint(1.0) == 1.0
	assert out_quint(0.5) == 0.96875
	assert out_quint(0.0) == 0.0
	assert out_quint(1.0) == 1.0
	assert in_out_quint(0.25) == 0.015625
	assert in_out_quint(0.75) == 0.984375
	assert in_out_quint(0.5) == 0.5
}

fn test_expo_endpoints_and_powers() {
	assert in_expo(0.0) == 0.0
	assert in_expo(1.0) == 1.0
	assert in_expo(0.1) == 0.001953125
	assert out_expo(0.0) == 0.0
	assert out_expo(1.0) == 1.0
	assert out_expo(0.9) == 0.998046875
	assert in_out_expo(0.0) == 0.0
	assert in_out_expo(1.0) == 1.0
	assert in_out_expo(0.5) == 0.5
}

fn test_circ_stays_inside_the_unit_interval() {
	assert in_circ(0.0) == 0.0
	assert in_circ(1.0) == 1.0
	assert out_circ(0.0) == 0.0
	assert out_circ(1.0) == 1.0
	assert in_out_circ(0.0) == 0.0
	assert approx(in_out_circ(1.0), 1.0)
	assert approx(in_out_circ(0.5), 0.5)
	// the circle arc must never leave [0, 1]
	mut x := 0.0
	for x <= 1.0 {
		assert in_circ(x) >= 0.0 && in_circ(x) <= 1.0
		assert out_circ(x) >= 0.0 && out_circ(x) <= 1.0
		assert in_out_circ(x) >= 0.0 && in_out_circ(x) <= 1.0
		x += 0.01
	}
}

fn test_back_overshoots_below_and_above_the_unit_interval() {
	assert in_back(0.0) == 0.0
	assert approx(in_back(1.0), 1.0)
	assert in_back(0.25) < 0.0
	assert out_back(1.0) == 1.0
	// anticipation: the anticipation dip makes out_back exceed 1 in the middle
	assert out_back(0.5) > 1.0
	assert in_out_back(0.0) == 0.0
	assert in_out_back(1.0) == 1.0
	assert in_out_back(0.5) == 0.5
	assert in_out_back(0.25) < 0.0
	assert in_out_back(0.75) > 1.0
}

fn test_elastic_oscillates_around_the_unit_interval() {
	assert in_elastic(0.0) == 0.0
	assert in_elastic(1.0) == 1.0
	assert out_elastic(0.0) == 0.0
	assert out_elastic(1.0) == 1.0
	assert in_out_elastic(0.0) == 0.0
	assert in_out_elastic(1.0) == 1.0
	assert in_out_elastic(0.5) == 0.5
	// the elastic families are not monotonic: they overshoot above 1
	assert out_elastic(0.1) > 1.0
	mut x := 0.0
	mut peak := 0.0
	for x <= 1.0 {
		v := out_elastic(x)
		if v > peak {
			peak = v
		}
		x += 0.05
	}
	assert peak > 1.0
}

fn test_bounce_keeps_the_endpoints() {
	assert in_bounce(0.0) == 0.0
	assert in_bounce(1.0) == 1.0
	assert out_bounce(0.0) == 0.0
	assert out_bounce(1.0) == 1.0
	assert in_out_bounce(0.0) == 0.0
	assert in_out_bounce(1.0) == 1.0
	assert in_out_bounce(0.5) == 0.5
	assert out_bounce(0.9) < 1.0
}

fn test_in_out_families_are_symmetric_about_the_midpoint() {
	mut x := 0.0
	for x <= 1.0 {
		assert approx(in_out_quad(x) + in_out_quad(1 - x), 1.0)
		assert approx(in_out_cubic(x) + in_out_cubic(1 - x), 1.0)
		assert approx(in_out_quart(x) + in_out_quart(1 - x), 1.0)
		assert approx(in_out_quint(x) + in_out_quint(1 - x), 1.0)
		assert approx(in_out_sine(x) + in_out_sine(1 - x), 1.0)
		x += 0.05
	}
}

fn test_in_out_bounce_is_symmetric() {
	mut x := 0.0
	for x <= 1.0 {
		assert approx(in_out_bounce(x) + in_out_bounce(1 - x), 1.0)
		x += 0.05
	}
}

fn test_in_and_out_are_duals() {
	mut x := 0.0
	for x <= 1.0 {
		assert approx(in_quad(x) + out_quad(1 - x), 1.0)
		assert approx(in_cubic(x) + out_cubic(1 - x), 1.0)
		assert approx(in_quart(x) + out_quart(1 - x), 1.0)
		assert approx(in_quint(x) + out_quint(1 - x), 1.0)
		assert approx(in_sine(x) + out_sine(1 - x), 1.0)
		assert approx(in_circ(x) + out_circ(1 - x), 1.0)
		assert approx(in_bounce(x) + out_bounce(1 - x), 1.0)
		x += 0.05
	}
}

fn test_monotone_families_do_not_go_backwards() {
	mut x := 0.0
	mut prev := in_quad(0.0)
	for x <= 1.0 {
		v := in_quad(x)
		assert v >= prev
		prev = v
		x += 0.05
	}
}

fn test_easing_fn_accepts_any_easing() {
	fns := [EasingFN(in_quad), out_quad, in_out_cubic, linear, in_sine]
	assert fns.len == 5
	assert fns[0](0.5) == 0.25
	assert fns[1](0.5) == 0.75
	assert fns[2](0.5) == 0.5
	assert fns[3](0.5) == 0.5
	mut sum := 0.0
	for f in fns {
		sum += f(1.0)
	}
	assert approx(sum, 5.0)
}
