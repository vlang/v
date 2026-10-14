module stats

import math

fn test_correlation() {
	assert math.abs(correlation([1.0, 2.0, 3.0], [2.0, 4.0, 6.0]) - 1.0) < 1e-15
	assert math.abs(correlation([1.0, 2.0, 3.0], [6.0, 4.0, 2.0]) + 1.0) < 1e-15
	assert correlation([1.0, 2.0, 3.0], [1.0, 1.0, 1.0]) != correlation([1.0, 2.0, 3.0],
		[1.0, 1.0, 1.0]), 'zero variance is nan'
	assert math.abs(correlation([1.0, 2.0, 3.0, 4.0], [2.0, 1.0, 4.0, 3.0]) - 0.6) < 1e-12
}

fn test_linear_regression() {
	lr := linear_regression([1.0, 2.0, 3.0, 4.0], [3.0, 5.0, 7.0, 9.0])!
	assert lr.slope == 2.0
	assert lr.intercept == 1.0
	assert lr.predict(5.0) == 11.0
	lr2 := linear_regression([1.0, 1.0], [2.0, 3.0]) or {
		assert err.msg().contains('slope is undefined')
		return
	}
	assert false, 'identical x must error'
}

fn test_quantiles() {
	q := quantiles([1.0, 2.0, 3.0, 4.0], 4)!
	assert q.len == 3
	assert q[0] == 1.75
	assert q[1] == 2.5
	assert q[2] == 3.25
	quartiles := quantiles([1.0, 2.0, 3.0], 2)!
	assert quartiles == [2.0]
	quantiles([1.0], 4) or {
		assert err.msg().contains('at least 2 data points')
		return
	}
	assert false, 'too few points must error'
}

fn test_multimode() {
	assert multimode([1, 1, 2, 2, 3]) == [1, 2]
	assert multimode(['a', 'b', 'a', 'b', 'c']) == ['a', 'b']
	assert multimode([5]) == [5]
	assert multimode([]int{}).len == 0
}

fn test_normal_dist() {
	nd := new_normal_dist(0.0, 1.0)!
	assert nd.pdf(0.0) == 0.3989422804014327
	assert nd.cdf(0.0) == 0.5
	assert math.abs(nd.cdf(1.959963984540054) - 0.975) < 1e-15
	assert math.abs(nd.inv_cdf(0.975)! - 1.959963984540054) < 1e-12
	assert nd.inv_cdf(0.5)! == 0.0
	nd2 := new_normal_dist(10.0, 2.0)!
	assert nd2.cdf(10.0) == 0.5
	assert nd2.inv_cdf(0.5)! == 10.0
	nd.inv_cdf(0.0) or {
		assert err.msg().contains('inv_cdf needs 0 < p < 1')
		return
	}
	assert false, 'p out of range must error'
}

fn test_stats_integer_samples_and_centered_offsets() {
	assert math.abs(correlation([1, 2, 3], [2, 4, 6]) - 1.0) < 1e-15
	assert math.is_nan(correlation([]int{}, []int{}))
	assert math.is_nan(correlation([1, 1], [2, 3]))
	lr := linear_regression([1, 2, 3], [1, 2, 2])!
	assert math.abs(lr.slope - 0.5) < 1e-15
	assert math.abs(lr.intercept - (2.0 / 3.0)) < 1e-15
	assert math.abs(lr.predict(4) - (8.0 / 3.0)) < 1e-15
	shifted := linear_regression([1e12, 1e12 + 1, 1e12 + 2], [1.0, 3.0, 5.0])!
	assert shifted.slope == 2.0
	assert shifted.intercept == 1.0 - 2e12
	assert shifted.predict(1e12 + 3) == 7.0
	assert math.abs(correlation([1e12, 1e12 + 1, 1e12 + 2], [1.0, 3.0, 5.0]) - 1.0) < 1e-15
	assert quantiles([1, 2, 3, 4], 4)! == [1.75, 2.5, 3.25]
	assert quantiles([1, 2], 1)! == []f64{}
}

fn test_multimode_uses_value_equality_and_first_seen_order() {
	a := 1.00000000001
	b := 1.00000000002
	assert a != b
	assert multimode([a, b, b]) == [b]
	assert multimode([3, 1, 3, 1, 2]) == [3, 1]
	assert multimode([0.0, -0.0, 1.0]) == [0.0]
	assert multimode(['b', 'a', 'b', 'a']) == ['b', 'a']
}

fn test_normal_dist_invalid_and_extreme_tail_inputs() {
	for std in [-1.0, math.nan(), math.inf(1)] {
		if _ := new_normal_dist(0.0, std) {
			assert false
		} else {
			assert err.msg().contains('finite mean')
		}
	}
	if _ := new_normal_dist(math.nan(), 1.0) {
		assert false
	} else {
		assert err.msg().contains('finite mean')
	}
	nd := new_normal_dist(0.0, 1.0)!
	if _ := nd.inv_cdf(math.nan()) {
		assert false
	} else {
		assert err.msg().contains('0 < p < 1')
	}
	tail := nd.inv_cdf(1e-300)!
	assert !math.is_nan(tail) && !math.is_inf(tail, 0)
	assert tail < -37.0 && tail > -37.1
	assert nd.cdf(-10.0) > 0.0
	assert math.abs(nd.cdf(-10.0) / 7.61985302416047e-24 - 1.0) < 1e-13
	point := new_normal_dist(5.0, 0.0)!
	assert point.inv_cdf(0.1)! == 5.0
	assert point.pdf(5.0) == math.inf(1)
	assert point.cdf(4.0) == 0.0 && point.cdf(5.0) == 1.0
	assert math.is_nan(point.pdf(math.nan()))
	assert math.is_nan(point.cdf(math.nan()))
}

fn test_correlation_avoids_square_overflow_and_underflow() {
	assert math.abs(correlation([1e200, 2e200, 3e200], [2e200, 4e200, 6e200]) - 1.0) < 1e-15
	assert math.abs(correlation([1e-200, 2e-200, 3e-200], [2e-200, 4e-200, 6e-200]) - 1.0) < 1e-15
	assert math.abs(correlation([-math.max_f64, math.max_f64], [-1.0, 1.0]) - 1.0) < 1e-15
	huge := linear_regression([1e200, 2e200, 3e200], [2e200, 4e200, 6e200])!
	assert math.abs(huge.slope - 2.0) < 1e-15
	assert math.abs(huge.predict(4e200) / 8e200 - 1.0) < 1e-15
}

fn test_normal_dist_finite_parameter_overflow_and_density_underflow() {
	wide := new_normal_dist(0.0, math.max_f64)!
	assert math.abs(wide.cdf(math.max_f64) - 0.8413447460685429) < 1e-15
	assert wide.cdf(math.inf(1)) == 1.0
	assert wide.cdf(math.inf(-1)) == 0.0
	assert wide.pdf(0.0) > 0.0
	assert math.abs(wide.pdf(0.0) * math.max_f64 - 0.3989422804014327) < 1e-14
	assert math.abs(wide.pdf(math.max_f64) * math.max_f64 - 0.24197072451914337) < 1e-14
	opposite := new_normal_dist(-math.max_f64, math.max_f64)!
	assert math.abs(opposite.cdf(math.max_f64) - 0.9772498680518208) < 1e-15
	inverse := opposite.inv_cdf(0.975)!
	assert !math.is_inf(inverse, 0) && !math.is_nan(inverse)
	assert math.abs(inverse / math.max_f64 - 0.959963984540054) < 1e-13
	tiny := new_normal_dist(0.0, 1e-300)!
	assert tiny.pdf(3.86e-299) > 0.0
	assert math.abs(tiny.pdf(3.86e-299) / 1.1487008370544425e-24 - 1.0) < 1e-12
	assert tiny.pdf(4e-299) > 0.0
	assert math.abs(tiny.pdf(4e-299) / 1.4632702508383032e-48 - 1.0) < 1e-12
}
