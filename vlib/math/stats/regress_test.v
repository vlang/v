module stats

import math

fn test_correlation() {
	assert correlation([1.0, 2.0, 3.0], [2.0, 4.0, 6.0]) == 1.0
	assert correlation([1.0, 2.0, 3.0], [6.0, 4.0, 2.0]) == -1.0
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
	assert nd.cdf(1.959963984540054) == 0.975
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
