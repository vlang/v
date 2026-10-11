module progress

fn test_estimator_needs_two_samples() {
	mut e := RateEstimator{}
	assert e.rate() < 0.0
	e.update(0, 0)
	assert e.rate() < 0.0
	e.update(10, 100_000_000) // 100ms later, +10 items
	assert e.rate() > 99.0 && e.rate() < 101.0
}

fn test_estimator_follows_speed_changes() {
	mut e := RateEstimator{}
	mut ns := i64(0)
	mut v := i64(0)
	e.update(v, ns)
	for _ in 0 .. 20 { // 2s at 100/s
		ns += 100_000_000
		v += 10
		e.update(v, ns)
	}
	assert e.rate() > 99.0 && e.rate() < 101.0
	for _ in 0 .. 10 { // 1s at 200/s: partly adapted, not yet there
		ns += 100_000_000
		v += 20
		e.update(v, ns)
	}
	assert e.rate() > 110.0 && e.rate() < 199.0
	for _ in 0 .. 100 { // 10s more: converged
		ns += 100_000_000
		v += 20
		e.update(v, ns)
	}
	assert e.rate() > 199.0 && e.rate() < 201.0
}

fn test_estimator_ignores_too_close_samples_and_backwards_values() {
	mut e := RateEstimator{}
	e.update(0, 0)
	e.update(5, 10_000_000) // 10ms < min_dt: ignored
	assert e.rate() < 0.0
	e.update(10, 100_000_000)
	e.update(2, 200_000_000) // value went backwards: counts as 0/s, never negative
	assert e.rate() >= 0.0
}
