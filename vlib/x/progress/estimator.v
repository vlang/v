module progress

import math
import time

// RateEstimator keeps an exponentially smoothed items-per-second rate. (The
// standard library has statistics helpers but no moving average.)
//
// The overall average (value / elapsed) reacts very slowly to a change in
// speed, which makes ETAs useless on uneven workloads. This weights recent
// samples more. It takes timestamps as arguments, so it needs no real clock
// and is trivial to test.
struct RateEstimator {
mut:
	// tau is the smoothing time constant in seconds: after `tau` seconds a
	// change in speed is about 63% reflected. Zero or negative disables
	// smoothing (the rate is just the latest sample).
	tau f64 = 2.0
	// min_dt is the smallest interval (seconds) between samples worth using.
	min_dt     f64 = 0.05
	current    f64 = -1.0
	last_value i64
	last_ns    i64
	primed     bool
}

// update feeds one observation: `value` items done at `now_ns` nanoseconds.
fn (mut e RateEstimator) update(value i64, now_ns i64) {
	if !e.primed {
		e.primed = true
		e.last_value = value
		e.last_ns = now_ns
		return
	}
	dt := time.Duration(now_ns - e.last_ns).seconds()
	if dt < e.min_dt {
		return
	}
	inst :=
		math.max(f64(value - e.last_value) / dt, 0.0) // a value that went backwards counts as 0/s
	if e.current < 0.0 || e.tau <= 0.0 {
		e.current = inst
	} else {
		alpha := 1.0 - math.exp(-dt / e.tau)
		e.current += alpha * (inst - e.current)
	}
	e.last_value = value
	e.last_ns = now_ns
}

// rate returns the smoothed items per second, or a negative number while
// there is not yet enough data.
fn (e RateEstimator) rate() f64 {
	return e.current
}
