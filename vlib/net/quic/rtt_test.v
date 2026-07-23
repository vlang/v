module quic

import time

fn test_rtt_estimator_new_is_seeded_with_kinitialrtt() {
	r := new_rtt_estimator()
	assert r.smoothed_rtt == initial_rtt
	assert r.rttvar == initial_rtt / 2
	assert !r.has_sample
}

// test_rtt_estimator_first_sample_seeds_directly is the plan's own
// explicitly named test: the FIRST RTT sample uses a different formula
// (direct seed) than every later sample (EWMA) -- RFC 9002 §5.3 calls this
// out as a distinct case, and a common bug applies the EWMA formula on the
// very first sample instead.
fn test_rtt_estimator_first_sample_seeds_directly() {
	mut r := new_rtt_estimator()
	// A huge ack_delay on the first sample must have zero effect -- the
	// first-sample path ignores ack_delay entirely, unlike every later one.
	r.update(.application_data, 100 * time.millisecond, 90 * time.millisecond,
		999 * time.millisecond, true)

	assert r.has_sample
	assert r.min_rtt == 100 * time.millisecond
	assert r.smoothed_rtt == 100 * time.millisecond
	assert r.rttvar == 50 * time.millisecond
	assert r.latest_rtt == 100 * time.millisecond
}

// test_rtt_estimator_subsequent_sample_uses_ewma exercises the EWMA
// formula (3/4 rttvar + 1/4 diff, 7/8 smoothed_rtt + 1/8 adjusted_rtt)
// against hand-derived exact values (chosen so every division is exact,
// with no floating-point rounding to account for).
fn test_rtt_estimator_subsequent_sample_uses_ewma() {
	mut r := new_rtt_estimator()
	r.update(.application_data, 100 * time.millisecond, 0, 0, false)
	assert r.smoothed_rtt == 100 * time.millisecond
	assert r.rttvar == 50 * time.millisecond

	// latest_rtt=140ms, ack_delay=10ms (plausible: latest >= min+ack_delay),
	// so adjusted_rtt = 140ms - 10ms = 130ms.
	r.update(.application_data, 140 * time.millisecond, 10 * time.millisecond,
		25 * time.millisecond, false)

	assert r.min_rtt == 100 * time.millisecond // unchanged: 140ms is not a new minimum
	assert r.rttvar == 45 * time.millisecond // (50*3 + |100-130|) / 4 = 180/4
	assert r.smoothed_rtt == 103_750_000 * time.nanosecond // (100*7 + 130) / 8 = 830/8
	assert r.latest_rtt == 140 * time.millisecond
}

// test_rtt_estimator_ack_delay_never_pushes_below_min_rtt confirms RFC
// 9002 §5.3's plausibility guard: ack_delay is only subtracted when doing
// so cannot drive the adjusted sample below min_rtt. Here latest_rtt
// itself becomes the new min_rtt in the same update, so ANY positive
// ack_delay must be ignored for this sample (adjusted_rtt stays
// latest_rtt unmodified) -- if the guard were missing, adjusted_rtt would
// come out to 20ms instead of 50ms, producing different smoothed_rtt/
// rttvar than asserted below.
fn test_rtt_estimator_ack_delay_never_pushes_below_min_rtt() {
	mut r := new_rtt_estimator()
	r.update(.application_data, 200 * time.millisecond, 0, 0, false)

	r.update(.application_data, 50 * time.millisecond, 30 * time.millisecond,
		999 * time.millisecond, false)

	assert r.min_rtt == 50 * time.millisecond
	assert r.rttvar == 112_500_000 * time.nanosecond // (100*3 + |200-50|) / 4 = 450/4
	assert r.smoothed_rtt == 181_250_000 * time.nanosecond // (200*7 + 50) / 8 = 1450/8
}

// test_rtt_estimator_ack_delay_ignored_for_initial_and_handshake confirms
// RFC 9002 §5.3's rule that ACK Delay is unconditionally ignored (treated
// as zero) for the Initial and Handshake packet number spaces, regardless
// of the field's actual value or handshake-confirmed status -- two
// estimators seeded identically, one fed a huge ack_delay and the other
// fed zero, must end up in IDENTICAL states when the space is `.initial`.
fn test_rtt_estimator_ack_delay_ignored_for_initial_and_handshake() {
	mut a := new_rtt_estimator()
	a.update(.initial, 200 * time.millisecond, 0, 0, false)
	a.update(.initial, 140 * time.millisecond, 999 * time.millisecond, 999 * time.millisecond, true)

	mut b := new_rtt_estimator()
	b.update(.initial, 200 * time.millisecond, 0, 0, false)
	b.update(.initial, 140 * time.millisecond, 0, 999 * time.millisecond, true)

	assert a.smoothed_rtt == b.smoothed_rtt
	assert a.rttvar == b.rttvar
	assert a.min_rtt == b.min_rtt
}

// test_rtt_estimator_max_ack_delay_clamp_only_after_confirmation confirms
// the max_ack_delay clamp (RFC 9002 §5.3) applies ONLY once the handshake
// is confirmed -- the same sample fed to two otherwise-identical
// estimators, differing only in `handshake_confirmed`, must produce
// DIFFERENT adjusted_rtt (and therefore different smoothed_rtt/rttvar)
// since the clamp changes the effective ack_delay from 40ms to 10ms.
fn test_rtt_estimator_max_ack_delay_clamp_only_after_confirmation() {
	mut unconfirmed := new_rtt_estimator()
	unconfirmed.update(.application_data, 100 * time.millisecond, 0, 0, false)
	unconfirmed.update(.application_data, 150 * time.millisecond, 40 * time.millisecond,
		10 * time.millisecond, false)
	// unclamped: adjusted_rtt = 150 - 40 = 110ms
	assert unconfirmed.rttvar == 40 * time.millisecond // (50*3 + |100-110|)/4 = 160/4
	assert unconfirmed.smoothed_rtt == 101_250_000 * time.nanosecond // (100*7+110)/8 = 810/8

	mut confirmed := new_rtt_estimator()
	confirmed.update(.application_data, 100 * time.millisecond, 0, 0, false)
	confirmed.update(.application_data, 150 * time.millisecond, 40 * time.millisecond,
		10 * time.millisecond, true)
	// clamped to max_ack_delay: adjusted_rtt = 150 - 10 = 140ms
	assert confirmed.rttvar == 47_500_000 * time.nanosecond // (50*3 + |100-140|)/4 = 190/4
	assert confirmed.smoothed_rtt == 105 * time.millisecond // (100*7+140)/8 = 840/8
}

fn test_rtt_estimator_pto_period_uses_kinitialrtt_before_any_sample() {
	r := new_rtt_estimator()
	// smoothed_rtt=333ms, 4*rttvar=666ms > kGranularity(1ms) -> 333+666=999ms
	assert r.pto_period() == 999 * time.millisecond
}

fn test_rtt_estimator_pto_period_floors_rttvar_term_at_granularity() {
	mut r := new_rtt_estimator()
	// First sample with a tiny RTT drives rttvar to an equally tiny value,
	// so 4*rttvar must be floored at kGranularity rather than used as-is.
	r.update(.application_data, 10 * time.microsecond, 0, 0, false)
	assert r.rttvar == 5 * time.microsecond
	assert r.pto_period() == 10 * time.microsecond + granularity
}
