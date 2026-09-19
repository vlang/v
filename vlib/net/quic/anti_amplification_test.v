module quic

fn test_anti_amplification_starts_at_zero() {
	lim := AntiAmplificationLimiter{}
	assert lim.available_to_send() == 0
	assert lim.is_validated() == false
}

fn test_anti_amplification_allows_up_to_3x_received() {
	mut lim := AntiAmplificationLimiter{}
	lim.note_received(100)
	assert lim.available_to_send() == 300
	lim.note_sent(300)!
	assert lim.available_to_send() == 0
}

fn test_anti_amplification_rejects_over_limit_send() {
	mut lim := AntiAmplificationLimiter{}
	lim.note_received(100)
	lim.note_sent(301) or {
		assert err.msg().contains('anti-amplification limit exceeded')
		return
	}
	assert false, 'expected sending 301 bytes on a 300-byte budget to fail'
}

fn test_anti_amplification_accumulates_across_multiple_receives() {
	mut lim := AntiAmplificationLimiter{}
	lim.note_received(50)
	lim.note_received(50)
	assert lim.available_to_send() == 300
}

fn test_anti_amplification_tracks_remaining_budget_across_sends() {
	mut lim := AntiAmplificationLimiter{}
	lim.note_received(100)
	lim.note_sent(120)!
	assert lim.available_to_send() == 180
	lim.note_sent(180)!
	assert lim.available_to_send() == 0
}

fn test_anti_amplification_mark_validated_lifts_the_limit() {
	mut lim := AntiAmplificationLimiter{}
	lim.note_received(10)
	lim.note_sent(30)!
	assert lim.available_to_send() == 0

	lim.mark_validated()
	assert lim.is_validated()
	assert lim.available_to_send() == max_u64
	// A send far exceeding any pre-validation budget must now succeed.
	lim.note_sent(1_000_000_000)!
}

fn test_anti_amplification_receiving_more_raises_the_budget_even_after_a_send() {
	mut lim := AntiAmplificationLimiter{}
	lim.note_received(100)
	lim.note_sent(300)!
	assert lim.available_to_send() == 0

	lim.note_received(50)
	// New limit is 3*150=450, already sent 300 -> 150 more available.
	assert lim.available_to_send() == 150
}

// note_sent_unconditional is a regression test for a real bug 13d-2's own
// adversarial review found in the caller that motivated adding this
// method: note_sent's own error path leaves `sent` unchanged, which is
// correct for note_sent's own atomic check-then-reserve contract but
// wrong for a caller recording bytes it has ALREADY sent (e.g. because
// the exact size wasn't knowable until after building the datagram) --
// silently dropping that update would leave available_to_send() reporting
// stale, too-generous budget for every later check, letting a single
// small overshoot compound into an unbounded one.
fn test_anti_amplification_note_sent_unconditional_records_even_past_the_limit() {
	mut lim := AntiAmplificationLimiter{}
	lim.note_received(100)
	// Budget is 300; deliberately record MORE than that, simulating a
	// caller whose pre-build size estimate undershot the real built size.
	lim.note_sent_unconditional(350)
	// Reflects reality (350 sent, not silently capped/dropped) --
	// available_to_send() clamps to 0 rather than underflowing.
	assert lim.available_to_send() == 0

	// Further receives still correctly raise the budget relative to the
	// TRUE (350) sent figure, not a stale pre-overshoot one.
	lim.note_received(100)
	// New limit is 3*200=600, already sent 350 -> 250 more available.
	assert lim.available_to_send() == 250
}

fn test_anti_amplification_note_sent_unconditional_never_fails() {
	mut lim := AntiAmplificationLimiter{}
	// No receives at all -- an ordinary note_sent(1) would fail here; the
	// unconditional variant must not.
	lim.note_sent_unconditional(1_000_000)
	assert lim.available_to_send() == 0
}
