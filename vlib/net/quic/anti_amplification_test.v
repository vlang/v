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
