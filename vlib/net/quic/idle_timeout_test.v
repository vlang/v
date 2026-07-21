module quic

import time

// test_effective_idle_timeout_all_four_zero_nonzero_combinations is the
// plan's own explicitly named test: min-of-non-zero-or-infinite across
// all 4 combinations of (local, peer) being zero or non-zero.
fn test_effective_idle_timeout_all_four_zero_nonzero_combinations() {
	// Both zero: no timeout at all.
	if _ := effective_idle_timeout(0, 0) {
		assert false, 'expected no timeout when both sides are zero'
	}

	// Local zero, peer non-zero: peer's value alone applies.
	only_peer := effective_idle_timeout(0, 5000) or {
		assert false, 'expected a timeout'
		return
	}
	assert only_peer == 5000 * time.millisecond

	// Local non-zero, peer zero: local's value alone applies.
	only_local := effective_idle_timeout(3000, 0) or {
		assert false, 'expected a timeout'
		return
	}
	assert only_local == 3000 * time.millisecond

	// Neither zero: the SMALLER of the two.
	smaller_local := effective_idle_timeout(2000, 9000) or {
		assert false, 'expected a timeout'
		return
	}
	assert smaller_local == 2000 * time.millisecond

	smaller_peer := effective_idle_timeout(9000, 2000) or {
		assert false, 'expected a timeout'
		return
	}
	assert smaller_peer == 2000 * time.millisecond
}

// test_idle_timeout_state_reset_asymmetry is the plan's own explicitly
// named test: an ack-eliciting RECEIVE resets the idle timer, a
// non-ack-eliciting receive does not, but ANY send does -- this is an
// asymmetry between the send and receive sides, not "any packet either
// direction".
fn test_idle_timeout_state_reset_asymmetry() {
	mut s := new_idle_timeout_state()
	timeout := 1000 * time.millisecond

	ms500 := u64(500 * time.millisecond)
	ms1400 := u64(1400 * time.millisecond)
	ms1500 := u64(1500 * time.millisecond)
	ms2400 := u64(2400 * time.millisecond)
	ms2500 := u64(2500 * time.millisecond)
	ms3500 := u64(3500 * time.millisecond)

	// Before any packet, elapsed time is measured from connection_start.
	assert !s.is_idle(timeout, ms500, 0)
	assert s.is_idle(timeout, ms1500, 0)

	// A non-ack-eliciting receive does NOT restart the timer.
	s.note_packet_received(false, ms1400)
	assert s.is_idle(timeout, ms1500, 0) // still measured from connection_start=0

	// An ack-eliciting receive DOES restart the timer.
	s.note_packet_received(true, ms1400)
	assert !s.is_idle(timeout, ms1500, 0) // now measured from ms1400
	assert s.is_idle(timeout, ms2500, 0)

	// ANY send restarts the timer, ack-eliciting or not.
	s.note_packet_sent(ms2400)
	assert !s.is_idle(timeout, ms2500, 0) // now measured from ms2400
	assert s.is_idle(timeout, ms3500, 0)
}

fn test_idle_timeout_state_never_idle_with_no_timeout() {
	mut s := new_idle_timeout_state()
	assert !s.is_idle(none, 999_999_999, 0)
}

// test_effective_idle_timeout_tolerates_peer_max_varint is a regression
// test for an integer-overflow bug: max_idle_timeout is a peer-supplied
// transport-parameter varint (RFC 9000 §18.2) with no upper bound of its
// own, so a hostile or buggy peer can send the largest possible varint
// value (2^62-1). Scaling that directly by time.millisecond would
// overflow the i64 backing time.Duration, silently producing a
// nonsensical (possibly negative) timeout. The clamp must produce a
// large-but-sane, strictly positive Duration instead of overflowing or
// erroring.
fn test_effective_idle_timeout_tolerates_peer_max_varint() {
	huge := u64(0x3FFF_FFFF_FFFF_FFFF) // max QUIC varint, 2^62-1

	d := effective_idle_timeout(0, huge) or {
		assert false, 'expected a (very large but finite) timeout, not none'
		return
	}
	assert d > 0
	assert d <= time.infinite

	// The neither-zero path takes the same clamp on whichever side is
	// smaller -- confirm it doesn't overflow even when BOTH sides are huge.
	d2 := effective_idle_timeout(huge, huge) or {
		assert false, 'expected a (very large but finite) timeout, not none'
		return
	}
	assert d2 > 0
	assert d2 <= time.infinite
}
