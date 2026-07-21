module quic

import time

// RFC 9000 §10.1 — Idle Timeout. The effective timeout an endpoint
// enforces is the SMALLER of the two peers' own `max_idle_timeout`
// transport parameters (milliseconds), where 0 means "this endpoint
// imposes no timeout at all" -- not a literal zero-length timeout that
// would close the connection instantly. All 4 combinations of
// zero/non-zero need distinct handling: both zero means no timeout at
// all (`none`); exactly one zero means the other's value alone applies;
// neither zero means the smaller of the two.

// max_safe_idle_timeout_ms is the largest millisecond value that can be
// scaled to nanoseconds (`* time.millisecond`) without overflowing the
// i64 that backs time.Duration. `max_idle_timeout` is a peer-supplied
// transport-parameter varint (RFC 9000 §18.2) with no upper bound of its
// own -- transport_parameters.v accepts anything up to 2^62-1 -- so a
// hostile or buggy peer can and must be assumed to send an arbitrarily
// large value here; scaling it unclamped would silently overflow into a
// nonsensical (possibly negative, or wrapped-small) Duration, turning a
// peer-supplied number into either a self-inflicted near-immediate
// timeout or an idle timeout that can never fire at all.
const max_safe_idle_timeout_ms = u64(time.infinite) / u64(time.millisecond)

fn clamped_ms_to_duration(ms u64) time.Duration {
	clamped := if ms > max_safe_idle_timeout_ms { max_safe_idle_timeout_ms } else { ms }
	return time.Duration(i64(clamped) * i64(time.millisecond))
}

// effective_idle_timeout resolves RFC 9000 §10.1's min-of-non-zero rule.
// `none` return means the connection has no idle timeout whatsoever.
pub fn effective_idle_timeout(local_max_idle_timeout_ms u64, peer_max_idle_timeout_ms u64) ?time.Duration {
	if local_max_idle_timeout_ms == 0 && peer_max_idle_timeout_ms == 0 {
		return none
	}
	if local_max_idle_timeout_ms == 0 {
		return clamped_ms_to_duration(peer_max_idle_timeout_ms)
	}
	if peer_max_idle_timeout_ms == 0 {
		return clamped_ms_to_duration(local_max_idle_timeout_ms)
	}
	smaller := if local_max_idle_timeout_ms < peer_max_idle_timeout_ms {
		local_max_idle_timeout_ms
	} else {
		peer_max_idle_timeout_ms
	}
	return clamped_ms_to_duration(smaller)
}

// IdleTimeoutState tracks when the idle timer last restarted -- RFC 9000
// §10.1's deliberately ASYMMETRIC reset rule, not "any packet either
// direction": an ack-eliciting packet RECEIVED restarts it, but a
// non-ack-eliciting receive (e.g. an ACK-only packet) does NOT; any
// packet SENT restarts it regardless of whether that packet itself was
// ack-eliciting.
pub struct IdleTimeoutState {
pub mut:
	last_reset ?u64 // time.sys_mono_now()-sourced instant
}

pub fn new_idle_timeout_state() IdleTimeoutState {
	return IdleTimeoutState{}
}

// note_packet_sent restarts the idle timer -- ANY packet sent qualifies.
pub fn (mut s IdleTimeoutState) note_packet_sent(now u64) {
	s.last_reset = now
}

// note_packet_received restarts the idle timer ONLY for an ack-eliciting
// packet -- a non-ack-eliciting receive (e.g. a lone ACK frame) must NOT
// restart it, otherwise two idle peers exchanging nothing but ACKs of
// each other's ACKs could keep the connection alive forever.
pub fn (mut s IdleTimeoutState) note_packet_received(is_ack_eliciting bool, now u64) {
	if is_ack_eliciting {
		s.last_reset = now
	}
}

// is_idle reports whether `timeout` has elapsed since the timer was last
// restarted. `timeout` being `none` (both peers disabled it) never
// expires. Before the very first restart (no packet sent or received
// yet), elapsed time is measured from `connection_start`.
pub fn (s &IdleTimeoutState) is_idle(timeout ?time.Duration, now u64, connection_start u64) bool {
	d := timeout or { return false }
	baseline := s.last_reset or { connection_start }
	elapsed := time.Duration(i64(now) - i64(baseline))
	return elapsed >= d
}
