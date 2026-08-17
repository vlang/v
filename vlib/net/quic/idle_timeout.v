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
// §10.1: "An endpoint restarts its idle timer when a packet from its peer
// is received and processed successfully" (RECEIVE side: unconditional,
// any packet) "[An endpoint] also restarts its idle timer when sending an
// ack-eliciting packet if no other ack-eliciting packets have been sent
// since last receiving and processing a packet" (SEND side: only
// ack-eliciting matters there, though restarting on every send, as this
// type does, is a superset -- more lenient, never less, so it still
// satisfies the MUST). The ack-eliciting condition in the RFC text is
// SEND-only; an earlier version of this type had it backwards, gating the
// RECEIVE side on ack-eliciting instead and never restarting on a
// non-ack-eliciting receive (e.g. a lone ACK frame) -- found via a
// maintainer "Local AI Review" on PR #28083.
pub struct IdleTimeoutState {
pub mut:
	last_reset ?u64 // time.sys_mono_now()-sourced instant
}

// new_idle_timeout_state returns a timer that has not yet been reset by any packet.
pub fn new_idle_timeout_state() IdleTimeoutState {
	return IdleTimeoutState{}
}

// note_packet_sent restarts the idle timer -- ANY packet sent qualifies.
pub fn (mut s IdleTimeoutState) note_packet_sent(now u64) {
	s.last_reset = now
}

// note_packet_received restarts the idle timer -- ANY successfully
// processed received packet qualifies, per RFC 9000 §10.1 (see this
// type's own doc comment); there is no ack-eliciting condition on the
// receive side.
pub fn (mut s IdleTimeoutState) note_packet_received(now u64) {
	s.last_reset = now
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
