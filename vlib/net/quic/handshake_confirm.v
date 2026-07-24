module quic

// RFC 9001 §4.1.2 — the TLS/QUIC handshake has two distinct completion
// checkpoints, and key-discard timing (RFC 9001 §4.9) depends on which one
// has occurred:
//
//   - "complete": this client has BOTH sent its own Finished message AND
//     verified the server's Finished message. Neither alone is
//     sufficient -- sending your own Finished without verifying the
//     peer's proves nothing about the peer's identity/state, and
//     verifying the peer's without having sent your own means the peer
//     cannot yet trust that THIS side has completed the handshake either.
//   - "confirmed": this client has received a HANDSHAKE_DONE frame -- a
//     1-RTT-only frame (RFC 9000 §19.20), so this can only happen AFTER
//     1-RTT keys already exist. RFC 9001 §4.9.2 requires discarding
//     Handshake keys only once the handshake is CONFIRMED, not merely
//     complete: "complete" alone doesn't prove the SERVER has seen this
//     client's Finished, so a Handshake-space retransmission of it may
//     still be needed until confirmation proves otherwise.
//
// RFC 9001 also permits an ALTERNATE confirmation path (a client MAY treat
// receipt of an ACK for a 1-RTT packet it sent as confirmation, without
// waiting for HANDSHAKE_DONE specifically -- useful if HANDSHAKE_DONE
// itself is lost). Deliberately NOT implemented here: v1 always waits for
// HANDSHAKE_DONE, a legitimate (if slightly less loss-tolerant) subset of
// spec-compliant behavior.
//
// A THIRD, separate key-discard checkpoint exists alongside these two:
// RFC 9001 §4.9.1 requires discarding Initial keys once this client has
// sent its FIRST Handshake-space packet -- independent of complete/
// confirmed, and normally reached much earlier (as soon as Handshake-level
// keys exist and there's anything to send in that space, e.g. an ACK for
// the server's Handshake packets).
pub struct HandshakeCompletionState {
mut:
	own_finished_sent           bool
	peer_finished_verified      bool
	handshake_done_received     bool
	sent_first_handshake_packet bool
}

pub fn new_handshake_completion_state() &HandshakeCompletionState {
	return &HandshakeCompletionState{}
}

// mark_own_finished_sent records that this client has sent its own
// Finished message.
pub fn (mut s HandshakeCompletionState) mark_own_finished_sent() {
	s.own_finished_sent = true
}

// mark_peer_finished_verified records that this client has verified the
// server's Finished message (Phase 2's process_finished having succeeded).
pub fn (mut s HandshakeCompletionState) mark_peer_finished_verified() {
	s.peer_finished_verified = true
}

// is_complete reports RFC 9001 §4.1.2's "handshake complete" checkpoint.
pub fn (s &HandshakeCompletionState) is_complete() bool {
	return s.own_finished_sent && s.peer_finished_verified
}

// mark_handshake_done_received records receipt of a HANDSHAKE_DONE frame.
// This function only tracks the state transition; validating that
// receiving one NOW is legal (e.g. rejecting it before the handshake is
// even complete) is the caller's frame-dispatch responsibility, not
// re-checked here.
pub fn (mut s HandshakeCompletionState) mark_handshake_done_received() {
	s.handshake_done_received = true
}

// is_confirmed reports RFC 9001 §4.1.2/§4.9.2's "handshake confirmed"
// checkpoint -- the trigger for discarding Handshake keys. Also requires
// is_complete() as a safety net: a HANDSHAKE_DONE frame should never
// legitimately arrive before completion, but confirmed implying complete
// is a sane invariant to enforce here regardless of caller ordering
// mistakes upstream.
pub fn (s &HandshakeCompletionState) is_confirmed() bool {
	return s.handshake_done_received && s.is_complete()
}

// mark_sent_first_handshake_packet records that this client has sent its
// first Handshake-space packet.
pub fn (mut s HandshakeCompletionState) mark_sent_first_handshake_packet() {
	s.sent_first_handshake_packet = true
}

// should_discard_initial_keys reports whether RFC 9001 §4.9.1's condition
// for discarding Initial keys has been met. Independent of is_complete/
// is_confirmed -- it typically happens well before either.
pub fn (s &HandshakeCompletionState) should_discard_initial_keys() bool {
	return s.sent_first_handshake_packet
}

// should_discard_handshake_keys reports whether RFC 9001 §4.9.2's
// condition for discarding Handshake keys has been met: the handshake is
// CONFIRMED, not merely complete.
pub fn (s &HandshakeCompletionState) should_discard_handshake_keys() bool {
	return s.is_confirmed()
}
