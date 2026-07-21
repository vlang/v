module quic

// RFC 9000 §10.2 — Immediate Close. Once either endpoint decides to close
// the connection, it enters one of two mutually-exclusive states:
//
//   - "closing": THIS endpoint sent (or is about to send) its own
//     CONNECTION_CLOSE and is waiting out the closing period. It MAY
//     still send -- specifically, a rate-limited retransmission of that
//     same CONNECTION_CLOSE in response to further incoming packets
//     (§10.2.1), to guard against the peer never having received the
//     first one, without allowing an unbounded ping-pong.
//   - "draining": THIS endpoint received a CONNECTION_CLOSE FROM the
//     peer (or otherwise learned the connection is being closed) and
//     MUST NOT send ANYTHING at all, not even its own CONNECTION_CLOSE
//     -- §10.2.2 is unambiguous that draining is fully silent.
//
// An endpoint already in "closing" that then RECEIVES a CONNECTION_CLOSE
// from the peer moves straight to "draining" (§10.2.2): the peer has
// already acknowledged the connection is ending, so this endpoint's own
// pending retransmit is moot and continuing to send would violate the
// silence requirement for no benefit.
pub enum ConnectionCloseState {
	active
	closing
	draining
}

pub struct ConnectionCloseTracker {
pub mut:
	state                          ConnectionCloseState
	packets_received_while_closing u64
}

pub fn new_connection_close_tracker() ConnectionCloseTracker {
	return ConnectionCloseTracker{
		state: .active
	}
}

// enter_closing transitions active -> closing: THIS endpoint has decided
// to close the connection (a locally-detected error, or the application
// closing it) and is about to send its own CONNECTION_CLOSE. A no-op once
// already closing or draining -- closing never regresses, and draining
// always wins over it (see enter_draining).
pub fn (mut t ConnectionCloseTracker) enter_closing() {
	if t.state == .active {
		t.state = .closing
	}
}

// enter_draining transitions active|closing -> draining: THIS endpoint
// received a CONNECTION_CLOSE from the peer. Always wins over an existing
// "closing" state (RFC 9000 §10.2.2) -- draining is a one-way absorbing
// state once reached.
pub fn (mut t ConnectionCloseTracker) enter_draining() {
	t.state = .draining
}

// note_packet_received_while_closing reports whether, on receiving
// another packet while in the closing state, this endpoint may resend
// its own CONNECTION_CLOSE -- RFC 9000 §10.2.1 rate-limits this to avoid
// an unbounded ping-pong: at most one retransmission per received
// packet, so a peer cannot extract more retransmissions than the number
// of packets it itself is willing to send. Always false outside the
// closing state (nothing to retransmit while active, and draining must
// stay fully silent regardless of what arrives).
pub fn (mut t ConnectionCloseTracker) note_packet_received_while_closing() bool {
	if t.state != .closing {
		return false
	}
	t.packets_received_while_closing++
	return true
}

// may_send reports whether ANY packet may be sent right now. Draining is
// fully silent (RFC 9000 §10.2.2); closing may still send the
// rate-limited CONNECTION_CLOSE retransmission above; active sends
// freely.
pub fn (t &ConnectionCloseTracker) may_send() bool {
	return t.state != .draining
}
