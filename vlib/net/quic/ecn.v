module quic

// RFC 9000 §13.4 — ECN (Explicit Congestion Notification). v1 scope
// decision: no OS-level socket option exists in V today to SET the ECT
// codepoint on an outgoing UDP datagram, so this endpoint never marks any
// packet it sends ECT(0)/ECT(1) -- and RFC 9000 §13.4.2's ECN validation
// procedure exists specifically to confirm that marks an endpoint itself
// applied survived the path unmodified, so an endpoint that never marks
// anything has nothing to validate. This is a legitimate, spec-sanctioned
// fallback ("an endpoint MAY revert to not setting ECN codepoints" --
// §13.4.2), not a protocol violation; RFC 9000 explicitly anticipates
// paths and endpoints that don't support ECN.
//
// What v1 DOES do: parse the ECN counts a peer reports in its ACK frames
// (frame.v's `EcnCounts`, already implemented) without erroring, and
// never treat them as validated -- so no future congestion-control
// integration can accidentally react to them as though ECN were actually
// working on this path.

// EcnState tracks the most recent cumulative ECN counts this endpoint has
// seen reported back to it, purely for bookkeeping/diagnostic parity with
// what a real ECN-capable implementation would track -- no
// congestion-control decision may ever be driven by it.
pub struct EcnState {
pub mut:
	last_ect0   u64
	last_ect1   u64
	last_ecn_ce u64
}

pub fn new_ecn_state() EcnState {
	return EcnState{}
}

// note_ack_ecn_counts records one ACK frame's reported ECN counts. RFC
// 9000 §13.4.2.1: these are CUMULATIVE totals the peer has ever reported,
// not per-ACK deltas -- a real ECN-reacting implementation would compare
// against the previous totals to compute what changed since the last ACK;
// v1 has no such reaction to drive, so simply recording the latest
// reported totals is sufficient for its bookkeeping-only purpose.
pub fn (mut s EcnState) note_ack_ecn_counts(counts EcnCounts) {
	s.last_ect0 = counts.ect0
	s.last_ect1 = counts.ect1
	s.last_ecn_ce = counts.ecn_ce
}

// is_validated always reports false in v1 -- see the file-level doc
// comment. This is the checkpoint any future congestion-control
// integration (Phase 9+) must consult before reacting to an ECN-CE mark
// (RFC 9000 §13.4.3); since it can never be true here, no such reaction
// is possible by construction, regardless of what note_ack_ecn_counts has
// recorded.
pub fn (s &EcnState) is_validated() bool {
	return false
}
