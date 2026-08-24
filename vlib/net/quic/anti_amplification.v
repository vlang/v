module quic

// AntiAmplificationLimiter enforces RFC 9000 §8's server-side send limit
// before a client's address is validated: "after receiving packets from an
// address that is not yet validated, an endpoint MUST limit the amount of
// data it sends to the unvalidated address to three times the amount of
// data received from that address." Deliberately dumb: this type only
// counts bytes and answers "how many more may I send right now" -- it has
// no opinion on WHEN address validation actually completes (RFC 9000 §8.1
// lists three independent ways: a Handshake-protected packet was received
// from the peer, the peer used a server-chosen connection ID with at least
// 64 bits of entropy, or a Retry/NEW_TOKEN token validated -- deciding
// which applies, and calling mark_validated() at that moment, is a future
// caller's job (13d's connection-acceptance path), not this primitive's).
//
// Mirrors flow_control.v's FlowControlWindow/ReceiveWindow shape
// deliberately -- same "dumb accounting primitive, caller decides policy"
// role, just for a different RFC-mandated limit.
pub struct AntiAmplificationLimiter {
mut:
	received  u64
	sent      u64
	validated bool
}

// note_received records `n` more bytes as received from this (still
// possibly unvalidated) address. RFC 9000 §8.1: "servers MUST count all of
// the payload bytes received in datagrams that are uniquely attributed to a
// single connection. This includes datagrams that contain packets that are
// successfully processed and datagrams that contain packets that are all
// discarded" -- the caller must count full UDP datagram payload bytes for
// every datagram attributed to this connection attempt, including ones
// this endpoint ultimately drops, not just the bytes of packets it
// successfully processes; this type has no visibility into that
// distinction and trusts the caller's count entirely.
pub fn (mut l AntiAmplificationLimiter) note_received(n u64) {
	l.received += n
}

// mark_validated permanently lifts the send limit -- RFC 9000 §8.1 imposes
// it only "prior to validating the client address"; once validated, this
// endpoint is constrained solely by its congestion controller, a
// completely separate mechanism (loss_detection.v/congestion_control.v)
// this type has no relationship to. There is no corresponding "un-validate"
// -- address validation, once achieved, does not lapse.
pub fn (mut l AntiAmplificationLimiter) mark_validated() {
	l.validated = true
}

// is_validated reports whether mark_validated has been called.
pub fn (l &AntiAmplificationLimiter) is_validated() bool {
	return l.validated
}

// available_to_send reports how many more bytes this endpoint may send
// right now without exceeding RFC 9000 §8.1's 3x limit -- max_u64 once
// validated, meaning this limit no longer applies at all, not merely that
// it has become large.
pub fn (l &AntiAmplificationLimiter) available_to_send() u64 {
	if l.validated {
		return max_u64
	}
	limit := l.received * 3
	if l.sent >= limit {
		return 0
	}
	return limit - l.sent
}

// note_sent records `n` more bytes as sent to this address, failing if that
// would exceed the current limit -- callers must check available_to_send()
// (or catch this error) BEFORE actually sending, never discover the
// violation only after the fact, the same convention
// FlowControlWindow.consume() already establishes for the analogous
// send-side check elsewhere in this module.
pub fn (mut l AntiAmplificationLimiter) note_sent(n u64) ! {
	if !l.validated && l.sent + n > l.received * 3 {
		return error('quic: anti-amplification limit exceeded: attempted to send ${n} bytes, only ${l.available_to_send()} available (received ${l.received}, already sent ${l.sent})')
	}
	l.sent += n
}
