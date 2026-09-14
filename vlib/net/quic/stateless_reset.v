module quic

import crypto.hmac
import crypto.sha256
import crypto.subtle

// RFC 9000 §10.3 — Stateless Reset. A stateless reset packet is
// indistinguishable on the wire from an ordinary short-header packet
// except by matching its LAST 16 bytes against a stateless-reset token
// previously advertised for the connection ID it targets. Because a
// legitimate packet's ciphertext could coincidentally end in the same 16
// bytes as an unrelated token, RFC 9000 §10.3.1 requires this be checked
// ONLY as a fallback, after normal AEAD decryption has already failed --
// never as a first-choice interpretation. This module intentionally does
// not decrypt anything itself; the caller (a future QuicConn, Phase 9)
// must have already tried and failed to process the datagram as an
// ordinary packet before ever calling is_stateless_reset.
//
// v1 scope: connection IDs and their tokens are recorded for matching
// purposes only -- full connection ID rotation/migration (NEW_CONNECTION_ID/
// RETIRE_CONNECTION_ID driving an active set of usable CIDs) is not
// implemented; this is deliberately just enough to recognize a reset for
// whichever connection ID(s) this endpoint is currently using.

// StatelessResetTracker records the stateless-reset tokens this endpoint
// has learned (from the peer's transport parameters and/or
// NEW_CONNECTION_ID frames), keyed by the connection ID they protect.
pub struct StatelessResetTracker {
mut:
	known_tokens map[string][]u8 // hex-encoded connection ID -> 16-byte token
}

// new_stateless_reset_tracker returns a tracker with no tokens recorded yet.
pub fn new_stateless_reset_tracker() StatelessResetTracker {
	return StatelessResetTracker{}
}

// record_token associates a 16-byte stateless-reset token with the
// connection ID it protects.
pub fn (mut t StatelessResetTracker) record_token(connection_id []u8, token []u8) ! {
	if token.len != 16 {
		return error('quic: stateless reset token must be exactly 16 bytes, got ${token.len}')
	}
	t.known_tokens[connection_id.hex()] = token.clone()
}

// is_stateless_reset reports whether `datagram`'s trailing 16 bytes match
// the token recorded for `connection_id`. MUST only be called after
// normal packet processing (header parse + AEAD decrypt) has already
// failed for this datagram -- see the file-level doc comment. The
// comparison is constant-time: a token is a secret shared only between
// this endpoint and the one that issued it, and a variable-time
// byte-by-byte compare would leak how many leading bytes an attacker's
// guess got right.
pub fn (t &StatelessResetTracker) is_stateless_reset(connection_id []u8, datagram []u8) bool {
	if datagram.len < 16 {
		return false
	}
	token := t.known_tokens[connection_id.hex()] or { return false }
	trailing := datagram[datagram.len - 16..]
	return subtle.constant_time_compare(token, trailing) == 1
}

// generate_stateless_reset_token derives the stateless-reset token this
// endpoint should advertise for `connection_id`, using RFC 9000 §10.3.2's
// recommended construction: "A single static key can be used across all
// connections to the same endpoint by generating the proof using a
// pseudorandom function that takes a static key and the connection ID...
// An endpoint could use HMAC... (for example, HMAC(static_key,
// connection_id))... truncated to 16 bytes." Deriving the token this way
// -- rather than randomly generating and storing one per connection -- is
// the entire point of a stateless reset (§10.3: it must remain computable
// after this endpoint has lost all per-connection state); `static_key` is
// a server-instance-local secret generated once (e.g. via
// crypto.rand.bytes) and never sent on the wire. HMAC-SHA-256 is an
// arbitrary but fixed choice among the RFC's own listed examples (HMAC or
// HKDF, with any hash) -- nothing about the wire format depends on which
// PRF produced the token, since only this endpoint itself ever
// recomputes it.
pub fn generate_stateless_reset_token(static_key []u8, connection_id []u8) ![]u8 {
	if connection_id.len == 0 {
		// RFC 9000 §10.3.2: this construction "cannot provide a
		// zero-length connection ID" -- there is no connection-ID
		// material left to key the derivation on.
		return error('quic: cannot generate a stateless reset token for a zero-length connection ID (RFC 9000 §10.3.2)')
	}
	mac := hmac.new(static_key, connection_id, sha256.sum256, sha256.block_size)
	return mac[..16].clone()
}
