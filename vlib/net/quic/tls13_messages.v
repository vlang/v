module quic

import crypto.hmac
import crypto.sha256

// RFC 8446 §B.3 — TLS 1.3 handshake message types. Excludes the TLS-1.2-era
// RESERVED values (hello_request, hello_verify_request, hello_retry_request
// as its own wire type — HelloRetryRequest is a ServerHello variant
// distinguished by its `random` field, not a separate msg_type —
// server_key_exchange, server_hello_done, client_key_exchange,
// certificate_url, certificate_status, supplemental_data): those can never
// legitimately appear on a TLS 1.3 wire, so a peer sending one is malformed
// input, correctly rejected by parse_handshake_message's "unknown type"
// path rather than accepted as a recognized-but-unused variant.
pub enum HandshakeType {
	client_hello         = 1
	server_hello         = 2
	new_session_ticket   = 4
	end_of_early_data    = 5
	encrypted_extensions = 8
	certificate          = 11
	certificate_request  = 13
	certificate_verify   = 15
	finished             = 20
	key_update           = 24
	message_hash         = 254
}

// handshake_type_from_u8 validates a wire byte against the known TLS 1.3
// HandshakeType set (RFC 8446 §B.3), rather than blindly casting it to
// `HandshakeType` — an arbitrary u8 does not correspond to a valid variant
// for most of the 0..255 range, and a cast alone would not catch that.
pub fn handshake_type_from_u8(b u8) !HandshakeType {
	return match b {
		1 { .client_hello }
		2 { .server_hello }
		4 { .new_session_ticket }
		5 { .end_of_early_data }
		8 { .encrypted_extensions }
		11 { .certificate }
		13 { .certificate_request }
		15 { .certificate_verify }
		20 { .finished }
		24 { .key_update }
		254 { .message_hash }
		else { error('quic: unknown TLS 1.3 HandshakeType ${b}') }
	}
}

pub struct HandshakeMessage {
pub:
	typ  HandshakeType
	body []u8
}

// encode_handshake_message wraps `body` in the standard TLS 1.3 handshake
// message header (RFC 8446 §4): a 1-byte HandshakeType followed by a
// 3-byte big-endian length. This framing is identical to the synthetic
// message_hash record Phase 2b's `synthetic_client_hello1_hash` builds by
// hand for the HelloRetryRequest transcript rule; that call site is not
// migrated to use this function, since encode_handshake_message requires a
// real HandshakeType and message_hash's synthetic record is deliberately
// NOT a real handshake message being framed for transmission.
pub fn encode_handshake_message(typ HandshakeType, body []u8) ![]u8 {
	if body.len > 0xff_ffff {
		return error('quic: handshake message body too large: ${body.len} bytes')
	}
	mut msg := []u8{cap: 4 + body.len}
	msg << u8(typ)
	msg << u8(body.len >> 16)
	msg << u8(body.len >> 8)
	msg << u8(body.len)
	msg << body
	return msg
}

// parse_handshake_message reads one TLS 1.3 handshake message from the
// front of `buf` and returns it along with the number of bytes consumed.
// `buf` may contain trailing bytes belonging to a LATER message — QUIC
// delivers handshake messages via CRYPTO frames that can span or share
// packet boundaries arbitrarily (Phase 4's crypto_stream.v job to
// reassemble into a contiguous byte stream); this function only peels off
// exactly one message and reports how much of `buf` it consumed, so a
// caller can loop.
pub fn parse_handshake_message(buf []u8) !(HandshakeMessage, int) {
	if buf.len < 4 {
		return error('quic: truncated handshake message header: need 4 bytes, have ${buf.len}')
	}
	typ := handshake_type_from_u8(buf[0])!
	length := int((u32(buf[1]) << 16) | (u32(buf[2]) << 8) | u32(buf[3]))
	total := 4 + length
	if buf.len < total {
		return error('quic: truncated handshake message body: need ${total} bytes, have ${buf.len}')
	}
	return HandshakeMessage{
		typ:  typ
		body: buf[4..total].clone()
	}, total
}

// compute_finished_verify_data implements RFC 8446 §4.4.4's Finished
// message computation:
//
//	finished_key = HKDF-Expand-Label(BaseKey, "finished", "", Hash.length)
//	verify_data  = HMAC(finished_key, Transcript-Hash(...))
//
// `base_secret` is whichever side's traffic secret applies — the client's
// Finished uses client_handshake_traffic_secret, the server's uses
// server_handshake_traffic_secret (Phase 2b's `HandshakeSecrets` fields);
// this function is side-agnostic, the caller picks. `transcript_hash` must
// already cover exactly the messages up to but NOT including this Finished
// message itself (for the server's Finished: ClientHello...
// CertificateVerify*; for the client's Finished: ClientHello...server
// Finished).
pub fn compute_finished_verify_data(base_secret []u8, transcript_hash []u8) ![]u8 {
	finished_key := derive_secret(base_secret, 'finished', []u8{})!
	return hmac.new(finished_key, transcript_hash, sha256.sum256, sha256.block_size)
}

// verify_finished checks a peer-supplied Finished message's verify_data
// against the expected value computed from our own key schedule and
// transcript state, using a constant-time comparison
// (`crypto.hmac.equal`) — verify_data is exactly the kind of
// peer-supplied authenticator where a naive `==` would leak timing
// information about how much of it matched.
pub fn verify_finished(base_secret []u8, transcript_hash []u8, peer_verify_data []u8) !bool {
	expected := compute_finished_verify_data(base_secret, transcript_hash)!
	return hmac.equal(expected, peer_verify_data)
}
