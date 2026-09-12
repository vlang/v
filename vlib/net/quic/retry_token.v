module quic

import crypto.aes
import crypto.rand

// Server-side address-validation token generation and validation (RFC 9000
// §8.1.1/§8.1.2/§8.1.4). v1 only issues tokens via Retry packets -- NEW_TOKEN
// frame issuance (§8.1.3, tokens usable across separate future connections)
// is out of scope, the same class of deliberate-defer choice this project
// makes elsewhere (see PROGRESS.md's Phase 13 checklist); RFC 9000 §8.1.1's
// only real cross-cutting requirement ("a token... MUST be constructed in a
// way that allows the server to identify how it was provided") has no
// second source to distinguish from yet, so it doesn't need addressing
// until NEW_TOKEN support, if ever, is added.
//
// There is no single well-defined wire format for a token (RFC 9000
// §8.1.4: "There is no need for a single well-defined format for the token
// because the server that generates the token also consumes it") -- this
// module's own choice: a random nonce, then an AES-128-GCM-sealed blob of
// RetryTokenClaims, authenticated (not merely encrypted) so a client can
// never forge or usefully tamper with one. AEAD authentication alone
// satisfies §8.1.4's "MUST be difficult to guess" requirement (a GCM tag
// provides far more than the RFC's suggested 128 bits of resistance) without
// needing a SEPARATE random component beyond the nonce GCM itself requires
// for security.

pub const retry_token_key_len = 16

// retry_token_nonce_len matches crypto.aes.AesGcm's own nonce_size() (12
// bytes) -- duplicated as a const rather than computed from a live AesGcm
// instance so token length can be validated before constructing one.
pub const retry_token_nonce_len = 12

// RetryTokenClaims is everything a validated token proves about the
// address-validation attempt it was issued for.
pub struct RetryTokenClaims {
pub:
	// A caller-serialized identifier for the client's source address (IP +
	// port) at issuance time -- opaque to this module; the caller decides
	// the exact byte representation as long as it's produced the same way
	// at issuance and at validation. RFC 9000 §8.1.4: "Tokens sent in
	// Retry packets SHOULD include information that allows the server to
	// verify that the source IP address and port in client packets remain
	// constant."
	client_addr []u8
	// The client's own Destination Connection ID on the Initial packet
	// that provoked the Retry this token was issued in -- becomes the
	// connection's original_destination_connection_id transport parameter
	// once the retried Initial arrives (RFC 9000 §7.3/§18.2), and lets
	// validation reject a token replayed against a DIFFERENT original
	// DCID than the one it was actually issued for.
	original_dcid []u8
	// A caller-supplied MILLISECOND-scale monotonic timestamp recording
	// when this token was issued -- see RetryPacketParams.issued_at_ms's
	// own doc comment (retry.v) for why this is deliberately millisecond-
	// scale rather than this module's usual nanosecond time.sys_mono_now()
	// convention.
	issued_at_ms u64
}

// encode_retry_token_claims serializes RetryTokenClaims to the plaintext
// this module encrypts -- a simple length-prefixed layout, since (per this
// file's own doc comment) there is no wire-interop requirement to satisfy,
// only round-trip fidelity with decode_retry_token_claims.
fn encode_retry_token_claims(c RetryTokenClaims) ![]u8 {
	if c.client_addr.len > 0xff {
		return error('quic: retry token client_addr too long: ${c.client_addr.len} bytes')
	}
	if c.original_dcid.len > 0xff {
		return error('quic: retry token original_dcid too long: ${c.original_dcid.len} bytes')
	}
	mut out := []u8{}
	out << u8(c.client_addr.len)
	out << c.client_addr
	out << u8(c.original_dcid.len)
	out << c.original_dcid
	out << u8(c.issued_at_ms >> 56)
	out << u8(c.issued_at_ms >> 48)
	out << u8(c.issued_at_ms >> 40)
	out << u8(c.issued_at_ms >> 32)
	out << u8(c.issued_at_ms >> 24)
	out << u8(c.issued_at_ms >> 16)
	out << u8(c.issued_at_ms >> 8)
	out << u8(c.issued_at_ms)
	return out
}

fn decode_retry_token_claims(buf []u8) !RetryTokenClaims {
	if buf.len < 1 {
		return error('quic: truncated retry token claims: missing client_addr length')
	}
	mut cursor := 0
	addr_len := int(buf[cursor])
	cursor += 1
	if cursor + addr_len > buf.len {
		return error('quic: truncated retry token claims: client_addr declares ${addr_len} bytes exceeding the remaining buffer')
	}
	client_addr := buf[cursor..cursor + addr_len].clone()
	cursor += addr_len

	if cursor >= buf.len {
		return error('quic: truncated retry token claims: missing original_dcid length')
	}
	dcid_len := int(buf[cursor])
	cursor += 1
	if cursor + dcid_len > buf.len {
		return error('quic: truncated retry token claims: original_dcid declares ${dcid_len} bytes exceeding the remaining buffer')
	}
	original_dcid := buf[cursor..cursor + dcid_len].clone()
	cursor += dcid_len

	if buf.len - cursor != 8 {
		return error('quic: truncated retry token claims: need exactly 8 bytes for issued_at_ms, have ${buf.len - cursor}')
	}
	issued_at_ms := (u64(buf[cursor]) << 56) | (u64(buf[cursor + 1]) << 48) | (u64(buf[cursor + 2]) << 40) | (u64(buf[
		cursor + 3]) << 32) | (u64(buf[cursor + 4]) << 24) | (u64(buf[cursor + 5]) << 16) | (u64(buf[
		cursor + 6]) << 8) | u64(buf[cursor + 7])

	return RetryTokenClaims{
		client_addr:   client_addr
		original_dcid: original_dcid
		issued_at_ms:  issued_at_ms
	}
}

// generate_retry_token produces a fresh, authenticated address-validation
// token for `claims`, encrypted under `key` (exactly retry_token_key_len
// bytes -- a server-instance-local secret the caller generates once, e.g.
// via crypto.rand.bytes(retry_token_key_len), and keeps ONLY on the server;
// RFC 9000 §8.1.4: "Only the server requires access to the integrity
// protection key for tokens"). The nonce is freshly randomized every call
// via crypto.rand -- the OS-backed CSPRNG this file imports, deliberately
// NOT V's general-purpose `rand` module (wyrand-backed, not cryptographically
// secure) -- and prepended to the sealed output, since AES-GCM requires a
// unique nonce per encryption under the same key and the server has no
// per-token state to derive one from statelessly.
pub fn generate_retry_token(key []u8, claims RetryTokenClaims) ![]u8 {
	if key.len != retry_token_key_len {
		return error('quic: retry token key must be exactly ${retry_token_key_len} bytes, got ${key.len}')
	}
	nonce := rand.bytes(retry_token_nonce_len)!
	plaintext := encode_retry_token_claims(claims)!
	aead := aes.new_aes_gcm(key)!
	sealed := aead.encrypt(plaintext, nonce, []u8{})!

	mut token := []u8{cap: nonce.len + sealed.len}
	token << nonce
	token << sealed
	return token
}

// validate_retry_token authenticates and decrypts `token`, returning its
// claims. A failure here (bad key, tampered bytes, or truncated input)
// means the token must be rejected outright -- RFC 9000 §8.1.2: "If a
// server receives a client Initial that contains an invalid Retry token...
// the server SHOULD immediately close the connection with an INVALID_TOKEN
// error." This function only performs the CRYPTOGRAPHIC check; the caller
// (validate_retry_token_for_attempt, below, or a future connection-
// acceptance path) is responsible for the address/expiry checks that need
// context this function doesn't have.
pub fn validate_retry_token(key []u8, token []u8) !RetryTokenClaims {
	if key.len != retry_token_key_len {
		return error('quic: retry token key must be exactly ${retry_token_key_len} bytes, got ${key.len}')
	}
	if token.len <= retry_token_nonce_len {
		return error('quic: retry token too short to contain a nonce and sealed payload')
	}
	nonce := token[..retry_token_nonce_len]
	sealed := token[retry_token_nonce_len..]
	aead := aes.new_aes_gcm(key)!
	plaintext := aead.decrypt(sealed, nonce, []u8{})!
	return decode_retry_token_claims(plaintext)!
}

// validate_retry_token_for_attempt is validate_retry_token plus the two
// context-dependent checks RFC 9000 §8.1.4 calls for: the token's bound
// client address must match where THIS Initial actually arrived from (a
// changed source address invalidates the token even if cryptographically
// genuine -- it was issued for a different address), and it must not have
// expired. `max_age_ms` is the caller's chosen short-lived window (RFC 9000
// §8.1.4: "Servers SHOULD ensure that tokens sent in Retry packets are only
// accepted for a short time, as they are returned immediately by clients").
//
// `now_ms`/the claims' own `issued_at_ms` are both MILLISECOND-scale
// monotonic instants from the SAME continuously-running server process
// that issued the token (deliberately NOT this module's usual nanosecond
// time.sys_mono_now() convention -- see issued_at_ms's own doc comment
// above for why) -- under that assumption `now_ms` can never legitimately
// precede `issued_at_ms`. The check below still treats that case as
// expired rather than underflowing the u64 subtraction, purely as a
// defensive guard against a caller passing an inconsistent `now_ms`, not
// because it's an expected code path.
//
// Single-use / replay-prevention beyond this short expiry window is NOT
// implemented -- RFC 9000 §8.1.4's "MUST ensure that replay of tokens is
// prevented or limited" (the OR: limited) is what a short max_age_ms
// satisfies; full single-use tracking would need either persistent server
// state (contradicting the stateless design this module otherwise follows)
// or a short-lived consumed-token cache, which belongs with 13d's
// connection-acceptance path once a real listening socket exists to own
// that cache's lifetime, not this stateless primitive.
pub fn validate_retry_token_for_attempt(key []u8, token []u8, client_addr []u8, now_ms u64, max_age_ms u64) !RetryTokenClaims {
	claims := validate_retry_token(key, token)!
	if claims.client_addr != client_addr {
		return error('quic: retry token was issued for a different client address')
	}
	if now_ms < claims.issued_at_ms || now_ms - claims.issued_at_ms > max_age_ms {
		return error('quic: retry token has expired')
	}
	return claims
}
