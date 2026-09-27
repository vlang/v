module quic

import crypto.hkdf
import crypto.sha256

// RFC 8446 §4.4.1's message_hash HandshakeType, used only as the synthetic
// first entry in a transcript after a HelloRetryRequest.
const hello_retry_request_message_hash_type = u8(0xfe)

// empty_transcript_hash returns Transcript-Hash(""), used for RFC 8446
// §7.1's two "derived" chaining steps (Early Secret -> Handshake Secret,
// Handshake Secret -> Master Secret). Both are defined over an EMPTY
// Messages input, not over any real handshake message — this is the spec's
// fixed value for that case, not a placeholder standing in for one.
pub fn empty_transcript_hash() []u8 {
	return sha256.sum256([]u8{})
}

// derive_secret implements RFC 8446 §7.1's Derive-Secret(Secret, Label,
// Messages):
//
//	Derive-Secret(Secret, Label, Messages) =
//	    HKDF-Expand-Label(Secret, Label, Transcript-Hash(Messages), Hash.length)
//
// The caller supplies the already-computed Transcript-Hash(Messages)
// directly. Accumulating raw handshake message bytes into a running hash —
// and, per RFC 8446 §4.4.1, covering only the handshake message bytes
// themselves with no QUIC/TLS record-layer framing — is Phase 2c's job
// (ClientHello...Finished), not this function's.
pub fn derive_secret(secret []u8, label string, transcript_hash []u8) ![]u8 {
	return hkdf_expand_label(secret, label, transcript_hash, sha256.size)
}

// derive_early_secret returns the TLS 1.3 Early Secret (RFC 8446 §7.1).
// v1 has no PSK/session resumption (0-RTT is Phase 14, out of committed
// scope), so this always takes the spec's no-PSK branch: both the
// HKDF-Extract salt and IKM are absent inputs, each replaced per §7.1's
// rule ("If a given secret is not available, then the 0-value ... is
// used") with a string of Hash.length zero bytes. The salt's zero value is
// supplied implicitly (`hkdf.extract` defaults an empty salt to
// Hash.length zero bytes); the IKM's zero value must be passed explicitly,
// since HKDF-Extract does not treat an empty IKM the same as a zero IKM.
//
// This step is still computed for real, not shortcut to a hardcoded zero
// Early Secret, because its OUTPUT feeds the "derived" Derive-Secret call
// that chains into the Handshake Secret below — skipping the computation
// would assume, rather than prove, that shortcut is equivalent.
pub fn derive_early_secret() ![]u8 {
	zero_ikm := []u8{len: sha256.size}
	return hkdf.extract(sha256_hash, zero_ikm, []u8{})
}

pub struct HandshakeSecrets {
pub:
	handshake_secret []u8
	client_secret    []u8 // client_handshake_traffic_secret
	server_secret    []u8 // server_handshake_traffic_secret
}

// derive_handshake_secrets computes the Handshake Secret and both
// handshake traffic secrets (RFC 8446 §7.1) from the Early Secret, the
// (EC)DHE shared secret, and Transcript-Hash(ClientHello...ServerHello).
pub fn derive_handshake_secrets(early_secret []u8, ecdhe_shared_secret []u8, transcript_hash_ch_sh []u8) !HandshakeSecrets {
	derived := derive_secret(early_secret, 'derived', empty_transcript_hash())!
	handshake_secret := hkdf.extract(sha256_hash, ecdhe_shared_secret, derived)!
	client_secret := derive_secret(handshake_secret, 'c hs traffic', transcript_hash_ch_sh)!
	server_secret := derive_secret(handshake_secret, 's hs traffic', transcript_hash_ch_sh)!
	return HandshakeSecrets{
		handshake_secret: handshake_secret
		client_secret:    client_secret
		server_secret:    server_secret
	}
}

pub struct ApplicationSecrets {
pub:
	master_secret []u8
	client_secret []u8 // client_application_traffic_secret_0
	server_secret []u8 // server_application_traffic_secret_0
}

// derive_application_secrets computes the Master Secret and both
// application traffic secrets (RFC 8446 §7.1) from the Handshake Secret
// and Transcript-Hash(ClientHello...server Finished).
//
// exporter_master_secret and resumption_master_secret are intentionally
// NOT computed here: v1 uses neither TLS exporters (QUIC derives its
// packet-protection keys directly from the traffic secrets returned here,
// via `hkdf_expand_label` with the "quic key"/"quic iv"/"quic hp" labels,
// never through the generic TLS exporter interface) nor session
// resumption (0-RTT is Phase 14, out of committed scope). Adding either
// output now would be untested, unused surface area.
pub fn derive_application_secrets(handshake_secret []u8, transcript_hash_ch_sfin []u8) !ApplicationSecrets {
	derived := derive_secret(handshake_secret, 'derived', empty_transcript_hash())!
	zero_ikm := []u8{len: sha256.size}
	master_secret := hkdf.extract(sha256_hash, zero_ikm, derived)!
	client_secret := derive_secret(master_secret, 'c ap traffic', transcript_hash_ch_sfin)!
	server_secret := derive_secret(master_secret, 's ap traffic', transcript_hash_ch_sfin)!
	return ApplicationSecrets{
		master_secret: master_secret
		client_secret: client_secret
		server_secret: server_secret
	}
}

// synthetic_client_hello1_hash implements RFC 8446 §4.4.1's
// HelloRetryRequest transcript rule: after a HelloRetryRequest, every
// Transcript-Hash computed for the rest of the handshake replaces the
// literal ClientHello1 bytes with a synthetic "message_hash" handshake
// message wrapping Hash(ClientHello1), instead of hashing ClientHello1
// itself twice over (once standalone in an earlier transcript, once as a
// prefix of a later one):
//
//	Transcript-Hash(ClientHello1, HelloRetryRequest, ... Mn) =
//	   Hash(message_hash ||        /* Handshake type */
//	        00 00 Hash.length ||   /* Handshake message length (bytes) */
//	        Hash(ClientHello1)  ||
//	        HelloRetryRequest || ... || Mn)
//
// This function returns that synthetic record's bytes (NOT yet hashed).
// The caller — Phase 2c's transcript accumulator, once a HelloRetryRequest
// is seen — feeds these bytes into the running hash IN PLACE OF the real
// ClientHello1 bytes, then continues normally with HelloRetryRequest and
// every later message appended as-is.
pub fn synthetic_client_hello1_hash(client_hello1 []u8) []u8 {
	digest := sha256.sum256(client_hello1)
	mut msg := []u8{cap: 4 + digest.len}
	msg << hello_retry_request_message_hash_type
	msg << u8(digest.len >> 16)
	msg << u8(digest.len >> 8)
	msg << u8(digest.len)
	msg << digest
	return msg
}
