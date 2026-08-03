module quic

import crypto.hkdf
import crypto.sha256
import hash

// RFC 9001 §5.2 — the QUIC v1 Initial packet protection salt. Used as the
// HKDF-Extract salt when deriving Initial secrets from a connection's
// client-chosen Destination Connection ID. This value is public and fixed
// per QUIC version; it provides domain separation between QUIC versions,
// not secrecy.
pub const initial_salt = [u8(0x38), 0x76, 0x2c, 0xf7, 0xf5, 0x59, 0x34, 0xb3, 0x4d, 0x17, 0x9a,
	0xe6, 0xa4, 0xc8, 0x0c, 0xad, 0xcc, 0xbb, 0x7f, 0x0a]!

// InitialSecrets holds the client and server Initial secrets derived per
// RFC 9001 §5.2. These are inputs to Phase 3's packet-protection key
// derivation (quic_key/quic_iv/quic_hp via `hkdf_expand_label`), not keys
// themselves.
pub struct InitialSecrets {
pub:
	client []u8
	server []u8
}

fn sha256_hash() hash.Hash {
	return sha256.new()
}

// hkdf_expand_label implements TLS 1.3's HKDF-Expand-Label (RFC 8446 §7.1).
// It is the single derivation primitive reused throughout the QUIC-TLS key
// schedule (RFC 9001 §5): Initial secrets here, the full Early/Handshake/
// Master secret chain in Phase 2b, and per-level quic_key/quic_iv/quic_hp in
// Phase 3. `secret` must already be a PRK (an HKDF-Extract or a prior
// HKDF-Expand-Label output) — this function only ever expands, never
// extracts. QUIC never populates the Context field; every call site in this
// module passes an empty context, but the parameter exists for API fidelity
// with RFC 8446 and Phase 2b's `Derive-Secret`, which does use it (as a
// transcript hash).
//
// HkdfLabel wire format (RFC 8446 §7.1):
//
//	uint16 length
//	opaque label<7..255>   = "tls13 " + label
//	opaque context<0..255> = context
pub fn hkdf_expand_label(secret []u8, label string, context []u8, length int) ![]u8 {
	if length < 0 || length > 0xffff {
		return error('quic: hkdf_expand_label: length ${length} out of range')
	}
	full_label := 'tls13 ${label}'
	if full_label.len < 7 || full_label.len > 255 {
		return error('quic: hkdf_expand_label: label length ${full_label.len} out of range')
	}
	if context.len > 255 {
		return error('quic: hkdf_expand_label: context length ${context.len} out of range')
	}
	mut hkdf_label := []u8{cap: 2 + 1 + full_label.len + 1 + context.len}
	hkdf_label << u8(length >> 8)
	hkdf_label << u8(length)
	hkdf_label << u8(full_label.len)
	hkdf_label << full_label.bytes()
	hkdf_label << u8(context.len)
	hkdf_label << context
	return hkdf.expand(sha256_hash, secret, hkdf_label.bytestr(), length)
}

// derive_initial_secrets computes the QUIC v1 Initial packet protection
// secrets (RFC 9001 §5.2) from a connection's client-chosen Destination
// Connection ID.
//
// Callers MUST key this off whatever DCID the client is CURRENTLY using on
// the wire for its Initial packets ("client_dst_connection_id" in RFC
// 9001 §5.2's own terms) -- NOT a fixed "original" value retained across
// the connection's whole lifetime. RFC 9001 §5.2 is explicit: "The
// secrets used for constructing subsequent Initial packets change when a
// server sends a Retry packet to use the connection ID value selected by
// the server." Concretely: before any Retry, `client_dcid` is whatever
// DCID the client itself randomly chose for its first Initial packet;
// after a Retry, the client switches its wire DCID to the Retry packet's
// Source Connection ID (RFC 9000 §17.2.5), and Initial secrets MUST be
// RE-DERIVED from that new value -- a server deriving its own Initial
// keys from the DCID it actually received would otherwise compute
// different keys than a client still using its pre-Retry secrets,
// making every post-Retry Initial packet undecryptable to a conforming
// peer. This is a previously-incorrect claim in this doc comment itself
// (Codex finding, vlang/v#27680 pullrequestreview-4781706846), verified
// against the RFC 9001 §5.2 text directly, not just re-derived from
// memory.
//
// Do NOT confuse this with the SEPARATE, unrelated
// `original_destination_connection_id` transport parameter (RFC 9000
// §7.3) -- that is purely an anti-tampering AUTHENTICATION value the
// server echoes back for the client to verify, carrying no bearing on
// which secrets protect which packets. This function itself is
// DCID-agnostic (it derives from whatever `client_dcid` it's given); it
// is the caller's responsibility (Phase 9's `QuicConn`) to track the
// CURRENT wire DCID and call this again whenever a Retry changes it.
pub fn derive_initial_secrets(client_dcid []u8) !InitialSecrets {
	initial_secret := hkdf.extract(sha256_hash, client_dcid, initial_salt[..])!
	client_secret := hkdf_expand_label(initial_secret, 'client in', []u8{}, sha256.size)!
	server_secret := hkdf_expand_label(initial_secret, 'server in', []u8{}, sha256.size)!
	return InitialSecrets{
		client: client_secret
		server: server_secret
	}
}
