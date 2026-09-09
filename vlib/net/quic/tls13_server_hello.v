module quic

// RFC 8446 §4.2 — extension types needed here but not already declared in
// tls13_client_hello.v.
const ext_cookie = u16(44)
const ext_early_data = u16(42)

// RFC 8446 §4.1.3 — SHA-256("HelloRetryRequest"), used as ServerHello's
// magic Random value to signal a HelloRetryRequest instead of a real
// ServerHello. Independently verified via a live SHA-256 computation of
// the ASCII string during review, not just transcribed from the RFC text.
const hello_retry_request_random = [
	u8(0xcf),
	0x21,
	0xad,
	0x74,
	0xe5,
	0x9a,
	0x61,
	0x11,
	0xbe,
	0x1d,
	0x8c,
	0x02,
	0x1e,
	0x65,
	0xb8,
	0x91,
	0xc2,
	0xa2,
	0x11,
	0x16,
	0x7a,
	0xbb,
	0x8c,
	0x5e,
	0x07,
	0x9e,
	0x09,
	0xe2,
	0xc8,
	0xa8,
	0x33,
	0x9c,
]!

// TlsExtension is one parsed (type, data) entry from a generic TLS
// extension list (RFC 8446 §4.2).
pub struct TlsExtension {
pub:
	typ  u16
	data []u8
}

// parse_extension_list walks a full extension list's inner bytes (the
// concatenated extensions themselves — callers strip whatever length
// prefix wrapped the whole list first) and returns every entry. RFC 8446
// §4.2: "There MUST NOT be more than one extension of the same type in a
// given extension block" — enforced here, mirroring
// transport_parameters.v's duplicate-ID rejection for the analogous
// QUIC-level TLV sequence.
pub fn parse_extension_list(buf []u8) ![]TlsExtension {
	mut extensions := []TlsExtension{}
	mut seen := map[u16]bool{}
	mut cursor := 0
	for cursor < buf.len {
		if buf.len - cursor < 4 {
			return error('quic: truncated extension header: need 4 bytes, have ${buf.len - cursor}')
		}
		typ := u16((u32(buf[cursor]) << 8) | u32(buf[cursor + 1]))
		length := int((u32(buf[cursor + 2]) << 8) | u32(buf[cursor + 3]))
		cursor += 4
		if cursor + length > buf.len {
			return error('quic: extension 0x${typ:x} declares length ${length} exceeding the remaining buffer')
		}
		if typ in seen {
			return error('quic: duplicate extension 0x${typ:x}')
		}
		seen[typ] = true
		extensions << TlsExtension{
			typ: typ
			data: buf[cursor..cursor + length].clone()
		}
		cursor += length
	}
	return extensions
}

// find_extension returns the first extension in `extensions` matching
// `typ`, or none if no such extension is present.
pub fn find_extension(extensions []TlsExtension, typ u16) ?TlsExtension {
	for e in extensions {
		if e.typ == typ {
			return e
		}
	}
	return none
}

// parse_supported_versions_from_server parses the SERVER-side
// supported_versions payload (RFC 8446 §4.2.1): a single 2-byte
// selected_version, NOT the ClientHello's length-prefixed version list —
// the two directions share an extension type but not a wire shape.
fn parse_supported_versions_from_server(data []u8) !u16 {
	if data.len != 2 {
		return error('quic: supported_versions (server) must be exactly 2 bytes, got ${data.len}')
	}
	return u16((u32(data[0]) << 8) | u32(data[1]))
}

// parse_cookie_extension unwraps RFC 8446 §4.2.2's Cookie structure
// (`opaque cookie<1..2^16-1>`) to the raw cookie bytes, which is what a
// future ClientHello re-encoder needs (RFC 8446 §4.1.2: "the client MUST
// copy the contents of the extension received in the HelloRetryRequest
// into a cookie extension in the new ClientHello").
fn parse_cookie_extension(data []u8) ![]u8 {
	if data.len < 2 {
		return error('quic: cookie extension truncated: need at least 2 bytes, have ${data.len}')
	}
	length := int((u32(data[0]) << 8) | u32(data[1]))
	if data.len != 2 + length {
		return error('quic: cookie extension length ${length} does not match remaining data ${data.len - 2}')
	}
	if length == 0 {
		return error('quic: cookie extension must not be empty (opaque cookie<1..2^16-1>)')
	}
	return data[2..].clone()
}

pub struct ParsedServerHello {
pub:
	random                 []u8
	cipher_suite           u16
	selected_version       u16
	key_share_group        u16
	key_share_key_exchange []u8
	extensions             []TlsExtension
}

pub struct ParsedHelloRetryRequest {
pub:
	cipher_suite     u16
	selected_version u16
	// ?u16, not u16: RFC 8446 §4.1.4 lets an HRR request only a cookie
	// round-trip with no key_share at all, when the client's already-
	// offered share is acceptable to the server -- key_share is not
	// mandatory in every HelloRetryRequest, only supported_versions is.
	selected_group ?u16
	cookie         ?[]u8
	extensions     []TlsExtension
}

pub type ServerHelloMessage = ParsedHelloRetryRequest | ParsedServerHello

// parse_server_hello parses a ServerHello-shaped handshake message BODY
// (the bytes after the 4-byte handshake header — callers get this via
// parse_handshake_message) and returns either a ParsedServerHello or a
// ParsedHelloRetryRequest, distinguished by RFC 8446 §4.1.3's magic Random
// value.
//
// Validates every field RFC 8446 §4.1.3 states a client MUST check EXCEPT
// the ones needing state this function doesn't have: whether
// cipher_suite/selected_version was actually offered (v1 offers exactly
// one of each — TLS_AES_128_GCM_SHA256 and TLS 1.3 — so those checks are
// fixed-value comparisons here, not a lookup against a caller-supplied
// offered set), and — for HRR — whether this is a SECOND
// HelloRetryRequest on this connection, which needs connection-level
// state and belongs to Phase 2c's still-pending client state machine.
// legacy_session_id_echo needs no caller-supplied state either: Phase 2c's
// build_client_hello never sends a session ID, so a non-empty echo is
// unconditionally wrong on its own.
// server_hello_allowed / hello_retry_request_allowed are RFC 8446 §4.2's own
// per-message applicability table restricted to a REAL ServerHello vs. a
// HelloRetryRequest respectively -- the two variants share a wire type but
// not an allowed-extension set (cookie is HRR-only; neither ever carries
// alpn/quic_transport_parameters/server_name, which are EncryptedExtensions-
// only). `pre_shared_key` is never included in EITHER: this client offers no
// PSK (0-RTT/resumption is out of scope), so RFC 8446 §4.2's own "MUST NOT
// send an extension response the client didn't request" rule makes it
// illegal here regardless of the per-message table, the same as any other
// unsolicited extension.
const server_hello_allowed = [ext_supported_versions, ext_key_share]
const hello_retry_request_allowed = [ext_supported_versions, ext_key_share, ext_cookie]

// reject_unsolicited_extensions returns an error (carrying RFC 8446 §4.2's
// unsupported_extension QUIC error code via error_with_code, matching
// parse_encrypted_extensions's identical convention) for the first
// extension in `extensions` not present in `allowed`.
fn reject_unsolicited_extensions(extensions []TlsExtension, allowed []u16, context string) ! {
	for e in extensions {
		if e.typ !in allowed {
			return error_with_code('quic: ${context} contains extension 0x${e.typ:04x}, which this client did not offer or which RFC 8446 §4.2 does not permit here', int(tls_alert_to_quic_error(.unsupported_extension)))
		}
	}
}

// parse_server_hello parses a ServerHello handshake message body (RFC 8446
// §4.1.3), returning either a ParsedServerHello or, when `random` matches the
// magic HelloRetryRequest value, a ParsedHelloRetryRequest -- the two share a
// wire type but are validated against distinct mandatory fields and allowed-
// extension sets (see server_hello_allowed/hello_retry_request_allowed
// above). Rejects a non-1.3 selected_version, a non-empty
// legacy_session_id_echo, a missing/malformed key_share, and any extension
// not on the applicable allowlist -- cipher_suite itself is parsed and
// returned but not validated here; the caller (process_server_hello) checks
// it against the single suite this client offers.
pub fn parse_server_hello(body []u8) !ServerHelloMessage {
	if body.len < 2 + 32 + 1 {
		return error('quic: truncated ServerHello: need at least 35 bytes for the fixed prefix, have ${body.len}')
	}
	if body[0] != 0x03 || body[1] != 0x03 {
		return error('quic: ServerHello legacy_version must be 0x0303, got 0x${body[0]:02x}${body[1]:02x}')
	}
	random := body[2..34].clone()
	mut cursor := 34
	session_id_len := int(body[cursor])
	cursor += 1
	if session_id_len != 0 {
		return error('quic: ServerHello legacy_session_id_echo must be empty, got ${session_id_len} bytes')
	}
	if body.len < cursor + 2 + 1 + 2 {
		return error('quic: truncated ServerHello after legacy_session_id_echo')
	}
	cipher_suite := u16((u32(body[cursor]) << 8) | u32(body[cursor + 1]))
	cursor += 2
	if body[cursor] != 0 {
		return error('quic: ServerHello legacy_compression_method must be 0, got ${body[cursor]}')
	}
	cursor += 1
	extensions_len := int((u32(body[cursor]) << 8) | u32(body[cursor + 1]))
	cursor += 2
	if cursor + extensions_len != body.len {
		return error('quic: ServerHello extensions length ${extensions_len} does not match remaining body ${body.len - cursor}')
	}
	extensions := parse_extension_list(body[cursor..])!

	sv_ext := find_extension(extensions, ext_supported_versions) or {
		return error('quic: ServerHello missing mandatory supported_versions extension')
	}
	selected_version := parse_supported_versions_from_server(sv_ext.data)!
	if selected_version != tls_version_1_3 {
		return error('quic: ServerHello selected_version 0x${selected_version:04x} is not TLS 1.3 (0x0304)')
	}

	if random == hello_retry_request_random[..] {
		reject_unsolicited_extensions(extensions, hello_retry_request_allowed, 'HelloRetryRequest')!
		// key_share is NOT mandatory here: RFC 8446 §4.1.4 permits a
		// HelloRetryRequest that requests only a cookie round-trip, when
		// the client's ALREADY-offered key_share is acceptable to the
		// server and the only reason for the retry is anti-DoS (Codex P2,
		// vlang/v#27680 pullrequestreview-4806500473) -- only
		// supported_versions is stated as mandatory.
		mut selected_group := ?u16(none)
		if ks_ext := find_extension(extensions, ext_key_share) {
			// KeyShareHelloRetryRequest carries only a NamedGroup (RFC 8446
			// §4.2.8) — no key_exchange data, unlike a real ServerHello's
			// KeyShareEntry below.
			if ks_ext.data.len != 2 {
				return error('quic: HelloRetryRequest key_share must be exactly 2 bytes (a NamedGroup), got ${ks_ext.data.len}')
			}
			selected_group = u16((u32(ks_ext.data[0]) << 8) | u32(ks_ext.data[1]))
		}
		mut cookie := ?[]u8(none)
		if cookie_ext := find_extension(extensions, ext_cookie) {
			cookie = parse_cookie_extension(cookie_ext.data)!
		}
		return ParsedHelloRetryRequest{
			cipher_suite: cipher_suite
			selected_version: selected_version
			selected_group: selected_group
			cookie: cookie
			extensions: extensions
		}
	}

	reject_unsolicited_extensions(extensions, server_hello_allowed, 'ServerHello')!
	ks_ext := find_extension(extensions, ext_key_share) or {
		return error('quic: ServerHello missing mandatory key_share extension')
	}
	// KeyShareServerHello carries a full KeyShareEntry (RFC 8446 §4.2.8):
	// group(2) + key_exchange_len(2) + key_exchange.
	if ks_ext.data.len < 4 {
		return error('quic: ServerHello key_share truncated: need at least 4 bytes, have ${ks_ext.data.len}')
	}
	key_share_group := u16((u32(ks_ext.data[0]) << 8) | u32(ks_ext.data[1]))
	key_exchange_len := int((u32(ks_ext.data[2]) << 8) | u32(ks_ext.data[3]))
	if ks_ext.data.len != 4 + key_exchange_len {
		return error('quic: ServerHello key_share length mismatch: declares ${key_exchange_len}-byte key_exchange, have ${ks_ext.data.len - 4} bytes')
	}
	if key_exchange_len == 0 {
		return error('quic: ServerHello key_share key_exchange must not be empty (opaque key_exchange<1..2^16-1>)')
	}

	return ParsedServerHello{
		random: random
		cipher_suite: cipher_suite
		selected_version: selected_version
		key_share_group: key_share_group
		key_share_key_exchange: ks_ext.data[4..].clone()
		extensions: extensions
	}
}

// encrypted_extensions_allowed is the intersection of RFC 8446 §4.2's own
// per-message applicability table (only server_name, max_fragment_length,
// supported_groups, use_srtp, heartbeat, alpn, client_certificate_type,
// server_certificate_type, and early_data may EVER appear in
// EncryptedExtensions -- key_share/supported_versions/signature_algorithms
// are ClientHello/ServerHello/HRR-only and are illegal here regardless of
// what was offered) and what this client's own ClientHello actually
// offers (tls13_client_hello.v's build_client_hello): server_name,
// supported_groups, and alpn. quic_transport_parameters is added on top --
// RFC 9001 §8.2 mandates it here, and RFC 8446's own table doesn't cover
// it (registered separately for QUIC-TLS). early_data is deliberately
// excluded even though this client never offers it (0-RTT is out of
// scope) -- kept out of this list rather than silently absent from BOTH
// axes, so its rejection below is explicit and testable.
const encrypted_extensions_allowed = [ext_server_name, ext_supported_groups, ext_alpn,
	ext_quic_transport_parameters]

// parse_encrypted_extensions parses an EncryptedExtensions handshake
// message BODY (RFC 8446 §4.3.1: just a length-prefixed extension list,
// nothing else), then rejects any extension outside
// `encrypted_extensions_allowed` -- RFC 8446 §4.2: "Implementations MUST
// NOT send extension responses if the remote endpoint did not send the
// corresponding extension requests... Upon receiving such an extension,
// an endpoint MUST abort the handshake with an 'unsupported_extension'
// alert." Previously only early_data was checked (a Codex finding,
// vlang/v#27680 pullrequestreview-4783410111, pointed out that other
// EE-illegal extensions like key_share/supported_versions passed through
// unrejected); early_data keeps its own explicit message for a clearer
// diagnostic, but both routes now carry the same `unsupported_extension`
// QUIC CONNECTION_CLOSE code via `error_with_code`, not a generic
// `error()` a caller would otherwise remap to `decode_error`.
pub fn parse_encrypted_extensions(body []u8) ![]TlsExtension {
	if body.len < 2 {
		return error('quic: truncated EncryptedExtensions: need at least 2 bytes, have ${body.len}')
	}
	extensions_len := int((u32(body[0]) << 8) | u32(body[1]))
	if 2 + extensions_len != body.len {
		return error('quic: EncryptedExtensions length ${extensions_len} does not match remaining body ${body.len - 2}')
	}
	extensions := parse_extension_list(body[2..])!
	if _ := find_extension(extensions, ext_early_data) {
		return error_with_code('quic: EncryptedExtensions must not contain early_data (0-RTT is not offered)', int(tls_alert_to_quic_error(.unsupported_extension)))
	}
	if sn_ext := find_extension(extensions, ext_server_name) {
		// RFC 6066 §3: "the server SHALL include an extension of type
		// 'server_name' in the (extended) server hello. The
		// 'extension_data' field of this extension SHALL be empty." A
		// non-empty payload here is a malformed acknowledgement, not a
		// hostname echo -- this client never expects one back (Codex P2,
		// vlang/v#27680 pullrequestreview-4806500473).
		if sn_ext.data.len != 0 {
			return error('quic: EncryptedExtensions server_name extension_data must be empty (RFC 6066 §3), got ${sn_ext.data.len} bytes')
		}
	}
	if sg_ext := find_extension(extensions, ext_supported_groups) {
		// RFC 8446 §4.2.7 / RFC 7919: NamedGroupList is `NamedGroup
		// named_group_list<2..2^16-1>` -- a 2-byte length prefix
		// followed by that many bytes of 2-byte NamedGroup codepoints
		// (this codebase's own encode_supported_groups_extension,
		// tls13_client_hello.v, produces exactly this shape). Only the
		// OUTER extension TLV framing was validated above (by
		// parse_extension_list) -- nothing checked the INNER
		// NamedGroupList's own length prefix against the extension_data
		// actually present, so a malformed inner length (e.g. declaring
		// 2 group-bytes while only 1 is present) parsed successfully
		// into an opaque, never-consumed TlsExtension (Codex P2,
		// vlang/v#27680 pullrequestreview-4822597219). This client never
		// reads this extension's value today -- key exchange is
		// negotiated via key_share, not echoed back through
		// supported_groups -- but accepting a structurally malformed TLS
		// field is wrong independent of whether anything consumes it.
		if sg_ext.data.len < 2 {
			return error('quic: EncryptedExtensions supported_groups extension_data too short: need at least 2 bytes, have ${sg_ext.data.len}')
		}
		inner_len := int((u32(sg_ext.data[0]) << 8) | u32(sg_ext.data[1]))
		if 2 + inner_len != sg_ext.data.len {
			return error('quic: EncryptedExtensions supported_groups NamedGroupList length ${inner_len} does not match remaining extension_data ${sg_ext.data.len - 2}')
		}
		if inner_len == 0 || inner_len % 2 != 0 {
			return error('quic: EncryptedExtensions supported_groups NamedGroupList must be a non-empty, even number of bytes (2-byte NamedGroup entries), got ${inner_len}')
		}
	}
	for e in extensions {
		if e.typ !in encrypted_extensions_allowed {
			return error_with_code('quic: EncryptedExtensions contains extension 0x${e.typ:04x}, which this client did not offer or which RFC 8446 §4.2 does not permit here', int(tls_alert_to_quic_error(.unsupported_extension)))
		}
	}
	return extensions
}
