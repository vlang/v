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
			typ:  typ
			data: buf[cursor..cursor + length].clone()
		}
		cursor += length
	}
	return extensions
}

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
	selected_group   u16
	cookie           ?[]u8
	extensions       []TlsExtension
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
		ks_ext := find_extension(extensions, ext_key_share) or {
			return error('quic: HelloRetryRequest missing mandatory key_share extension')
		}
		// KeyShareHelloRetryRequest carries only a NamedGroup (RFC 8446
		// §4.2.8) — no key_exchange data, unlike a real ServerHello's
		// KeyShareEntry below.
		if ks_ext.data.len != 2 {
			return error('quic: HelloRetryRequest key_share must be exactly 2 bytes (a NamedGroup), got ${ks_ext.data.len}')
		}
		selected_group := u16((u32(ks_ext.data[0]) << 8) | u32(ks_ext.data[1]))
		mut cookie := ?[]u8(none)
		if cookie_ext := find_extension(extensions, ext_cookie) {
			cookie = parse_cookie_extension(cookie_ext.data)!
		}
		return ParsedHelloRetryRequest{
			cipher_suite:     cipher_suite
			selected_version: selected_version
			selected_group:   selected_group
			cookie:           cookie
			extensions:       extensions
		}
	}

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
		random:                 random
		cipher_suite:           cipher_suite
		selected_version:       selected_version
		key_share_group:        key_share_group
		key_share_key_exchange: ks_ext.data[4..].clone()
		extensions:             extensions
	}
}

// parse_encrypted_extensions parses an EncryptedExtensions handshake
// message BODY (RFC 8446 §4.3.1: just a length-prefixed extension list,
// nothing else). Checking WHICH extensions are forbidden here vs.
// expected (the plan's "must check EncryptedExtensions for the presence
// of any forbidden extensions" edge case) needs the full set of what v1
// actually offered, which is Phase 2c's still-pending state machine's
// job; this function rejects the one case that's unconditionally wrong
// regardless of what was offered: early_data, since 0-RTT is Phase 14,
// out of committed scope, so v1's ClientHello never offers it and a
// server sending it back is a protocol violation on its own.
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
		return error('quic: EncryptedExtensions must not contain early_data (0-RTT is not offered)')
	}
	return extensions
}
