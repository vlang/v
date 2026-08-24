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
			return error_with_code('quic: ${context} contains extension 0x${e.typ:04x}, which this client did not offer or which RFC 8446 §4.2 does not permit here',
				int(tls_alert_to_quic_error(.unsupported_extension)))
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
			cipher_suite:     cipher_suite
			selected_version: selected_version
			selected_group:   selected_group
			cookie:           cookie
			extensions:       extensions
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
		random:                 random
		cipher_suite:           cipher_suite
		selected_version:       selected_version
		key_share_group:        key_share_group
		key_share_key_exchange: ks_ext.data[4..].clone()
		extensions:             extensions
	}
}

// encode_key_share_extension_server encodes the SERVER-side key_share
// payload (RFC 8446 §4.2.8): a BARE KeyShareEntry (group(2) +
// key_exchange_len(2) + key_exchange), with no outer list-length wrapper.
// This is NOT the same shape as tls13_client_hello.v's
// encode_key_share_extension, which wraps its entry in an extra
// client_shares<0..2^16-1> list-length prefix -- the two directions share an
// extension type but not a wire shape, the same asymmetry
// encode_supported_versions_extension_server documents for
// supported_versions. Confirmed against parse_server_hello's own parsing
// (this file), which reads ks_ext.data[0..2] directly as the group with no
// list-length prefix to skip first.
fn encode_key_share_extension_server(group u16, key_exchange []u8) ![]u8 {
	if key_exchange.len == 0 || key_exchange.len > 0xffff - 4 {
		return error('quic: key_exchange length ${key_exchange.len} out of range')
	}
	mut data := []u8{}
	data << u8(group >> 8)
	data << u8(group)
	data << u8(key_exchange.len >> 8)
	data << u8(key_exchange.len)
	data << key_exchange
	return encode_extension(ext_key_share, data)
}

// encode_supported_versions_extension_server encodes the SERVER-side
// supported_versions payload (RFC 8446 §4.2.1): a bare 2-byte
// selected_version, not the ClientHello's length-prefixed version list --
// see parse_supported_versions_from_server's identical distinction on the
// parse side. v1 only ever selects TLS 1.3, matching the single version
// build_client_hello offers.
fn encode_supported_versions_extension_server() ![]u8 {
	mut data := []u8{}
	data << u8(tls_version_1_3 >> 8)
	data << u8(tls_version_1_3)
	return encode_extension(ext_supported_versions, data)
}

// ServerHelloParams is everything build_server_hello needs beyond what's
// fixed by v1's scope decisions (single cipher suite, single selected
// version, single named group).
pub struct ServerHelloParams {
pub:
	// Exactly 32 bytes. Caller supplies so a real caller can use a genuine
	// CSPRNG while tests stay deterministic -- same convention as
	// ClientHelloParams.random. MUST NOT equal the RFC 8446 §4.1.3 magic
	// HelloRetryRequest value; a caller that wants to send an HRR uses
	// build_hello_retry_request (below) instead, never this function with a
	// hand-picked random.
	random []u8
	// This SERVER's own ephemeral ECDHE public key for the selected group
	// (Phase 1 PublicKey.uncompressed_bytes() output, 65 bytes for P-256).
	// v1 only ever selects named_group_secp256r1, matching the single group
	// build_client_hello offers -- a real caller has already confirmed the
	// ClientHello's own key_share offered this group before calling here.
	ecdhe_public_key []u8
}

// build_server_hello constructs a complete, real (non-HelloRetryRequest)
// TLS 1.3 ServerHello handshake message (RFC 8446 §4.1.3), framed via
// encode_handshake_message. Sends exactly two extensions: supported_versions
// and key_share -- the full server_hello_allowed set this same file's
// parse_server_hello enforces on the client side, kept in sync by
// construction rather than duplicated as a separate list. legacy_session_id
// is always echoed as empty: RFC 9001 §8.4 states a server "SHOULD treat the
// receipt of a TLS ClientHello with a non-empty legacy_session_id field as a
// connection error" -- a spec-compliant server that reached this point has
// already rejected any handshake where the client sent a non-empty session
// ID, so there is never a non-empty value to echo back.
pub fn build_server_hello(p ServerHelloParams) ![]u8 {
	if p.random.len != 32 {
		return error('quic: ServerHello random must be exactly 32 bytes, got ${p.random.len}')
	}
	if p.random == hello_retry_request_random[..] {
		return error('quic: ServerHello random must not equal the RFC 8446 §4.1.3 HelloRetryRequest magic value -- use build_hello_retry_request to send an HRR')
	}

	mut body := []u8{}
	// legacy_version MUST be 0x0303 (RFC 8446 §4.1.3), matching
	// build_client_hello's identical fixed value -- the real version is
	// negotiated via supported_versions below.
	body << u8(0x03)
	body << u8(0x03)
	body << p.random
	// legacy_session_id_echo: always empty, see the doc comment above.
	body << u8(0)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256 >> 8)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256)
	// legacy_compression_method MUST be 0 (null), RFC 8446 §4.1.3.
	body << u8(0)

	mut extensions := []u8{}
	extensions << encode_key_share_extension_server(named_group_secp256r1, p.ecdhe_public_key)!
	extensions << encode_supported_versions_extension_server()!

	if extensions.len > 0xffff {
		return error('quic: ServerHello extensions block too large: ${extensions.len} bytes')
	}
	body << u8(extensions.len >> 8)
	body << u8(extensions.len)
	body << extensions

	return encode_handshake_message(.server_hello, body)!
}

// HelloRetryRequestParams is everything build_hello_retry_request needs.
// Both fields are optional -- only supported_versions is mandatory in a
// real HelloRetryRequest (RFC 8446 §4.1.4), mirroring exactly what this
// same file's ParsedHelloRetryRequest.selected_group/cookie already model
// on the parse side.
pub struct HelloRetryRequestParams {
pub:
	// Present when this server is requesting a DIFFERENT group than the one
	// the client's ClientHello key_share offered (RFC 8446 §4.1.4: "the
	// server corrects the mismatch with a HelloRetryRequest"). None when
	// the HRR is purely a cookie round-trip and the client's
	// already-offered key_share is acceptable to this server.
	selected_group ?u16
	// Present when this server wants a stateless retry cookie round-trip
	// (RFC 8446 §4.2.2) instead of holding per-connection state across the
	// two ClientHellos this exchange produces.
	cookie ?[]u8
}

// build_hello_retry_request constructs a complete HelloRetryRequest
// handshake message (RFC 8446 §4.1.4), framed via encode_handshake_message.
// A HelloRetryRequest shares ServerHello's wire TYPE but is distinguished
// by the fixed hello_retry_request_random magic value (this same file) in
// place of a genuine random -- callers must never call build_server_hello
// to send one (that function explicitly rejects this exact value).
// key_share, when present, carries only a bare NamedGroup (RFC 8446 §4.2.8's
// KeyShareHelloRetryRequest), NOT a full KeyShareEntry -- a different, third
// wire shape from both build_server_hello's real-ServerHello key_share
// (encode_key_share_extension_server) and build_client_hello's
// (encode_key_share_extension), matching what this file's own
// parse_server_hello already expects for the HRR branch.
pub fn build_hello_retry_request(p HelloRetryRequestParams) ![]u8 {
	mut body := []u8{}
	body << u8(0x03)
	body << u8(0x03)
	body << hello_retry_request_random[..].clone()
	// legacy_session_id_echo: always empty, see build_server_hello's doc
	// comment (RFC 9001 §8.4).
	body << u8(0)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256 >> 8)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256)
	body << u8(0) // legacy_compression_method

	mut extensions := []u8{}
	extensions << encode_supported_versions_extension_server()!
	if group := p.selected_group {
		mut data := []u8{}
		data << u8(group >> 8)
		data << u8(group)
		extensions << encode_extension(ext_key_share, data)!
	}
	if cookie := p.cookie {
		// RFC 8446 §4.2.2: `opaque cookie<1..2^16-1>` -- same overhead-aware
		// bound style as encode_server_name_extension/
		// encode_key_share_extension (this module), checked against the
		// 2-byte length-prefix overhead this extension's own inner
		// structure adds on top of the raw cookie bytes.
		if cookie.len == 0 || cookie.len > 0xffff - 2 {
			return error('quic: HelloRetryRequest cookie length ${cookie.len} out of range')
		}
		mut data := []u8{}
		data << u8(cookie.len >> 8)
		data << u8(cookie.len)
		data << cookie
		extensions << encode_extension(ext_cookie, data)!
	}

	if extensions.len > 0xffff {
		return error('quic: HelloRetryRequest extensions block too large: ${extensions.len} bytes')
	}
	body << u8(extensions.len >> 8)
	body << u8(extensions.len)
	body << extensions

	// HelloRetryRequest is wire-framed as a server_hello handshake message
	// (RFC 8446 §4.1.4: "it is not a separate message from the perspective
	// of the wire format"), same as parse_server_hello's own type
	// dispatch above.
	return encode_handshake_message(.server_hello, body)!
}

// EncryptedExtensionsParams is everything build_encrypted_extensions needs.
pub struct EncryptedExtensionsParams {
pub:
	// This SERVER's own transport parameters (RFC 9001 §8.2). Every field is
	// the server's own value, not the client's -- e.g.
	// initial_max_stream_data_bidi_local/_remote here describe streams from
	// THIS server's perspective, resolved the same way flow_control.v's
	// initial_send_limit_for_stream/initial_receive_limit_for_stream already
	// do for the connection's actual flow-control windows.
	transport_parameters QuicTransportParameters
	// The single protocol this server selected from the client's ALPN offer
	// list (RFC 7301 §3.2: "the server SHALL include only one protocol name
	// in the ProtocolNameList"). Never empty -- a server with no matching
	// protocol MUST fail the handshake with no_application_protocol (RFC
	// 9001 §8.1) rather than reach this function at all.
	selected_alpn string
	// True when the ClientHello carried a server_name extension this server
	// wants to acknowledge. RFC 6066 §3: the acknowledgement's
	// extension_data is always empty -- this server never echoes the
	// hostname back, matching what parse_encrypted_extensions (this same
	// file, client-role) already requires of a peer.
	acknowledge_server_name bool
}

// build_encrypted_extensions constructs a complete EncryptedExtensions
// handshake message (RFC 8446 §4.3.1: a length-prefixed extension list,
// nothing else), framed via encode_handshake_message. Sends only extensions
// this client's own parse_encrypted_extensions (this same file) actually
// permits -- alpn and quic_transport_parameters unconditionally,
// server_name only when acknowledging one. supported_groups is
// deliberately never sent: v1 offers no session resumption or 0-RTT (Phase
// 14, out of scope), so there is nothing for a future-connection group hint
// to usefully inform.
pub fn build_encrypted_extensions(p EncryptedExtensionsParams) ![]u8 {
	if p.selected_alpn.len == 0 {
		return error('quic: EncryptedExtensions must select exactly one ALPN protocol (RFC 7301 §3.2) -- a server with no match must fail the handshake before reaching here (RFC 9001 §8.1)')
	}
	// RFC 9000 §7.3 / §18.2: "An endpoint MUST treat the absence of the
	// initial_source_connection_id transport parameter from either endpoint
	// ... as a connection error of type TRANSPORT_PARAMETER_ERROR" -- this
	// server's own SCID choice, mirroring build_client_hello's identical
	// check for the client's own value.
	if p.transport_parameters.initial_source_connection_id == none {
		return error('quic: EncryptedExtensions transport parameters must include initial_source_connection_id (RFC 9000 §7.3)')
	}
	// RFC 9000 §7.3/§18.2: "...or the absence of the
	// original_destination_connection_id transport parameter from the
	// server as a connection error of type TRANSPORT_PARAMETER_ERROR" --
	// unlike initial_source_connection_id, this one is server-only and has
	// no client-side analog to mirror.
	if p.transport_parameters.original_destination_connection_id == none {
		return error('quic: EncryptedExtensions transport parameters must include original_destination_connection_id (RFC 9000 §7.3, server-only)')
	}

	mut extensions := []u8{}
	if p.acknowledge_server_name {
		extensions << encode_extension(ext_server_name, []u8{})!
	}
	extensions << encode_alpn_extension([p.selected_alpn])!
	extensions << encode_quic_transport_parameters_extension(p.transport_parameters)!

	if extensions.len > 0xffff {
		return error('quic: EncryptedExtensions block too large: ${extensions.len} bytes')
	}
	mut body := []u8{}
	body << u8(extensions.len >> 8)
	body << u8(extensions.len)
	body << extensions

	return encode_handshake_message(.encrypted_extensions, body)!
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
		return error_with_code('quic: EncryptedExtensions must not contain early_data (0-RTT is not offered)',
			int(tls_alert_to_quic_error(.unsupported_extension)))
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
			return error_with_code('quic: EncryptedExtensions contains extension 0x${e.typ:04x}, which this client did not offer or which RFC 8446 §4.2 does not permit here',
				int(tls_alert_to_quic_error(.unsupported_extension)))
		}
	}
	return extensions
}
