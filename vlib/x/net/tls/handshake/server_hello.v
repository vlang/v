// Copyright (c) 2026 blackshirt.
//
// server_hello.v decodes ServerHello and HelloRetryRequest messages and checks
// the fields that must agree with the ClientHello (RFC 8446 §4.1.3/§4.1.4).
module handshake

import tls.wire

// hello_retry_request_random is the HRR sentinel random (RFC 8446 §4.1.3); an HRR is
// a ServerHello, so only this fixed value tells them apart.
const hello_retry_request_random = [u8(0xcf), 0x21, 0xad, 0x74, 0xe5, 0x9a, 0x61, 0x11, 0xbe, 0x1d,
	0x8c, 0x02, 0x1e, 0x65, 0xb8, 0x91, 0xc2, 0xa2, 0x11, 0x16, 0x7a, 0xbb, 0x8c, 0x5e, 0x07, 0x9e,
	0x09, 0xe2, 0xc8, 0xa8, 0x33, 0x9c]

// max_hrr_cookie_len bounds the HelloRetryRequest cookie the client must echo
// verbatim in the retry ClientHello. §4.1.4/§5.1 keep that ClientHello inside one
// record, so a near-64 KiB cookie (the wire maximum) could make our own reply
// unframeable; real cookies are tens of bytes, so this stays far below any limit.
const max_hrr_cookie_len = 8 * 1024

// ServerHello holds the parsed ServerHello/HelloRetryRequest fields the client acts on.
// Extension extraction obeys the §4.2 placement allowlist; semantic checks live on validate.
struct ServerHello {
mut:
	session_id_echo []u8          // legacy_session_id echoed from the ClientHello (middlebox compatibility)
	cipher_suite    CipherSuite   // suite selected by the server
	server_share    KeyShareEntry // server's ECDHE public share
	// is_hello_retry_req: set solely by the §4.1.3 sentinel-random comparison.
	is_hello_retry_req bool
	// has_key_share: key_share present; on an HRR it separates group change from cookie-only.
	has_key_share bool
	// cookie holds the cookie value from a HelloRetryRequest, empty otherwise.
	cookie []u8
}

// validate performs the ServerHello/HelloRetryRequest checks independent of the message
// variant (RFC 8446 §4.1.3): the echoed session id must match and the suite must be offered.
fn (sh ServerHello) validate(cfg &ClientHelloConfig) ! {
	// A session-id mismatch means the hello we answer is not the one we sent (illegal_parameter).
	if sh.session_id_echo != cfg.session_id {
		return error('handshake: legacy_session_id_echo does not match ClientHello (illegal_parameter)')
	}
	// An unoffered suite would let an attacker downgrade us to one the state machine never configured.
	if !sh.is_offered_cipher_suite(cfg) {
		return error('handshake: server selected unoffered cipher suite ${sh.cipher_suite} (illegal_parameter)')
	}
}

// is_offered_cipher_suite reports whether the client advertised the suite.
fn (sh ServerHello) is_offered_cipher_suite(cfg &ClientHelloConfig) bool {
	// `in` is a linear scan; the offered list is at most the three TLS 1.3 AEAD suites.
	return sh.cipher_suite in offered_cipher_suites(cfg)
}

// parse_server_hello decodes a ServerHello message body (RFC 8446 §4.1.3).
// §4.2 placement rules: only supported_versions, key_share and (HRR only) cookie may appear.
@[direct_array_access]
fn parse_server_hello(body []u8) !ServerHello {
	// The body follows §4.1.3's fixed field order; a length mismatch desyncs the rest and is caught here.
	mut r := wire.new_reader(body)
	// legacy_version is pinned to 0x0303 (TLS 1.2); the real version is in supported_versions (§4.2.1).
	legacy_version := r.read_u16()!
	if legacy_version != 0x0303 {
		return error('handshake: ServerHello legacy_version must be 0x0303, got 0x${legacy_version:04x}')
	}
	// random: equal to the fixed sentinel means this is a HelloRetryRequest (§4.1.3).
	random := r.read_bytes(client_hello_random_len)!
	// legacy_session_id_echo: must echo our ClientHello's session id (checked in validate).
	session_id_echo := r.read_vector_u8()!
	// cipher_suite: exactly one 2-byte suite, and one we offered (checked in validate).
	cs_raw := r.read_u16()!
	cipher_suite := cipher_suite_from_u16(cs_raw)!
	// legacy_compression_method: TLS 1.3 fixes this to the null method (0).
	compression := r.read_u8()!
	if compression != 0 {
		return error('handshake: compression method must be 0, got ${compression}')
	}

	ext_bytes := r.read_vector_u16()!

	mut got_version := false
	mut selected_version := u16(0)
	mut server_share := KeyShareEntry{}
	mut has_key_share := false
	mut cookie := []u8{}
	is_hrr := random == hello_retry_request_random // §4.1.3: fixed sentinel random identifies an HRR

	exts := parse_extensions(ext_bytes)!
	for ext in exts {
		mut data_r := wire.new_reader(ext.data)

		match ext.typ {
			u16(ExtensionType.supported_versions) {
				// A bare u16 version here, not a length-prefixed vector as in ClientHello (§4.2.1).
				selected_version = data_r.read_u16()!
				got_version = true
				if data_r.remaining() != 0 {
					return error('handshake: trailing bytes in supported_versions extension')
				}
			}
			u16(ExtensionType.key_share) {
				has_key_share = true
				if is_hrr {
					// HelloRetryRequest contains only the selected group (2 bytes, §4.1.4)
					grp := data_r.read_u16()!
					if data_r.remaining() != 0 {
						return error('handshake: trailing bytes in HelloRetryRequest key_share')
					}
					server_share = KeyShareEntry{
						group:        named_group_from_u16(grp)!
						key_exchange: []u8{}
					}
				} else {
					grp := data_r.read_u16()!
					key_ex := data_r.read_vector_u16()!
					if data_r.remaining() != 0 {
						// ServerHello carries exactly one KeyShareEntry (§4.2.8).
						return error('handshake: trailing bytes in ServerHello key_share')
					}
					server_share = KeyShareEntry{
						group:        named_group_from_u16(grp)!
						key_exchange: key_ex
					}
				}
			}
			u16(ExtensionType.cookie) {
				// §4.2: cookie appears in a ServerHello-type message only as part of an HRR.
				if !is_hrr {
					return error('handshake: cookie extension in a normal ServerHello (unsupported_extension)')
				}
				// data_r already wraps this extension's bytes; reuse it.
				cookie = data_r.read_vector_u16()!
				if cookie.len == 0 {
					return error('handshake: HelloRetryRequest cookie must be non-empty')
				}
				if cookie.len > max_hrr_cookie_len {
					return error('handshake: HelloRetryRequest cookie exceeds ${max_hrr_cookie_len} bytes')
				}
				if data_r.remaining() != 0 {
					return error('handshake: trailing bytes in HelloRetryRequest cookie')
				}
			}
			else {
				// The §4.2 allowlist: TLS 1.2 leftovers must never reach client state.
				return error('handshake: extension 0x${ext.typ:04x} not permitted in ServerHello (unsupported_extension)')
			}
		}
	}

	// The negotiated version MUST be confirmed via supported_versions = 0x0304.
	// Distinguish the two failures: an absent mandatory extension is a missing
	// extension (§4.2.1/§4.1.3), not a wrong version — reporting 0x0000 would
	// blame a version the peer never sent.
	if !got_version {
		return error('handshake: ServerHello missing required supported_versions extension (missing_extension)')
	}
	if selected_version != 0x0304 {
		return error('handshake: ServerHello negotiated version must be 0x0304 (TLS 1.3), got 0x${selected_version:04x}')
	}

	return ServerHello{
		session_id_echo:    session_id_echo
		cipher_suite:       cipher_suite
		server_share:       server_share
		is_hello_retry_req: is_hrr
		has_key_share:      has_key_share
		cookie:             cookie
	}
}
