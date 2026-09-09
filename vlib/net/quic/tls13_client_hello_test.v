// vtest build: !sanitize-memory-clang
module quic

import encoding.hex
import crypto.ecdsa

// RFC 8448 §3's own ClientHello contains sub-structures that are wire-
// identical to what this file's encoders must produce for the SAME
// extension with the SAME input, even though RFC 8448's overall
// ClientHello differs from ours (x25519 instead of P-256, no QUIC
// extension, a different signature_algorithms/supported_groups list) --
// these two are exact, real, independently-sourced cross-checks, not
// self-referential round trips.
const rfc8448_supported_versions_extension = '002b0003020304'
const rfc8448_server_name_extension_hostname_server = '0000000b0009000006736572766572'

fn test_encode_supported_versions_extension_matches_rfc8448_vector() {
	got := encode_supported_versions_extension()!
	assert got == hex.decode(rfc8448_supported_versions_extension)!
}

fn test_encode_server_name_extension_matches_rfc8448_vector() {
	got := encode_server_name_extension('server')!
	assert got == hex.decode(rfc8448_server_name_extension_hostname_server)!
}

fn test_encode_server_name_extension_rejects_empty_hostname() {
	encode_server_name_extension('') or {
		assert err.msg().contains('out of range')
		return
	}
	assert false, 'expected an error for an empty hostname'
}

// test_encode_server_name_extension_rejects_hostname_near_65535_bound is a
// regression test for a Codex finding (vlang/v#27680
// pullrequestreview-4806500473): the bound only checked
// `name_bytes.len > 0xffff`, ignoring that a hostname entry (NameType(1) +
// length(2) + name_bytes) is ITSELF wrapped in the ServerNameList's own
// length(2) prefix -- 5 bytes of combined overhead that must fit in the
// SAME u16 space as `data` overall (enforced generically by
// encode_extension). Empirically, encode_extension's own generic guard
// already rejected every value this loose bound let through (so no
// malformed ClientHello was ever actually producible), but with a
// misattributed "extension data too large" message instead of naming the
// hostname; this fix makes the bound and the message correct at the right
// layer.
fn test_encode_server_name_extension_rejects_hostname_near_65535_bound() {
	encode_server_name_extension('a'.repeat(65531)) or {
		assert err.msg().contains('out of range')
		return
	}
	assert false, 'expected an error for a hostname whose entry would overflow the available u16 space'
}

fn test_encode_server_name_extension_accepts_boundary_hostname_length() {
	// 65530 is the largest hostname length that still leaves room for the
	// 5 bytes of combined NameType/length/ServerNameList-length overhead
	// under the u16 space encode_extension's own `data` is limited to:
	// server_name.len = 3+65530 = 65533, data.len = 2+65533 = 65535 (fits
	// exactly).
	got := encode_server_name_extension('a'.repeat(65530))!
	declared_len := (u32(got[4]) << 8) | u32(got[5])
	assert declared_len == u32(65533)
	assert got.len - 6 == 65533
}

fn test_encode_supported_groups_extension_wire_format() {
	got := encode_supported_groups_extension()!
	// type=000a, ext_data_len=0004, named_group_list_len=0002, secp256r1=0017
	assert got == [u8(0x00), 0x0a, 0x00, 0x04, 0x00, 0x02, 0x00, 0x17]
}

fn test_encode_signature_algorithms_extension_wire_format() {
	got := encode_signature_algorithms_extension()!
	// type=000d, ext_data_len=000a (2+8), list_len=0008 (4 schemes x 2 bytes)
	assert got == [u8(0x00), 0x0d, 0x00, 0x0a, 0x00, 0x08, 0x04, 0x03, 0x08, 0x04, 0x08, 0x05, 0x08,
		0x06]
}

// test_encode_signature_algorithms_cert_extension_wire_format is a
// regression test for a Codex finding (vlang/v#27680
// pullrequestreview-4791164664): the ClientHello never advertised
// signature_algorithms_cert, so a server whose certificate chain is signed
// with a common RSA-PKCS1v1.5 algorithm (still very common among real-world
// CAs) had no matching entry to select against and could refuse the chain
// entirely, even though this client's own certificate-chain verification
// (mbedTLS's generic X.509 verifier) already supports validating such
// signatures -- CertificateVerify itself (signature_algorithms, unchanged)
// stays strict and never accepts RSA-PKCS1v1.5.
fn test_encode_signature_algorithms_cert_extension_wire_format() {
	got := encode_signature_algorithms_cert_extension()!
	// type=0032 (IANA registry value 50), ext_data_len=0014 (2+18),
	// list_len=0012 (9 schemes x 2 bytes): ecdsa_secp256r1_sha256,
	// ecdsa_secp384r1_sha384, ecdsa_secp521r1_sha512 (the latter two added
	// for a Codex finding, vlang/v#27680 pullrequestreview-4806500473),
	// rsa_pss_rsae_sha256/384/512, rsa_pkcs1_sha256/384/512.
	assert got == [u8(0x00), 0x32, 0x00, 0x14, 0x00, 0x12, 0x04, 0x03, 0x05, 0x03, 0x06, 0x03, 0x08,
		0x04, 0x08, 0x05, 0x08, 0x06, 0x04, 0x01, 0x05, 0x01, 0x06, 0x01]
}

fn test_encode_key_share_extension_wire_format() {
	key_exchange := []u8{len: 65, init: 0x42}
	got := encode_key_share_extension(named_group_secp256r1, key_exchange)!
	// entry = group(2) + key_exchange_len(2) + key_exchange(65) = 69 = 0x45.
	// data = client_shares_len_prefix(2) + entry(69) = 71 = 0x47.
	assert got[0..4] == [u8(0x00), 0x33, 0x00, 0x47]
	assert got[4..6] == [u8(0x00), 0x45]
	assert got[6..8] == [u8(0x00), 0x17]
	assert got[8..10] == [u8(0x00), 0x41]
	assert got[10..].len == 65
	assert got[10..] == key_exchange
}

fn test_encode_key_share_extension_rejects_empty_key_exchange() {
	encode_key_share_extension(named_group_secp256r1, []u8{}) or {
		assert err.msg().contains('out of range')
		return
	}
	assert false, 'expected an error for an empty key_exchange'
}

fn test_encode_quic_transport_parameters_extension_wraps_inner_encoding() {
	params := QuicTransportParameters{
		max_idle_timeout: 30000
	}
	inner := encode_transport_parameters(params)!
	got := encode_quic_transport_parameters_extension(params)!
	assert got[0..2] == [u8(0x00), 0x39]
	assert got[2..4] == [u8(inner.len >> 8), u8(inner.len)]
	assert got[4..] == inner
}

fn test_build_client_hello_rejects_wrong_random_length() {
	build_client_hello(ClientHelloParams{
		random: []u8{len: 31}
	}) or {
		assert err.msg().contains('32 bytes')
		return
	}
	assert false, 'expected an error for a random value that is not 32 bytes'
}

// test_build_client_hello_rejects_empty_alpn_protocols is a regression test
// for a Codex finding (vlang/v#27680 pullrequestreview-4781706846): the
// ClientHello never advertised ALPN at all, so a real HTTP/3 server would
// terminate the handshake with no_application_protocol. RFC 9001 §8.1 makes
// ALPN mandatory for QUIC (no fallback protocol-negotiation mechanism), so
// an empty offered-protocols list must be rejected here rather than
// silently producing an ALPN-less ClientHello.
fn test_build_client_hello_rejects_empty_alpn_protocols() {
	build_client_hello(ClientHelloParams{
		random: []u8{len: 32}
		server_name: 'example.com'
		ecdhe_public_key: []u8{len: 65, init: 0x04}
	}) or {
		assert err.msg().contains('ALPN')
		return
	}
	assert false, 'expected an error for an empty ALPN protocol list'
}

// test_build_client_hello_rejects_duplicate_alpn_protocols: RFC 7301 itself
// has no uniqueness requirement for ProtocolNameList entries (checked
// directly against §3.1's text, Codex P2 on vlang/v#27680
// pullrequestreview-4822597219, correctly refuted as an RFC violation at the
// time), but a maintainer requested rejecting duplicates anyway as hygiene
// (discussion_r3690498809): a repeated entry wastes wire bytes for no
// negotiation benefit, since the server can select at most one match no
// matter how many times it appears.
fn test_build_client_hello_rejects_duplicate_alpn_protocols() {
	build_client_hello(ClientHelloParams{
		random: []u8{len: 32}
		server_name: 'example.com'
		ecdhe_public_key: []u8{len: 65, init: 0x04}
		alpn_protocols: ['h3', 'h3']
		transport_parameters: QuicTransportParameters{
			initial_source_connection_id: [u8(1), 2, 3, 4]
		}
	}) or {
		assert err.msg().contains('duplicate')
		return
	}
	assert false, 'expected an error for a duplicate ALPN protocol entry'
}

// test_build_client_hello_accepts_distinct_alpn_protocols is the sibling
// positive case: multiple DISTINCT protocol names must still succeed
// unaffected by the duplicate check above.
fn test_build_client_hello_accepts_distinct_alpn_protocols() {
	client_hello := build_client_hello(ClientHelloParams{
		random: []u8{len: 32}
		server_name: 'example.com'
		ecdhe_public_key: []u8{len: 65, init: 0x04}
		alpn_protocols: ['h3', 'http/1.1']
		transport_parameters: QuicTransportParameters{
			initial_source_connection_id: [u8(1), 2, 3, 4]
		}
	})!
	assert client_hello.len > 0
}

// test_build_client_hello_rejects_missing_initial_source_connection_id is a
// regression test for a gap this session's QUIC conformance-matrix audit
// found (not a Codex report): build_client_hello validated that four
// server-only transport parameters were ABSENT from the client's own
// ClientHello, but never validated that initial_source_connection_id --
// mandatory on BOTH sides per RFC 9000 §7.3 -- was actually PRESENT. Every
// ClientHello this code could produce omitted it, which a conforming server
// MUST treat as TRANSPORT_PARAMETER_ERROR.
fn test_build_client_hello_rejects_missing_initial_source_connection_id() {
	build_client_hello(ClientHelloParams{
		random: []u8{len: 32}
		server_name: 'example.com'
		ecdhe_public_key: []u8{len: 65, init: 0x04}
		alpn_protocols: ['h3']
	}) or {
		assert err.msg().contains('initial_source_connection_id')
		return
	}
	assert false, 'expected an error for a ClientHello missing initial_source_connection_id'
}

fn test_build_client_hello_rejects_original_destination_connection_id() {
	p := ClientHelloParams{
		random: []u8{len: 32}
		server_name: 'example.com'
		ecdhe_public_key: []u8{len: 65, init: 0x04}
		alpn_protocols: ['h3']
		transport_parameters: QuicTransportParameters{
			initial_source_connection_id: [u8(1), 2, 3, 4]
			original_destination_connection_id: [u8(1), 2, 3]
		}
	}
	build_client_hello(p) or {
		assert err.msg().contains('original_destination_connection_id')
		return
	}
	assert false, 'expected an error for a client-side original_destination_connection_id'
}

fn test_build_client_hello_rejects_stateless_reset_token() {
	p := ClientHelloParams{
		random: []u8{len: 32}
		server_name: 'example.com'
		ecdhe_public_key: []u8{len: 65, init: 0x04}
		alpn_protocols: ['h3']
		transport_parameters: QuicTransportParameters{
			initial_source_connection_id: [u8(1), 2, 3, 4]
			stateless_reset_token: []u8{len: 16}
		}
	}
	build_client_hello(p) or {
		assert err.msg().contains('stateless_reset_token')
		return
	}
	assert false, 'expected an error for a client-side stateless_reset_token'
}

fn test_build_client_hello_rejects_preferred_address() {
	p := ClientHelloParams{
		random: []u8{len: 32}
		server_name: 'example.com'
		ecdhe_public_key: []u8{len: 65, init: 0x04}
		alpn_protocols: ['h3']
		transport_parameters: QuicTransportParameters{
			initial_source_connection_id: [u8(1), 2, 3, 4]
			preferred_address: PreferredAddress{
				connection_id: [u8(1), 2, 3]
				stateless_reset_token: []u8{len: 16}
			}
		}
	}
	build_client_hello(p) or {
		assert err.msg().contains('preferred_address')
		return
	}
	assert false, 'expected an error for a client-side preferred_address'
}

fn test_build_client_hello_rejects_retry_source_connection_id() {
	p := ClientHelloParams{
		random: []u8{len: 32}
		server_name: 'example.com'
		ecdhe_public_key: []u8{len: 65, init: 0x04}
		alpn_protocols: ['h3']
		transport_parameters: QuicTransportParameters{
			initial_source_connection_id: [u8(1), 2, 3, 4]
			retry_source_connection_id: [u8(1), 2, 3]
		}
	}
	build_client_hello(p) or {
		assert err.msg().contains('retry_source_connection_id')
		return
	}
	assert false, 'expected an error for a client-side retry_source_connection_id'
}

// test_build_client_hello_structure parses the encoded message back apart
// by hand (there is no production ClientHello parser -- a client never
// needs to parse its own message) and checks every field against RFC
// 8446 §4.1.2's structure, plus that all six expected extensions are
// present with the right types, in a real generated key's presence.
fn test_build_client_hello_structure() {
	priv := ecdsa.PrivateKey.new()!
	defer {
		priv.free()
	}
	pub_key := priv.public_key()!
	defer {
		pub_key.free()
	}
	ecdhe_public_key := pub_key.uncompressed_bytes()!
	random := []u8{len: 32, init: 0xab}

	params := ClientHelloParams{
		random: random
		server_name: 'example.com'
		ecdhe_public_key: ecdhe_public_key
		alpn_protocols: ['h3']
		transport_parameters: QuicTransportParameters{
			initial_source_connection_id: [u8(1), 2, 3, 4]
		}
	}
	encoded := build_client_hello(params)!

	msg, consumed := parse_handshake_message(encoded)!
	assert consumed == encoded.len
	assert msg.typ == .client_hello

	body := msg.body
	mut cursor := 0
	assert body[cursor] == 0x03 && body[cursor + 1] == 0x03 // legacy_version

	cursor += 2
	assert body[cursor..cursor + 32] == random
	cursor += 32
	session_id_len := int(body[cursor])
	assert session_id_len == 0
	cursor += 1 + session_id_len
	cipher_suites_len := int((u32(body[cursor]) << 8) | u32(body[cursor + 1]))
	assert cipher_suites_len == 2
	cursor += 2
	assert body[cursor] == 0x13 && body[cursor + 1] == 0x01 // TLS_AES_128_GCM_SHA256

	cursor += cipher_suites_len
	compression_len := int(body[cursor])
	assert compression_len == 1
	cursor += 1
	assert body[cursor] == 0
	cursor += compression_len
	extensions_len := int((u32(body[cursor]) << 8) | u32(body[cursor + 1]))
	cursor += 2
	assert cursor + extensions_len == body.len

	mut seen_types := []u16{}
	mut alpn_data := []u8{}
	mut ext_cursor := cursor
	for ext_cursor < body.len {
		typ := (u16(body[ext_cursor]) << 8) | u16(body[ext_cursor + 1])
		length := int((u32(body[ext_cursor + 2]) << 8) | u32(body[ext_cursor + 3]))
		if typ == ext_alpn {
			alpn_data = body[ext_cursor + 4..ext_cursor + 4 + length].clone()
		}
		ext_cursor += 4 + length
		seen_types << typ
	}
	assert ext_cursor == body.len
	assert seen_types == [
		ext_server_name,
		ext_supported_versions,
		ext_supported_groups,
		ext_signature_algorithms,
		ext_signature_algorithms_cert,
		ext_alpn,
		ext_key_share,
		ext_quic_transport_parameters,
	]

	// RFC 7301 §3.1 ProtocolNameList: 2-byte list length, then a 1-byte
	// length + bytes per protocol name -- verify the wire bytes, not just
	// that SOME data is present under the right extension type.
	assert alpn_data.len == 5
	list_len := (u32(alpn_data[0]) << 8) | u32(alpn_data[1])
	assert list_len == 3
	assert alpn_data[2] == 2 // protocol name length

	assert alpn_data[3..5].bytestr() == 'h3'
}
