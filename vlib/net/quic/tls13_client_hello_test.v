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

fn test_encode_supported_groups_extension_wire_format() {
	got := encode_supported_groups_extension()!
	// type=000a, ext_data_len=0004, named_group_list_len=0002, secp256r1=0017
	assert got == [u8(0x00), 0x0a, 0x00, 0x04, 0x00, 0x02, 0x00, 0x17]
}

fn test_encode_signature_algorithms_extension_wire_format() {
	got := encode_signature_algorithms_extension()!
	// type=000d, ext_data_len=000a (2+8), list_len=0008 (4 schemes x 2 bytes)
	assert got == [u8(0x00), 0x0d, 0x00, 0x0a, 0x00, 0x08, 0x04, 0x03, 0x08, 0x04, 0x08, 0x05,
		0x08, 0x06]
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

fn test_build_client_hello_rejects_original_destination_connection_id() {
	p := ClientHelloParams{
		random:               []u8{len: 32}
		server_name:          'example.com'
		ecdhe_public_key:     []u8{len: 65, init: 0x04}
		transport_parameters: QuicTransportParameters{
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
		random:               []u8{len: 32}
		server_name:          'example.com'
		ecdhe_public_key:     []u8{len: 65, init: 0x04}
		transport_parameters: QuicTransportParameters{
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
		random:               []u8{len: 32}
		server_name:          'example.com'
		ecdhe_public_key:     []u8{len: 65, init: 0x04}
		transport_parameters: QuicTransportParameters{
			preferred_address: PreferredAddress{
				connection_id:         [u8(1), 2, 3]
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
		random:               []u8{len: 32}
		server_name:          'example.com'
		ecdhe_public_key:     []u8{len: 65, init: 0x04}
		transport_parameters: QuicTransportParameters{
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
		random:               random
		server_name:          'example.com'
		ecdhe_public_key:     ecdhe_public_key
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
	mut ext_cursor := cursor
	for ext_cursor < body.len {
		typ := (u16(body[ext_cursor]) << 8) | u16(body[ext_cursor + 1])
		length := int((u32(body[ext_cursor + 2]) << 8) | u32(body[ext_cursor + 3]))
		ext_cursor += 4 + length
		seen_types << typ
	}
	assert ext_cursor == body.len
	assert seen_types == [
		ext_server_name,
		ext_supported_versions,
		ext_supported_groups,
		ext_signature_algorithms,
		ext_key_share,
		ext_quic_transport_parameters,
	]
}
