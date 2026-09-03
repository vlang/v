module quic

import encoding.hex

fn test_handshake_type_from_u8_accepts_all_known_types() {
	assert handshake_type_from_u8(1)! == .client_hello
	assert handshake_type_from_u8(2)! == .server_hello
	assert handshake_type_from_u8(4)! == .new_session_ticket
	assert handshake_type_from_u8(5)! == .end_of_early_data
	assert handshake_type_from_u8(8)! == .encrypted_extensions
	assert handshake_type_from_u8(11)! == .certificate
	assert handshake_type_from_u8(13)! == .certificate_request
	assert handshake_type_from_u8(15)! == .certificate_verify
	assert handshake_type_from_u8(20)! == .finished
	assert handshake_type_from_u8(24)! == .key_update
	assert handshake_type_from_u8(254)! == .message_hash
}

fn test_handshake_type_from_u8_rejects_tls12_reserved_values() {
	// RFC 8446 §B.3: these are TLS-1.2-era wire values that a TLS 1.3
	// peer must never send; they are not valid-but-unused variants.
	reserved := [u8(0), 3, 6, 12, 14, 16, 21, 22, 23]
	for b in reserved {
		handshake_type_from_u8(b) or {
			assert err.msg().contains('unknown')
			continue
		}
		assert false, 'expected an error for reserved HandshakeType ${b}'
	}
}

fn test_handshake_type_from_u8_rejects_unassigned_value() {
	handshake_type_from_u8(99) or {
		assert err.msg().contains('unknown')
		return
	}
	assert false, 'expected an error for an unassigned HandshakeType byte'
}

fn test_encode_handshake_message_wire_format() {
	body := [u8(0xaa), 0xbb, 0xcc]
	msg := encode_handshake_message(.client_hello, body)!
	assert msg == [u8(0x01), 0x00, 0x00, 0x03, 0xaa, 0xbb, 0xcc]
}

fn test_handshake_message_round_trip() {
	body := []u8{len: 300, init: 0x42}
	encoded := encode_handshake_message(.certificate, body)!

	parsed, consumed := parse_handshake_message(encoded)!
	assert consumed == encoded.len
	assert parsed.typ == .certificate
	assert parsed.body == body
}

fn test_parse_handshake_message_only_consumes_one_message() {
	// A caller loops parse_handshake_message over a reassembled CRYPTO
	// stream that may contain more than one message; this function must
	// not assume `buf` is exactly one message and must report the exact
	// byte count so the caller can advance correctly.
	first := encode_handshake_message(.server_hello, [u8(1), 2, 3])!
	second := encode_handshake_message(.finished, [u8(9), 9])!
	mut combined := []u8{}
	combined << first
	combined << second

	parsed, consumed := parse_handshake_message(combined)!
	assert consumed == first.len
	assert parsed.typ == .server_hello
	assert parsed.body == [u8(1), 2, 3]

	rest := combined[consumed..].clone()
	parsed2, consumed2 := parse_handshake_message(rest)!
	assert consumed2 == second.len
	assert parsed2.typ == .finished
	assert parsed2.body == [u8(9), 9]
}

fn test_parse_handshake_message_rejects_truncated_header() {
	parse_handshake_message([u8(0x01), 0x00, 0x00]) or {
		assert err.msg().contains('truncated')
		return
	}
	assert false, 'expected an error for a header shorter than 4 bytes'
}

fn test_parse_handshake_message_rejects_truncated_body() {
	// Header claims a 5-byte body but only 2 bytes follow.
	parse_handshake_message([u8(0x01), 0x00, 0x00, 0x05, 0xaa, 0xbb]) or {
		assert err.msg().contains('truncated')
		return
	}
	assert false, 'expected an error for a body shorter than the declared length'
}

fn test_encode_handshake_message_rejects_oversized_body() {
	oversized := []u8{len: 0x100_0001} // one byte over the 3-byte length field's max
	encode_handshake_message(.certificate, oversized) or {
		assert err.msg().contains('too large')
		return
	}
	assert false, 'expected an error for a body exceeding the 3-byte length field'
}

fn test_encode_handshake_message_accepts_maximum_length_body() {
	max_body := []u8{len: 0xff_ffff}
	msg := encode_handshake_message(.certificate, max_body)!
	assert msg.len == 4 + 0xff_ffff
	assert msg[1] == 0xff
	assert msg[2] == 0xff
	assert msg[3] == 0xff
}

// RFC 8448 §3 test vectors for the Finished computation — same trace as
// tls13_keyschedule_test.v, extended with EncryptedExtensions/Certificate/
// CertificateVerify bytes and the server's Finished PRK/expanded/finished
// fields. All extracted programmatically from the raw RFC text.
const rfc8448_server_hs_traffic_finished = 'b67b7d690cc16c4e75e54213cb2d37b4e9c912bcded9105d42befd59d391ad38'
const rfc8448_transcript_hash_ch_thru_certverify = 'edb7725fa7a3473b031ec8ef65a2485493900138a2b91291407d7951a06110ed'
const rfc8448_finished_key = '008d3b66f816ea559f96b537e885c31fc068bf492c652f01f288a1d8cdc19fc8'
const rfc8448_server_verify_data = '9b9b141d906337fbd2cbdce71df4deda4ab42c309572cb7fffee5454b78f0718'
const rfc8448_client_hs_traffic_finished = 'b3eddb126e067f35a780b3abf45e2d8f3b1a950738f52e9600746a0e27a55a21'
// Duplicated locally (rather than referenced from tls13_keyschedule_test.v)
// deliberately -- each _test.v file should be self-contained regardless of
// how V's test runner scopes cross-test-file symbol visibility.
const rfc8448_transcript_hash_ch_sh_local = '860c06edc07858ee8e78f0e7428c58edd6b43f2ca3e6e95f02ed063cf0e1cad8'

fn test_compute_finished_verify_data_finished_key_matches_rfc8448_vector() {
	base_secret := hex.decode(rfc8448_server_hs_traffic_finished)!
	finished_key := derive_secret(base_secret, 'finished', []u8{})!
	assert finished_key == hex.decode(rfc8448_finished_key)!
}

fn test_compute_finished_verify_data_matches_rfc8448_vector() {
	base_secret := hex.decode(rfc8448_server_hs_traffic_finished)!
	transcript_hash := hex.decode(rfc8448_transcript_hash_ch_thru_certverify)!
	verify_data := compute_finished_verify_data(base_secret, transcript_hash)!
	assert verify_data == hex.decode(rfc8448_server_verify_data)!
}

fn test_verify_finished_accepts_correct_verify_data() {
	base_secret := hex.decode(rfc8448_server_hs_traffic_finished)!
	transcript_hash := hex.decode(rfc8448_transcript_hash_ch_thru_certverify)!
	peer_verify_data := hex.decode(rfc8448_server_verify_data)!
	assert verify_finished(base_secret, transcript_hash, peer_verify_data)!
}

fn test_verify_finished_rejects_tampered_verify_data() {
	base_secret := hex.decode(rfc8448_server_hs_traffic_finished)!
	transcript_hash := hex.decode(rfc8448_transcript_hash_ch_thru_certverify)!
	mut tampered := hex.decode(rfc8448_server_verify_data)!
	tampered[0] ^= 0xff
	assert verify_finished(base_secret, transcript_hash, tampered)! == false
}

fn test_verify_finished_rejects_wrong_base_secret() {
	// The client's handshake traffic secret used where the server's was
	// required -- a real implementation bug this must catch, not just a
	// deliberately-tampered MAC.
	wrong_secret := hex.decode(rfc8448_client_hs_traffic_finished)!
	transcript_hash := hex.decode(rfc8448_transcript_hash_ch_thru_certverify)!
	peer_verify_data := hex.decode(rfc8448_server_verify_data)!
	assert verify_finished(wrong_secret, transcript_hash, peer_verify_data)! == false
}

fn test_verify_finished_rejects_stale_transcript_hash() {
	// A transcript hash checkpointed too early (e.g. still at
	// ClientHello...ServerHello, before Certificate/CertificateVerify were
	// folded in) must not validate -- this is the "correct transcript
	// checkpoint" edge case the plan calls out explicitly.
	base_secret := hex.decode(rfc8448_server_hs_traffic_finished)!
	stale_transcript_hash := hex.decode(rfc8448_transcript_hash_ch_sh_local)!
	peer_verify_data := hex.decode(rfc8448_server_verify_data)!
	assert verify_finished(base_secret, stale_transcript_hash, peer_verify_data)! == false
}
