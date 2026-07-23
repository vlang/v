module quic

import encoding.hex

// RFC 8446 §4.4.3's own worked example: transcript hash = 32 bytes of
// 0x01, server context -> this exact 130-byte signed content. Extracted
// programmatically from the raw RFC text, independently reconstructed
// from the pad/context/separator/hash pieces, and cross-checked to match
// byte-for-byte before use (see PROGRESS.md).
const rfc8446_certificate_verify_worked_example = '20202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020202020544c5320312e332c20736572766572204365727469666963617465566572696679000101010101010101010101010101010101010101010101010101010101010101'

fn test_certificate_verify_signed_content_matches_rfc8446_worked_example() {
	transcript_hash := []u8{len: 32, init: 0x01}
	got := certificate_verify_signed_content(.server, transcript_hash)
	assert got == hex.decode(rfc8446_certificate_verify_worked_example)!
}

fn test_certificate_verify_signed_content_client_context_differs() {
	transcript_hash := []u8{len: 32, init: 0x01}
	server_content := certificate_verify_signed_content(.server, transcript_hash)
	client_content := certificate_verify_signed_content(.client, transcript_hash)
	assert server_content != client_content
	assert server_content.len == client_content.len
	// Both share the identical 64-byte pad prefix and the identical
	// transcript hash suffix -- only the context string in the middle
	// differs.
	assert server_content[..64] == client_content[..64]
	assert server_content[server_content.len - 32..] == client_content[client_content.len - 32..]
}

fn build_test_certificate_entry(cert_data []u8, extensions []u8) []u8 {
	mut entry := []u8{}
	entry << u8(cert_data.len >> 16)
	entry << u8(cert_data.len >> 8)
	entry << u8(cert_data.len)
	entry << cert_data
	entry << u8(extensions.len >> 8)
	entry << u8(extensions.len)
	entry << extensions
	return entry
}

fn build_test_certificate_body(ctx []u8, entries []u8) []u8 {
	mut body := []u8{}
	body << u8(ctx.len)
	body << ctx
	body << u8(entries.len >> 16)
	body << u8(entries.len >> 8)
	body << u8(entries.len)
	body << entries
	return body
}

fn test_parse_certificate_single_entry_round_trip() {
	cert_data := []u8{len: 300, init: 0x42}
	entry := build_test_certificate_entry(cert_data, []u8{})
	body := build_test_certificate_body([]u8{}, entry)

	parsed := parse_certificate(body)!
	assert parsed.certificate_request_context.len == 0
	assert parsed.certificate_list.len == 1
	assert parsed.certificate_list[0].cert_data == cert_data
	assert parsed.certificate_list[0].extensions.len == 0
}

fn test_parse_certificate_multiple_entries_with_extensions() {
	leaf_cert := []u8{len: 200, init: 0xaa}
	intermediate_cert := []u8{len: 150, init: 0xbb}

	mut sv := []u8{}
	sv << u8(tls_version_1_3 >> 8)
	sv << u8(tls_version_1_3)
	leaf_ext := encode_extension(ext_supported_versions, sv)!

	leaf_entry := build_test_certificate_entry(leaf_cert, leaf_ext)
	intermediate_entry := build_test_certificate_entry(intermediate_cert, []u8{})
	mut entries := []u8{}
	entries << leaf_entry
	entries << intermediate_entry
	body := build_test_certificate_body([]u8{}, entries)

	parsed := parse_certificate(body)!
	assert parsed.certificate_list.len == 2
	assert parsed.certificate_list[0].cert_data == leaf_cert
	assert parsed.certificate_list[0].extensions.len == 1
	assert parsed.certificate_list[0].extensions[0].typ == ext_supported_versions
	assert parsed.certificate_list[1].cert_data == intermediate_cert
	assert parsed.certificate_list[1].extensions.len == 0
}

fn test_parse_certificate_rejects_empty_certificate_list() {
	body := build_test_certificate_body([]u8{}, []u8{})
	parse_certificate(body) or {
		assert err.msg().contains('must not be empty')
		return
	}
	assert false, 'expected an error for an empty certificate_list'
}

fn test_parse_certificate_rejects_empty_cert_data() {
	entry := build_test_certificate_entry([]u8{}, []u8{})
	body := build_test_certificate_body([]u8{}, entry)
	parse_certificate(body) or {
		assert err.msg().contains('must not be empty')
		return
	}
	assert false, 'expected an error for an empty cert_data'
}

fn test_parse_certificate_rejects_truncated_context() {
	// Declares a 5-byte certificate_request_context but supplies none.
	parse_certificate([u8(0x05)]) or {
		assert err.msg().contains('truncated')
		return
	}
	assert false, 'expected an error for a truncated certificate_request_context'
}

fn test_parse_certificate_rejects_list_length_mismatch() {
	cert_data := []u8{len: 10, init: 0x01}
	entry := build_test_certificate_entry(cert_data, []u8{})
	mut body := build_test_certificate_body([]u8{}, entry)
	// Corrupt the declared certificate_list length (at offset 1..4, since
	// certificate_request_context is empty) to claim one extra byte.
	body[3] += 1
	parse_certificate(body) or {
		assert err.msg().contains('does not match')
		return
	}
	assert false, 'expected an error for a certificate_list length not matching the body'
}

fn build_test_certificate_verify_body(algorithm u16, signature []u8) []u8 {
	mut body := []u8{}
	body << u8(algorithm >> 8)
	body << u8(algorithm)
	body << u8(signature.len >> 8)
	body << u8(signature.len)
	body << signature
	return body
}

fn test_parse_certificate_verify_round_trip() {
	signature := []u8{len: 64, init: 0x99}
	body := build_test_certificate_verify_body(sig_scheme_ecdsa_secp256r1_sha256, signature)
	parsed := parse_certificate_verify(body)!
	assert parsed.algorithm == sig_scheme_ecdsa_secp256r1_sha256
	assert parsed.signature == signature
}

fn test_parse_certificate_verify_accepts_all_offered_algorithms() {
	schemes := [sig_scheme_ecdsa_secp256r1_sha256, sig_scheme_rsa_pss_rsae_sha256,
		sig_scheme_rsa_pss_rsae_sha384, sig_scheme_rsa_pss_rsae_sha512]
	for s in schemes {
		body := build_test_certificate_verify_body(s, [u8(1), 2, 3])
		parsed := parse_certificate_verify(body)!
		assert parsed.algorithm == s
	}
}

fn test_parse_certificate_verify_rejects_unoffered_algorithm() {
	// rsa_pkcs1_sha256 (0x0401) -- a real SignatureScheme, just not one
	// v1 offers.
	body := build_test_certificate_verify_body(0x0401, [u8(1), 2, 3])
	parse_certificate_verify(body) or {
		assert err.msg().contains('not offered')
		return
	}
	assert false, 'expected an error for an algorithm not in signature_algorithms'
}

fn test_parse_certificate_verify_accepts_empty_signature() {
	// RFC 8446 §4.4.3 declares `opaque signature<0..2^16-1>` -- zero
	// length is syntactically legal at the wire-parsing layer, even
	// though no real signature is ever actually empty. Rejecting it here
	// would exceed what the grammar itself requires.
	body := build_test_certificate_verify_body(sig_scheme_ecdsa_secp256r1_sha256, []u8{})
	parsed := parse_certificate_verify(body)!
	assert parsed.signature.len == 0
}

fn test_parse_certificate_verify_rejects_length_mismatch() {
	mut body := build_test_certificate_verify_body(sig_scheme_ecdsa_secp256r1_sha256, [
		u8(1),
		2,
		3,
	])
	body[3] += 1 // claim one extra byte of signature that isn't there
	parse_certificate_verify(body) or {
		assert err.msg().contains('length mismatch')
		return
	}
	assert false, 'expected an error for a signature length not matching the body'
}

fn test_parse_certificate_verify_rejects_truncated_header() {
	parse_certificate_verify([u8(0x04), 0x03, 0x00]) or {
		assert err.msg().contains('truncated')
		return
	}
	assert false, 'expected an error for a header shorter than 4 bytes'
}
