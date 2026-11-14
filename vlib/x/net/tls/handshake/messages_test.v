// Copyright (c) 2026 blackshirt.
//
// Codec tests for the handshake message layer: ClientHello construction, server
// message decoding from hand-built payloads plus the RFC 8448 §3 ServerHello
// example, the §4.4.3 CertificateVerify layout, and extensions framing.
module handshake

import encoding.hex
import tls.wire

// test_build_client_hello checks that the 4-byte handshake header and the body
// length agree with the built ClientHello.
fn test_build_client_hello() ! {
	cfg := ClientHelloConfig{
		server_name:  'example.com'
		alpn:         ['http/1.1']
		client_share: KeyShareEntry{
			group:        .x25519
			key_exchange: []u8{len: 32, init: 0xaa}
		}
	}
	body, full_msg := build_client_hello(cfg)!
	assert full_msg.len == 4 + body.len
	assert full_msg[0] == u8(HandshakeType.client_hello)
	assert full_msg[1] == u8((body.len >> 16) & 0xff)
	assert full_msg[2] == u8((body.len >> 8) & 0xff)
	assert full_msg[3] == u8(body.len & 0xff)
}

// client_hello_error calls the framing primitive and returns its rejection
// message, or '' when the build succeeds. The tuple result of
// build_client_hello cannot be defaulted inside a counted-rejection `or`
// block, so the message is captured through a diverging return instead.
fn client_hello_error(cfg ClientHelloConfig) string {
	_, _ = build_client_hello(cfg) or {
		return err.msg()
	}
	return ''
}

// test_build_client_hello_enforces_legacy_field_sizes guards the framing
// primitive itself: a direct ClientHelloConfig builder bypasses
// validate_handshake_options, so an overlong legacy_session_id (RFC 8446 §4.1.2
// caps it at 32) or a mis-sized fixed Random field must be refused here instead
// of framed onto the wire.
fn test_build_client_hello_enforces_legacy_field_sizes() ! {
	base := ClientHelloConfig{
		server_name:  'example.com'
		client_share: KeyShareEntry{
			group:        .x25519
			key_exchange: []u8{len: 32, init: 0xaa}
		}
	}
	mut cfg := base
	cfg.session_id = []u8{len: 33, init: 0x11} // one past the §4.1.2 cap
	mut rejected := 0
	mut msg := client_hello_error(cfg)
	if msg.contains('legacy_session_id exceeds 32 bytes') {
		rejected++
	}
	cfg = base
	cfg.random = [u8(0x22)] // Random is a fixed 32-byte field; only 0 or 32 is legal
	msg = client_hello_error(cfg)
	if msg.contains('random must be exactly 32 bytes') {
		rejected++
	}
	assert rejected == 2, 'over-cap and mis-sized fixed fields must be refused at build time'
	// A short-but-legal session id (§4.1.2 allows up to 32) must pass through with
	// its own length byte, untouched.
	cfg = base
	cfg.session_id = []u8{len: 16, init: 0x11}
	_, full := build_client_hello(cfg)!
	// Layout: 4-byte handshake header + legacy_version(2) + random(32) puts the
	// legacy_session_id length byte at offset 38.
	assert full[38] == u8(16)
	assert full[39] == u8(0x11)
}

// test_build_client_hello_quic_uses_empty_session_id pins RFC 9001 §8.4: QUIC
// disables middlebox compatibility mode, so legacy_session_id is empty and must not
// be auto-generated the way the record path does. A direct builder supplying a
// non-empty value is refused rather than emitting an illegal parameter.
fn test_build_client_hello_quic_uses_empty_session_id() ! {
	cfg := ClientHelloConfig{
		server_name:           'example.com'
		client_share:          KeyShareEntry{
			group:        .x25519
			key_exchange: []u8{len: 32, init: 0xaa}
		}
		is_quic:               true
		quic_transport_params: []u8{len: 1, init: 0x01}
	}
	_, full := build_client_hello(cfg)!
	// Layout: 4-byte handshake header + legacy_version(2) + random(32) puts the
	// legacy_session_id length byte at offset 38.
	assert full[38] == 0, 'QUIC ClientHello must carry an empty legacy_session_id'

	mut bad := cfg
	bad.session_id = [u8(0x11)]
	msg := client_hello_error(bad)
	assert msg.contains('legacy_session_id must be empty'), 'got: ${msg}'
}

// test_build_client_hello_rejects_malformed_key_share mirrors the peer-side share
// gates onto the client's own key_share: wrong size, a missing uncompressed-point
// marker, or a group absent from supported_groups must be refused at build time,
// not surfacing as the server's illegal_parameter far from the cause.
fn test_build_client_hello_rejects_malformed_key_share() ! {
	base := ClientHelloConfig{
		server_name:  'example.com'
		client_share: KeyShareEntry{
			group:        .x25519
			key_exchange: []u8{len: 32, init: 0xaa}
		}
	}
	mut cfg := base
	cfg.client_share.key_exchange = []u8{len: 31, init: 0xaa}
	mut rejected := 0
	mut msg := client_hello_error(cfg)
	if msg.contains('key_share is 31 bytes, expected 32') {
		rejected++
	}
	cfg = base
	cfg.client_share.group = .secp256r1 // an x25519 blob is the wrong size for P-256
	msg = client_hello_error(cfg)
	if msg.contains('expected 65') {
		rejected++
	}
	cfg = base
	cfg.client_share.group = .secp256r1
	cfg.client_share.key_exchange = []u8{len: 65, init: 0x05} // right size, no point marker
	msg = client_hello_error(cfg)
	if msg.contains('uncompressed point') {
		rejected++
	}
	cfg = base
	cfg.supported_groups = [.secp256r1] // share group not among those offered
	msg = client_hello_error(cfg)
	if msg.contains('not in supported_groups') {
		rejected++
	}
	assert rejected == 4, 'a malformed key_share must be refused at build time'
	// Positive control: a well-formed P-256 share passes the same gate.
	cfg = base
	cfg.client_share.group = .secp256r1
	mut p256 := [u8(0x04)]
	p256 << []u8{len: 64, init: 0x11}
	cfg.client_share.key_exchange = p256
	_, _ = build_client_hello(cfg)!
}

// The handshake is a courier for QUIC transport parameters: it frames the bytes
// its adapter encoded and must not rewrite or interpret them, so these fixtures
// are already-encoded extension content (RFC 9001 §8.2).
fn test_build_client_hello_carries_quic_transport_parameters_verbatim() ! {
	cfg := ClientHelloConfig{
		client_share:          KeyShareEntry{
			group:        .x25519
			key_exchange: []u8{len: 32, init: 0xaa}
		}
		is_quic:               true
		quic_transport_params: [u8(0x0f), 0x02, 0x12, 0x34]
	}
	_, full_msg := build_client_hello(cfg)!
	assert client_hello_extension(full_msg, .quic_transport_parameters)! == [u8(0x0f), 0x02, 0x12,
		0x34]
}

fn test_build_record_client_hello_omits_quic_transport_parameters() ! {
	cfg := ClientHelloConfig{
		client_share:          KeyShareEntry{
			group:        .x25519
			key_exchange: []u8{len: 32, init: 0xaa}
		}
		quic_transport_params: [u8(0x0f), 0x02, 0x12, 0x34]
	}
	_, full_msg := build_client_hello(cfg)!
	if _ := client_hello_extension(full_msg, .quic_transport_parameters) {
		assert false
	}
}

fn test_build_quic_client_hello_requires_transport_parameters() {
	cfg := ClientHelloConfig{
		is_quic:      true
		client_share: KeyShareEntry{
			group:        .x25519
			key_exchange: []u8{len: 32, init: 0xaa}
		}
	}
	if _, _ := build_client_hello(cfg) {
		assert false
	}
}

// TLS does not judge the parameter list: a value the QUIC codec would refuse is
// still framed and sent, leaving rejection to the peer's adapter. This pins the
// courier role so QUIC semantics cannot creep back into the handshake.
fn test_build_quic_client_hello_does_not_judge_transport_parameters() ! {
	cfg := ClientHelloConfig{
		client_share:          KeyShareEntry{
			group:        .x25519
			key_exchange: []u8{len: 32, init: 0xaa}
		}
		is_quic:               true
		quic_transport_params: [u8(0x00)]
	}
	_, full_msg := build_client_hello(cfg)!
	assert client_hello_extension(full_msg, .quic_transport_parameters)! == [u8(0x00)]
}

fn test_parse_encrypted_extensions_quic_transport_parameters() ! {
	// extension block: quic_transport_parameters = { initial_source_connection_id: [0x12, 0x34] }
	body := [u8(0x00), 0x08, 0x00, 0x39, 0x00, 0x04, 0x0f, 0x02, 0x12, 0x34]
	alpn, params := parse_encrypted_extensions_for_transport(body, true)!
	assert alpn == ''
	raw := params or { panic('expected server QUIC transport parameters') }
	assert raw == [u8(0x0f), 0x02, 0x12, 0x34]
}

// Decoding the triplets is the adapter's job. A parameter list this stack would
// reject as malformed still has to survive the handshake intact, so the rejection
// happens once, in quic, with the quic error vocabulary.
fn test_parse_encrypted_extensions_does_not_decode_quic_transport_parameters() ! {
	body := [u8(0x00), 0x08, 0x00, 0x39, 0x00, 0x04, 0x0f, 0x00, 0x0f, 0x00]
	_, params := parse_encrypted_extensions_for_transport(body, true)!
	raw := params or { panic('expected raw QUIC transport parameters') }
	assert raw == [u8(0x0f), 0x00, 0x0f, 0x00]
}

fn test_parse_encrypted_extensions_quic_requires_transport_parameters() {
	if _, _ := parse_encrypted_extensions_for_transport([u8(0x00), 0x00], true) {
		assert false
	}
}

fn client_hello_extension(full_msg []u8, wanted ExtensionType) ![]u8 {
	mut r := wire.new_reader(full_msg[4..])
	_ = r.read_u16()!
	_ = r.read_bytes(client_hello_random_len)!
	_ = r.read_vector_u8()!
	_ = r.read_vector_u16()!
	_ = r.read_vector_u8()!
	ext_bytes := r.read_vector_u16()!
	mut ext_reader := wire.new_reader(ext_bytes)
	for ext_reader.remaining() > 0 {
		typ := ext_reader.read_u16()!
		data := ext_reader.read_vector_u16()!
		if typ == u16(wanted) {
			return data
		}
	}
	return error('test: ClientHello extension ${wanted} not found')
}

fn test_build_client_hello_uses_configured_preferences() ! {
	cfg := ClientHelloConfig{
		client_share:         KeyShareEntry{
			group:        .secp256r1
			key_exchange: []u8{len: 65, init: 0x04}
		}
		supported_groups:     [.secp256r1, .x25519]
		cipher_suites:        [.tls_aes_256_gcm_sha384, .tls_aes_128_gcm_sha256]
		signature_algorithms: [.ed25519, .ecdsa_secp256r1_sha256]
		session_id:           []u8{len: 32, init: 0x11}
		random:               []u8{len: client_hello_random_len, init: 0x22}
	}
	_, full_msg := build_client_hello(cfg)!

	mut groups_reader := wire.new_reader(client_hello_extension(full_msg, .supported_groups)!)
	groups := groups_reader.read_vector_u16()!
	mut group_reader := wire.new_reader(groups)
	assert group_reader.read_u16()! == u16(NamedGroup.secp256r1)
	assert group_reader.read_u16()! == u16(NamedGroup.x25519)
	assert group_reader.remaining() == 0

	mut key_share_reader := wire.new_reader(client_hello_extension(full_msg, .key_share)!)
	shares := key_share_reader.read_vector_u16()!
	mut share_reader := wire.new_reader(shares)
	assert share_reader.read_u16()! == u16(NamedGroup.secp256r1)
	assert share_reader.read_vector_u16()!.len == 65

	mut suites := wire.new_reader(full_msg[4..])
	_ = suites.read_u16()!
	_ = suites.read_bytes(client_hello_random_len)!
	assert suites.read_vector_u8()! == cfg.session_id
	mut offered_suites := wire.new_reader(suites.read_vector_u16()!)
	assert offered_suites.read_u16()! == u16(CipherSuite.tls_aes_256_gcm_sha384)
	assert offered_suites.read_u16()! == u16(CipherSuite.tls_aes_128_gcm_sha256)
	assert offered_suites.remaining() == 0

	mut signature_reader := wire.new_reader(client_hello_extension(full_msg, .signature_algorithms)!)
	schemes := signature_reader.read_vector_u16()!
	mut scheme_reader := wire.new_reader(schemes)
	assert scheme_reader.read_u16()! == u16(SignatureScheme.ed25519)
	assert scheme_reader.read_u16()! == u16(SignatureScheme.ecdsa_secp256r1_sha256)
	assert scheme_reader.remaining() == 0
	assert full_msg[6..6 + client_hello_random_len] == cfg.random
}

// test_parse_rfc8448_server_hello checks that the RFC 8448 §3 ServerHello
// example decodes to its suite and key share.
fn test_parse_rfc8448_server_hello() ! {
	raw_sh_hex := '020000560303a6af06a4121860dc5e6e60249cd34c95930c8ac5cb1434dac155772ed3e2692800130100002e00330024001d0020c9828876112095fe66762bdbf7c672e156d6cc253b833df1dd69b1b04e751f0f002b00020304'
	raw_sh := hex.decode(raw_sh_hex)!

	// Body starts at offset 4
	sh := parse_server_hello(raw_sh[4..])!
	assert sh.is_hello_retry_req == false
	assert sh.cipher_suite == CipherSuite.tls_aes_128_gcm_sha256
	assert sh.server_share.group == NamedGroup.x25519
	assert hex.encode(sh.server_share.key_exchange) == 'c9828876112095fe66762bdbf7c672e156d6cc253b833df1dd69b1b04e751f0f'
}

// test_parse_certificate_chain checks that an empty request_context plus a
// one-certificate list decodes to the single certificate.
fn test_parse_certificate_chain() ! {
	// request_context (empty) + certificate_list holding one 2-byte cert with no extensions
	body := [u8(0x00), 0x00, 0x00, 0x07, 0x00, 0x00, 0x02, 0xde, 0xad, 0x00, 0x00]
	certs := parse_certificate_message(body)!
	assert certs.len == 1
	assert certs[0] == [u8(0xde), 0xad]
}

// test_parse_encrypted_extensions_no_alpn checks that an empty extension list
// yields no ALPN selection.
fn test_parse_encrypted_extensions_no_alpn() ! {
	body := [u8(0x00), 0x00] // empty extension list
	alpn, _ := parse_encrypted_extensions_for_transport(body, false)!
	assert alpn == ''
}

// test_parse_encrypted_extensions_extension_policy checks the §4.3.1 placement
// rules: a key_share extension must abort, while renegotiation_info stays tolerated.
fn test_parse_encrypted_extensions_extension_policy() ! {
	// extensions vector (len 6) = key_share entry with 2 bytes of data
	bad := [u8(0x00), 0x06, 0x00, 0x33, 0x00, 0x02, 0xaa, 0xbb]
	if _, _ := parse_encrypted_extensions_for_transport(bad, false) {
		assert false // 'bad' must be rejected per §4.3.1
	}
	// extensions vector (len 5) = renegotiation_info (0xff01), 1 byte of data
	ok := [u8(0x00), 0x05, 0xff, 0x01, 0x00, 0x01, 0x00]
	alpn, _ := parse_encrypted_extensions_for_transport(ok, false)!
	assert alpn == ''
}

// test_parse_encrypted_extensions_server_name_ack checks that an empty
// server_name is a harmless ack, a non-empty one is rejected (RFC 6066 §3).
fn test_parse_encrypted_extensions_server_name_ack() ! {
	// extensions vector (len 4) = server_name (0x0000) with zero-length data
	empty := [u8(0x00), 0x04, 0x00, 0x00, 0x00, 0x00]
	alpn, _ := parse_encrypted_extensions_for_transport(empty, false)!
	assert alpn == ''
	// extensions vector (len 5) = server_name (0x0000) with 1 byte of data
	bad := [u8(0x00), 0x05, 0x00, 0x00, 0x00, 0x01, 0x00]
	if _, _ := parse_encrypted_extensions_for_transport(bad, false) {
		assert false // a non-empty server_name in EncryptedExtensions must be rejected
	}
}

// test_parse_encrypted_extensions_single_alpn checks the RFC 7301 §3.1
// exactly-one rule: a well-formed ALPN extension is returned verbatim.
fn test_parse_encrypted_extensions_single_alpn() ! {
	// ext_block(0x0f) = alpn(0x0010), list len 0x09 || [len 0x08]"http/1.1"
	body := [u8(0x00), 0x0f, 0x00, 0x10, 0x00, 0x0b, 0x00, 0x09, 0x08, 0x68, 0x74, 0x74, 0x70,
		0x2f, 0x31, 0x2e, 0x31]
	alpn, _ := parse_encrypted_extensions_for_transport(body, false)!
	assert alpn == 'http/1.1'
}

// test_parse_encrypted_extensions_rejects_empty_alpn checks that a zero-length
// protocol list selects nothing, which RFC 7301 §3.1 forbids.
fn test_parse_encrypted_extensions_rejects_empty_alpn() ! {
	body := [u8(0x00), 0x06, 0x00, 0x10, 0x00, 0x02, 0x00, 0x00]
	if _, _ := parse_encrypted_extensions_for_transport(body, false) {
		assert false // empty ALPN list must be rejected
	}
}

// test_parse_encrypted_extensions_rejects_multi_alpn checks that a second
// protocol entry is refused, not silently dropped (RFC 7301 §3.1).
fn test_parse_encrypted_extensions_rejects_multi_alpn() ! {
	// list(0x0c) = [2]"h2" [8]"http/1.1"
	body := [u8(0x00), 0x12, 0x00, 0x10, 0x00, 0x0e, 0x00, 0x0c, 0x02, 0x68, 0x32, 0x08, 0x68,
		0x74, 0x74, 0x70, 0x2f, 0x31, 0x2e, 0x31]
	if _, _ := parse_encrypted_extensions_for_transport(body, false) {
		assert false // multi-protocol ALPN list must be rejected
	}
}

// test_parse_encrypted_extensions_rejects_renegotiation_info_content checks
// that a non-empty renegotiated_connection is rejected (RFC 5746 §3.4).
fn test_parse_encrypted_extensions_rejects_renegotiation_info_content() ! {
	// renegotiation_info(0xff01), data len 4 = [len 3]"abc"
	body := [u8(0x00), 0x08, 0xff, 0x01, 0x00, 0x04, 0x03, 0x61, 0x62, 0x63]
	if _, _ := parse_encrypted_extensions_for_transport(body, false) {
		assert false // non-empty renegotiated_connection must be rejected
	}
}

// test_parse_server_hello_rejects_unpermitted_extension checks that
// supported_versions plus renegotiation_info is refused in ServerHello (§4.2).
fn test_parse_server_hello_rejects_unpermitted_extension() ! {
	mut body := []u8{len: 0, cap: 64}
	body << 0x03
	body << 0x03 // legacy_version
	for _ in 0 .. 32 {
		body << 0x00 // random (not the HRR sentinel)
	}
	body << 0x00 // empty legacy session id
	body << 0x13
	body << 0x01 // TLS_AES_128_GCM_SHA256
	body << 0x00 // null compression
	// extensions block, 11 bytes: supported_versions(=0304) + renegotiation_info
	body << 0x00
	body << 0x0b
	body << 0x00
	body << 0x2b
	body << 0x00
	body << 0x02
	body << 0x03
	body << 0x04
	body << 0xff
	body << 0x01
	body << 0x00
	body << 0x01
	body << 0x00
	if _ := parse_server_hello(body) {
		assert false // renegotiation_info in ServerHello must be rejected per §4.2
	}
}

// test_parse_server_hello_rejects_trailing_key_share_bytes checks that one byte
// past the selected_group is refused (§4.1.4).
fn test_parse_server_hello_rejects_trailing_key_share_bytes() ! {
	mut body := []u8{len: 0, cap: 64}
	body << 0x03
	body << 0x03 // legacy_version
	for b in hello_retry_request_random {
		body << b // sentinel random marks this as an HRR
	}
	body << 0x00 // empty legacy session id
	body << 0x13
	body << 0x01 // TLS_AES_128_GCM_SHA256
	body << 0x00 // null compression
	// extensions, 13 bytes: supported_versions(0304) + key_share x'00 1d FF'
	body << 0x00
	body << 0x0d
	body << 0x00
	body << 0x2b
	body << 0x00
	body << 0x02
	body << 0x03
	body << 0x04
	body << 0x00
	body << 0x33
	body << 0x00
	body << 0x03
	body << 0x00
	body << 0x1d
	body << 0xff // trailing byte after the named group
	if _ := parse_server_hello(body) {
		assert false // then-branch = success; surplus bytes must have been rejected
	}
}

// test_parse_key_update checks the §4.6.3 wire bytes: update_not_requested(0)
// and update_requested(1) round-trip.
fn test_parse_key_update() ! {
	assert parse_key_update(build_key_update(true)[4..])! == true
	assert parse_key_update(build_key_update(false)[4..])! == false
	assert build_key_update(true)[4] == 1 // update_requested(1)
	assert build_key_update(false)[4] == 0 // update_not_requested(0)
	if _ := parse_key_update([]u8{}) {
		assert false
	}
	if _ := parse_key_update([u8(2)]) {
		assert false
	}
}

// test_parse_certificate_verify checks that a scheme plus signature vector
// decodes, pinning the scheme mapping and the vector payload (§4.4.3).
fn test_parse_certificate_verify() ! {
	cv := parse_certificate_verify([u8(0x08), 0x07, 0x00, 0x02, 0xaa, 0xbb])!
	assert cv.algorithm == .ed25519
	assert cv.signature == [u8(0xaa), 0xbb]
}

// test_parse_certificate_verify_rejects_unknown_scheme checks that a scheme
// outside the supported list never reaches the verifier.
fn test_parse_certificate_verify_rejects_unknown_scheme() ! {
	parse_certificate_verify([u8(0x08), 0x99, 0x00, 0x01, 0x00]) or {
		assert err.msg().contains('unsupported signature scheme')
		return
	}
	assert false
}

// test_parse_certificate_verify_rejects_empty_signature checks that a
// zero-length signature vector is refused.
fn test_parse_certificate_verify_rejects_empty_signature() ! {
	parse_certificate_verify([u8(0x08), 0x07, 0x00, 0x00]) or {
		assert err.msg().contains('empty signature')
		return
	}
	assert false
}

// test_parse_certificate_verify_rejects_trailing_bytes checks that one octet
// past the signature vector is rejected (§4.4.3).
fn test_parse_certificate_verify_rejects_trailing_bytes() ! {
	parse_certificate_verify([u8(0x08), 0x07, 0x00, 0x01, 0x42, 0x00]) or {
		assert err.msg().contains('trailing bytes in CertificateVerify')
		return
	}
	assert false
}

// test_client_hello_ip_literal_omits_sni verifies RFC 6066 §3: literal IPv4 and
// IPv6 addresses must not be sent in the server_name (SNI) extension.
fn test_client_hello_ip_literal_omits_sni() ! {
	cfg_ipv4 := ClientHelloConfig{
		server_name:  '192.168.1.1'
		alpn:         ['http/1.1']
		client_share: KeyShareEntry{
			group:        .x25519
			key_exchange: []u8{len: 32, init: 0xaa}
		}
	}
	_, full_ipv4 := build_client_hello(cfg_ipv4)!
	assert !full_ipv4.bytestr().contains('192.168.1.1')

	cfg_ipv6 := ClientHelloConfig{
		server_name:  '2001:db8::1'
		alpn:         ['http/1.1']
		client_share: KeyShareEntry{
			group:        .x25519
			key_exchange: []u8{len: 32, init: 0xaa}
		}
	}
	_, full_ipv6 := build_client_hello(cfg_ipv6)!
	assert !full_ipv6.bytestr().contains('2001:db8::1')
}

// test_parse_certificate_rejects_non_empty_request_context checks that a
// non-zero-length certificate_request_context is refused (RFC 8446 §4.4.2).
fn test_parse_certificate_rejects_non_empty_request_context() ! {
	// request_context of length 1 (value 0x01) followed by a 1-cert list
	bad_cert_msg := [u8(0x01), 0xaa, 0x00, 0x00, 0x07, 0x00, 0x00, 0x02, 0xde, 0xad, 0x00, 0x00]
	if _ := parse_certificate_message(bad_cert_msg) {
		assert false
	}
}

// test_parse_new_session_ticket_bounds verifies ticket_lifetime limit (RFC 8446 §4.6.1).
fn test_parse_new_session_ticket_bounds() ! {
	// ticket_lifetime = 604801 (> 7 days)
	mut w := wire.new_writer(64)
	w.write_u32(604801) // lifetime
	w.write_u32(100) // age_add
	w.write_vector_u8([u8(1), 2])! // nonce
	w.write_vector_u16([u8(0xaa), 0xbb])! // ticket
	w.write_vector_u16([]u8{})! // extensions
	if _ := parse_new_session_ticket(w.bytes()) {
		assert false
	}
}

// test_parse_certificate_message_rejects_overlong_chain checks the parse-time
// depth cap: a chain longer than max_cert_chain_depth is refused.
fn test_parse_certificate_message_rejects_overlong_chain() ! {
	mut list := []u8{}
	for _ in 0 .. 33 {
		list << [u8(0x00), 0x00, 0x01, 0xab, 0x00, 0x00] // cert_len=1 (0xab) + empty extensions
	}
	mut body := []u8{}
	body << u8(0x00) // empty certificate_request_context
	body << u8((list.len >> 16) & 0xff)
	body << u8((list.len >> 8) & 0xff)
	body << u8(list.len & 0xff)
	body << list
	if _ := parse_certificate_message(body) {
		assert false
	}
}

fn test_parse_certificate_message_rejects_malformed_certificate_extensions() ! {
	mut cert_entry := wire.new_writer(16)
	cert_entry.write_vector_u24([u8(0xab)])!
	cert_entry.write_vector_u16([u8(0xff)])!

	mut body := wire.new_writer(32)
	body.write_vector_u8([]u8{})!
	body.write_vector_u24(cert_entry.bytes())!
	parse_certificate_message(body.bytes()) or {
		assert err.msg().contains('unexpected EOF')
		return
	}
	assert false
}

fn test_parse_certificate_message_rejects_duplicate_certificate_extensions() ! {
	mut extensions := wire.new_writer(16)
	for _ in 0 .. 2 {
		extensions.write_u16(0x000a)
		extensions.write_vector_u16([]u8{})!
	}

	mut cert_entry := wire.new_writer(32)
	cert_entry.write_vector_u24([u8(0xab)])!
	cert_entry.write_vector_u16(extensions.bytes())!

	mut body := wire.new_writer(48)
	body.write_vector_u8([]u8{})!
	body.write_vector_u24(cert_entry.bytes())!
	parse_certificate_message(body.bytes()) or {
		assert err.msg().contains('duplicate extension type')
		return
	}
	assert false
}

// test_parse_new_session_ticket_rejects_trailing_bytes checks that one stray
// byte past the extension block is not silently accepted.
fn test_parse_new_session_ticket_rejects_trailing_bytes() ! {
	mut w := wire.new_writer(64)
	w.write_u32(3600) // valid lifetime
	w.write_u32(100) // age_add
	w.write_vector_u8([u8(1), 2])! // nonce
	w.write_vector_u16([u8(0xaa), 0xbb])! // ticket
	w.write_vector_u16([]u8{})! // extensions
	mut bytes := w.bytes()
	bytes << 0xff // one byte beyond the extension vector
	parse_new_session_ticket(bytes) or {
		assert err.msg().contains('trailing bytes in NewSessionTicket')
		return
	}
	assert false
}

// test_certificate_verify_signed_content checks the §4.4.3 signed-content
// layout: 64 spaces, the context string, a 0x00, and the transcript hash.
fn test_certificate_verify_signed_content() {
	th := [u8(0x01), 0x02, 0x03, 0x04]
	content := certificate_verify_signed_content(th)

	// 64 spaces (0x20) + "TLS 1.3, server CertificateVerify" (33 bytes) + 0x00 +
	// transcript_hash (4 bytes)
	assert content.len == 64 + 33 + 1 + 4
	for i in 0 .. 64 {
		assert content[i] == 0x20
	}
	assert content[64..97].bytestr() == 'TLS 1.3, server CertificateVerify'
	assert content[97] == 0x00
	assert content[98..] == th
}

// test_parse_extensions_frames_each_type checks that a mixed extensions block
// decodes with type, exact data slice, and order preserved.
fn test_parse_extensions_frames_each_type() ! {
	// supported_versions(0x002b)=0304 followed by cookie(0x002c)=aabb
	block := [u8(0x00), 0x2b, 0x00, 0x02, 0x03, 0x04, 0x00, 0x2c, 0x00, 0x02, 0xaa, 0xbb]
	exts := parse_extensions(block)!
	assert exts.len == 2
	assert exts[0].typ == u16(ExtensionType.supported_versions)
	assert exts[0].data == [u8(0x03), 0x04]
	assert exts[1].typ == u16(ExtensionType.cookie)
	assert exts[1].data == [u8(0xaa), 0xbb]

	empty := parse_extensions([]u8{})!
	assert empty.len == 0
}

// test_parse_extensions_rejects_duplicate checks that two instances of one
// extension type are rejected outright (RFC 8446 §4.2).
fn test_parse_extensions_rejects_duplicate() ! {
	dup := [u8(0x00), 0x2b, 0x00, 0x02, 0x03, 0x04, 0x00, 0x2b, 0x00, 0x02, 0x03, 0x05]
	if _ := parse_extensions(dup) {
		assert false
	}
}

// test_parse_finished_requires_hash_length checks that a wrong-sized
// verify_data body is a framing error (RFC 8446 §4.4.4).
fn test_parse_finished_requires_hash_length() ! {
	good := parse_finished([]u8{len: 32}, 32)!
	assert good.len == 32
	parse_finished([]u8{len: 31}, 32) or {
		assert err.msg().contains('expected 32 (Hash.length, RFC 8446 §4.4.4)')
		return
	}
	assert false
}
