// vtest build: present_openssl?
module quic

import crypto.ecdsa
import crypto.sha256
import encoding.base64
import net.mbedtls

// Fixture-handshake harness duplicated from conn_test.v -- deliberately,
// per this module's own established convention documented at the top of
// conn_test.v: each _test.v file is its own independent compilation unit
// (V does not share symbols between sibling test files, only between a
// test file and the module's non-test .v files), confirmed the hard way
// while writing this file (a first draft tried to call conn_test.v's
// drive_to_established directly and failed with "unknown function").
//
// Fixture strategy for THIS file's own tests, once established: compose
// this harness with Phase 10/11's own already-tested encoders -- no new
// low-level byte-fixture authoring needed. A "fake server" here is just
// this test constructing QUIC-level STREAM frames for server-initiated
// stream IDs (uni base 3, step 4: 3, 7, 11...) carrying real h3_frame.v/
// qpack_*.v-encoded bytes, fed through the exact same c.poll(bytes, now)
// path a real datagram would take.

const conn_test_cert_pem = '-----BEGIN CERTIFICATE-----\nMIIEOTCCAyECFG64Q2g46jZb3kRbDOJWX/BwjSp6MA0GCSqGSIb3DQEBCwUAMEUx\nCzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRl\ncm5ldCBXaWRnaXRzIFB0eSBMdGQwIBcNMjMwODAyMTcyOTQyWhgPMjA1MDEyMTcx\nNzI5NDJaMGsxCzAJBgNVBAYTAlVTMRMwEQYDVQQIDApDYWxpZm9ybmlhMRQwEgYD\nVQQHDAtMb3MgQW5nZWxlczEdMBsGA1UECgwUQ2F0YWx5c3QgRGV2ZWxvcG1lbnQx\nEjAQBgNVBAMMCWxvY2FsaG9zdDCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoC\nggIBALqAI4fqUi+QBVWcsXglouLdOML5+w0+1hSR1KdO0Q5XPdQAs/yYWJ+KUkDw\nG++rfy9DUPq7FNRBVurXQkcAtn6gXdllGUSjwUiDo/N4mMOyS/2sufBuaeww7jVi\nrppH+zwP1tUnjRd6khl6bi1Ian9VSzr3Iy9CkXIg1GU4CPXkOydLeoQfepXxWoK1\nOUNwT3VKC/stAfY3j/NIIeiJYkyuRGFCkxn/BUjN+AsXiTugRcYKEFHdIPkOuCXp\nYbhf+lLsczpxCs3rdZG9b/N6mEDCzXTmeHkmsjdPTf+1k5DZZvKzVBBrgdxCgBb7\n5RwjF5v9WmnIc33wWgfJC6FaUzj9NYxYUbPHD+jTz0rJB/jj4u/xJlM/e5NRmXdW\n70pOMKXtWjRSolLOFIPKLY1qs3KMTAZxKKWPDDF7WlMJxMRt7nnnks5yw43Nog4C\njDLk1ZgETnPpLgo3jbmJdIv+OHKTJrBlVvDq7VTyixCoS5G8KoOmyQJhaXG6NwE2\niVhH5JIKgzgCfetfDsnjxqJ/qtrFXPa8FF2TsomD0NK/GZmIcs+9OeVB75Jn5uhF\nfLHScpiTbuu5w3P/LI/MqihLRB6RRNnRzPH8fIg5bYC9b770ta/8GcFRuYE8t+UR\nGtqXJoIKixbDlqV54kal8FQzYzhETf9+NM6Kb/lKEfG/pslvAgMBAAEwDQYJKoZI\nhvcNAQELBQADggEBALI3uNiNO0QE1brA3QYFK+d9ZroB72NrJ0UNkzYHDg2Fc6xg\n4aVVfaxY08+TmKc0JlMOW+pUxeCW/+UBSngdQiR9EE9xm0k0XIrAsy9RXxRvEtPu\nM1VI2h7ayp1Y2BrnQinevTSgtqLRyS1VbOFRl1FiyVvinw2I0KsDdAMNevAPXcOa\nQ8pUgUq6f56DkhocQaj+hxD/uV8HryNxuoSXnPhvfTN3z4YRGzsaWevJ9EYJliOM\n+XugcqfFJ+W7/QCEcAHCL+Bw6OydG5NFORr3p57PXjjcL/uKmxPBrWg2Bz6uT4uR\nMhj0zttiFHLAt9jGfyk6W57UNUja1e1ggftJJhs=\n-----END CERTIFICATE-----\n'

const conn_test_key_pem = '-----BEGIN RSA PRIVATE KEY-----\n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\n-----END RSA PRIVATE KEY-----\n'

fn conn_test_pem_to_der(pem string) []u8 {
	body :=
		pem.replace('-----BEGIN CERTIFICATE-----', '').replace('-----END CERTIFICATE-----', '').replace('\n', '').trim_space()
	return base64.decode(body)
}

fn conn_test_sign_certificate_verify(signed_content []u8) ![]u8 {
	hash := sha256.sum256(signed_content)

	mut ctr_drbg := C.mbedtls_ctr_drbg_context{}
	mut entropy := C.mbedtls_entropy_context{}
	C.mbedtls_ctr_drbg_init(&ctr_drbg)
	C.mbedtls_entropy_init(&entropy)
	defer {
		C.mbedtls_ctr_drbg_free(&ctr_drbg)
		C.mbedtls_entropy_free(&entropy)
	}
	seed_ret := C.mbedtls_ctr_drbg_seed(&ctr_drbg, C.mbedtls_entropy_func, &entropy, 0, 0)
	if seed_ret != 0 {
		return error('test: failed to seed RNG, mbedtls ret: ${seed_ret}')
	}

	mut pk := C.mbedtls_pk_context{}
	C.mbedtls_pk_init(&pk)
	defer {
		C.mbedtls_pk_free(&pk)
	}
	unsafe {
		parse_ret := C.mbedtls_pk_parse_key(&pk, conn_test_key_pem.str, conn_test_key_pem.len + 1,
			0, 0, C.mbedtls_ctr_drbg_random, &ctr_drbg)
		if parse_ret != 0 {
			return error('test: failed to parse RSA private key, mbedtls ret: ${parse_ret}')
		}
	}
	// pk_type 6 = MBEDTLS_PK_RSASSA_PSS, md_alg 9 = MBEDTLS_MD_SHA256.
	mut sig := []u8{len: 600}
	mut sig_len := usize(0)
	ret := C.mbedtls_pk_sign_ext(6, &pk, 9, hash.data, usize(hash.len), sig.data, usize(sig.len),
		&sig_len, C.mbedtls_ctr_drbg_random, &ctr_drbg)
	if ret != 0 {
		return error('test: mbedtls_pk_sign_ext failed, mbedtls ret: ${ret}')
	}
	return sig[..int(sig_len)].clone()
}

fn conn_test_extract_client_hello_key_exchange(client_hello_framed []u8) ![]u8 {
	msg, _ := parse_handshake_message(client_hello_framed)!
	body := msg.body
	mut cursor := 2 + 32 // legacy_version + random
	session_id_len := int(body[cursor])
	cursor += 1 + session_id_len
	cipher_suites_len := int((u32(body[cursor]) << 8) | u32(body[cursor + 1]))
	cursor += 2 + cipher_suites_len
	compression_len := int(body[cursor])
	cursor += 1 + compression_len
	extensions_len := int((u32(body[cursor]) << 8) | u32(body[cursor + 1]))
	cursor += 2
	extensions := parse_extension_list(body[cursor..cursor + extensions_len])!
	ks_ext := find_extension(extensions, ext_key_share) or {
		return error('test: ClientHello missing key_share')
	}
	group := u16((u32(ks_ext.data[2]) << 8) | u32(ks_ext.data[3]))
	if group != named_group_secp256r1 {
		return error('test: unexpected client key_share group 0x${group:04x}')
	}
	key_exchange_len := int((u32(ks_ext.data[4]) << 8) | u32(ks_ext.data[5]))
	return ks_ext.data[6..6 + key_exchange_len].clone()
}

fn conn_test_build_fake_server_hello(server_random []u8, key_exchange []u8) ![]u8 {
	mut body := []u8{}
	body << u8(0x03)
	body << u8(0x03)
	body << server_random
	body << u8(0) // legacy_session_id_echo length = 0
	body << u8(cipher_suite_tls_aes_128_gcm_sha256 >> 8)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256)
	body << u8(0) // legacy_compression_method

	mut sv_data := []u8{}
	sv_data << u8(tls_version_1_3 >> 8)
	sv_data << u8(tls_version_1_3)
	sv_ext := encode_extension(ext_supported_versions, sv_data)!

	mut ks_entry := []u8{}
	ks_entry << u8(named_group_secp256r1 >> 8)
	ks_entry << u8(named_group_secp256r1)
	ks_entry << u8(key_exchange.len >> 8)
	ks_entry << u8(key_exchange.len)
	ks_entry << key_exchange
	ks_ext := encode_extension(ext_key_share, ks_entry)!

	mut extensions := []u8{}
	extensions << sv_ext
	extensions << ks_ext
	body << u8(extensions.len >> 8)
	body << u8(extensions.len)
	body << extensions

	return encode_handshake_message(.server_hello, body)!
}

fn conn_test_build_fake_encrypted_extensions(server_transport_params QuicTransportParameters) ![]u8 {
	encoded_tp := encode_transport_parameters(server_transport_params)!
	tp_ext := encode_extension(ext_quic_transport_parameters, encoded_tp)!

	name_bytes := 'h3'.bytes()
	mut alpn_list := []u8{}
	alpn_list << u8(name_bytes.len)
	alpn_list << name_bytes
	mut alpn_data := []u8{}
	alpn_data << u8(alpn_list.len >> 8)
	alpn_data << u8(alpn_list.len)
	alpn_data << alpn_list
	alpn_ext := encode_extension(ext_alpn, alpn_data)!

	mut extensions := []u8{}
	extensions << alpn_ext
	extensions << tp_ext
	mut body := []u8{}
	body << u8(extensions.len >> 8)
	body << u8(extensions.len)
	body << extensions
	return encode_handshake_message(.encrypted_extensions, body)!
}

fn conn_test_build_fake_certificate(cert_der []u8) ![]u8 {
	mut entry := []u8{}
	entry << u8(cert_der.len >> 16)
	entry << u8(cert_der.len >> 8)
	entry << u8(cert_der.len)
	entry << cert_der
	entry << u8(0) // per-CertificateEntry extensions length = 0
	entry << u8(0)

	mut body := []u8{}
	body << u8(0) // certificate_request_context length = 0
	body << u8(entry.len >> 16)
	body << u8(entry.len >> 8)
	body << u8(entry.len)
	body << entry
	return encode_handshake_message(.certificate, body)!
}

fn conn_test_build_fake_certificate_verify(algorithm u16, signature []u8) ![]u8 {
	mut body := []u8{}
	body << u8(algorithm >> 8)
	body << u8(algorithm)
	body << u8(signature.len >> 8)
	body << u8(signature.len)
	body << signature
	return encode_handshake_message(.certificate_verify, body)!
}

fn conn_test_concat_bytes(a []u8, b []u8) []u8 {
	mut out := []u8{cap: a.len + b.len}
	out << a
	out << b
	return out
}

fn generous_transport_params() QuicTransportParameters {
	return QuicTransportParameters{
		initial_max_data:                    1_000_000
		initial_max_stream_data_bidi_local:  100_000
		initial_max_stream_data_bidi_remote: 100_000
		initial_max_stream_data_uni:         100_000
		initial_max_streams_bidi:            10
		initial_max_streams_uni:             10
	}
}

fn build_fake_long_header_packet(typ LongPacketType, dcid []u8, scid []u8, pn u64, payload []u8, keys QuicPacketProtectionKeys) !QuicDatagram {
	pn_length := 2
	h := QuicLongHeader{
		typ:     typ
		version: quic_v1
		dcid:    dcid
		scid:    scid
		token:   []u8{}
		length:  u64(pn_length) + u64(payload.len) + aead_tag_len
	}
	mut header := encode_long_header(h, 0, u8(pn_length - 1))!
	header << [u8(pn >> 8), u8(pn)]
	protected := protect_packet(header, .long, pn, pn_length, payload, keys)!
	return QuicDatagram{
		bytes: protected
	}
}

fn build_fake_one_rtt_packet(dcid []u8, pn u64, payload []u8, keys QuicPacketProtectionKeys, key_phase bool) !QuicDatagram {
	pn_length := 2
	header_prefix := encode_short_header(dcid, false, 0, key_phase, u8(pn_length - 1))!
	mut header := header_prefix.clone()
	header << [u8(pn >> 8), u8(pn)]
	protected := protect_packet(header, .short, pn, pn_length, payload, keys)!
	return QuicDatagram{
		bytes: protected
	}
}

// drive_to_established is duplicated from conn_test.v verbatim (see this
// file's own top-of-file note) -- dials a real QuicConn and drives it
// against a fake in-memory server all the way through RFC 9001 §4.1.2's
// "confirmed" checkpoint.
fn drive_to_established(own_params QuicTransportParameters, peer_params QuicTransportParameters) !(&QuicConn, []u8, u64) {
	mut now := u64(1000)
	mut c, initial_dg := dial(DialParams{
		server_name:          'example.com'
		ca_bundle_pem:        conn_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: own_params
	}, now)!
	assert initial_dg.bytes.len >= min_initial_datagram_size

	server_initial_scid := [u8(0xaa), 0xbb, 0xcc, 0xdd]

	server_pub, server_priv := ecdsa.generate_key(nid: .prime256v1)!
	defer {
		server_pub.free()
		unsafe { server_priv.free() }
	}
	server_ecdhe_public_bytes := server_pub.uncompressed_bytes()!
	server_random := []u8{len: 32, init: 0x22}
	client_key_exchange := conn_test_extract_client_hello_key_exchange(c.client_hello)!
	client_pub := ecdsa.PublicKey.from_uncompressed_bytes(client_key_exchange, nid: .prime256v1)!
	defer {
		client_pub.free()
	}
	server_shared_secret := server_priv.derive_shared_secret(client_pub)!
	server_hello_framed := conn_test_build_fake_server_hello(server_random,
		server_ecdhe_public_bytes)!

	early_secret := derive_early_secret()!
	ch_sh_hash := sha256.sum256(conn_test_concat_bytes(c.client_hello, server_hello_framed))
	server_handshake_secrets := derive_handshake_secrets(early_secret, server_shared_secret,
		ch_sh_hash)!

	sh_payload := encode_crypto_frame(0, server_hello_framed)!
	sh_datagram := build_fake_long_header_packet(.initial, c.scid, server_initial_scid, 0,
		sh_payload, c.initial_keys_server)!
	result1 := c.poll(sh_datagram.bytes, now)!
	assert result1.events.len == 0
	assert c.handshake.state() == .wait_encrypted_extensions
	now += 10

	hs_keys_server := c.handshake_keys_server or { panic('unreachable: just asserted != none') }
	mut ee_params := peer_params
	ee_params.initial_source_connection_id = server_initial_scid
	ee_params.original_destination_connection_id = c.original_dcid
	ee_framed := conn_test_build_fake_encrypted_extensions(ee_params)!
	ee_payload := encode_crypto_frame(0, ee_framed)!
	ee_datagram := build_fake_long_header_packet(.handshake, c.scid, server_initial_scid, 0,
		ee_payload, hs_keys_server)!
	result2 := c.poll(ee_datagram.bytes, now)!
	assert result2.events.len == 0
	assert c.handshake.state() == .wait_certificate
	now += 10

	server_der := conn_test_pem_to_der(conn_test_cert_pem)
	cert_framed := conn_test_build_fake_certificate(server_der)!
	cert_msg, _ := parse_handshake_message(cert_framed)!
	c.handshake.process_certificate_or_request(cert_msg, cert_framed) or {}
	real_chain := mbedtls.build_certificate_chain([server_der])!
	unsafe {
		c.handshake.verified_chain = &VerifiedCertificateChain{
			chain: real_chain
		}
	}
	c.handshake.certificate_transcript_hash = c.handshake.transcript_hash()
	c.handshake.state = .wait_certificate_verify

	signed_content := certificate_verify_signed_content(.server,
		c.handshake.certificate_transcript_hash)
	sig := conn_test_sign_certificate_verify(signed_content)!
	cv_framed := conn_test_build_fake_certificate_verify(sig_scheme_rsa_pss_rsae_sha256, sig)!
	cv_msg, _ := parse_handshake_message(cv_framed)!
	c.handshake.process_certificate_verify(cv_msg, cv_framed)!
	assert c.handshake.state() == .wait_finished

	finished_verify_data := compute_finished_verify_data(server_handshake_secrets.server_secret,
		c.handshake.transcript_hash())!
	finished_framed := encode_handshake_message(.finished, finished_verify_data)!
	finished_payload := encode_crypto_frame(u64(ee_framed.len), finished_framed)!
	finished_datagram := build_fake_long_header_packet(.handshake, c.scid, server_initial_scid, 1,
		finished_payload, hs_keys_server)!
	result3 := c.poll(finished_datagram.bytes, now)!
	assert result3.events.len == 0
	assert c.handshake.state() == .connected
	now += 10

	read_keys := c.app_read_keys or { panic('unreachable: just asserted != none') }
	server_app_keys := read_keys.current_keys
	mut hs_done_payload := [u8(frame_type_handshake_done)]
	hs_done_payload << [u8(0), 0, 0, 0]
	hs_done_datagram := build_fake_one_rtt_packet(c.scid, 0, hs_done_payload, server_app_keys,
		false)!
	result4 := c.poll(hs_done_datagram.bytes, now)!
	assert result4.events.any(it.kind == .handshake_confirmed)
	assert c.state() == .established
	now += 10

	return c, server_initial_scid, now
}

// -----------------------------------------------------------------------
// H3Conn tests
// -----------------------------------------------------------------------

fn h3_test_conn() !(&QuicConn, &H3Conn, []u8, u64) {
	mut c, server_initial_scid, now := drive_to_established(generous_transport_params(),
		generous_transport_params())!
	mut h := new_h3_conn(mut c, H3ConnParams{
		settings:                     [
			H3Setting{
				identifier: qpack_settings_max_table_capacity_id
				value:      4096
			},
		]
		own_qpack_max_table_capacity: 4096
	})
	// established()/open_own_streams_if_ready need at least one poll() past
	// drive_to_established's own last poll() to actually queue our own
	// control+QPACK streams (queue-now-drain-later, same as QuicConn
	// itself) -- feed `none` to just run the H3Conn-side bookkeeping.
	h.poll(none, now)!
	return c, h, server_initial_scid, now
}

// server_uni_stream_frame builds a STREAM frame for the `n`th server-
// initiated unidirectional stream (RFC 9000 §2.1: base 3, step 4) carrying
// `payload` at offset 0, FIN'd -- for tests that send exactly one shot per
// fake stream.
fn server_uni_stream_frame(n u64, payload []u8) ![]u8 {
	stream_id := u64(3) + n * 4
	return encode_stream_frame(stream_id, 0, payload, true, true)
}

// server_uni_stream_continuation_frame builds a NON-FIN STREAM frame for
// the `n`th server-initiated unidirectional stream at `offset` -- for
// tests that deliver one logical uni stream's bytes across more than one
// poll() call (e.g. simulating an encoder-stream instruction that arrives
// later than an earlier frame on that SAME stream, not a second stream).
fn server_uni_stream_continuation_frame(n u64, offset u64, payload []u8) ![]u8 {
	stream_id := u64(3) + n * 4
	return encode_stream_frame(stream_id, offset, payload, false, true)
}

// read_keys returns c's own current 1-RTT read (== the fake server's
// write) key set -- a small shared helper since every test in this file
// needs it to build a fake-server-originated datagram.
fn read_keys(mut c QuicConn) QuicPacketProtectionKeys {
	rk := c.app_read_keys or { panic('unreachable: established asserts this') }
	return rk.current_keys
}

fn test_h3_conn_established_opens_own_control_and_qpack_streams() {
	mut c, mut h, _, now := h3_test_conn()!
	defer {
		c.handshake.free()
	}
	assert h.established()
	result := h.poll(none, now)!
	assert result.outgoing.len > 0
}

fn test_h3_conn_peer_control_stream_requires_settings_first() {
	mut c, mut h, _, now := h3_test_conn()!
	defer {
		c.handshake.free()
	}
	mut header := encode_h3_control_stream_header()!
	header << encode_goaway_frame(0)!
	frame := server_uni_stream_frame(0, header)!
	datagram := build_fake_one_rtt_packet(c.scid, 0, frame, read_keys(mut c), false)!
	if _ := h.poll(datagram.bytes, now) {
		assert false, 'missing SETTINGS as the first control-stream frame must be a connection error'
	}
}

fn test_h3_conn_peer_control_stream_settings_then_second_settings_rejected() {
	mut c, mut h, _, now := h3_test_conn()!
	defer {
		c.handshake.free()
	}
	mut buf := encode_h3_control_stream_header()!
	buf << encode_settings_frame([]H3Setting{})!
	buf << encode_settings_frame([]H3Setting{})!
	frame := server_uni_stream_frame(0, buf)!
	datagram := build_fake_one_rtt_packet(c.scid, 0, frame, read_keys(mut c), false)!
	if _ := h.poll(datagram.bytes, now) {
		assert false, 'a second SETTINGS frame on the control stream must be a connection error'
	}
}

fn test_h3_conn_peer_control_stream_settings_accepted_and_settings_received_event() {
	mut c, mut h, _, now := h3_test_conn()!
	defer {
		c.handshake.free()
	}
	mut buf := encode_h3_control_stream_header()!
	buf << encode_settings_frame([]H3Setting{})!
	frame := server_uni_stream_frame(0, buf)!
	datagram := build_fake_one_rtt_packet(c.scid, 0, frame, read_keys(mut c), false)!
	result := h.poll(datagram.bytes, now)!
	assert result.events.any(it.kind == .settings_received)
}

fn test_h3_conn_rejects_push_promise_on_control_stream() {
	mut c, mut h, _, now := h3_test_conn()!
	defer {
		c.handshake.free()
	}
	mut buf := encode_h3_control_stream_header()!
	buf << encode_settings_frame([]H3Setting{})!
	buf << encode_cancel_push_frame(0)!
	frame := server_uni_stream_frame(0, buf)!
	datagram := build_fake_one_rtt_packet(c.scid, 0, frame, read_keys(mut c), false)!
	if _ := h.poll(datagram.bytes, now) {
		assert false, 'CANCEL_PUSH must be rejected -- this client never authorizes push'
	}
}

fn test_h3_conn_ignores_unknown_unidirectional_stream_type() {
	mut c, mut h, _, now := h3_test_conn()!
	defer {
		c.handshake.free()
	}
	mut buf := encode_varint(u64(0x40))!
	buf << [u8(1), 2, 3, 4]
	frame := server_uni_stream_frame(0, buf)!
	datagram := build_fake_one_rtt_packet(c.scid, 0, frame, read_keys(mut c), false)!
	result := h.poll(datagram.bytes, now)!
	assert result.events.len == 0, result.events.str()
}

fn test_h3_conn_qpack_glue_loop_end_to_end_with_section_ack() {
	mut c, mut h, _, now := h3_test_conn()!
	defer {
		c.handshake.free()
	}
	mut peer_encoder := new_qpack_encoder()
	set_cap_instr := peer_encoder.set_capacity(4096, 4096)!
	encoded := peer_encoder.encode_field_section(0, [
		QpackFieldLine{
			name:  'x-test'
			value: 'hello'
		},
	])!

	mut enc_stream_buf := encode_qpack_encoder_stream_header()!
	enc_stream_buf << set_cap_instr
	enc_stream_buf << encoded.encoder_instructions
	enc_frame := server_uni_stream_frame(1, enc_stream_buf)!
	enc_datagram := build_fake_one_rtt_packet(c.scid, 0, enc_frame, read_keys(mut c), false)!
	h.poll(enc_datagram.bytes, now)!

	dec_header := encode_qpack_decoder_stream_header()!
	dec_frame := server_uni_stream_frame(2, dec_header)!
	dec_datagram := build_fake_one_rtt_packet(c.scid, 1, dec_frame, read_keys(mut c), false)!
	h.poll(dec_datagram.bytes, now)!

	stream_id := h.open_request_stream()!
	h.poll(none, now)!

	headers_frame := encode_headers_frame(encoded.field_section)!
	req_stream_frame := encode_stream_frame(stream_id, 0, headers_frame, true, true)!
	req_datagram := build_fake_one_rtt_packet(c.scid, 2, req_stream_frame, read_keys(mut c), false)!
	result := h.poll(req_datagram.bytes, now)!
	headers_ev := result.events.filter(it.kind == .response_headers)
	assert headers_ev.len == 1, result.events.str()
	assert headers_ev[0].headers.len == 1
	assert headers_ev[0].headers[0].name == 'x-test'
	assert headers_ev[0].headers[0].value == 'hello'
}

// test_h3_conn_1xx_interim_response_is_discarded_not_misdelivered_as_final_or_trailers
// is a regression test for a real bug: before this fix, the message-framing
// state machine had no concept of RFC 9110 §15.2's 1xx informational
// responses, so a legitimate `:status: 103` HEADERS block followed by the
// real `:status: 200` response caused the 103 to be delivered as "the"
// response (wrong final status) and the real 200 response to be misdelivered
// as TRAILERS (h3_mux_conn.v dumps trailer fields into resp.headers with no
// :status filtering at all). Uses ONLY exact QPACK static-table entries
// (both ':status'/'103' and ':status'/'200' are static-table hits -- see
// qpack_static_table.v) so this exercises no dynamic-table/encoder-
// instruction machinery at all, keeping the two HEADERS blocks decodable
// synchronously in one poll() call.
fn test_h3_conn_1xx_interim_response_is_discarded_not_misdelivered_as_final_or_trailers() {
	mut c, mut h, _, now := h3_test_conn()!
	defer {
		c.handshake.free()
	}
	mut peer_encoder := new_qpack_encoder()

	stream_id := h.open_request_stream()!
	h.poll(none, now)!

	interim := peer_encoder.encode_field_section(stream_id, [
		QpackFieldLine{
			name:  ':status'
			value: '103'
		},
	])!
	assert interim.encoder_instructions.len == 0, 'a static-table-only reference must not need an encoder instruction'
	final := peer_encoder.encode_field_section(stream_id, [
		QpackFieldLine{
			name:  ':status'
			value: '200'
		},
	])!
	assert final.encoder_instructions.len == 0

	mut body := encode_headers_frame(interim.field_section)!
	body << encode_headers_frame(final.field_section)!
	req_stream_frame := encode_stream_frame(stream_id, 0, body, false, true)!
	req_datagram := build_fake_one_rtt_packet(c.scid, 0, req_stream_frame, read_keys(mut c), false)!
	result := h.poll(req_datagram.bytes, now)!

	trailers_ev := result.events.filter(it.kind == .response_trailers)
	assert trailers_ev.len == 0, '103 must never be misdelivered as trailers: ${result.events.str()}'
	headers_ev := result.events.filter(it.kind == .response_headers)
	assert headers_ev.len == 1, 'exactly one response_headers event, for the 200, not the discarded 103: ${result.events.str()}'
	assert headers_ev[0].headers.len == 1
	assert headers_ev[0].headers[0].name == ':status'
	assert headers_ev[0].headers[0].value == '200', 'the delivered response_headers must be the FINAL response, not the discarded 1xx interim one'
}

// test_h3_conn_prunes_request_stream_state_once_finalized is a regression
// test for unbounded memory growth: request_streams/request_decoders used
// to grow with the TOTAL number of requests ever opened on a pooled
// connection, never shrinking as requests completed, since nothing ever
// deleted their entries.
fn test_h3_conn_prunes_request_stream_state_once_finalized() {
	mut c, mut h, _, now := h3_test_conn()!
	defer {
		c.handshake.free()
	}
	mut peer_encoder := new_qpack_encoder()

	stream_id := h.open_request_stream()!
	h.poll(none, now)!
	assert stream_id in h.request_streams

	final := peer_encoder.encode_field_section(stream_id, [
		QpackFieldLine{
			name:  ':status'
			value: '200'
		},
	])!
	headers_frame := encode_headers_frame(final.field_section)!
	req_stream_frame := encode_stream_frame(stream_id, 0, headers_frame, true, true)!
	req_datagram := build_fake_one_rtt_packet(c.scid, 0, req_stream_frame, read_keys(mut c), false)!
	result := h.poll(req_datagram.bytes, now)!
	assert result.events.any(it.kind == .response_ended)

	// Once a request stream is fully finalized, its per-request state must
	// not linger -- a long-lived pooled connection serving many sequential
	// requests would otherwise accumulate one entry per request EVER
	// opened, not per request currently in flight.
	assert stream_id !in h.request_streams
	assert stream_id !in h.request_decoders
}

fn test_h3_conn_prunes_request_stream_state_on_failure_too() {
	mut c, mut h, _, now := h3_test_conn()!
	defer {
		c.handshake.free()
	}
	stream_id := h.open_request_stream()!
	h.poll(none, now)!
	assert stream_id in h.request_streams

	// A DATA frame first, with no HEADERS at all, is a request-stream
	// framing error (§4.1) -- fail_request_stream must prune this stream's
	// state even on the failure path, not just on successful completion.
	data_frame := encode_data_frame([u8(1), 2, 3])!
	req_stream_frame := encode_stream_frame(stream_id, 0, data_frame, false, true)!
	req_datagram := build_fake_one_rtt_packet(c.scid, 0, req_stream_frame, read_keys(mut c), false)!
	result := h.poll(req_datagram.bytes, now)!
	assert result.events.any(it.kind == .request_error)

	assert stream_id !in h.request_streams
	assert stream_id !in h.request_decoders
	assert stream_id in h.dead_request_streams
}

// test_h3_conn_prunes_dead_request_stream_once_peer_side_fully_terminal is a
// regression test for the third sibling of the SAME unbounded-growth bug
// class the two tests above already cover: dead_request_streams itself was
// never pruned (a repo-review miss caught externally, PR #28129 review
// comment 2026-08-21 -- see code-review-misses.md), unlike request_streams/
// request_decoders right next to it, on which fail_request_stream already
// prunes above. Reproduces the growth first (the entry must still be
// present immediately after failure, since the peer may legally keep
// sending on that stream ID -- fail_request_stream's own doc comment),
// then drives the underlying QUIC stream to ITS OWN terminal receive state
// (an empty FIN-carrying continuation frame here; a RESET_STREAM would
// work identically) and asserts the entry is gone on the next poll(), once
// the transport itself guarantees no further frames can ever arrive.
fn test_h3_conn_prunes_dead_request_stream_once_peer_side_fully_terminal() {
	mut c, mut h, _, now := h3_test_conn()!
	defer {
		c.handshake.free()
	}
	stream_id := h.open_request_stream()!
	h.poll(none, now)!
	assert stream_id in h.request_streams

	// Same framing error as the sibling test above: fails the stream at the
	// H3 layer while leaving the underlying QUIC stream open (no FIN yet).
	data_frame := encode_data_frame([u8(1), 2, 3])!
	req_stream_frame := encode_stream_frame(stream_id, 0, data_frame, false, true)!
	req_datagram := build_fake_one_rtt_packet(c.scid, 0, req_stream_frame, read_keys(mut c), false)!
	fail_result := h.poll(req_datagram.bytes, now)!
	assert fail_result.events.any(it.kind == .request_error)
	assert stream_id in h.dead_request_streams, 'must still be tracked immediately after failure -- the peer may legally keep sending on this ID'

	// The peer now finishes sending on the same stream -- its receive side
	// reaches a terminal QUIC-layer state, so no further frames for this ID
	// can ever legally arrive.
	fin_frame := encode_stream_frame(stream_id, u64(data_frame.len), []u8{}, true, true)!
	fin_datagram := build_fake_one_rtt_packet(c.scid, 1, fin_frame, read_keys(mut c), false)!
	h.poll(fin_datagram.bytes, now)!

	assert stream_id !in h.dead_request_streams, 'dead_request_streams must not grow unboundedly for the lifetime of a long-lived pooled connection -- once the QUIC layer confirms the peer can never send more on this stream ID, this bookkeeping entry must be pruned too'
}

// test_h3_conn_blocked_headers_retry_after_delayed_encoder_instruction is
// the standout new-integration-behavior case (no direct Phase 10/11 test
// precedent): a HEADERS frame referencing a dynamic-table entry arrives
// BEFORE the encoder-stream instruction that inserts it -- entirely
// ordinary under real network reordering/pacing, since request and
// encoder streams are independent QUIC streams with no ordering guarantee
// between them. QpackDecoder.decode_field_section must report the section
// blocked (not deliver garbage, not error), and H3Conn must queue it and
// automatically retry once the withheld instruction finally arrives.
fn test_h3_conn_blocked_headers_retry_after_delayed_encoder_instruction() {
	mut c, mut h, _, now := h3_test_conn()!
	defer {
		c.handshake.free()
	}
	mut peer_encoder := new_qpack_encoder()
	set_cap_instr := peer_encoder.set_capacity(4096, 4096)!
	encoded := peer_encoder.encode_field_section(0, [
		QpackFieldLine{
			name:  'x-blocked'
			value: 'later'
		},
	])!

	// Set the capacity so the decoder is READY to accept an insert, but
	// deliberately withhold the insert instruction itself -- a NON-FIN
	// frame on the encoder stream (index 1), since a second frame carrying
	// the withheld instruction follows LATER on this SAME stream, not as a
	// new one (QPACK permits at most one encoder stream per peer).
	enc_header := encode_qpack_encoder_stream_header()!
	cap_bytes := conn_test_concat_bytes(enc_header, set_cap_instr)
	cap_frame := server_uni_stream_continuation_frame(1, 0, cap_bytes)!
	cap_datagram := build_fake_one_rtt_packet(c.scid, 0, cap_frame, read_keys(mut c), false)!
	h.poll(cap_datagram.bytes, now)!

	stream_id := h.open_request_stream()!
	h.poll(none, now)!

	headers_frame := encode_headers_frame(encoded.field_section)!
	req_stream_frame := encode_stream_frame(stream_id, 0, headers_frame, false, true)!
	req_datagram := build_fake_one_rtt_packet(c.scid, 1, req_stream_frame, read_keys(mut c), false)!
	blocked_result := h.poll(req_datagram.bytes, now)!
	assert !blocked_result.events.any(it.kind == .response_headers), blocked_result.events.str()

	// Deliver the previously-withheld insert instruction as a continuation
	// of the SAME encoder stream, in a LATER poll() call.
	insert_frame := server_uni_stream_continuation_frame(1, u64(cap_bytes.len),
		encoded.encoder_instructions)!
	insert_datagram := build_fake_one_rtt_packet(c.scid, 2, insert_frame, read_keys(mut c), false)!
	unblocked_result := h.poll(insert_datagram.bytes, now)!
	headers_ev := unblocked_result.events.filter(it.kind == .response_headers)
	assert headers_ev.len == 1, unblocked_result.events.str()
	assert headers_ev[0].stream_id? == stream_id
	assert headers_ev[0].headers[0].name == 'x-blocked'
	assert headers_ev[0].headers[0].value == 'later'
}
