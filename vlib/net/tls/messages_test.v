module tls

import crypto.sha256

fn test_handshake_message_roundtrip() {
	body := [u8(1), 2, 3, 4, 5]
	enc := encode_handshake_message(.client_hello, body) or { panic(err) }
	assert enc.len == 4 + body.len
	msg, n := parse_handshake_message(enc) or { panic(err) }
	assert n == enc.len
	assert msg.typ == .client_hello
	assert msg.body == body
}

fn test_handshake_message_truncated() {
	parse_handshake_message([u8(1), 0, 0]) or {
		assert err.msg().contains('truncated')
		return
	}
	assert false
}

fn test_client_hello_roundtrip() {
	ch := ClientHello{
		legacy_version:      tls_version_1_3
		random:              []u8{len: 32, init: 7}
		session_id:          [u8(1), 2]
		cipher_suites:       [.tls_aes_128_gcm_sha256]
		compression_methods: [u8(0)]
		extensions:          [Extension{ typ: ext_supported_versions, data: [u8(2), 3, 4] }]
	}
	body := encode_client_hello(ch) or { panic(err) }
	got := parse_client_hello(body) or { panic(err) }
	assert got.legacy_version == ch.legacy_version
	assert got.random == ch.random
	assert got.session_id == ch.session_id
	assert got.cipher_suites == ch.cipher_suites
	assert got.compression_methods == ch.compression_methods
	assert got.extensions.len == 1
	assert got.extensions[0].typ == ext_supported_versions
}

fn test_server_hello_roundtrip() {
	sh := ServerHello{
		legacy_version:     tls_version_1_3
		random:             []u8{len: 32, init: 9}
		session_id:         [u8(3)]
		cipher_suite:       .tls_aes_128_gcm_sha256
		compression_method: 0
		extensions:         [Extension{ typ: ext_supported_versions, data: [u8(3), 4] }]
	}
	body := encode_server_hello(sh) or { panic(err) }
	got := parse_server_hello(body) or { panic(err) }
	assert got.legacy_version == sh.legacy_version
	assert got.random == sh.random
	assert got.session_id == sh.session_id
	assert got.cipher_suite == sh.cipher_suite
	assert got.compression_method == 0
	assert got.extensions.len == 1
}

fn test_finished_roundtrip() {
	vd := []u8{len: 32, init: 5}
	f := Finished{ verify_data: vd }
	enc := encode_finished(f)
	got := parse_finished(enc, 32) or { panic(err) }
	assert got.verify_data == vd
}

fn test_finished_wrong_len() {
	parse_finished([]u8{len: 16}, 32) or {
		assert err.msg().contains('mismatch')
		return
	}
	assert false
}

fn test_extension_helpers() {
	e := encode_supported_versions_client()
	assert e.len > 4
	e2 := encode_key_share_client([]u8{len: 32, init: 1})
	assert e2.len > 4
	e3 := encode_sni('example.com')
	assert e3.len > 4
	e4 := encode_alpn(['h2', 'http/1.1'])
	assert e4.len > 4
}

fn test_key_schedule_derive() {
	es := derive_early_secret() or { panic(err) }
	assert es.len == 32
	// shared secret dummy 32 bytes
	ss := []u8{len: 32, init: 1}
	ch_sh := sha256.sum256([]u8{len: 10, init: 2})
	hs := derive_handshake_secrets(es, ss, ch_sh) or { panic(err) }
	assert hs.handshake_secret.len == 32
	assert hs.client_secret.len == 32
	assert hs.server_secret.len == 32
	as_ := derive_application_secrets(hs.handshake_secret, sha256.sum256([]u8{len: 20, init: 3})) or { panic(err) }
	assert as_.master_secret.len == 32
	tk := derive_traffic_keys(hs.client_secret) or { panic(err) }
	assert tk.key.len == 16
	assert tk.iv.len == 12
	vd := finished_verify_data(hs.server_secret, sha256.sum256([]u8{len: 5})) or { panic(err) }
	assert vd.len == 32
}
