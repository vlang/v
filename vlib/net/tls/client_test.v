module tls

import x.crypto.curve25519

fn test_client_build_hello() {
	mut h := new_client_handshake('example.com', ['h2'], .off) or { panic(err) }
	msg := h.build_client_hello() or { panic(err) }
	assert msg.len > 4
	parsed, n := parse_handshake_message(msg) or { panic(err) }
	assert n == msg.len
	assert parsed.typ == .client_hello
	ch := parse_client_hello(parsed.body) or { panic(err) }
	assert ch.extensions.len >= 5
	// find key_share
	mut found := false
	for e in ch.extensions {
		if e.typ == ext_key_share {
			found = true
			assert e.data.len > 4
		}
	}
	assert found
}

fn test_client_process_server_hello() {
	// Build a client hello first
	mut h := new_client_handshake('example.com', []string{}, .off) or { panic(err) }
	_ = h.build_client_hello() or { panic(err) }

	// Craft a minimal ServerHello with X25519 key share using a fresh server key
	srv_priv := curve25519.PrivateKey.new() or { panic(err) }
	srv_pub := srv_priv.public_key() or { panic(err) }
	srv_pub_bytes := srv_pub.bytes() or { panic(err) }

	sh := ServerHello{
		legacy_version:     tls_version_1_3
		random:             []u8{len: 32, init: 7}
		session_id:         []u8{}
		cipher_suite:       .tls_aes_128_gcm_sha256
		compression_method: 0
		extensions:         [
			Extension{
				typ:  ext_supported_versions
				data: [
					u8(supported_version_tls_1_3 >> 8),
					u8(supported_version_tls_1_3),
				]
			},
			Extension{ typ: ext_key_share, data: key_share_server_data(srv_pub_bytes) },
		]
	}
	sh_body := encode_server_hello(sh) or { panic(err) }
	sh_msg := encode_handshake_message(.server_hello, sh_body) or { panic(err) }
	h.process_server_hello(sh_msg) or { panic(err) }
	assert h.client_hs_traffic.key.len == 16
	assert h.server_hs_traffic.key.len == 16
}

fn key_share_server_data(pubkey []u8) []u8 {
	mut entry := []u8{}
	entry << u8(named_group_x25519 >> 8)
	entry << u8(named_group_x25519)
	entry << u8(pubkey.len >> 8)
	entry << u8(pubkey.len)
	entry << pubkey
	return entry
}
