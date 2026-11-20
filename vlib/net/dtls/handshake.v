// dtls handshake (RFC 9147 minimal, epoch 0/1, no 0-RTT).
// Reuses net.tls message codecs via import; no large codec copy-paste.
module dtls

import net.tls
import crypto.subtle

// effective_suites resolves Config.cipher_suites, defaulting to TLS defaults.
pub fn effective_suites(cfg Config) []tls.CipherSuite {
	if cfg.cipher_suites.len > 0 {
		return cfg.cipher_suites.clone()
	}
	return tls.default_cipher_suites()
}

// select_suite picks the first server-preference suite present in offered.
pub fn select_suite(offered []tls.CipherSuite, prefs []tls.CipherSuite) !tls.CipherSuite {
	for want in prefs {
		if want in offered {
			return want
		}
	}
	return error('dtls: no supported cipher suite')
}

// build_client_hello_msg builds a minimal ClientHello handshake message.
// build_client_hello_msg body is client_random(32) + u16 count + suites.
pub fn build_client_hello_msg(client_random []u8, suites []tls.CipherSuite) ![]u8 {
	if client_random.len != 32 {
		return error('dtls: client random must be 32 bytes')
	}
	if suites.len == 0 || suites.len > 16 {
		return error('dtls: bad suite offer')
	}
	mut body := []u8{cap: 34 + suites.len * 2}
	body << client_random
	body << u8(suites.len >> 8)
	body << u8(suites.len & 0xff)
	for s in suites {
		v := int(s)
		body << u8(v >> 8)
		body << u8(v & 0xff)
	}
	return tls.encode_handshake_message(tls.HandshakeType.client_hello, body)
}

// parse_client_hello_msg parses a minimal ClientHello message.
pub fn parse_client_hello_msg(msg []u8) !([]u8, []tls.CipherSuite) {
	parsed, n := tls.parse_handshake_message(msg)!
	if n != msg.len {
		return error('dtls: trailing data after ClientHello')
	}
	if parsed.typ != tls.HandshakeType.client_hello {
		return error('dtls: expected ClientHello')
	}
	body := parsed.body
	if body.len < 34 {
		return error('dtls: truncated ClientHello')
	}
	random := body[..32].clone()
	count := (int(body[32]) << 8) | int(body[33])
	if count <= 0 || count > 16 {
		return error('dtls: bad suite count')
	}
	if body.len != 34 + count * 2 {
		return error('dtls: ClientHello length mismatch')
	}
	mut suites := []tls.CipherSuite{}
	for i in 0 .. count {
		v := (int(body[34 + i * 2]) << 8) | int(body[34 + i * 2 + 1])
		s := match v {
			0x1301 { tls.CipherSuite.tls_aes_128_gcm_sha256 }
			0x1302 { tls.CipherSuite.tls_aes_256_gcm_sha384 }
			0x1303 { tls.CipherSuite.tls_chacha20_poly1305_sha256 }
			else { continue }
		}
		suites << s
	}
	if suites.len == 0 {
		return error('dtls: no supported cipher suite')
	}
	return random, suites
}

// build_server_hello_msg builds a minimal ServerHello handshake message.
// build_server_hello_msg body is server_random(32) + u16 selected suite.
pub fn build_server_hello_msg(server_random []u8, suite tls.CipherSuite) ![]u8 {
	if server_random.len != 32 {
		return error('dtls: server random must be 32 bytes')
	}
	mut body := []u8{cap: 34}
	body << server_random
	v := int(suite)
	body << u8(v >> 8)
	body << u8(v & 0xff)
	return tls.encode_handshake_message(tls.HandshakeType.server_hello, body)
}

// parse_server_hello_msg parses a minimal ServerHello message.
pub fn parse_server_hello_msg(msg []u8) !([]u8, tls.CipherSuite) {
	parsed, n := tls.parse_handshake_message(msg)!
	if n != msg.len {
		return error('dtls: trailing data after ServerHello')
	}
	if parsed.typ != tls.HandshakeType.server_hello {
		return error('dtls: expected ServerHello')
	}
	if parsed.body.len != 34 {
		return error('dtls: bad ServerHello length')
	}
	random := parsed.body[..32].clone()
	v := (int(parsed.body[32]) << 8) | int(parsed.body[33])
	suite := match v {
		0x1301 { tls.CipherSuite.tls_aes_128_gcm_sha256 }
		0x1302 { tls.CipherSuite.tls_aes_256_gcm_sha384 }
		0x1303 { tls.CipherSuite.tls_chacha20_poly1305_sha256 }
		else { return error('dtls: unsupported cipher suite') }
	}
	return random, suite
}

// build_finished_msg builds a Finished message covering transcript.
pub fn build_finished_msg(secret []u8, transcript []u8, suite tls.CipherSuite) ![]u8 {
	th := tls.transcript_hash_for_suite(transcript, suite)
	vd := tls.finished_verify_data_for_suite(secret, th, suite)!
	body := tls.encode_finished(tls.Finished{
		verify_data: vd
	})
	return tls.encode_handshake_message(tls.HandshakeType.finished, body)
}

// verify_finished_msg verifies a Finished message against transcript.
pub fn verify_finished_msg(msg []u8, secret []u8, transcript []u8, suite tls.CipherSuite) ! {
	parsed, n := tls.parse_handshake_message(msg)!
	if n != msg.len {
		return error('dtls: trailing data after Finished')
	}
	if parsed.typ != tls.HandshakeType.finished {
		return error('dtls: expected Finished')
	}
	th := tls.transcript_hash_for_suite(transcript, suite)
	expected := tls.finished_verify_data_for_suite(secret, th, suite)!
	fin := tls.parse_finished(parsed.body, tls.cipher_suite_hash_len(suite))!
	if subtle.constant_time_compare(fin.verify_data, expected) != 1 {
		return error('dtls: Finished mismatch')
	}
}

// derive_handshake_secrets derives handshake secrets from randoms.
// derive_handshake_secrets uses the TLS 1.3 key schedule with a minimal
// shared secret (client_random + server_random); no forward secrecy is
// claimed in this minimal profile.
pub fn derive_handshake_secrets(suite tls.CipherSuite, transcript_ch_sh []u8, client_random []u8, server_random []u8) !tls.HandshakeSecrets {
	early := tls.derive_early_secret_for_suite(suite)!
	mut shared := []u8{cap: 64}
	shared << client_random
	shared << server_random
	th := tls.transcript_hash_for_suite(transcript_ch_sh, suite)
	return tls.derive_handshake_secrets_for_suite(early, shared, th, suite)
}

// derive_app_traffic derives application traffic keys bound through server Finished.
pub fn derive_app_traffic(suite tls.CipherSuite, handshake_secret []u8, transcript_up_to_sfin []u8) !(tls.TrafficKeys, tls.TrafficKeys) {
	th := tls.transcript_hash_for_suite(transcript_up_to_sfin, suite)
	app := tls.derive_application_secrets_for_suite(handshake_secret, th, suite)!
	client_app := tls.derive_traffic_keys(app.client_secret, suite)!
	server_app := tls.derive_traffic_keys(app.server_secret, suite)!
	return client_app, server_app
}
