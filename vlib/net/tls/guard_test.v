module tls

import crypto.aes
import crypto.ecdsa
import crypto.sha256
import crypto.x509
import time
import x.crypto.curve25519

// test_guard_handshake_required locks the handshake_done gate on app-data paths.
fn test_guard_handshake_required() {
	mut c := new_conn(Config{}) or { panic(err) }
	mut buf := []u8{len: 8}
	c.read(mut buf) or {
		assert err.msg().contains('handshake')
		return
	}
	assert false, 'read without handshake must fail'
}

fn test_guard_handshake_required_write() {
	mut c := new_conn(Config{}) or { panic(err) }
	c.write('ping'.bytes()) or {
		assert err.msg().contains('handshake')
		return
	}
	assert false, 'write without handshake must fail'
}

// test_guard_seal_lengths locks key/iv/plaintext validation in seal_record.
fn test_guard_seal_lengths() {
	good_iv := []u8{len: 12}
	pt := [u8(1), 2, 3]
	seal_record(.tls_aes_128_gcm_sha256, []u8{len: 15}, good_iv, 0, .application_data, pt) or {
		assert err.msg().contains('key length')
		return
	}
	assert false, 'short key must fail'
}

fn test_guard_seal_iv_plaintext() {
	key := []u8{len: 16}
	seal_record(.tls_aes_128_gcm_sha256, key, []u8{len: 11}, 0, .application_data, [u8(1)]) or {
		assert err.msg().contains('iv length')
		return
	}
	seal_record(.tls_aes_128_gcm_sha256, key, []u8{len: 12}, 0, .application_data, []u8{len: max_plaintext_len + 1}) or {
		assert err.msg().contains('plaintext')
		return
	}
	assert false, 'bad iv/plaintext must fail'
}

// test_guard_open_lengths locks key/iv/outer/fragment validation in open_record.
fn test_guard_open_lengths() {
	key := []u8{len: 16}
	iv := []u8{len: 12}
	rec := CipherRecord{
		content_type:   .application_data
		legacy_version: tls_version_1_3
		fragment:       []u8{len: 40}
	}
	open_record(.tls_aes_128_gcm_sha256, []u8{len: 8}, iv, 0, rec) or {
		assert err.msg().contains('key length')
		return
	}
	open_record(.tls_aes_128_gcm_sha256, key, []u8{len: 8}, 0, rec) or {
		assert err.msg().contains('iv length')
		return
	}
	assert false, 'bad key/iv must fail'
}

fn test_guard_open_outer_and_tag() {
	key := []u8{len: 16}
	iv := []u8{len: 12}
	bad_outer := CipherRecord{
		content_type:   .handshake
		legacy_version: tls_version_1_3
		fragment:       []u8{len: 40}
	}
	open_record(.tls_aes_128_gcm_sha256, key, iv, 0, bad_outer) or {
		assert err.msg().contains('outer')
		return
	}
	short := CipherRecord{
		content_type:   .application_data
		legacy_version: tls_version_1_3
		fragment:       []u8{len: 10}
	}
	open_record(.tls_aes_128_gcm_sha256, key, iv, 0, short) or {
		assert err.msg().contains('truncated')
		return
	}
	assert false, 'bad outer/short fragment must fail'
}

// test_guard_rfc_aad locks the RFC 8446 §5.2 AAD (ciphertext length, not inner length).
fn test_guard_rfc_aad() {
	key := []u8{len: 16, init: 1}
	iv := []u8{len: 12, init: 2}
	pt := 'hello'.bytes()
	rec_bytes := seal_record(.tls_aes_128_gcm_sha256, key, iv, 7, .application_data, pt) or {
		panic(err)
	}
	rec, _ := decode_cipher_record(rec_bytes) or { panic(err) }
	// rebuild the nonce exactly like open_record does
	mut nonce := iv.clone()
	for i in 0 .. 8 {
		nonce[4 + i] = iv[4 + i] ^ u8((u64(7) >> (8 * (7 - i))) & 0xff)
	}
	// RFC AAD uses the on-wire ciphertext length and must succeed.
	mut rfc_aad := []u8{cap: 5}
	rfc_aad << u8(ContentType.application_data)
	rfc_aad << u8((tls_version_1_3 >> 8) & 0xff)
	rfc_aad << u8(tls_version_1_3 & 0xff)
	rfc_aad << u8(rec.fragment.len >> 8)
	rfc_aad << u8(rec.fragment.len & 0xff)
	gcm := aes.new_aes_gcm(key) or { panic(err) }
	inner := gcm.decrypt(rec.fragment, nonce, rfc_aad) or { panic('rfc aad must verify: ${err}') }
	assert inner.len == pt.len + 1
	// The old (wrong) AAD with the inner plaintext length must fail.
	old_len := rec.fragment.len - aes.gcm_tag_size
	mut old_aad := []u8{cap: 5}
	old_aad << u8(ContentType.application_data)
	old_aad << u8((tls_version_1_3 >> 8) & 0xff)
	old_aad << u8(tls_version_1_3 & 0xff)
	old_aad << u8(old_len >> 8)
	old_aad << u8(old_len & 0xff)
	gcm.decrypt(rec.fragment, nonce, old_aad) or { return }
	assert false, 'inner-length AAD must not verify'
}

// test_guard_plain_record locks version and plaintext-cap checks.
fn test_guard_plain_version_and_cap() {
	good := encode_plain_record(PlainRecord{
		content_type: .handshake
		version:      tls_version_1_3
		fragment:     [u8(1)]
	})
	mut compat := good.clone()
	compat[1] = 0x03
	compat[2] = 0x01
	decoded, _ := decode_plain_record(compat) or {
		assert false, '0x0301 plaintext must be accepted: ${err}'
		return
	}
	assert decoded.version == 0x0301
	mut bad_ver := good.clone()
	bad_ver[1] = 0x03
	bad_ver[2] = 0x00
	decode_plain_record(bad_ver) or {
		assert err.msg().contains('version')
		return
	}
	assert false, '0x0300 version must fail'
}

fn test_guard_plain_too_large() {
	mut data := []u8{len: 5}
	data[0] = u8(ContentType.handshake)
	data[1] = 0x03
	data[2] = 0x03
	// 16500 > 16384 plaintext cap.
	data[3] = u8(16500 >> 8)
	data[4] = u8(16500 & 0xff)
	decode_plain_record(data) or {
		assert err.msg().contains('too large')
		return
	}
	assert false, 'oversize plaintext must fail'
}

// test_guard_cipher_record locks outer-type/version/size checks.
fn test_guard_cipher_outer() {
	mut good := encode_cipher_record(CipherRecord{
		content_type:   .application_data
		legacy_version: tls_version_1_3
		fragment:       []u8{len: 32}
	})
	good[0] = u8(ContentType.handshake)
	decode_cipher_record(good) or {
		assert err.msg().contains('outer')
		return
	}
	assert false, 'non-23 outer must fail'
}

fn test_guard_cipher_version_and_size() {
	mut good := encode_cipher_record(CipherRecord{
		content_type:   .application_data
		legacy_version: tls_version_1_3
		fragment:       []u8{len: 32}
	})
	good[1] = 0x03
	good[2] = 0x01
	decode_cipher_record(good) or {
		assert err.msg().contains('version')
		return
	}
	// oversize: length 16641 > max_ciphertext_len.
	mut big := []u8{len: 5}
	big[0] = u8(ContentType.application_data)
	big[1] = 0x03
	big[2] = 0x03
	big[3] = u8(16641 >> 8)
	big[4] = u8(16641 & 0xff)
	decode_cipher_record(big) or {
		assert err.msg().contains('too large')
		return
	}
	assert false, 'bad version/size must fail'
}

// test_guard_inner locks inner content-type validation.
fn test_guard_inner_all_zero_and_unknown() {
	decode_inner([u8(0), 0, 0]) or {
		assert err.msg().contains('invalid')
		return
	}
	assert false, 'all-zero inner must fail'
}

fn test_guard_inner_unknown_type() {
	decode_inner([u8(9), 8, 0x63]) or {
		assert err.msg().contains('invalid')
		return
	}
	assert false, 'unknown inner type must fail'
}

// test_guard_alert locks level and description validation.
fn test_guard_alert_values() {
	parse_alert([u8(2), 0x09]) or {
		assert err.msg().contains('description')
		return
	}
	parse_alert([u8(9), 0x00]) or {
		assert err.msg().contains('level')
		return
	}
	assert false, 'bad alert bytes must fail'
}

fn guard_key_share_server_data(pubkey []u8) []u8 {
	mut entry := []u8{}
	entry << u8(named_group_x25519 >> 8)
	entry << u8(named_group_x25519)
	entry << u8(pubkey.len >> 8)
	entry << u8(pubkey.len)
	entry << pubkey
	return entry
}

// test_guard_server_hello_suite locks the server-hello suite parse whitelist:
// unknown suite values are rejected at parse time, while the client handshake
// additionally requires a suite it offered.
fn test_guard_server_hello_suite() {
	srv_priv := curve25519.PrivateKey.new() or { panic(err) }
	srv_pub := srv_priv.public_key() or { panic(err) }
	srv_pub_bytes := srv_pub.bytes() or { panic(err) }
	new_sh_body := fn (srv_pub_bytes []u8, cs CipherSuite) ![]u8 {
		sh := ServerHello{
			legacy_version:     tls_version_1_3
			random:             []u8{len: 32, init: 7}
			session_id:         []u8{}
			cipher_suite:       cs
			compression_method: 0
			extensions:         [
				Extension{
					typ:  ext_supported_versions
					data: [u8(supported_version_tls_1_3 >> 8), u8(supported_version_tls_1_3)]
				},
				Extension{ typ: ext_key_share, data: guard_key_share_server_data(srv_pub_bytes) },
			]
		}
		return encode_server_hello(sh)
	}
	// 0x1302 is a real suite so it parses, but a client that offered only
	// 0x1301 must reject the ServerHello at negotiation time.
	sh_body := new_sh_body(srv_pub_bytes, .tls_aes_256_gcm_sha384) or { panic(err) }
	sh_msg := encode_handshake_message(.server_hello, sh_body) or { panic(err) }
	mut h := new_client_handshake('example.com', []string{}, .off, [.tls_aes_128_gcm_sha256]) or {
		panic(err)
	}
	_ = h.build_client_hello() or { panic(err) }
	h.process_server_hello(sh_msg) or {
		assert err.msg().contains('cipher suite')
		return
	}
	assert false, 'non-offered suite must fail negotiation'
}

fn test_guard_server_hello_unknown_suite_parse() {
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
				data: [u8(supported_version_tls_1_3 >> 8), u8(supported_version_tls_1_3)]
			},
			Extension{ typ: ext_key_share, data: guard_key_share_server_data(srv_pub_bytes) },
		]
	}
	// Patch the cipher_suite field (empty sid: ver(2) + random(32) + sid_len(1))
	// to an unknown value.
	mut sh_body := encode_server_hello(sh) or { panic(err) }
	sh_body[2 + 32 + 1] = 0x00
	sh_body[2 + 32 + 2] = 0x00
	parse_server_hello(sh_body) or {
		assert err.msg().contains('unsupported')
		return
	}
	assert false, 'unknown suite must fail to parse'
}

// test_guard_client_hello_suites locks unknown-suite skipping on parse.
fn test_guard_client_hello_unknown_suites() {
	mut h := new_client_handshake('example.com', []string{}, .off, []CipherSuite{}) or { panic(err) }
	ch_msg := h.build_client_hello() or { panic(err) }
	parsed, _ := parse_handshake_message(ch_msg) or { panic(err) }
	mut ch := parse_client_hello(parsed.body) or { panic(err) }
	assert ch.cipher_suites == [.tls_aes_128_gcm_sha256, .tls_chacha20_poly1305_sha256,
		.tls_aes_256_gcm_sha384]
	// A body offering only an unknown suite (0x0000) must yield no known suites.
	mut unknown_body := []u8{}
	unknown_body << u8(0x03)
	unknown_body << u8(0x03)
	unknown_body << []u8{len: 32}
	unknown_body << u8(0)
	unknown_body << u8(0)
	unknown_body << u8(2)
	unknown_body << u8(0)
	unknown_body << u8(0)
	unknown_body << u8(1)
	unknown_body << u8(0)
	unknown_body << u8(0)
	unknown_body << u8(0)
	ch2 := parse_client_hello(unknown_body) or { panic(err) }
	assert ch2.cipher_suites.len == 0
}

// test_guard_client_hello_version locks the legacy_version check.
fn test_guard_client_hello_version() {
	mut h := new_client_handshake('example.com', []string{}, .off, []CipherSuite{}) or { panic(err) }
	ch_msg := h.build_client_hello() or { panic(err) }
	parsed, _ := parse_handshake_message(ch_msg) or { panic(err) }
	// Patch legacy_version to 0x0301 inside the body (first two bytes).
	mut bad_body := parsed.body.clone()
	bad_body[0] = 0x03
	bad_body[1] = 0x01
	parse_client_hello(bad_body) or {
		assert err.msg().contains('version')
		return
	}
	assert false, '0x0301 ClientHello must fail'
}

fn test_server_handshake_trailing_rejected() {
	mut h := new_client_handshake('example.com', []string{}, .off, []CipherSuite{}) or { panic(err) }
	ch_msg := h.build_client_hello() or { panic(err) }
	mut trailing := ch_msg.clone()
	trailing << u8(0)
	mut s := new_server_handshake(.off) or { panic(err) }
	s.process_client_hello(trailing) or {
		assert err.msg().contains('trailing')
		return
	}
	assert false, 'trailing ClientHello bytes must fail'
}

fn test_server_handshake_no_suite() {
	priv := curve25519.PrivateKey.new() or { panic(err) }
	pk_bytes := priv.public_key()!.bytes()!
	ch := ClientHello{
		legacy_version:      tls_version_1_3
		random:              []u8{len: 32, init: 3}
		session_id:          []u8{}
		cipher_suites:       []CipherSuite{}
		compression_methods: [u8(0)]
		extensions:          [
			Extension{ typ: ext_key_share, data: key_share_client_data(pk_bytes) },
			Extension{
				typ:  ext_supported_versions
				data: [u8(3), u8(0x03), u8(0x04), u8(0x03), u8(0x03)]
			},
		]
	}
	body := encode_client_hello(ch) or { panic(err) }
	msg := encode_handshake_message(.client_hello, body) or { panic(err) }
	mut s := new_server_handshake(.off) or { panic(err) }
	s.process_client_hello(msg) or {
		assert err.msg().contains('cipher suite')
		return
	}
	assert false, 'hello with no suites must fail'
}

fn test_server_handshake_no_tls13() {
	priv := curve25519.PrivateKey.new() or { panic(err) }
	pk_bytes := priv.public_key()!.bytes()!
	ch := ClientHello{
		legacy_version:      tls_version_1_3
		random:              []u8{len: 32, init: 3}
		session_id:          []u8{}
		cipher_suites:       [.tls_aes_128_gcm_sha256]
		compression_methods: [u8(0)]
		extensions:          [
			Extension{ typ: ext_key_share, data: key_share_client_data(pk_bytes) },
		]
	}
	body := encode_client_hello(ch) or { panic(err) }
	msg := encode_handshake_message(.client_hello, body) or { panic(err) }
	mut s := new_server_handshake(.off) or { panic(err) }
	s.process_client_hello(msg) or {
		assert err.msg().contains('TLS 1.3')
		return
	}
	assert false, 'hello without supported_versions must fail'
}

fn test_server_handshake_bad_versions_vector() {
	priv := curve25519.PrivateKey.new() or { panic(err) }
	pk_bytes := priv.public_key()!.bytes()!
	ch := ClientHello{
		legacy_version:      tls_version_1_3
		random:              []u8{len: 32, init: 3}
		session_id:          []u8{}
		cipher_suites:       [.tls_aes_128_gcm_sha256]
		compression_methods: [u8(0)]
		extensions:          [
			Extension{ typ: ext_key_share, data: key_share_client_data(pk_bytes) },
			Extension{ typ: ext_supported_versions, data: [u8(0x03), 0x04, 0x00] },
		]
	}
	body := encode_client_hello(ch) or { panic(err) }
	msg := encode_handshake_message(.client_hello, body) or { panic(err) }
	mut s := new_server_handshake(.off) or { panic(err) }
	s.process_client_hello(msg) or {
		assert err.msg().contains('supported_versions')
		return
	}
	assert false, 'malformed versions vector must fail'
}

// test_guard_client_sh_trailing locks consumed-length checks on ServerHello.
fn test_guard_client_sh_trailing() {
	mut h := new_client_handshake('example.com', []string{}, .off, []CipherSuite{}) or { panic(err) }
	_ = h.build_client_hello() or { panic(err) }
	srv_priv := curve25519.PrivateKey.new() or { panic(err) }
	srv_pub_bytes := srv_priv.public_key()!.bytes()!
	sh := ServerHello{
		legacy_version:     tls_version_1_3
		random:             []u8{len: 32, init: 7}
		session_id:         []u8{}
		cipher_suite:       .tls_aes_128_gcm_sha256
		compression_method: 0
		extensions:         [
			Extension{
				typ:  ext_supported_versions
				data: [u8(supported_version_tls_1_3 >> 8), u8(supported_version_tls_1_3)]
			},
			Extension{ typ: ext_key_share, data: guard_key_share_server_data(srv_pub_bytes) },
		]
	}
	sh_body := encode_server_hello(sh) or { panic(err) }
	sh_msg := encode_handshake_message(.server_hello, sh_body) or { panic(err) }
	mut trailing := sh_msg.clone()
	trailing << u8(0)
	h.process_server_hello(trailing) or {
		assert err.msg().contains('trailing')
		return
	}
	assert false, 'trailing ServerHello bytes must fail'
}

// test_guard_client_sh_no_tls13 locks the client-side version confirmation.
fn test_guard_client_sh_no_tls13() {
	mut h := new_client_handshake('example.com', []string{}, .off, []CipherSuite{}) or { panic(err) }
	_ = h.build_client_hello() or { panic(err) }
	srv_priv := curve25519.PrivateKey.new() or { panic(err) }
	srv_pub_bytes := srv_priv.public_key()!.bytes()!
	sh := ServerHello{
		legacy_version:     tls_version_1_3
		random:             []u8{len: 32, init: 7}
		session_id:         []u8{}
		cipher_suite:       .tls_aes_128_gcm_sha256
		compression_method: 0
		extensions:         [
			Extension{ typ: ext_key_share, data: guard_key_share_server_data(srv_pub_bytes) },
		]
	}
	sh_body := encode_server_hello(sh) or { panic(err) }
	sh_msg := encode_handshake_message(.server_hello, sh_body) or { panic(err) }
	h.process_server_hello(sh_msg) or {
		assert err.msg().contains('TLS 1.3')
		return
	}
	assert false, 'ServerHello without supported_versions must fail'
}

// test_guard_client_keyshare_trailing locks the exact-length key_share check.
fn test_guard_client_keyshare_trailing() {
	mut h := new_client_handshake('example.com', []string{}, .off, []CipherSuite{}) or { panic(err) }
	_ = h.build_client_hello() or { panic(err) }
	srv_priv := curve25519.PrivateKey.new() or { panic(err) }
	srv_pub_bytes := srv_priv.public_key()!.bytes()!
	mut ks := guard_key_share_server_data(srv_pub_bytes)
	ks << u8(0xde)
	sh := ServerHello{
		legacy_version:     tls_version_1_3
		random:             []u8{len: 32, init: 7}
		session_id:         []u8{}
		cipher_suite:       .tls_aes_128_gcm_sha256
		compression_method: 0
		extensions:         [
			Extension{
				typ:  ext_supported_versions
				data: [u8(supported_version_tls_1_3 >> 8), u8(supported_version_tls_1_3)]
			},
			Extension{ typ: ext_key_share, data: ks },
		]
	}
	sh_body := encode_server_hello(sh) or { panic(err) }
	sh_msg := encode_handshake_message(.server_hello, sh_body) or { panic(err) }
	h.process_server_hello(sh_msg) or {
		assert err.msg().contains('key_share')
		return
	}
	assert false, 'trailing key_share bytes must fail'
}

// test_guard_verify_finished_trailing locks the whole-body Finished check.
fn test_guard_verify_finished_trailing() {
	fin_body := encode_finished(Finished{ verify_data: []u8{len: 32, init: 5} })
	fin_msg := encode_handshake_message(.finished, fin_body) or { panic(err) }
	mut trailing := fin_msg.clone()
	trailing << u8(0)
	mut s := new_server_handshake(.off) or { panic(err) }
	s.verify_client_finished(trailing) or {
		assert err.msg().contains('trailing')
		return
	}
	assert false, 'trailing Finished bytes must fail'
}

// test_guard_cert_verify_scheme locks scheme dispatch: implemented schemes
// parse with their scheme value, unknown schemes are rejected.
fn test_guard_cert_verify_scheme() {
	sig := []u8{len: 64, init: 1}
	mut body := []u8{}
	body << u8(0x08)
	body << u8(0x04)
	body << u8(0)
	body << u8(sig.len)
	body << sig
	scheme, _ := parse_cert_verify_sig(body) or { panic('pss scheme must parse: ${err}') }
	assert scheme == u16(0x0804)
	mut bad := body.clone()
	bad[0] = 0x99
	bad[1] = 0x99
	parse_cert_verify_sig(bad) or {
		assert err.msg().contains('scheme')
		return
	}
	assert false, 'unknown scheme must fail'
}

// test_guard_sni_alpn locks SNI/ALPN length validation.
fn test_guard_sni_alpn() {
	encode_sni('') or {
		assert err.msg().contains('server name')
		return
	}
	assert false, 'empty SNI must fail'
}

fn test_guard_sni_alpn_long() {
	encode_sni('x'.repeat(256)) or {
		assert err.msg().contains('server name')
		return
	}
	assert false, 'overlong SNI must fail'
}

fn test_guard_alpn_bad() {
	encode_alpn(['']) or {
		assert err.msg().contains('ALPN')
		return
	}
	assert false, 'empty ALPN must fail'
}

fn test_guard_alpn_long() {
	encode_alpn(['x'.repeat(300)]) or {
		assert err.msg().contains('ALPN')
		return
	}
	assert false, 'overlong ALPN must fail'
}

fn test_guard_client_hello_long_host() {
	mut h := new_client_handshake('x'.repeat(300), []string{}, .off, []CipherSuite{}) or { panic(err) }
	h.build_client_hello() or {
		assert err.msg().contains('server name')
		return
	}
	assert false, 'overlong host must fail hello build'
}

// test_guard_ecdsa_oid locks the sigAlg gate in verify_cert_signature_ecdsa.
fn test_guard_ecdsa_oid() {
	_, ca_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('ca keygen') }
	_, leaf_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('leaf keygen') }
	now := time.now()
	ca_cert, _ := x509.generate_self_signed(x509.CertGenOptions{
		common_name: 'Test CA'
		not_before:  now
		not_after:   now.add_days(3650)
	}, ca_priv) or { panic('ca gen') }
	leaf_opts := x509.CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now
		not_after:   now.add_days(365)
	}
	_, leaf_der := x509.generate_ca_signed(leaf_opts, leaf_priv, &ca_cert, ca_priv) or {
		panic('leaf gen')
	}
	ca_pk := ca_cert.public_key() or { panic('ca pubkey') }
	match ca_pk {
		x509.EcdsaP256PublicKey {
			// Sanity: the genuine cert verifies.
			verify_cert_signature_ecdsa(leaf_der, ca_pk.point) or {
				panic('genuine verify failed: ${err}')
			}
			// Flip ecdsa-with-SHA256 (06082a8648ce3d040302) to
			// ecdsa-with-SHA1 (...01) at its last (outer sigAlg) occurrence.
			pat := [u8(0x06), 0x08, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x04, 0x03, 0x02]
			mut idx := -1
			for i in 0 .. leaf_der.len - pat.len + 1 {
				mut found := true
				for j in 0 .. pat.len {
					if leaf_der[i + j] != pat[j] {
						found = false
						break
					}
				}
				if found {
					idx = i
				}
			}
			assert idx >= 0, 'sigAlg OID must be present'
			mut bad := leaf_der.clone()
			bad[idx + pat.len - 1] = 0x01
			verify_cert_signature_ecdsa(bad, ca_pk.point) or {
				assert err.msg().contains('algorithm')
				return
			}
			assert false, 'wrong sigAlg OID must fail'
		}
		else {
			panic('expected ecdsa')
		}
	}
}

// test_guard_oid_helper locks der_contains_oid directly.
fn test_guard_oid_helper() {
	good := [u8(0x30), 0x06, 0x08, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x04, 0x03, 0x02]
	assert der_contains_oid(good, [u8(0x2a), 0x86, 0x48, 0xce, 0x3d, 0x04, 0x03, 0x02])
	assert !der_contains_oid(good, [u8(0x2a), 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0b])
	assert !der_contains_oid([]u8{}, [u8(0x2a)])
}

// test_guard_rfc_cert_verify locks the RFC 8446 §4.4.3 content (64×0x20 ||
// context || 0x00 || hash) by verifying the server flight independently.
fn test_guard_rfc_cert_verify() {
	mut client := new_client_handshake('example.com', []string{}, .off, []CipherSuite{}) or { panic(err) }
	ch_msg := client.build_client_hello() or { panic(err) }
	mut server := new_server_handshake(.off) or { panic(err) }
	flight := server.process_client_hello(ch_msg) or { panic(err) }
	mut off := 0
	mut msgs := [][]u8{}
	for off < flight.len {
		parsed, n := parse_handshake_message(flight[off..]) or { panic(err) }
		msgs << flight[off..off + n]
		_ = parsed
		off += n
	}
	assert msgs.len == 5
	// Transcript at CertificateVerify time: CH + SH + EE + Cert.
	mut transcript := ch_msg.clone()
	transcript << msgs[0]
	transcript << msgs[1]
	transcript << msgs[2]
	cv_parsed, _ := parse_handshake_message(msgs[3]) or { panic(err) }
	assert cv_parsed.typ == .certificate_verify
	scheme, sig := parse_cert_verify_sig(cv_parsed.body) or { panic(err) }
	assert scheme == u16(0x0403)
	th := sha256.sum256(transcript)
	mut content := []u8{}
	for _ in 0 .. 64 {
		content << u8(0x20)
	}
	content << 'TLS 1.3, server CertificateVerify'.bytes()
	content << u8(0)
	content << th
	ok := server.ecdsa_pub.verify(content, sig, ecdsa.SignerOpts{}) or { panic(err) }
	assert ok, 'RFC content must verify against the server flight'
}
