// vtest build: present_openssl?
module quic

import encoding.hex

// RFC 8448 §3's own ServerHello handshake message (90 octets, including
// the 4-byte handshake header), extracted programmatically from the raw
// RFC text (same source as tls13_keyschedule_test.v's vectors). A real,
// independently-sourced happy-path test for parse_server_hello.
const rfc8448_server_hello_message = '020000560303a6af06a4121860dc5e6e60249cd34c95930c8ac5cb1434dac155772ed3e2692800130100002e00330024001d0020c9828876112095fe66762bdbf7c672e156d6cc253b833df1dd69b1b04e751f0f002b00020304'
const rfc8448_server_hello_random = 'a6af06a4121860dc5e6e60249cd34c95930c8ac5cb1434dac155772ed3e26928'
const rfc8448_server_hello_x25519_key_exchange = 'c9828876112095fe66762bdbf7c672e156d6cc253b833df1dd69b1b04e751f0f'

fn test_parse_server_hello_matches_rfc8448_vector() {
	full := hex.decode(rfc8448_server_hello_message)!
	msg, consumed := parse_handshake_message(full)!
	assert consumed == full.len
	assert msg.typ == .server_hello

	result := parse_server_hello(msg.body)!
	match result {
		ParsedServerHello {
			assert result.random == hex.decode(rfc8448_server_hello_random)!
			assert result.cipher_suite == cipher_suite_tls_aes_128_gcm_sha256
			assert result.selected_version == tls_version_1_3
			assert result.key_share_group == 0x001d // x25519, per RFC 8448's own trace
			assert result.key_share_key_exchange == hex.decode(rfc8448_server_hello_x25519_key_exchange)!
			assert result.extensions.len == 2
		}
		ParsedHelloRetryRequest {
			assert false, 'RFC 8448 vector must decode as a real ServerHello, not HRR'
		}
	}
}

fn build_test_server_hello(random []u8, key_share_ext []u8, extra_extensions []u8) []u8 {
	mut body := []u8{}
	body << u8(0x03)
	body << u8(0x03)
	body << random
	body << u8(0) // empty legacy_session_id_echo
	body << u8(cipher_suite_tls_aes_128_gcm_sha256 >> 8)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256)
	body << u8(0) // legacy_compression_method
	mut supported_versions := []u8{}
	supported_versions << u8(tls_version_1_3 >> 8)
	supported_versions << u8(tls_version_1_3)
	sv_ext := encode_extension(ext_supported_versions, supported_versions) or { panic(err) }
	mut extensions := []u8{}
	extensions << key_share_ext
	extensions << sv_ext
	extensions << extra_extensions
	body << u8(extensions.len >> 8)
	body << u8(extensions.len)
	body << extensions
	return body
}

fn build_test_key_share_server_entry(group u16, key_exchange []u8) []u8 {
	mut entry := []u8{}
	entry << u8(group >> 8)
	entry << u8(group)
	entry << u8(key_exchange.len >> 8)
	entry << u8(key_exchange.len)
	entry << key_exchange
	return encode_extension(ext_key_share, entry) or { panic(err) }
}

fn test_parse_server_hello_detects_hello_retry_request() {
	// KeyShareHelloRetryRequest: just a NamedGroup, no key_exchange data.
	ks_ext := encode_extension(ext_key_share, [u8(0x00), 0x17]) or { panic(err) }
	body := build_test_server_hello(hello_retry_request_random[..].clone(), ks_ext, []u8{})

	result := parse_server_hello(body)!
	match result {
		ParsedHelloRetryRequest {
			assert result.cipher_suite == cipher_suite_tls_aes_128_gcm_sha256
			assert result.selected_version == tls_version_1_3
			selected_group := result.selected_group or {
				panic('expected a key_share-carried group to be present')
			}

			assert selected_group == 0x0017
			assert result.cookie == none
		}
		ParsedServerHello {
			assert false, 'the magic HelloRetryRequest random must decode as HRR, not a real ServerHello'
		}
	}
}

// test_parse_server_hello_hello_retry_request_cookie_only is a regression
// test for a Codex finding (vlang/v#27680 pullrequestreview-4806500473):
// key_share was previously treated as mandatory in every HelloRetryRequest,
// rejecting a valid RFC 8446 §4.1.4 cookie-only HRR (requesting only an
// anti-DoS cookie round-trip, since the client's already-offered key_share
// is acceptable) before it could ever be represented as a
// ParsedHelloRetryRequest.
fn test_parse_server_hello_hello_retry_request_cookie_only() {
	cookie_value := [u8(9), 8, 7, 6, 5]
	mut cookie_data := []u8{}
	cookie_data << u8(cookie_value.len >> 8)
	cookie_data << u8(cookie_value.len)
	cookie_data << cookie_value
	cookie_ext := encode_extension(ext_cookie, cookie_data) or { panic(err) }
	body := build_test_server_hello(hello_retry_request_random[..].clone(), []u8{}, cookie_ext)

	result := parse_server_hello(body)!
	match result {
		ParsedHelloRetryRequest {
			assert result.selected_group == none
			got_cookie := result.cookie or { panic('expected a cookie to be present') }
			assert got_cookie == cookie_value
		}
		ParsedServerHello {
			assert false, 'expected a cookie-only HelloRetryRequest'
		}
	}
}

fn test_parse_server_hello_hello_retry_request_with_cookie() {
	ks_ext := encode_extension(ext_key_share, [u8(0x00), 0x17]) or { panic(err) }
	cookie_value := [u8(1), 2, 3, 4, 5]
	mut cookie_data := []u8{}
	cookie_data << u8(cookie_value.len >> 8)
	cookie_data << u8(cookie_value.len)
	cookie_data << cookie_value
	cookie_ext := encode_extension(ext_cookie, cookie_data) or { panic(err) }
	body := build_test_server_hello(hello_retry_request_random[..].clone(), ks_ext, cookie_ext)

	result := parse_server_hello(body)!
	match result {
		ParsedHelloRetryRequest {
			got_cookie := result.cookie or { panic('expected a cookie to be present') }
			assert got_cookie == cookie_value
		}
		ParsedServerHello {
			assert false, 'expected HRR'
		}
	}
}

fn test_parse_server_hello_rejects_wrong_legacy_version() {
	mut body := []u8{}
	body << u8(0x03)
	body << u8(0x04) // wrong: must be 0x0303
	body << []u8{len: 32}
	body << u8(0)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256 >> 8)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256)
	body << u8(0)
	body << u8(0)
	body << u8(0)
	parse_server_hello(body) or {
		assert err.msg().contains('legacy_version')
		return
	}
	assert false, 'expected an error for a wrong legacy_version'
}

fn test_parse_server_hello_rejects_nonempty_session_id_echo() {
	ks_ext := encode_extension(ext_key_share, [u8(0x00), 0x17, 0x00, 0x01, 0x00]) or { panic(err) }
	mut body := []u8{}
	body << u8(0x03)
	body << u8(0x03)
	body << []u8{len: 32}
	body << u8(1) // non-empty session id echo -- always wrong, we never send one
	body << u8(0xaa)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256 >> 8)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256)
	body << u8(0)
	mut sv := []u8{}
	sv << u8(tls_version_1_3 >> 8)
	sv << u8(tls_version_1_3)
	sv_ext := encode_extension(ext_supported_versions, sv) or { panic(err) }
	mut extensions := []u8{}
	extensions << ks_ext
	extensions << sv_ext
	body << u8(extensions.len >> 8)
	body << u8(extensions.len)
	body << extensions
	parse_server_hello(body) or {
		assert err.msg().contains('legacy_session_id_echo')
		return
	}
	assert false, 'expected an error for a non-empty legacy_session_id_echo'
}

fn test_parse_server_hello_rejects_nonzero_compression_method() {
	ks_ext := build_test_key_share_server_entry(0x0017, []u8{len: 65})
	mut body := []u8{}
	body << u8(0x03)
	body << u8(0x03)
	body << []u8{len: 32}
	body << u8(0)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256 >> 8)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256)
	body << u8(1) // wrong: must be 0
	mut sv := []u8{}
	sv << u8(tls_version_1_3 >> 8)
	sv << u8(tls_version_1_3)
	sv_ext := encode_extension(ext_supported_versions, sv) or { panic(err) }
	mut extensions := []u8{}
	extensions << ks_ext
	extensions << sv_ext
	body << u8(extensions.len >> 8)
	body << u8(extensions.len)
	body << extensions
	parse_server_hello(body) or {
		assert err.msg().contains('legacy_compression_method')
		return
	}
	assert false, 'expected an error for a non-zero legacy_compression_method'
}

fn test_parse_server_hello_rejects_missing_supported_versions() {
	ks_ext := build_test_key_share_server_entry(0x0017, []u8{len: 65})
	mut body := []u8{}
	body << u8(0x03)
	body << u8(0x03)
	body << []u8{len: 32}
	body << u8(0)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256 >> 8)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256)
	body << u8(0)
	body << u8(ks_ext.len >> 8)
	body << u8(ks_ext.len)
	body << ks_ext
	parse_server_hello(body) or {
		assert err.msg().contains('supported_versions')
		return
	}
	assert false, 'expected an error for a missing supported_versions extension'
}

fn test_parse_server_hello_rejects_missing_key_share() {
	mut sv := []u8{}
	sv << u8(tls_version_1_3 >> 8)
	sv << u8(tls_version_1_3)
	sv_ext := encode_extension(ext_supported_versions, sv) or { panic(err) }
	mut body := []u8{}
	body << u8(0x03)
	body << u8(0x03)
	body << []u8{len: 32}
	body << u8(0)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256 >> 8)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256)
	body << u8(0)
	body << u8(sv_ext.len >> 8)
	body << u8(sv_ext.len)
	body << sv_ext
	parse_server_hello(body) or {
		assert err.msg().contains('key_share')
		return
	}
	assert false, 'expected an error for a missing key_share extension'
}

fn test_parse_server_hello_rejects_empty_key_exchange() {
	// group=0017, key_exchange_len=0000, no key_exchange bytes -- violates
	// RFC 8446 §4.2.8's opaque key_exchange<1..2^16-1> minimum.
	ks_ext := encode_extension(ext_key_share, [u8(0x00), 0x17, 0x00, 0x00]) or { panic(err) }
	mut sv := []u8{}
	sv << u8(tls_version_1_3 >> 8)
	sv << u8(tls_version_1_3)
	sv_ext := encode_extension(ext_supported_versions, sv) or { panic(err) }
	mut body := []u8{}
	body << u8(0x03)
	body << u8(0x03)
	body << []u8{len: 32}
	body << u8(0)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256 >> 8)
	body << u8(cipher_suite_tls_aes_128_gcm_sha256)
	body << u8(0)
	mut extensions := []u8{}
	extensions << ks_ext
	extensions << sv_ext
	body << u8(extensions.len >> 8)
	body << u8(extensions.len)
	body << extensions
	parse_server_hello(body) or {
		assert err.msg().contains('must not be empty')
		return
	}
	assert false, 'expected an error for an empty key_exchange'
}

fn test_parse_extension_list_rejects_duplicate_extension() {
	mut sv := []u8{}
	sv << u8(tls_version_1_3 >> 8)
	sv << u8(tls_version_1_3)
	sv_ext := encode_extension(ext_supported_versions, sv) or { panic(err) }
	mut buf := []u8{}
	buf << sv_ext
	buf << sv_ext
	parse_extension_list(buf) or {
		assert err.msg().contains('duplicate')
		return
	}
	assert false, 'expected an error for a duplicate extension'
}

fn test_parse_extension_list_rejects_truncated_header() {
	parse_extension_list([u8(0x00), 0x2b, 0x00]) or {
		assert err.msg().contains('truncated')
		return
	}
	assert false, 'expected an error for a truncated extension header'
}

fn test_parse_extension_list_rejects_length_exceeding_buffer() {
	parse_extension_list([u8(0x00), 0x2b, 0x00, 0x05, 0x01]) or {
		assert err.msg().contains('exceeding')
		return
	}
	assert false, 'expected an error for a declared length exceeding the buffer'
}

fn test_parse_encrypted_extensions_empty() {
	body := [u8(0x00), 0x00]
	extensions := parse_encrypted_extensions(body)!
	assert extensions.len == 0
}

fn test_parse_encrypted_extensions_with_quic_transport_parameters() {
	params := QuicTransportParameters{
		max_idle_timeout: 30000
	}
	ext := encode_quic_transport_parameters_extension(params)!
	mut body := []u8{}
	body << u8(ext.len >> 8)
	body << u8(ext.len)
	body << ext

	extensions := parse_encrypted_extensions(body)!
	found := find_extension(extensions, ext_quic_transport_parameters) or {
		panic('expected quic_transport_parameters to be present')
	}
	decoded := decode_transport_parameters(found.data)!
	assert decoded.max_idle_timeout? == 30000
}

fn test_parse_encrypted_extensions_rejects_early_data() {
	ext := encode_extension(ext_early_data, []u8{}) or { panic(err) }
	mut body := []u8{}
	body << u8(ext.len >> 8)
	body << u8(ext.len)
	body << ext
	parse_encrypted_extensions(body) or {
		assert err.msg().contains('early_data')
		return
	}
	assert false, 'expected an error for a forbidden early_data extension'
}

// test_parse_encrypted_extensions_rejects_key_share is a regression test for
// a Codex finding (vlang/v#27680 pullrequestreview-4783410111):
// parse_encrypted_extensions only ever rejected early_data, so an
// EncryptedExtensions carrying key_share or supported_versions -- both
// ClientHello/ServerHello/HelloRetryRequest-only per RFC 8446 §4.2's own
// per-message table, illegal in EncryptedExtensions regardless of what was
// offered -- passed through unrejected.
fn test_parse_encrypted_extensions_rejects_key_share() {
	ext := encode_extension(ext_key_share, [u8(0), 0x17, 0, 0]) or { panic(err) }
	mut body := []u8{}
	body << u8(ext.len >> 8)
	body << u8(ext.len)
	body << ext
	parse_encrypted_extensions(body) or {
		assert err.msg().contains('0x0033') // ext_key_share == 0x33
		assert err.code() == int(tls_alert_to_quic_error(.unsupported_extension))
		return
	}
	assert false, 'expected an error for a key_share extension in EncryptedExtensions'
}

fn test_parse_encrypted_extensions_accepts_empty_server_name() {
	ext := encode_extension(ext_server_name, []u8{}) or { panic(err) }
	mut body := []u8{}
	body << u8(ext.len >> 8)
	body << u8(ext.len)
	body << ext
	extensions := parse_encrypted_extensions(body)!
	found := find_extension(extensions, ext_server_name) or {
		panic('expected server_name to be present')
	}
	assert found.data.len == 0
}

// test_parse_encrypted_extensions_rejects_nonempty_server_name is a
// regression test for a Codex finding (vlang/v#27680
// pullrequestreview-4806500473): RFC 6066 §3 requires the server's
// server_name acknowledgement extension_data to be empty ("the server
// SHALL include an extension of type 'server_name' in the (extended)
// server hello. The 'extension_data' field of this extension SHALL be
// empty."), but this was never validated -- a malformed non-empty payload
// passed through silently.
fn test_parse_encrypted_extensions_rejects_nonempty_server_name() {
	ext := encode_extension(ext_server_name, [u8(1), 2, 3]) or { panic(err) }
	mut body := []u8{}
	body << u8(ext.len >> 8)
	body << u8(ext.len)
	body << ext
	parse_encrypted_extensions(body) or {
		assert err.msg().contains('server_name')
		assert err.msg().contains('empty')
		return
	}
	assert false, 'expected an error for a non-empty server_name extension_data'
}

fn test_parse_encrypted_extensions_accepts_wellformed_supported_groups() {
	ext := encode_extension(ext_supported_groups, [u8(0), 2, u8(named_group_secp256r1 >> 8),
		u8(named_group_secp256r1)]) or { panic(err) }
	mut body := []u8{}
	body << u8(ext.len >> 8)
	body << u8(ext.len)
	body << ext
	extensions := parse_encrypted_extensions(body)!
	found := find_extension(extensions, ext_supported_groups) or {
		panic('expected supported_groups to be present')
	}
	assert found.data.len == 4
}

// test_parse_encrypted_extensions_rejects_malformed_supported_groups is a
// regression test for a Codex finding (vlang/v#27680
// pullrequestreview-4822597219): only the outer extension TLV framing was
// validated for supported_groups; its inner NamedGroupList length prefix
// (RFC 8446 §4.2.7 / RFC 7919) was never checked against the bytes actually
// present. A payload claiming a 2-byte NamedGroupList with only 1 byte
// present previously parsed successfully into an opaque, never-consumed
// extension.
fn test_parse_encrypted_extensions_rejects_malformed_supported_groups() {
	ext := encode_extension(ext_supported_groups, [u8(0), 2, 0xAA]) or { panic(err) }
	mut body := []u8{}
	body << u8(ext.len >> 8)
	body << u8(ext.len)
	body << ext
	parse_encrypted_extensions(body) or {
		assert err.msg().contains('supported_groups')
		assert err.msg().contains('does not match')
		return
	}
	assert false, 'expected an error for a malformed supported_groups NamedGroupList'
}

fn test_parse_encrypted_extensions_rejects_length_mismatch() {
	parse_encrypted_extensions([u8(0x00), 0x05]) or {
		assert err.msg().contains('does not match')
		return
	}
	assert false, 'expected an error for an extensions length not matching the body'
}

// test_parse_server_hello_rejects_unsolicited_alpn and
// test_parse_server_hello_hello_retry_request_rejects_unsolicited_extension
// are regression tests for a Codex finding (vlang/v#27680
// pullrequestreview-4791164664): parse_server_hello had no extension
// allowlist at all -- alpn (EncryptedExtensions-only per RFC 8446 §4.2's
// per-message table) or any other extension this client didn't request
// passed through both the real-ServerHello and HelloRetryRequest branches
// silently ignored, instead of the mandatory unsupported_extension abort
// (RFC 8446 §4.2).
fn test_parse_server_hello_rejects_unsolicited_alpn() {
	ks_ext := build_test_key_share_server_entry(named_group_secp256r1, []u8{len: 65, init: 0x04})
	alpn_ext := encode_extension(ext_alpn, [u8(0), 3, 2, u8(`h`), u8(`3`)]) or { panic(err) }
	body := build_test_server_hello([]u8{len: 32}, ks_ext, alpn_ext)
	parse_server_hello(body) or {
		assert err.msg().contains('0x0010') // ext_alpn == 0x10
		assert err.code() == int(tls_alert_to_quic_error(.unsupported_extension))
		return
	}
	assert false, 'expected an error for an unsolicited alpn extension in ServerHello'
}

fn test_parse_server_hello_hello_retry_request_rejects_unsolicited_extension() {
	ks_ext := encode_extension(ext_key_share, [u8(0x00), 0x17]) or { panic(err) }
	alpn_ext := encode_extension(ext_alpn, [u8(0), 3, 2, u8(`h`), u8(`3`)]) or { panic(err) }
	body := build_test_server_hello(hello_retry_request_random[..].clone(), ks_ext, alpn_ext)
	parse_server_hello(body) or {
		assert err.msg().contains('HelloRetryRequest')
		assert err.code() == int(tls_alert_to_quic_error(.unsupported_extension))
		return
	}
	assert false, 'expected an error for an unsolicited alpn extension in HelloRetryRequest'
}
