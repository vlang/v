// vtest build: present_openssl?
module quic

import crypto.ecdsa
import crypto.sha256
import encoding.base64
import net.mbedtls
import time

// Duplicated locally (rather than referenced from any other _test.v file)
// deliberately, per this module's own established convention -- each
// _test.v file is its own independent compilation unit (confirmed the hard
// way: a first draft of this file tried to call tls13_handshake_test.v's
// helpers directly and failed with "unknown function" -- test files do NOT
// share symbols with each other, only with the module's non-test .v
// files). Same real self-signed test cert + matching RSA private key used
// throughout this module's test suite -- see tls13_certificate_chain_test.v
// and tls13_handshake_test.v's own identical notes for provenance.
const conn_test_cert_pem = '-----BEGIN CERTIFICATE-----\nMIIEOTCCAyECFG64Q2g46jZb3kRbDOJWX/BwjSp6MA0GCSqGSIb3DQEBCwUAMEUx\nCzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRl\ncm5ldCBXaWRnaXRzIFB0eSBMdGQwIBcNMjMwODAyMTcyOTQyWhgPMjA1MDEyMTcx\nNzI5NDJaMGsxCzAJBgNVBAYTAlVTMRMwEQYDVQQIDApDYWxpZm9ybmlhMRQwEgYD\nVQQHDAtMb3MgQW5nZWxlczEdMBsGA1UECgwUQ2F0YWx5c3QgRGV2ZWxvcG1lbnQx\nEjAQBgNVBAMMCWxvY2FsaG9zdDCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoC\nggIBALqAI4fqUi+QBVWcsXglouLdOML5+w0+1hSR1KdO0Q5XPdQAs/yYWJ+KUkDw\nG++rfy9DUPq7FNRBVurXQkcAtn6gXdllGUSjwUiDo/N4mMOyS/2sufBuaeww7jVi\nrppH+zwP1tUnjRd6khl6bi1Ian9VSzr3Iy9CkXIg1GU4CPXkOydLeoQfepXxWoK1\nOUNwT3VKC/stAfY3j/NIIeiJYkyuRGFCkxn/BUjN+AsXiTugRcYKEFHdIPkOuCXp\nYbhf+lLsczpxCs3rdZG9b/N6mEDCzXTmeHkmsjdPTf+1k5DZZvKzVBBrgdxCgBb7\n5RwjF5v9WmnIc33wWgfJC6FaUzj9NYxYUbPHD+jTz0rJB/jj4u/xJlM/e5NRmXdW\n70pOMKXtWjRSolLOFIPKLY1qs3KMTAZxKKWPDDF7WlMJxMRt7nnnks5yw43Nog4C\njDLk1ZgETnPpLgo3jbmJdIv+OHKTJrBlVvDq7VTyixCoS5G8KoOmyQJhaXG6NwE2\niVhH5JIKgzgCfetfDsnjxqJ/qtrFXPa8FF2TsomD0NK/GZmIcs+9OeVB75Jn5uhF\nfLHScpiTbuu5w3P/LI/MqihLRB6RRNnRzPH8fIg5bYC9b770ta/8GcFRuYE8t+UR\nGtqXJoIKixbDlqV54kal8FQzYzhETf9+NM6Kb/lKEfG/pslvAgMBAAEwDQYJKoZI\nhvcNAQELBQADggEBALI3uNiNO0QE1brA3QYFK+d9ZroB72NrJ0UNkzYHDg2Fc6xg\n4aVVfaxY08+TmKc0JlMOW+pUxeCW/+UBSngdQiR9EE9xm0k0XIrAsy9RXxRvEtPu\nM1VI2h7ayp1Y2BrnQinevTSgtqLRyS1VbOFRl1FiyVvinw2I0KsDdAMNevAPXcOa\nQ8pUgUq6f56DkhocQaj+hxD/uV8HryNxuoSXnPhvfTN3z4YRGzsaWevJ9EYJliOM\n+XugcqfFJ+W7/QCEcAHCL+Bw6OydG5NFORr3p57PXjjcL/uKmxPBrWg2Bz6uT4uR\nMhj0zttiFHLAt9jGfyk6W57UNUja1e1ggftJJhs=\n-----END CERTIFICATE-----\n'

const conn_test_key_pem = '-----BEGIN RSA PRIVATE KEY-----\nMIIJKQIBAAKCAgEAuoAjh+pSL5AFVZyxeCWi4t04wvn7DT7WFJHUp07RDlc91ACz\n/JhYn4pSQPAb76t/L0NQ+rsU1EFW6tdCRwC2fqBd2WUZRKPBSIOj83iYw7JL/ay5\n8G5p7DDuNWKumkf7PA/W1SeNF3qSGXpuLUhqf1VLOvcjL0KRciDUZTgI9eQ7J0t6\nhB96lfFagrU5Q3BPdUoL+y0B9jeP80gh6IliTK5EYUKTGf8FSM34CxeJO6BFxgoQ\nUd0g+Q64JelhuF/6UuxzOnEKzet1kb1v83qYQMLNdOZ4eSayN09N/7WTkNlm8rNU\nEGuB3EKAFvvlHCMXm/1aachzffBaB8kLoVpTOP01jFhRs8cP6NPPSskH+OPi7/Em\nUz97k1GZd1bvSk4wpe1aNFKiUs4Ug8otjWqzcoxMBnEopY8MMXtaUwnExG3ueeeS\nznLDjc2iDgKMMuTVmAROc+kuCjeNuYl0i/44cpMmsGVW8OrtVPKLEKhLkbwqg6bJ\nAmFpcbo3ATaJWEfkkgqDOAJ9618OyePGon+q2sVc9rwUXZOyiYPQ0r8ZmYhyz705\n5UHvkmfm6EV8sdJymJNu67nDc/8sj8yqKEtEHpFE2dHM8fx8iDltgL1vvvS1r/wZ\nwVG5gTy35REa2pcmggqLFsOWpXniRqXwVDNjOERN/340zopv+UoR8b+myW8CAwEA\nAQKCAgEAkcoffF0JOBMOiHlAJhrNtSiX+ZruzNDlCxlgshUjyWEbfQG7sWbqSHUZ\njZflTrqyZqDpyca7Jp2ZM2Vocxa0klIMayfj08trCaOWY3pPeROE4d3HUJMPjEpH\nvEXTFdnVJIOBPgl3+vWfBfm17QIh9j4X3BVbVNNl3WCaiDGAl699Kl+Pe38cFeCh\nD3JZPEWsZ5SlvwjU8sNGbThjAWN8C1NjMuCXG4hGej5Ae3M/nPPR91jgnw4Me4Ut\nIL3K3RVyGqaqAPJjLsu0kWQUArJAGMfvUkXjwVklkaUV5SHtJBs+pdTXjyprTmJR\nvSXWWON5zkAEEJNY7QcZaeKYi96PFLUFI+ciEdnXn74CfSKhgZCBo+OyFZjDWW5R\nNmgAbZTN2RW0z+V54Lg36JfJrmiGs8TN06KwNjFo+iOJCdQnoUSIhTlmMfVbXPah\ntRfQvwqtfqVS9W/jkiGq9yDDqyXx093R/QTM/XqDlWJ2iOJFppOJefGFCWF6Fwll\nVT9povTAGQmXFiAxwFZxWtbFa0i8fP5QG80X6l/gRklSd6ZXAVvcLkaFGqxunDAe\nrYC2jBwHWRpVmbxw880SWRzlAsJXc7M8PQnBTlyX1mFZNnwAJgqplz0BQHQhQh4V\nqNfisUm9smtda+Hr9GBBUxs09ulery3I0lQjsArVxPqPVgUbFPECggEBANqLA5fH\n2LupOBoFH/fK5jixyGdSB8eJvU+XuS8RBBexnzTQApmDHiU7Axa/cKvxAfUgwBpU\n6OIsL6Lq6wowVInBgo7GraACwspGMIP8Z7+A8qDgSWIcpXP21Ny2RW+nukdH8ZnV\nTFtiFxLYU9GRfzSUcqvE0miKfMGP/S9Cqbew00K6CQ2xurLTR2AchfUQZJJIg7eF\nRBoftthXLQ+s1JoiLJX2gqCliFy32RMAUP+pKvKVJmVQh8bxEkoEzTV2eY7eTxsH\nJDH5hD66EZ5bW/nVAMruJ3iKjy3WvjDbnddNAz9IFKrd1RMP9dgSEKuSv/HhqwPe\n1q9Wm6LWZo8BlYcCggEBANp3M14QMcMxRlZE0TiSopi1CaE8OG0C9apToS1dol2s\n4lCsWHVPIC516LMPGU0bmCdtwJey1mgXQEKVxCWHkVhhoCKT/tN53o5qkptrhrXL\npbqmRfoMXI7LwJU+Vqi5fwSPGrSR/IzHwCUL7pHTbYN7wT5rr2rcC84XYSX31TFm\nNfMnbDuUk33ycAo07Vqts5A5FN+xViEUMFSDmfA2XmOAV77awz0l/3n3qOg9lQYe\nU4Av2nT19lGELirLInkB1ndLirWAcLaCBXKOLW4bzpNm9Bt8aiziVzcUzlJlLa+1\nnb/7//xzKi0eM/BhyJfhsmOz5B8AQ6Ca/keDk8M7JtkCggEARl8DDinE6VCpBv/l\ndlX4YgMlQ9fPN3pr4ig58iTpi3Ofj1L3s1TcLSLecMG+Vy9o8PTVxuTWhJWz1SMO\nAh7j6ePM1Yq2N9MLxDRrxOROyASOnCz8lEIjKL8vdc6fdz+sJO3OpzleuAJS6beM\n7euK6XRvpE3hbtZBK9bgsQonOkYPEOp0pds4AgM0dYdZvzrDF7OP7lVUQ5E4wFr5\n4JVHdEZS0wsoru/+g9STaqHscxaXBLvwPCl9Pxs7R2haZ7+5jr6Y/FwFVK5C3ivu\nJm7GpCDpe27KeO8tAZancXYWUlCzHfpo5Ug/Jz85a5UNlyHO+uUuuzVTLeyWew3M\nwnnBGwKCAQEAqGTBP3wUH3TX1p9s9cJxemvxZEra44woeIXF8wX9pV8hgzWVabb4\nA1f3ai31Pq5KdfnvPf8nrUxex/RRIOyCaDG4EW8qOS/zEKutHgef6nly4ZBQ2BC3\nN4pug5ttiNiSw5za5NyyYoGF5ghweA8UlwjJR6gRqri6kL0MsQt7VXyHkUmN787y\ncV5yZiut2PuTMVQOdu5miVDagAqAmdwOnXvMJtzRKU0kw4rWs0zklbbCfkhkh0sf\n9m2AeJPjmoqEGags3wKF3ugR8t8MvZbJgG0XNCiOXtKIj3iGIJTExm+jjNxd0OWk\nWOqy9lMpH4lky91ZtVuqxR0za0RMnWv24QKCAQBe8l0w9AYVNGDLv1jyPcbsncty\nNYI81yqe2mL+TC00sMCeil7C7WCP7kRklY01rH5q5gJ9Q1UV+bOj2fQdXDmQ5Bgo\n41jseh44gkbuXAeWcSDrDkJCrfvlNqFobTmUb8cdb9aQlHYfOJ31367LJspiw2SY\nmCbnLQ5sMnyBiMkcn0GfBV6IAkZVN73DPa8a1m/0Qrrv1GmBJFVbuZd9d/hAWpHa\nekhXPq0Sta+RNDfBR3aI5lAmVA17qRGiubQYJ+Ldq0aRJ40fGE51ctoSU/5RMcmh\n6+Qro+jSC94L46xMFp+1J5atgB1p/jVzTT/Ws7SLyotYUSL8zU7tcLiycQXs\n-----END RSA PRIVATE KEY-----\n'

fn conn_test_pem_to_der(pem string) []u8 {
	body :=
		pem.replace('-----BEGIN CERTIFICATE-----', '').replace('-----END CERTIFICATE-----', '').replace('\n', '').trim_space()
	return base64.decode(body)
}

// conn_test_sign_certificate_verify signs `signed_content` with the test RSA
// private key using real RSA-PSS/SHA-256 -- see tls13_handshake_test.v's
// fake_server_sign_certificate_verify (duplicated, not shared -- see this
// file's top-of-file note) for the full rationale.
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

// conn_test_extract_client_hello_key_exchange hand-parses a built
// ClientHello far enough to pull out the key_share extension's
// key_exchange bytes -- see tls13_handshake_test.v's identically-named
// helper for the full rationale (duplicated, not shared).
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

// conn_test_build_fake_server_hello builds a real ServerHello (RFC 8446
// §4.1.3) offering exactly what build_client_hello sent
// (TLS_AES_128_GCM_SHA256, secp256r1) -- see tls13_handshake_test.v's
// build_fake_server_hello for the full rationale (duplicated, not shared).
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

// conn_test_build_fake_encrypted_extensions builds a real EncryptedExtensions
// offering ALPN 'h3' and the given transport parameters -- see
// tls13_handshake_test.v's build_fake_encrypted_extensions_with_alpn for the
// full rationale (duplicated, not shared; the ALPN-parametrized variant
// isn't needed here so only the 'h3' shape is kept).
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

// conn_test_build_fake_certificate builds a real Certificate message
// carrying one DER-encoded certificate with no per-entry extensions -- see
// tls13_handshake_test.v's build_fake_certificate (duplicated, not shared).
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

// generous_transport_params is a reusable QuicTransportParameters value
// with every flow-control/stream-count limit set high enough that Phase 9b
// stream/flow-control tests aren't ALSO incidentally testing "what happens
// at limit zero" (drive_to_established's own default -- an empty
// QuicTransportParameters{} -- deliberately leaves every limit at zero,
// which is what Phase 9a's own capstone test needed since it never opens a
// stream at all). Tests that specifically want to probe a limit (e.g.
// MAX_STREAMS blocking) start from this and override just the one field
// they care about.
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

// build_fake_long_header_packet builds one long-header (Initial/Handshake)
// datagram as if from a fake server: encodes the header, appends the
// packet number, and protects it with `keys` (the same-direction keys the
// client independently derived -- QuicPacketProtectionKeys is a plain AEAD
// key/iv/hp bundle, not client/server-typed, so the identical struct value
// works for both the client's decrypt and this fake server's encrypt).
// No RFC 9000 §14.1 1200-byte padding is applied -- that requirement is on
// the CLIENT's first flight only (anti-amplification), never the server's.
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

// build_fake_one_rtt_packet mirrors build_fake_long_header_packet for a
// fake server's short-header (1-RTT) datagram.
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

// conn_test_decrypt_one_rtt decrypts and frame-parses a 1-RTT datagram
// USING A SINGLE, KNOWN generation of keys passed by the caller -- suitable
// for tests inspecting what the CLIENT itself just sent. c.app_write_keys
// DOES advance generations now (RFC 9001 §6.2: this endpoint follows a
// peer-initiated key update, via sync_write_keys_to_peer_update), so a
// caller testing anything AFTER a key-update exchange must pass the
// CURRENT c.app_write_keys at the point of decryption, not an earlier
// snapshot. NOT a substitute for conn.v's own process_one_rtt_packet
// (which must handle key-phase resolution via KeyUpdateState); this is a
// test-only, single-generation-only decrypt used purely to verify what
// conn.v's outgoing path built.
fn conn_test_decrypt_one_rtt(datagram []u8, dcid_len int, keys QuicPacketProtectionKeys) ![]QuicFrame {
	mut packet := datagram.clone()
	_, offset := parse_short_header(datagram, dcid_len)!
	pn_length := unprotect_header(mut packet, offset, keys.hp, .short)!
	mut truncated := u64(0)
	for i in 0 .. pn_length {
		truncated = (truncated << 8) | u64(packet[offset + i])
	}
	full_pn := decode_packet_number(truncated, pn_length, none)!
	header := packet[..offset + pn_length].clone()
	// unsafe, not .clone(): mirrors conn.v's process_one_rtt_packet's own
	// identical slice -- read-only pass into decrypt_packet_payload, no
	// mutation of `packet` happens after this point.
	ciphertext := unsafe { packet[offset + pn_length..] }
	payload := decrypt_packet_payload(keys, full_pn, header, ciphertext)!
	return parse_frames(payload)!
}

// drive_to_established is the shared Phase 9a/9b test fixture: dials a real
// QuicConn and drives it, against a fake in-memory server, all the way
// through RFC 9001 §4.1.2's "confirmed" checkpoint -- see
// test_full_handshake_reaches_confirmed_over_fake_transport's own (fuller)
// doc comment below for why Certificate/CertificateVerify go through
// c.handshake's direct API rather than the wire. `own_params`/`peer_params`
// let callers control flow-control/stream-count limits on either side;
// drive_to_established forces `peer_params`' two connection-ID identity
// fields (initial_source_connection_id/original_destination_connection_id)
// regardless of what the caller passed, since those MUST match this
// specific handshake's actual IDs or EncryptedExtensions processing itself
// would (correctly) fail RFC 9000 §7.3's anti-tampering check.
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

	// --- Fake server: ECDHE key exchange against the real ClientHello dial() built ---
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

	// --- Deliver ServerHello as a real, protected Initial packet ---
	sh_payload := encode_crypto_frame(0, server_hello_framed)!
	sh_datagram := build_fake_long_header_packet(.initial, c.scid, server_initial_scid, 0,
		sh_payload, c.initial_keys_server)!
	result1 := c.poll(sh_datagram.bytes, now)!
	assert result1.events.len == 0
	assert c.handshake.state() == .wait_encrypted_extensions
	now += 10

	// --- Deliver EncryptedExtensions as a real, protected Handshake packet ---
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

	// --- Certificate + CertificateVerify: direct API, not over the wire (see doc comment below) ---
	server_der := conn_test_pem_to_der(conn_test_cert_pem)
	cert_framed := conn_test_build_fake_certificate(server_der)!
	cert_msg, _ := parse_handshake_message(cert_framed)!
	c.handshake.process_certificate_or_request(cert_msg, cert_framed) or {
		// Expected: this repo's test cert is self-signed, so trust
		// validation fails -- matches tls13_handshake_test.v's own
		// documented, accepted limitation. The transcript is still
		// correctly updated by this call despite the error.
	}
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

	// --- Deliver Finished as a real, protected Handshake packet ---
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

	// --- Fake server: deliver HANDSHAKE_DONE over a real 1-RTT packet ---
	read_keys := c.app_read_keys or { panic('unreachable: just asserted != none') }
	server_app_keys := read_keys.current_keys
	// RFC 9001 §5.4.2: header protection sampling needs at least 4 bytes of
	// packet number plus a 16-byte sample after it -- a bare 1-byte
	// HANDSHAKE_DONE frame's resulting packet is 1 byte too short for that,
	// so pad with a few PADDING frames (0x00, RFC 9000 §19.1 -- legal
	// anywhere, silently ignored).
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

// test_dial_produces_a_valid_padded_initial_datagram checks dial()'s own
// contract in isolation, independent of the much larger fake-transport
// tests below: a freshly dialed connection is .handshaking, and its first
// outgoing datagram meets RFC 9000 §14.1's 1200-byte floor (the same
// property initial_exchange_test.v already proves for build_client_hello +
// manual packet construction -- this test proves dial()'s OWN composition
// of that same pipeline produces the same result).
fn test_dial_produces_a_valid_padded_initial_datagram() {
	mut c, dg := dial(DialParams{
		server_name:          'example.com'
		ca_bundle_pem:        conn_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: QuicTransportParameters{}
	}, u64(0))!
	defer {
		c.handshake.free()
	}
	assert dg.bytes.len >= min_initial_datagram_size
	assert c.state() == .handshaking
	assert c.role() == .client
}

// test_full_handshake_reaches_confirmed_over_fake_transport is Phase 9a's
// capstone integration test: drives a REAL QuicConn from dial() through the
// full RFC 9001 §4.1.2 "confirmed" checkpoint against a fake, in-memory
// server -- real ECDHE, real QUIC packet protection, real CRYPTO framing,
// real key derivation/promotion/discard at every level. This is the first
// test anywhere in the project to exercise conn.v's own composition logic
// (no prior phase had a QuicConn to test); the underlying TLS message
// semantics (extension validation, signature verification, transcript
// hashing) are already exhaustively covered by tls13_handshake_test.v and
// are NOT re-proven here.
//
// Certificate/CertificateVerify are deliberately NOT sent over the fake
// wire: this repo has no CA-flagged test certificate (documented,
// long-standing limitation -- see tls13_handshake_test.v's own note), so a
// real Certificate message routed through conn.v's normal poll() path
// would fail trust validation and conn.v would (correctly) treat that as a
// fatal protocol violation, closing the connection. Instead, following
// tls13_handshake_test.v's OWN established pattern for testing past this
// gap, Certificate/CertificateVerify are processed by calling
// c.handshake's methods DIRECTLY (white-box, same-module access -- conn.v
// itself has no other side effects for these two dispatch arms beyond
// c.handshake's own state, so nothing about conn.v's own wiring is skipped
// by this shortcut). ServerHello, EncryptedExtensions, Finished, and
// HANDSHAKE_DONE all go through the REAL wire path (dial()/poll()), since
// those four are exactly where conn.v's own key-derivation/promotion/
// discard and event-reporting logic lives.
fn test_full_handshake_reaches_confirmed_over_fake_transport() {
	mut c, _, _ := drive_to_established(QuicTransportParameters{}, QuicTransportParameters{})!
	defer {
		c.handshake.free()
	}
	assert c.handshake_completion_is_complete()
	assert c.app_write_keys != none
	assert c.app_read_keys != none
	assert c.initial_keys_discarded
	assert c.handshake_keys_discarded
}

// handshake_completion_is_complete is a tiny same-module accessor so tests
// can assert RFC 9001 §4.1.2's "complete" checkpoint without reaching into
// HandshakeCompletionState's own mut fields directly.
fn (c &QuicConn) handshake_completion_is_complete() bool {
	return c.handshake_completion.is_complete()
}

// -------------------------------------------------------------------------
// Phase 9b: steady-state 1-RTT tests
// -------------------------------------------------------------------------

// test_stream_write_read_round_trip_over_fake_transport covers
// open_stream/write_stream's OUTGOING path (verified by decrypting the
// datagram conn.v itself built, using the same fake-server technique as
// drive_to_established) and read_stream's INCOMING path (fed a real,
// protected STREAM frame from the fake server).
fn test_stream_write_read_round_trip_over_fake_transport() {
	mut c, server_initial_scid, mut now := drive_to_established(generous_transport_params(),
		generous_transport_params())!
	defer {
		c.handshake.free()
	}

	stream_id := c.open_stream(true)!
	assert stream_id == 0 // first client-initiated bidi stream id, RFC 9000 §2.1
	c.write_stream(stream_id, 'hello from client'.bytes(), true)!
	result := c.poll(none, now)!
	assert result.outgoing.len > 0
	now += 10

	write_keys := c.app_write_keys or { panic('unreachable: established asserts this') }
	frames := conn_test_decrypt_one_rtt(result.outgoing[0].bytes, server_initial_scid.len,
		write_keys)!
	mut found_stream_frame := false
	for f in frames {
		if f is StreamFrame {
			assert f.stream_id == stream_id
			assert f.offset == 0
			assert f.data == 'hello from client'.bytes()
			assert f.fin
			found_stream_frame = true
		}
	}
	assert found_stream_frame

	// --- Fake server replies with STREAM data on the same bidi stream ---
	read_keys := c.app_read_keys or { panic('unreachable: established asserts this') }
	server_app_keys := read_keys.current_keys
	reply_frame := encode_stream_frame(stream_id, 0, 'hello from server'.bytes(), true, true)!
	reply_datagram := build_fake_one_rtt_packet(c.scid, 0, reply_frame, server_app_keys, false)!
	result2 := c.poll(reply_datagram.bytes, now)!
	assert result2.events.len == 0, result2.events.str()

	got := c.read_stream(stream_id)!
	assert got == 'hello from server'.bytes()
	// A second read before any new data arrives returns nothing new.
	assert c.read_stream(stream_id)!.len == 0
}

// test_open_stream_respects_peer_max_streams_and_streams_blocked drives
// open_stream() into RFC 9000 §4.6's peer-imposed limit (advertised as 0
// bidi streams here), confirms it errors and queues STREAMS_BLOCKED (RFC
// 9000 §19.14) rather than silently failing, then confirms a MAX_STREAMS
// frame from the peer unblocks it.
fn test_open_stream_respects_peer_max_streams_and_streams_blocked() {
	own_params := generous_transport_params()
	mut restrictive_peer_params := generous_transport_params()
	restrictive_peer_params.initial_max_streams_bidi = 0
	mut c, server_initial_scid, mut now :=
		drive_to_established(own_params, restrictive_peer_params)!
	defer {
		c.handshake.free()
	}

	c.open_stream(true) or { assert err.msg().contains('STREAM_LIMIT') }

	result := c.poll(none, now)!
	assert result.outgoing.len > 0
	now += 10
	write_keys := c.app_write_keys or { panic('unreachable: established asserts this') }
	frames := conn_test_decrypt_one_rtt(result.outgoing[0].bytes, server_initial_scid.len,
		write_keys)!
	mut found_blocked := false
	for f in frames {
		if f is StreamsBlockedFrame {
			assert f.direction == .bidirectional
			assert f.maximum_streams == 0
			found_blocked = true
		}
	}
	assert found_blocked

	// --- Fake server raises the limit via MAX_STREAMS ---
	read_keys := c.app_read_keys or { panic('unreachable: established asserts this') }
	server_app_keys := read_keys.current_keys
	max_streams_frame := encode_max_streams_frame(.bidirectional, 5)!
	incoming := build_fake_one_rtt_packet(c.scid, 0, max_streams_frame, server_app_keys, false)!
	result2 := c.poll(incoming.bytes, now)!
	assert result2.events.len == 0

	stream_id := c.open_stream(true)!
	assert stream_id == 0
}

// test_close_sends_connection_close_and_transitions_to_closing covers the
// public close() API's deferred-send contract (see close()'s own doc
// comment): nothing happens until the next poll() call, which then builds
// and sends a real, protected CONNECTION_CLOSE and transitions to .closing.
fn test_close_sends_connection_close_and_transitions_to_closing() {
	mut c, server_initial_scid, now := drive_to_established(generous_transport_params(),
		generous_transport_params())!
	defer {
		c.handshake.free()
	}

	c.close(42, 'bye')
	assert c.state() == .established // deferred -- nothing happens synchronously
	result := c.poll(none, now)!
	assert c.state() == .closing
	assert result.outgoing.len > 0

	write_keys := c.app_write_keys or { panic('unreachable: established asserts this') }
	frames := conn_test_decrypt_one_rtt(result.outgoing[0].bytes, server_initial_scid.len,
		write_keys)!
	mut found_close := false
	for f in frames {
		if f is ConnectionCloseFrame {
			assert f.error_code == 42
			assert f.is_application_error
			assert f.reason == 'bye'
			found_close = true
		}
	}
	assert found_close
}

// test_close_before_one_rtt_keys_downgrades_to_transport_connection_close
// covers RFC 9000 §10.2.3: a CONNECTION_CLOSE of application-error type
// (0x1d) MUST be replaced by the transport-error type (0x1c) -- with the
// Reason Phrase field cleared -- when it has to be sent in an Initial or
// Handshake packet, since those levels aren't fully authenticated/protected
// yet and could expose application state to an observer. close() always
// marks its request as application-level (is_application_error: true,
// carrying the caller's real reason string) with no way for the caller to
// know which packet level will actually carry it -- reproduced here by
// calling close() in the window between EncryptedExtensions and Finished,
// where only Handshake keys are available (app_write_keys is still none).
fn test_close_before_one_rtt_keys_downgrades_to_transport_connection_close() {
	mut now := u64(1000)
	mut c, initial_dg := dial(DialParams{
		server_name:          'example.com'
		ca_bundle_pem:        conn_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: QuicTransportParameters{}
	}, now)!
	defer {
		c.handshake.free()
	}
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

	sh_payload := encode_crypto_frame(0, server_hello_framed)!
	sh_datagram := build_fake_long_header_packet(.initial, c.scid, server_initial_scid, 0,
		sh_payload, c.initial_keys_server)!
	result1 := c.poll(sh_datagram.bytes, now)!
	assert result1.events.len == 0
	now += 10

	hs_keys_server := c.handshake_keys_server or { panic('unreachable: just asserted != none') }
	mut ee_params := QuicTransportParameters{}
	ee_params.initial_source_connection_id = server_initial_scid
	ee_params.original_destination_connection_id = c.original_dcid
	ee_framed := conn_test_build_fake_encrypted_extensions(ee_params)!
	ee_payload := encode_crypto_frame(0, ee_framed)!
	ee_datagram := build_fake_long_header_packet(.handshake, c.scid, server_initial_scid, 0,
		ee_payload, hs_keys_server)!
	result2 := c.poll(ee_datagram.bytes, now)!
	assert result2.events.len == 0
	assert c.handshake.state() == .wait_certificate
	assert c.app_write_keys == none // still pre-1-RTT -- the window this bug lives in
	now += 10

	c.close(99, 'application-level reason that must not leak')
	poll_result := c.poll(none, now)!
	assert c.state() == .closing
	assert poll_result.outgoing.len > 0

	sent := c.sent_close_payload or {
		panic('unreachable: close_with_error always sets this on success')
	}

	frame, _ := parse_frame(sent)!
	close_frame := frame as ConnectionCloseFrame
	assert !close_frame.is_application_error
	assert close_frame.reason == ''
}

// test_non_ack_eliciting_packet_does_not_elicit_an_ack covers RFC 9000
// §13.2.1: "An endpoint MUST NOT send a non-ack-eliciting packet in
// response to a non-ack-eliciting packet, even if there are packet gaps
// that precede the received packet. This avoids an infinite feedback loop
// of acknowledgments, which could prevent the connection from ever
// becoming idle." process_one_rtt_packet/process_initial_or_handshake add
// EVERY successfully-processed packet's number to the space's
// `_received_pns` map regardless of whether any of its frames were
// ack-eliciting, and drain_outgoing unconditionally sends an ACK-only
// packet (itself non-ack-eliciting) whenever that map is non-empty -- so a
// peer's own ACK-only packet (ACK is explicitly non-ack-eliciting, RFC
// 9000 Table 3) triggers an ACK-only reply, which is exactly the
// "non-ack-eliciting in response to non-ack-eliciting" case the RFC
// prohibits.
fn test_non_ack_eliciting_packet_does_not_elicit_an_ack() {
	mut c, _, now := drive_to_established(generous_transport_params(), generous_transport_params())!
	defer {
		c.handshake.free()
	}

	read_keys := c.app_read_keys or { panic('unreachable: established asserts this') }
	server_app_keys := read_keys.current_keys
	ack_only_payload := encode_ack_frame([AckRange{
		smallest: 0
		largest:  0
	}], 0, none)!
	incoming := build_fake_one_rtt_packet(c.scid, 0, ack_only_payload, server_app_keys, false)!
	result := c.poll(incoming.bytes, now)!
	assert result.outgoing.len == 0
}

// test_handle_ack_frame_uses_peer_advertised_ack_delay_exponent covers RFC
// 9002's on_ack_received contract (its own doc comment: "ack_delay_exponent
// is the PEER's own ack_delay_exponent transport parameter") -- conn.v's
// handle_ack_frame passed the bare frame.v default_ack_delay_exponent
// constant (3) instead of c.handshake.peer_transport_parameters.
// ack_delay_exponent, so any peer advertising a non-default value (legal up
// to 20 per RFC 9000 §18.2) had every one of its ACK Delay fields
// misinterpreted, corrupting the smoothed_rtt/rttvar the PTO timer is built
// from. Verified by comparing the actual post-second-sample smoothed_rtt
// against the value the CORRECT exponent (12, set on this test's own peer
// transport parameters) predicts -- a wrong exponent (the buggy default, 3)
// predicts a measurably different value (raw wire ack_delay=5 scales to
// ~20.48ms under exponent 12 vs ~0.04ms under exponent 3, a >2ms difference
// in the resulting smoothed_rtt, easily distinguishable from floating-point-
// style rounding noise).
fn test_handle_ack_frame_uses_peer_advertised_ack_delay_exponent() {
	mut peer_params := generous_transport_params()
	peer_params.ack_delay_exponent = 12
	mut c, _, now := drive_to_established(generous_transport_params(), peer_params)!
	defer {
		c.handshake.free()
	}

	// First RTT sample -- seeds has_sample; RttEstimator.update's
	// first-sample path ignores ack_delay entirely regardless of exponent,
	// so this step is identical either way.
	c.loss_detection.on_packet_sent(.application_data, 100, 50, true, true, now)
	first_ack := AckFrame{
		largest_acknowledged: 100
		ack_delay:            0
		ranges:               [AckRange{
			smallest: 100
			largest:  100
		}]
	}
	c.handle_ack_frame(.application_data, first_ack, now + 10_000_000)
	assert c.loss_detection.rtt.smoothed_rtt == time.Duration(10_000_000)

	// Second sample: raw wire ack_delay=5. min_rtt stays 10ms (130ms isn't
	// smaller). adjusted_rtt = latest_rtt(130ms) - effective_ack_delay,
	// where effective_ack_delay = scaled_ack_delay_micros(5, exponent) in
	// nanoseconds, clamped to max_ack_delay (25ms default, not reached by
	// either candidate exponent here).
	c.loss_detection.on_packet_sent(.application_data, 101, 50, true, true, now + 20_000_000)
	second_ack := AckFrame{
		largest_acknowledged: 101
		ack_delay:            5
		ranges:               [AckRange{
			smallest: 101
			largest:  101
		}]
	}
	c.handle_ack_frame(.application_data, second_ack, now + 20_000_000 + 130_000_000)

	// scaled_ack_delay_micros(5, 12) == 5 << 12 == 20480 us == 20_480_000 ns
	adjusted_rtt_correct := time.Duration(130_000_000 - 20_480_000)
	expected_smoothed_rtt := (time.Duration(10_000_000) * 7 + adjusted_rtt_correct) / 8
	assert c.loss_detection.rtt.smoothed_rtt == expected_smoothed_rtt

	// scaled_ack_delay_micros(5, 3) (the buggy always-default value) ==
	// 5 << 3 == 40 us == 40_000 ns -- confirm the actual result does NOT
	// match what the bug would have produced, not just that it matches the
	// correct value (guards against both computations coincidentally
	// landing on the same number).
	adjusted_rtt_wrong_default := time.Duration(130_000_000 - 40_000)
	smoothed_rtt_if_bug_present := (time.Duration(10_000_000) * 7 + adjusted_rtt_wrong_default) / 8
	assert c.loss_detection.rtt.smoothed_rtt != smoothed_rtt_if_bug_present
}

// test_client_follows_server_initiated_key_update covers RFC 9001 §6.2:
// "If a packet is successfully processed using the next key and IV, then
// the peer has initiated a key update. The endpoint MUST update its send
// keys to the corresponding key phase in response... Sending keys MUST be
// updated before sending an acknowledgment for the packet that was
// received with updated keys." key_update.v's KeyUpdateState only ever
// tracked the READ direction; c.app_write_keys was set once at handshake
// completion and never advanced, so this client's own sends -- including
// the ACK for the very packet that triggers a server-initiated update --
// stayed on generation 0 forever, which RFC 9001 §6.2's last paragraph
// permits the SERVER to treat as a fatal KEY_UPDATE_ERROR.
fn test_client_follows_server_initiated_key_update() {
	mut c, server_initial_scid, now := drive_to_established(generous_transport_params(),
		generous_transport_params())!
	defer {
		c.handshake.free()
	}

	read_keys := c.app_read_keys or { panic('unreachable: established asserts this') }
	server_gen0_secret := read_keys.current_secret
	server_gen0_hp := read_keys.current_keys.hp

	// Simulate the SERVER initiating a key update: derive its next-generation
	// secret/keys via RFC 9001 §6.1's own derivation and send a packet using
	// them with the toggled Key Phase bit.
	server_gen1_secret := derive_updated_secret(server_gen0_secret)!
	server_gen1_keys := derive_updated_packet_protection_keys(server_gen1_secret, server_gen0_hp)!

	mut ping_payload := [u8(frame_type_ping)]
	ping_payload << [u8(0), 0, 0, 0]
	incoming := build_fake_one_rtt_packet(c.scid, 0, ping_payload, server_gen1_keys, true)!
	result := c.poll(incoming.bytes, now)!
	assert result.events.len == 0

	// RFC 9001 §6.2: the client's OWN send generation MUST have followed.
	assert c.app_write_generation == 1

	// The auto-ACK drain_outgoing queues for this ack-eliciting PING (same
	// poll() call) MUST be protected with the NEW (generation-1) write
	// keys -- exactly the "sending keys MUST be updated before sending an
	// acknowledgment" requirement. Decrypting it with the stale
	// generation-0 keys, or the correct generation-1 keys fetched fresh
	// from c.app_write_keys, distinguishes the two.
	assert result.outgoing.len > 0
	write_keys := c.app_write_keys or { panic('unreachable: established asserts this') }
	frames := conn_test_decrypt_one_rtt(result.outgoing[0].bytes, server_initial_scid.len,
		write_keys)!
	mut found_ack := false
	for f in frames {
		if f is AckFrame {
			found_ack = true
		}
	}
	assert found_ack
}

// test_peer_connection_close_enters_draining covers the receive side of
// RFC 9000 §10.2.2: a CONNECTION_CLOSE from the peer transitions this
// connection straight to .draining (never .closing -- draining requires no
// close of our own to be sent) and reports a connection_closed event.
fn test_peer_connection_close_enters_draining() {
	mut c, _, now := drive_to_established(generous_transport_params(), generous_transport_params())!
	defer {
		c.handshake.free()
	}

	read_keys := c.app_read_keys or { panic('unreachable: established asserts this') }
	server_app_keys := read_keys.current_keys
	cc_frame := encode_connection_close_frame(false, 7, 0, 'server done')!
	incoming := build_fake_one_rtt_packet(c.scid, 0, cc_frame, server_app_keys, false)!
	result := c.poll(incoming.bytes, now)!
	assert c.state() == .draining
	assert result.events.any(it.kind == .connection_closed)

	// Maintainer "Local AI Review" finding (2026-08-14): closing_deadline
	// being set (previous assertion block, enter_draining) is not enough on
	// its own -- compute_next_timeout() must also surface it, or a
	// caller-driven event loop with no other active timer (no idle timeout
	// configured here, no loss-detection PTO armed after a clean reply)
	// never learns it needs to call process_timeouts() again, and the
	// connection would sit in .draining forever despite the deadline being
	// correctly recorded.
	assert result.next_timeout != none

	// RFC 9000 §10.2.2: draining MUST be bounded (same period as closing,
	// recommended 3x PTO) -- a peer-initiated close is the MOST common
	// real-world close path, so process_timeouts() must eventually retire
	// this connection to .closed, not leave it draining forever. `now`
	// throughout this module is a time.sys_mono_now()-sourced nanosecond
	// instant (see IdleTimeoutState.last_reset's own doc comment), so
	// "far future" must be nanosecond-scale too -- 10 real seconds safely
	// exceeds even a first-sample PTO (default smoothed_rtt=333ms) x3.
	far_future := now + 10_000_000_000
	timeout_result := c.process_timeouts(far_future)!
	assert c.state() == .closed
	assert timeout_result.events.any(it.kind == .connection_closed)
}

// test_closing_deadline_not_extended_by_peer_close_while_already_closing
// covers RFC 9000 §10.2.2's "same end time" rule: "An endpoint MAY enter
// the draining state from the closing state if it receives a
// CONNECTION_CLOSE frame... the endpoint uses the same end time but
// ceases transmission." enter_draining unconditionally recomputed
// closing_deadline from the `now` at the moment of the closing->draining
// transition -- if that transition happens meaningfully later than the
// original close_with_error call (this endpoint sent its own close, THEN
// received the peer's close some time afterward, while still within the
// original closing period), the recomputed deadline is now+3xPTO from the
// LATER time, extending the period instead of preserving it. Self-found
// while auditing §10.2 for the conformance matrix -- process_datagram only
// special-cases .draining (not .closing) before dispatching frames, so a
// peer CONNECTION_CLOSE arriving while already .closing is a real,
// reachable path, not a theoretical one.
fn test_closing_deadline_not_extended_by_peer_close_while_already_closing() {
	mut c, _, now0 :=
		drive_to_established(generous_transport_params(), generous_transport_params())!
	defer {
		c.handshake.free()
	}

	c.close(1, 'bye')
	result1 := c.poll(none, now0)!
	assert c.state() == .closing
	assert result1.outgoing.len > 0
	original_deadline := c.closing_deadline or {
		panic('unreachable: close_with_error always sets this')
	}

	// Advance `now` by a small amount, still well within the original
	// closing period (3x PTO, at minimum hundreds of ms with default
	// smoothed_rtt) -- large enough to prove recomputation would actually
	// change the deadline, small enough the ORIGINAL deadline hasn't
	// elapsed yet.
	now1 := now0 + 50_000_000 // 50ms, nanosecond-scale per this module's convention
	read_keys := c.app_read_keys or { panic('unreachable: established asserts this') }
	server_app_keys := read_keys.current_keys
	cc_frame := encode_connection_close_frame(false, 7, 0, 'server done too')!
	incoming := build_fake_one_rtt_packet(c.scid, 1, cc_frame, server_app_keys, false)!
	c.poll(incoming.bytes, now1)!
	assert c.state() == .draining

	preserved_deadline := c.closing_deadline or {
		panic('unreachable: enter_draining always sets this')
	}

	assert preserved_deadline == original_deadline
}

// test_idle_timeout_mechanism_fires_after_configured_window is a sanity
// regression test for the idle-timeout mechanism itself, written while
// verifying a maintainer "Local AI Review" finding on PR #28083
// (2026-08-14). It also caught a genuine self-inflicted bug: an earlier
// attempt at this fix "corrected" idle_timeout_deadline() to route its
// already-real, nanosecond-scale `time.Duration` through `.milliseconds()`
// before combining with `now` -- but `now` is nanosecond-scale throughout
// this module (time.sys_mono_now()-sourced; see loss_detection_test.v's
// own RTT-sample assertions, e.g. `ld.rtt.latest_rtt == 1000 *
// time.nanosecond`, for independent confirmation), so that "fix" shrank the
// real timeout to a tiny millisecond COUNT and made the deadline arrive
// almost immediately instead of after the configured window. The ORIGINAL
// code (a bare `u64(timeout)`, no conversion) was correct all along; this
// test's own first draft (using tiny `now` deltas modeled on the wrong,
// millisecond assumption) could not have caught that mistake either, since
// small deltas pass trivially regardless of which formula is used.
fn test_idle_timeout_mechanism_fires_after_configured_window() {
	mut own_params := generous_transport_params()
	own_params.max_idle_timeout = 5000 // milliseconds, per RFC 9000 §18.2's wire format
	mut peer_params := generous_transport_params()
	peer_params.max_idle_timeout = 5000
	mut c, _, mut now := drive_to_established(own_params, peer_params)!
	defer {
		c.handshake.free()
	}

	// Genuinely idle (nothing received or sent since establishment) and
	// well past the real 5-second (5_000_000_000ns) window -- must close.
	now += 6_000_000_000
	final_result := c.process_timeouts(now)!
	assert c.state() == .closed
	assert final_result.events.any(it.kind == .connection_closed)
}

// test_process_one_rtt_packet_resets_idle_timer_on_receive is a regression
// test for a maintainer "Local AI Review" finding on PR #28083 (2026-08-14,
// a different LLM than the closing_deadline round): conn.v never called
// IdleTimeoutState.note_packet_received at all -- only note_packet_sent was
// wired in (build_initial_packet/build_handshake_packet/
// build_one_rtt_packet). An otherwise active connection that only RECEIVES
// data (never itself initiates a send) would incorrectly idle-timeout at
// the original deadline. Verifying this surfaced a second, deeper bug:
// note_packet_received itself had RFC 9000 §10.1's rule backwards --
// gating the RECEIVE-side restart on ack-eliciting, when the RFC's
// ack-eliciting condition applies only to the SEND side; receive restarts
// unconditionally on ANY successfully processed packet (fixed in
// idle_timeout.v, which also dropped the now-unnecessary is_ack_eliciting
// parameter).
//
// White-box (same-module), calling process_one_rtt_packet directly rather
// than through poll(): this module's own drain_outgoing unconditionally
// ACKs every received 1-RTT packet in the SAME poll() call
// (`app_received_pns.len > 0` gates an outgoing ACK with no ack-eliciting
// check), and building that ACK packet calls note_packet_sent -- so a
// poll()-driven test can never isolate "receive alone resets the timer"
// from "the auto-generated ACK response resets it," the same masking shape
// already seen once this session (a stale PTO masking the closing_deadline
// gap). Calling process_one_rtt_packet directly, before drain_outgoing ever
// runs, is the only way to observe the receive-side effect in isolation.
fn test_process_one_rtt_packet_resets_idle_timer_on_receive() {
	mut c, _, mut now := drive_to_established(generous_transport_params(),
		generous_transport_params())!
	defer {
		c.handshake.free()
	}

	read_keys := c.app_read_keys or { panic('unreachable: established asserts this') }
	server_app_keys := read_keys.current_keys
	stream_frame := encode_stream_frame(1, 0, 'keepalive'.bytes(), false, false)!
	incoming := build_fake_one_rtt_packet(c.scid, 0, stream_frame, server_app_keys, false)!

	now += 4000
	mut result := PollResult{}
	c.process_one_rtt_packet(incoming.bytes, now, mut result)!
	last_reset := c.idle_timeout.last_reset or { u64(0) }
	assert last_reset == now
}

// test_handshake_done_rejected_when_role_is_server is a regression test for
// a maintainer "Local AI Review" finding on PR #28083 (2026-08-14): RFC
// 9000 §19.20 -- "A HANDSHAKE_DONE frame can only be sent by the server...
// A server MUST treat receipt of a HANDSHAKE_DONE frame as a connection
// error of type PROTOCOL_VIOLATION" -- was never checked;
// dispatch_one_rtt_frame's HandshakeDoneFrame arm unconditionally advanced
// handshake state regardless of role.
//
// White-box (same-module), forcing `c.role = .server`: this codebase only
// ever constructs clients in v1 (dial() hardcodes role: .client, no server
// constructor exists), so this exact path is currently unreachable through
// any real call path -- forcing the role is the only way to exercise it.
// Still worth fixing now rather than deferring to Phase 13: stream.v's own
// QuicRole doc comment and PROGRESS.md both state the role field exists
// specifically so Phases 1-9's dispatch code needs no rework when server
// support lands, and this is a one-line, zero-risk guard matching an
// explicit RFC MUST.
fn test_handshake_done_rejected_when_role_is_server() {
	mut c, _, now := drive_to_established(generous_transport_params(), generous_transport_params())!
	defer {
		c.handshake.free()
	}
	c.role = .server

	mut result := PollResult{}
	c.dispatch_one_rtt_frame(HandshakeDoneFrame{}, now, mut result) or {
		assert err.msg().contains('PROTOCOL_VIOLATION')
		return
	}
	assert false, 'expected dispatch_one_rtt_frame to reject HANDSHAKE_DONE when role is server'
}

// test_discarded_initial_keys_reject_further_packets is a regression test
// for a reviewer finding on PR #28083: process_initial_or_handshake used
// c.initial_keys_server/c.handshake_keys_server to decrypt without checking
// c.initial_keys_discarded/c.handshake_keys_discarded first. discard_initial_
// keys/discard_handshake_keys (RFC 9001 §4.9) only reset per-space
// bookkeeping -- they never clear the key material itself -- so a stale,
// replayed, or attacker-injected packet in a discarded space was still
// successfully decrypted and its frames dispatched after the RFC-required
// discard point. RFC 9001 §4.9: "once an endpoint has discarded its
// Initial/Handshake keys, it MUST discard all packets it receives in that
// space." Observable via c.initial_received_pns staying empty: discard
// resets it to a fresh map, and only a successfully-processed packet in
// that space would repopulate it.
fn test_discarded_initial_keys_reject_further_packets() {
	mut c, server_initial_scid, mut now := drive_to_established(generous_transport_params(),
		generous_transport_params())!
	defer {
		c.handshake.free()
	}
	assert c.initial_keys_discarded

	// A bare PING frame (RFC 9000 §19.2, legal in the Initial space, always
	// a harmless no-op) so the ONLY thing that can explain a difference in
	// c.initial_received_pns is whether the discard check runs -- a CRYPTO
	// frame with arbitrary content would ALSO get rejected by the TLS layer
	// post-handshake for an unrelated reason (unexpected message), giving a
	// false pass/fail signal.
	// RFC 9001 §5.4.2: header protection sampling needs at least 4 bytes of
	// packet number plus a 16-byte sample after it -- a bare 1-byte PING
	// frame's resulting packet is a few bytes too short, so pad with
	// PADDING frames (0x00, RFC 9000 §19.1 -- legal anywhere).
	mut ping_payload := [u8(frame_type_ping)]
	ping_payload << [u8(0), 0, 0, 0]
	stale_datagram := build_fake_long_header_packet(.initial, c.scid, server_initial_scid, 5,
		ping_payload, c.initial_keys_server)!
	result := c.poll(stale_datagram.bytes, now)!
	assert result.events.len == 0
	assert c.initial_received_pns.len == 0
}

// test_discarded_handshake_keys_reject_further_packets is the Handshake-space
// sibling of test_discarded_initial_keys_reject_further_packets above -- same
// finding, same RFC 9001 §4.9 requirement, same gap in
// process_initial_or_handshake's .handshake branch.
fn test_discarded_handshake_keys_reject_further_packets() {
	mut c, server_initial_scid, mut now := drive_to_established(generous_transport_params(),
		generous_transport_params())!
	defer {
		c.handshake.free()
	}
	assert c.handshake_keys_discarded
	hs_keys_server := c.handshake_keys_server or {
		panic('unreachable: discard does not clear this')
	}

	// RFC 9001 §5.4.2: header protection sampling needs at least 4 bytes of
	// packet number plus a 16-byte sample after it -- a bare 1-byte PING
	// frame's resulting packet is a few bytes too short, so pad with
	// PADDING frames (0x00, RFC 9000 §19.1 -- legal anywhere).
	mut ping_payload := [u8(frame_type_ping)]
	ping_payload << [u8(0), 0, 0, 0]
	stale_datagram := build_fake_long_header_packet(.handshake, c.scid, server_initial_scid, 5,
		ping_payload, hs_keys_server)!
	result := c.poll(stale_datagram.bytes, now)!
	assert result.events.len == 0
	assert c.handshake_received_pns.len == 0
}

// test_wrong_destination_cid_rejected_on_long_header is a regression test for
// a reviewer finding on PR #28083: process_initial_or_handshake accepts any
// authenticated server packet and updates c.dcid/c.peer_scid from its source
// CID without ever checking header.dcid == c.scid. Initial keys are publicly
// derivable (RFC 9001 §5.2, from the DCID alone), so an off-path attacker
// who observes (or guesses) the client's chosen DCID can forge a
// well-encrypted Initial packet addressed with an ARBITRARY (wrong)
// destination CID and have it accepted as if it were a legitimate reply,
// letting it redirect the client's subsequent packets to a CID of the
// attacker's choosing. Fed as the very FIRST incoming packet (mirroring
// drive_to_established's own first step) so a pre-fix acceptance is
// observable via c.peer_scid/c.dcid changing from empty.
fn test_wrong_destination_cid_rejected_on_long_header() {
	mut c, initial_dg := dial(DialParams{
		server_name:          'example.com'
		ca_bundle_pem:        conn_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: QuicTransportParameters{}
	}, u64(0))!
	defer {
		c.handshake.free()
	}
	assert initial_dg.bytes.len >= min_initial_datagram_size
	assert c.peer_scid.len == 0

	// A bare PING frame (harmless no-op) rather than a CRYPTO frame with
	// arbitrary content -- garbage handshake-message bytes would ALSO get
	// rejected by the TLS layer as an unexpected/malformed message,
	// producing a false pass/fail signal unrelated to the DCID check.
	wrong_dcid := [u8(0xde), 0xad, 0xbe, 0xef]
	attacker_scid := [u8(0x99), 0x99, 0x99, 0x99]
	mut forged_payload := [u8(frame_type_ping)]
	forged_payload << [u8(0), 0, 0, 0]
	forged_datagram := build_fake_long_header_packet(.initial, wrong_dcid, attacker_scid, 0,
		forged_payload, c.initial_keys_server)!
	result := c.poll(forged_datagram.bytes, u64(10))!
	assert result.events.len == 0
	assert c.peer_scid.len == 0
	assert c.handshake.state() == .wait_server_hello
}

// test_wrong_destination_cid_rejected_on_short_header is the 1-RTT sibling of
// test_wrong_destination_cid_rejected_on_long_header above -- same finding,
// same missing check, in process_one_rtt_packet's use of parse_short_header's
// discarded header return value (previously `_, offset := ...`).
fn test_wrong_destination_cid_rejected_on_short_header() {
	mut c, server_initial_scid, mut now := drive_to_established(generous_transport_params(),
		generous_transport_params())!
	defer {
		c.handshake.free()
	}

	read_keys := c.app_read_keys or { panic('unreachable: established asserts this') }
	server_app_keys := read_keys.current_keys
	// Short headers don't self-describe their DCID length (the receiver
	// supplies it from its own connection state, c.scid.len) -- unlike the
	// long-header test above, this MUST be the same length as c.scid or
	// the parse itself is corrupted (a length mismatch, not a content
	// mismatch), which would fail for an unrelated reason.
	wrong_dcid := []u8{len: c.scid.len, init: 0xde}
	stream_frame := encode_stream_frame(1, 0, 'forged'.bytes(), false, false)!
	forged_datagram := build_fake_one_rtt_packet(wrong_dcid, 0, stream_frame, server_app_keys,
		false)!
	result := c.poll(forged_datagram.bytes, now)!
	assert result.events.len == 0
	assert c.streams.len() == 0
}

// test_wrong_source_cid_rejected_after_peer_scid_established is a regression
// test SELF-FOUND while auditing RFC 9000 §7.2/§17.2.2.1 to build a proper
// conn.v conformance-matrix section (in direct response to a maintainer
// reviewer's separate DCID/discarded-key findings on PR #28083, plus the
// user's explicit feedback that /vreview kept missing this class of gap).
// RFC 9000 §7.2 (verbatim): "Once a client has received a valid Initial
// packet from the server, it MUST discard any subsequent packet it
// receives on that connection with a different Source Connection ID."
// process_initial_or_handshake only ever WRITES c.peer_scid on the first
// packet (`if c.peer_scid.len == 0 {...}`) -- it never checks a LATER
// packet's header.scid against the already-established c.peer_scid. Unlike
// the destination-CID finding, this is concretely exploitable: Initial
// packet protection keys are derived from the DCID ALONE (RFC 9001 §5.2),
// with no dependency on the real server's identity, so an attacker who
// observes the client's chosen DCID can forge a second, well-encrypted
// Initial packet claiming an ARBITRARY source CID and have its frames
// (e.g. injected CRYPTO data) processed as if from the already-established
// peer. (Handshake-space packets are NOT exploitable the same way --
// Handshake keys derive from the real ECDHE shared secret -- but the fix
// still covers both spaces uniformly, matching the RFC's own unscoped
// "any subsequent packet... on that connection" wording and this file's
// established practice of implementing the full MUST rather than only the
// practically-exploitable subset.)
fn test_wrong_source_cid_rejected_after_peer_scid_established() {
	mut c, initial_dg := dial(DialParams{
		server_name:          'example.com'
		ca_bundle_pem:        conn_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: QuicTransportParameters{}
	}, u64(0))!
	defer {
		c.handshake.free()
	}
	assert initial_dg.bytes.len >= min_initial_datagram_size

	real_server_scid := [u8(0xaa), 0xbb, 0xcc, 0xdd]
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
	_ := server_priv.derive_shared_secret(client_pub)!
	server_hello_framed := conn_test_build_fake_server_hello(server_random,
		server_ecdhe_public_bytes)!
	sh_payload := encode_crypto_frame(0, server_hello_framed)!
	sh_datagram := build_fake_long_header_packet(.initial, c.scid, real_server_scid, 0, sh_payload,
		c.initial_keys_server)!
	result1 := c.poll(sh_datagram.bytes, u64(10))!
	assert result1.events.len == 0
	assert c.peer_scid == real_server_scid
	assert !c.initial_keys_discarded

	// Forged SECOND Initial packet: correct dcid (=c.scid, so it passes the
	// separate destination-CID check), but a DIFFERENT scid than the one
	// just established above.
	forged_scid := [u8(0x99), 0x99, 0x99, 0x99]
	mut forged_payload := [u8(frame_type_ping)]
	forged_payload << [u8(0), 0, 0, 0]
	forged_datagram := build_fake_long_header_packet(.initial, c.scid, forged_scid, 1,
		forged_payload, c.initial_keys_server)!
	result2 := c.poll(forged_datagram.bytes, u64(20))!
	assert result2.events.len == 0
	assert c.peer_scid == real_server_scid
	// initial_received_pns itself is drained (reset to empty) by
	// drain_outgoing's own ACK-building logic on every poll() call, so it
	// can't distinguish "nothing new arrived" from "something arrived and
	// got ACKed" across two separate poll() calls -- pn_spaces.initial's
	// own largest_received is the stable, persistent signal: it stays at 0
	// (the first legit packet) if the forged pn=1 packet was correctly
	// rejected, or advances to 1 if it was wrongly processed.
	largest_received := c.pn_spaces.initial.largest_received or { u64(999) }
	assert largest_received == 0
}

// test_compute_next_timeout_includes_closing_deadline is a regression test
// for a maintainer "Local AI Review" finding on PR #28083 (2026-08-14):
// closing_deadline being set (by enter_draining/close_with_error) is not
// enough on its own -- compute_next_timeout() must also surface it in its
// merged deadline, or a caller-driven event loop with no other active timer
// (no idle timeout configured, nothing currently in flight) never learns it
// needs to call process_timeouts() again, leaving the connection stuck in
// .closing/.draining forever despite the deadline being correctly recorded.
// Same-module (white-box) test, deliberately isolating this from
// loss-detection/idle-timeout noise: a fixture driven through the full
// fake-transport flow always has SOME packet outstanding (the fixture never
// constructs ACK frames back to the client), which happens to keep a PTO
// timer armed and masks this exact gap -- force `bytes_in_flight` to 0 to
// reproduce the precise "nothing else armed" scenario the finding
// describes.
fn test_compute_next_timeout_includes_closing_deadline() {
	mut c, _ := dial(DialParams{
		server_name:          'example.com'
		ca_bundle_pem:        conn_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: QuicTransportParameters{}
	}, u64(0))!
	defer {
		c.handshake.free()
	}
	c.congestion_control.bytes_in_flight = 0
	assert c.idle_timeout_deadline() == none
	assert c.compute_next_timeout() == none

	c.enter_draining(u64(500))
	assert c.compute_next_timeout() != none
}

// test_write_stream_on_auto_created_sibling_stream_is_not_stuck is a
// regression test for a bug found during /vreview: QuicStreamSet.
// get_or_create (stream.v) auto-creates every LOWER-numbered same-category
// stream when the peer references a higher one directly (RFC 9000 §2.1 --
// "before a stream is created, all streams of the same type with
// lower-numbered stream IDs MUST be created"), but handle_stream_frame/
// handle_reset_stream_frame only called ensure_stream_windows for the named
// frame.stream_id, never for those auto-created siblings. write_stream() on
// such a sibling queued data that drain_pending_stream_writes could never
// send (its `c.stream_send_windows[stream_id] or { continue }` lookup
// always missed), silently and permanently -- no error, no event, just a
// write that never reaches the wire.
fn test_write_stream_on_auto_created_sibling_stream_is_not_stuck() {
	mut c, server_initial_scid, mut now := drive_to_established(generous_transport_params(),
		generous_transport_params())!
	defer {
		c.handshake.free()
	}

	// Server-initiated bidi stream ids are 1, 5, 9, ... (base 1, step 4).
	// Referencing id=9 directly auto-creates siblings 1 and 5 with NO
	// flow-control windows under the pre-fix code.
	read_keys := c.app_read_keys or { panic('unreachable: established asserts this') }
	server_app_keys := read_keys.current_keys
	stream_frame := encode_stream_frame(9, 0, 'from server on stream 9'.bytes(), true, true)!
	incoming := build_fake_one_rtt_packet(c.scid, 0, stream_frame, server_app_keys, false)!
	result1 := c.poll(incoming.bytes, now)!
	// Stream 9 was named directly by this frame, so it fires
	// peer_stream_opened (Phase 12a) -- but its auto-created siblings 1/5
	// were never themselves named by any frame, so they must NOT.
	assert result1.events.len == 1, result1.events.str()
	assert result1.events[0].kind == .peer_stream_opened
	assert result1.events[0].stream_id? == u64(9)
	now += 10

	// Stream 1 (an auto-created sibling, never itself named in any frame)
	// is a real bidi stream from the client's perspective -- writing to it
	// must eventually reach the wire, not silently vanish.
	c.write_stream(1, 'sibling data'.bytes(), true)!
	result2 := c.poll(none, now)!
	write_keys := c.app_write_keys or { panic('unreachable: established asserts this') }
	mut found_sibling_stream_frame := false
	for dg in result2.outgoing {
		frames := conn_test_decrypt_one_rtt(dg.bytes, server_initial_scid.len, write_keys)!
		for f in frames {
			if f is StreamFrame {
				if f.stream_id == 1 {
					assert f.data == 'sibling data'.bytes()
					assert f.fin
					found_sibling_stream_frame = true
				}
			}
		}
	}
	assert found_sibling_stream_frame
}

// -----------------------------------------------------------------------
// Phase 12a: negotiated_alpn(), peer_stream_opened, stream_recv_status()
// -----------------------------------------------------------------------

// test_negotiated_alpn_is_none_before_established_and_selected_value_after
// checks negotiated_alpn()'s own two-state contract directly: none before
// EncryptedExtensions is processed (a freshly dialed connection, still
// .handshaking), the actual selected value once .established.
// drive_to_established's fake server hardcodes selecting "h3" from the
// offered ["h3"] list (conn_test_build_fake_encrypted_extensions), so the
// post-established value asserted here is exactly what a real Phase 12
// caller would check to confirm h3 was actually negotiated.
fn test_negotiated_alpn_is_none_before_established_and_selected_value_after() {
	mut fresh, _ := dial(DialParams{
		server_name:          'example.com'
		ca_bundle_pem:        conn_test_cert_pem
		alpn_protocols:       ['h3']
		transport_parameters: QuicTransportParameters{}
	}, u64(0))!
	defer {
		fresh.handshake.free()
	}
	assert fresh.state() == .handshaking
	assert fresh.negotiated_alpn() == none

	mut c, _, _ := drive_to_established(generous_transport_params(), generous_transport_params())!
	defer {
		c.handshake.free()
	}
	assert c.negotiated_alpn()? == 'h3'
}

// test_peer_stream_opened_never_fires_for_a_locally_opened_stream is the
// regression case for the bug this project's own /vreview process caught
// while first writing note_peer_stream_discovered: get_or_create's fast
// path (`if existing := s.streams[raw_id] { return existing }`) returns
// early for ANY already-known ID with no re-check of who initiated it, so
// naively firing the event after every successful get_or_create call wrongly
// reported a peer's ORDINARY REPLY on a stream this endpoint itself opened
// as a newly "peer-opened" stream. The fix (checking
// StreamId.is_locally_initiated before ever firing) is what this test
// pins down.
fn test_peer_stream_opened_never_fires_for_a_locally_opened_stream() {
	mut c, server_initial_scid, mut now := drive_to_established(generous_transport_params(),
		generous_transport_params())!
	defer {
		c.handshake.free()
	}
	stream_id := c.open_stream(true)!
	c.write_stream(stream_id, 'hello from client'.bytes(), true)!
	opened := c.poll(none, now)!
	assert opened.events.len == 0, opened.events.str()
	now += 10

	read_keys := c.app_read_keys or { panic('unreachable: established asserts this') }
	server_app_keys := read_keys.current_keys
	reply_frame := encode_stream_frame(stream_id, 0, 'hello from server'.bytes(), true, true)!
	reply_datagram := build_fake_one_rtt_packet(c.scid, 0, reply_frame, server_app_keys, false)!
	replied := c.poll(reply_datagram.bytes, now)!
	assert replied.events.len == 0, replied.events.str()
	_ = server_initial_scid
}

// test_peer_stream_opened_survives_reordering_past_an_auto_created_filler
// is the standout regression case this sub-phase's plan specifically calls
// for: get_or_create auto-creates every LOWER-numbered same-category
// stream as an empty filler when a higher one is referenced directly (RFC
// 9000 §2.1). If discovery were keyed off "was this ID newly inserted into
// c.streams" instead of "was this ID directly named by a frame," a stream
// arriving out of order (ordinary over UDP -- e.g. a QPACK decoder stream
// reaching this client before its encoder stream) would have its lower ID
// silently pre-created as a filler by the HIGHER stream's frame, and its
// own later, real STREAM frame would then hit get_or_create's fast
// already-exists path and never be announced at all.
fn test_peer_stream_opened_survives_reordering_past_an_auto_created_filler() {
	mut c, _, mut now := drive_to_established(generous_transport_params(),
		generous_transport_params())!
	defer {
		c.handshake.free()
	}
	read_keys := c.app_read_keys or { panic('unreachable: established asserts this') }
	server_app_keys := read_keys.current_keys

	// Server-initiated bidi ids are 1, 5, 9, ...; referencing 9 first
	// silently auto-creates fillers for 1 and 5 with no frame ever having
	// named them directly.
	high_frame := encode_stream_frame(9, 0, 'high'.bytes(), true, true)!
	high_datagram := build_fake_one_rtt_packet(c.scid, 0, high_frame, server_app_keys, false)!
	high_result := c.poll(high_datagram.bytes, now)!
	assert high_result.events.len == 1, high_result.events.str()
	assert high_result.events[0].stream_id? == u64(9)
	now += 10

	// Stream 1 (an auto-created filler above) now gets its OWN, real frame
	// -- must still be announced, exactly once, not silently skipped
	// because it already technically existed in c.streams.
	low_frame := encode_stream_frame(1, 0, 'low'.bytes(), true, true)!
	low_datagram := build_fake_one_rtt_packet(c.scid, 0, low_frame, server_app_keys, false)!
	low_result := c.poll(low_datagram.bytes, now)!
	assert low_result.events.len == 1, low_result.events.str()
	assert low_result.events[0].kind == .peer_stream_opened
	assert low_result.events[0].stream_id? == u64(1)
}

// test_peer_stream_opened_fires_from_a_bare_reset_stream_frame confirms
// discovery isn't STREAM-frame-specific: a RESET_STREAM can legitimately be
// the very first frame this connection ever sees for a given stream ID
// (RFC 9000 §3.2's Receive Stream State Machine allows Recv -> Reset Recvd
// directly), and handle_reset_stream_frame must announce it exactly like
// handle_stream_frame does.
fn test_peer_stream_opened_fires_from_a_bare_reset_stream_frame() {
	mut c, _, now := drive_to_established(generous_transport_params(), generous_transport_params())!
	defer {
		c.handshake.free()
	}
	read_keys := c.app_read_keys or { panic('unreachable: established asserts this') }
	server_app_keys := read_keys.current_keys

	reset_frame := encode_reset_stream_frame(1, 42, 0)!
	reset_datagram := build_fake_one_rtt_packet(c.scid, 0, reset_frame, server_app_keys, false)!
	result := c.poll(reset_datagram.bytes, now)!
	assert result.events.len == 1, result.events.str()
	assert result.events[0].kind == .peer_stream_opened
	assert result.events[0].stream_id? == u64(1)
}

// test_stream_recv_status_reports_all_three_terminal_states drives one
// stream through open (a non-FIN STREAM frame) -> fin_received (a later
// FIN-carrying STREAM frame) and a second, independent stream through
// open -> reset_received (a bare RESET_STREAM), and confirms
// stream_recv_status returns none for a stream ID this connection has
// never seen and for a locally-initiated UNI stream (send-only on this
// endpoint -- has_recv() is false).
fn test_stream_recv_status_reports_all_three_terminal_states() {
	mut c, _, now := drive_to_established(generous_transport_params(), generous_transport_params())!
	defer {
		c.handshake.free()
	}
	assert c.stream_recv_status(1) == none // never seen at all

	read_keys := c.app_read_keys or { panic('unreachable: established asserts this') }
	server_app_keys := read_keys.current_keys

	// Stream 1: open (a non-FIN STREAM frame -- data arrived, no terminal
	// condition yet) -> fin_received via a later FIN-carrying STREAM frame.
	mid_frame := encode_stream_frame(1, 0, 'partial'.bytes(), false, true)!
	mid_datagram := build_fake_one_rtt_packet(c.scid, 0, mid_frame, server_app_keys, false)!
	c.poll(mid_datagram.bytes, now)!
	open_status := c.stream_recv_status(1) or { panic('expected a status for a known stream') }
	assert open_status.state == .open
	assert open_status.reset_error == none

	fin_frame := encode_stream_frame(1, u64('partial'.len), 'complete'.bytes(), true, true)!
	fin_datagram := build_fake_one_rtt_packet(c.scid, 0, fin_frame, server_app_keys, false)!
	c.poll(fin_datagram.bytes, now)!
	fin_status := c.stream_recv_status(1) or { panic('expected a status for a known stream') }
	assert fin_status.state == .fin_received
	assert fin_status.reset_error == none

	// Stream 5: open -> reset_received via a bare RESET_STREAM, no prior
	// STREAM frame at all.
	reset_frame := encode_reset_stream_frame(5, 99, 0)!
	reset_datagram := build_fake_one_rtt_packet(c.scid, 0, reset_frame, server_app_keys, false)!
	c.poll(reset_datagram.bytes, now)!
	reset_status := c.stream_recv_status(5) or { panic('expected a status for a known stream') }
	assert reset_status.state == .reset_received
	assert reset_status.reset_error? == u64(99)

	// A locally-opened UNI stream has no receive side on this endpoint.
	uni_id := c.open_stream(false)!
	assert c.stream_recv_status(uni_id) == none
}
