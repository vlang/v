// Copyright (c) 2026 V project. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// DTLS 1.2 handshake (RFC 6347 minimal, epoch 0/1, no 0-RTT).
// Standalone: own suites/PRF/codecs; shares no code with net.tls or
// net.tls12. Cookie exchange (HelloVerifyRequest) is optional and disabled
// by default; see Config.enable_cookie.
module dtls12

import crypto.hmac
import crypto.sha256
import crypto.subtle

// Cipher suite IDs (same values as the TLS 1.2 GCM/CBC subset).
pub const suite_ecdhe_rsa_aes128_gcm_sha256 = u16(0xC02F)
pub const suite_ecdhe_ecdsa_aes128_gcm_sha256 = u16(0xC02B)
pub const suite_ecdhe_rsa_aes128_cbc_sha256 = u16(0xC027)
pub const suite_ecdhe_ecdsa_aes128_cbc_sha256 = u16(0xC023)

// default_suites is the offer order: GCM first, then CBC.
pub const default_suites = [u16(suite_ecdhe_rsa_aes128_gcm_sha256),
	suite_ecdhe_ecdsa_aes128_gcm_sha256, suite_ecdhe_rsa_aes128_cbc_sha256,
	suite_ecdhe_ecdsa_aes128_cbc_sha256]

// suite_is_gcm reports whether the suite uses AES-GCM.
pub fn suite_is_gcm(suite u16) bool {
	return suite == suite_ecdhe_rsa_aes128_gcm_sha256
		|| suite == suite_ecdhe_ecdsa_aes128_gcm_sha256
}

// suite_is_supported reports whether this module implements the suite.
pub fn suite_is_supported(suite u16) bool {
	return suite == suite_ecdhe_rsa_aes128_gcm_sha256
		|| suite == suite_ecdhe_ecdsa_aes128_gcm_sha256
		|| suite == suite_ecdhe_rsa_aes128_cbc_sha256
		|| suite == suite_ecdhe_ecdsa_aes128_cbc_sha256
}

// suite_name returns the IANA name of a suite (or 'UNKNOWN').
pub fn suite_name(suite u16) string {
	return match suite {
		suite_ecdhe_rsa_aes128_gcm_sha256 { 'TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256' }
		suite_ecdhe_ecdsa_aes128_gcm_sha256 { 'TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256' }
		suite_ecdhe_rsa_aes128_cbc_sha256 { 'TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256' }
		suite_ecdhe_ecdsa_aes128_cbc_sha256 { 'TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256' }
		else { 'UNKNOWN' }
	}
}

// suite_key_lengths returns (mac_len, enc_key_len, fixed_iv_len).
pub fn suite_key_lengths(suite u16) (int, int, int) {
	if suite_is_gcm(suite) {
		return 0, 16, 4
	}
	return 32, 16, 16
}

// Handshake message types (RFC 5246 §7.4, plus HelloVerifyRequest).
pub const hstype_hello_request = u8(0)
pub const hstype_client_hello = u8(1)
pub const hstype_server_hello = u8(2)
pub const hstype_hello_verify_request = u8(3)
pub const hstype_server_key_exchange = u8(12)
pub const hstype_server_hello_done = u8(14)
pub const hstype_client_key_exchange = u8(16)
pub const hstype_finished = u8(20)

// put_u16 appends a big-endian u16.
fn put_u16(mut out []u8, v u16) {
	out << u8(v >> 8)
	out << u8(v & 0xff)
}

// put_u24 appends a big-endian u24.
fn put_u24(mut out []u8, v int) {
	out << u8((v >> 16) & 0xff)
	out << u8((v >> 8) & 0xff)
	out << u8(v & 0xff)
}

// get_u16 reads a big-endian u16.
fn get_u16(b []u8) u16 {
	return (u16(b[0]) << 8) | u16(b[1])
}

// get_u24 reads a big-endian u24.
fn get_u24(b []u8) int {
	return (int(b[0]) << 16) | (int(b[1]) << 8) | int(b[2])
}

// hs_msg frames a handshake message (type + u24 length + body).
fn hs_msg(typ u8, body []u8) []u8 {
	mut out := []u8{cap: 4 + body.len}
	out << typ
	put_u24(mut out, body.len)
	out << body
	return out
}

// parse_hs_msg splits one framed handshake message; returns type, body.
fn parse_hs_msg(msg []u8) !(u8, []u8) {
	if msg.len < 4 {
		return error('dtls12: short handshake message')
	}
	ln := get_u24(msg[1..4])
	if 4 + ln != msg.len {
		return error('dtls12: handshake length mismatch')
	}
	return msg[0], msg[4..].clone()
}

// prf_sha256 is the TLS 1.2 PRF with SHA256 (RFC 5246 §5).
fn prf_sha256(secret []u8, label []u8, seed []u8, out_len int) []u8 {
	mut lbl_seed := []u8{cap: label.len + seed.len}
	lbl_seed << label
	lbl_seed << seed
	return p_hash_sha256(secret, lbl_seed, out_len)
}

// p_hash_sha256 expands secret/seed via HMAC-SHA256.
fn p_hash_sha256(secret []u8, seed []u8, out_len int) []u8 {
	mut out := []u8{cap: out_len}
	mut a := seed.clone()
	for out.len < out_len {
		a = hmac.new(secret, a, sha256.sum256, sha256.block_size)
		mut chunk_in := []u8{cap: a.len + seed.len}
		chunk_in << a
		chunk_in << seed
		out << hmac.new(secret, chunk_in, sha256.sum256, sha256.block_size)
	}
	return out[..out_len]
}

// master_secret derives the 48-byte master secret (RFC 5246 §8.1).
fn master_secret(premaster []u8, client_random []u8, server_random []u8) []u8 {
	mut seed := []u8{cap: 64}
	seed << client_random
	seed << server_random
	return prf_sha256(premaster, 'master secret'.bytes(), seed, 48)
}

// key_expansion derives the key block (RFC 5246 §6.3).
fn key_expansion(master []u8, server_random []u8, client_random []u8, n int) []u8 {
	mut seed := []u8{cap: 64}
	seed << server_random
	seed << client_random
	return prf_sha256(master, 'key expansion'.bytes(), seed, n)
}

// finished_verify_data computes Finished.verify_data (RFC 5246 §7.4.9).
fn finished_verify_data(master []u8, label string, handshake_hash []u8) []u8 {
	return prf_sha256(master, label.bytes(), handshake_hash, 12)
}

// effective_suites resolves Config.cipher_suites, defaulting to all.
pub fn effective_suites(suites []u16) []u16 {
	if suites.len > 0 {
		mut out := []u16{}
		for s in suites {
			if suite_is_supported(s) {
				out << s
			}
		}
		if out.len > 0 {
			return out
		}
	}
	return default_suites.clone()
}

// select_suite picks the first server-preference suite present in offered.
pub fn select_suite(offered []u16, prefs []u16) !u16 {
	for want in prefs {
		if want in offered {
			return want
		}
	}
	return error('dtls12: no supported cipher suite')
}

// build_client_hello_msg builds a minimal ClientHello (without framing? with
// framing): body is client_random(32) + cookie + suites. Cookie is empty
// unless the server sent HelloVerifyRequest (cookie exchange).
pub fn build_client_hello_msg(client_random []u8, suites []u16, cookie []u8) ![]u8 {
	if client_random.len != 32 {
		return error('dtls12: client random must be 32 bytes')
	}
	if suites.len == 0 || suites.len > 16 {
		return error('dtls12: bad suite offer')
	}
	if cookie.len > 32 {
		return error('dtls12: cookie too long')
	}
	mut body := []u8{cap: 34 + cookie.len + suites.len * 2}
	body << client_random
	body << u8(cookie.len)
	body << cookie
	put_u16(mut body, u16(suites.len * 2))
	for s in suites {
		put_u16(mut body, s)
	}
	body << u8(1)
	body << u8(0)
	return hs_msg(hstype_client_hello, body)
}

// parse_client_hello_msg parses a ClientHello message.
pub fn parse_client_hello_msg(msg []u8) !([]u8, []u16, []u8) {
	typ, body := parse_hs_msg(msg)!
	if typ != hstype_client_hello {
		return error('dtls12: expected ClientHello')
	}
	if body.len < 34 {
		return error('dtls12: truncated ClientHello')
	}
	random := body[..32].clone()
	cookie_len := int(body[32])
	if 33 + cookie_len + 2 > body.len {
		return error('dtls12: bad ClientHello cookie')
	}
	cookie := body[33..33 + cookie_len].clone()
	mut pos := 33 + cookie_len
	cs_len := int(get_u16(body[pos..pos + 2]))
	pos += 2
	if cs_len % 2 != 0 || pos + cs_len + 2 > body.len {
		return error('dtls12: bad ClientHello suites')
	}
	mut suites := []u16{}
	for i := 0; i < cs_len; i += 2 {
		suites << get_u16(body[pos + i..pos + i + 2])
	}
	return random, suites, cookie
}

// build_server_hello_msg builds a minimal ServerHello handshake message.
pub fn build_server_hello_msg(server_random []u8, suite u16) ![]u8 {
	if server_random.len != 32 {
		return error('dtls12: server random must be 32 bytes')
	}
	mut body := []u8{cap: 36}
	body << server_random
	put_u16(mut body, suite)
	body << u8(0)
	return hs_msg(hstype_server_hello, body)
}

// parse_server_hello_msg parses a minimal ServerHello message.
pub fn parse_server_hello_msg(msg []u8) !([]u8, u16) {
	typ, body := parse_hs_msg(msg)!
	if typ != hstype_server_hello {
		return error('dtls12: expected ServerHello')
	}
	if body.len != 35 {
		return error('dtls12: bad ServerHello length')
	}
	random := body[..32].clone()
	suite := get_u16(body[32..34])
	if body[34] != 0 {
		return error('dtls12: non-null compression not supported')
	}
	if !suite_is_supported(suite) {
		return error('dtls12: unsupported cipher suite')
	}
	return random, suite
}

// build_hello_verify_request builds a HelloVerifyRequest (version + cookie).
pub fn build_hello_verify_request(cookie []u8) ![]u8 {
	if cookie.len == 0 || cookie.len > 32 {
		return error('dtls12: bad cookie length')
	}
	mut body := []u8{cap: 3 + cookie.len}
	body << u8(0xfe)
	body << u8(0xfd)
	body << u8(cookie.len)
	body << cookie
	return hs_msg(hstype_hello_verify_request, body)
}

// parse_hello_verify_request parses a HelloVerifyRequest, returning cookie.
pub fn parse_hello_verify_request(msg []u8) ![]u8 {
	typ, body := parse_hs_msg(msg)!
	if typ != hstype_hello_verify_request {
		return error('dtls12: expected HelloVerifyRequest')
	}
	if body.len < 3 {
		return error('dtls12: truncated HelloVerifyRequest')
	}
	if body[0] != 0xfe || body[1] != 0xfd {
		return error('dtls12: bad HelloVerifyRequest version')
	}
	cookie_len := int(body[2])
	if 3 + cookie_len != body.len {
		return error('dtls12: bad HelloVerifyRequest cookie')
	}
	return body[3..].clone()
}

// build_finished_msg builds a Finished message covering transcript.
pub fn build_finished_msg(master []u8, label string, transcript []u8) ![]u8 {
	th := sha256.sum256(transcript)
	vd := finished_verify_data(master, label, th)
	return hs_msg(hstype_finished, vd)
}

// verify_finished_msg verifies a Finished message against transcript.
pub fn verify_finished_msg(msg []u8, master []u8, label string, transcript []u8) ! {
	typ, body := parse_hs_msg(msg)!
	if typ != hstype_finished {
		return error('dtls12: expected Finished')
	}
	if body.len != 12 {
		return error('dtls12: bad Finished length')
	}
	th := sha256.sum256(transcript)
	expected := finished_verify_data(master, label, th)
	if subtle.constant_time_compare(body, expected) != 1 {
		return error('dtls12: Finished mismatch')
	}
}

// derive_master derives the 48-byte master secret from the ECDHE premaster
// (the 32-byte ECDH shared secret X-coordinate) via the standard TLS 1.2 PRF
// (RFC 5246 §8.1). The premaster is never sent on the wire: a passive
// observer that sees only client_random/server_random cannot derive it, and
// fresh ephemeral shares per handshake give forward secrecy.
pub fn derive_master(premaster []u8, client_random []u8, server_random []u8) ![]u8 {
	if premaster.len != 32 {
		return error('dtls12: premaster must be 32 bytes (P-256 ECDH secret)')
	}
	if client_random.len != 32 || server_random.len != 32 {
		return error('dtls12: randoms must be 32 bytes')
	}
	return master_secret(premaster, client_random, server_random)
}

// seal_for protects a fragment of any content type with the write keys of
// the current epoch (DTLS 1.2 AAD binds epoch + sequence + type).
fn (c &Conn) seal_for(epoch u16, seq u64, typ ContentType, plaintext []u8) ![]u8 {
	t := u8(typ)
	if c.is_server {
		if c.is_gcm {
			return gcm_seal(c.server_keys.key, c.server_keys.iv, epoch, seq, t, plaintext)
		}
		return cbc_seal(c.server_keys.mac, c.server_keys.key, epoch, seq, t, plaintext)
	}
	if c.is_gcm {
		return gcm_seal(c.client_keys.key, c.client_keys.iv, epoch, seq, t, plaintext)
	}
	return cbc_seal(c.client_keys.mac, c.client_keys.key, epoch, seq, t, plaintext)
}

// open_for unprotects a fragment of any content type with the read keys.
fn (c &Conn) open_for(epoch u16, seq u64, typ ContentType, sealed []u8) ![]u8 {
	t := u8(typ)
	if c.is_server {
		if c.is_gcm {
			return gcm_open(c.client_keys.key, c.client_keys.iv, epoch, seq, t, sealed)
		}
		return cbc_open(c.client_keys.mac, c.client_keys.key, epoch, seq, t, sealed)
	}
	if c.is_gcm {
		return gcm_open(c.server_keys.key, c.server_keys.iv, epoch, seq, t, sealed)
	}
	return cbc_open(c.server_keys.mac, c.server_keys.key, epoch, seq, t, sealed)
}

// seal_hs protects a handshake fragment (e.g. Finished) under the epoch-1
// handshake keys. The caller picks the epoch/sequence; Finished uses
// epoch 1, sequence 0 (sequence space restarts per epoch, RFC 6347 §4.1).
fn (c &Conn) seal_hs(epoch u16, seq u64, fragment []u8) ![]u8 {
	return c.seal_for(epoch, seq, .handshake, fragment)
}

// open_hs unprotects a handshake fragment (e.g. Finished) with the read keys.
fn (c &Conn) open_hs(epoch u16, seq u64, sealed []u8) ![]u8 {
	return c.open_for(epoch, seq, .handshake, sealed)
}

// make_cookie mints a stateless HelloVerifyRequest cookie: the first 16
// bytes of HMAC-SHA256 over (client address + client_random) under the
// per-socket cookie key. No per-handshake state is stored; the cookie is
// recomputed and compared on the second ClientHello.
fn make_cookie(cookie_key []u8, peer_str string, client_random []u8) ![]u8 {
	if cookie_key.len < 16 {
		return error('dtls12: no cookie key')
	}
	if client_random.len != 32 {
		return error('dtls12: client random must be 32 bytes')
	}
	mut data := peer_str.bytes()
	data << client_random
	mac := hmac.new(cookie_key, data, sha256.sum256, sha256.block_size)
	return mac[..16].clone()
}

// check_cookie verifies a HelloVerifyRequest cookie in constant time.
fn check_cookie(cookie []u8, cookie_key []u8, peer_str string, client_random []u8) bool {
	want := make_cookie(cookie_key, peer_str, client_random) or { return false }
	if cookie.len != want.len {
		return false
	}
	return subtle.constant_time_compare(cookie, want) == 1
}
