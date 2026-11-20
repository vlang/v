// Copyright (c) 2026 V project. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// TLS 1.2 handshake (RFC 5246 §7, RFC 4492 §5, RFC 5288/5289/6655): full
// client and server flows with ECDHE key exchange, Certificate,
// ServerKeyExchange (ECDHE), ServerHelloDone, ClientKeyExchange,
// ChangeCipherSpec and Finished, plus the SHA256 PRF. Adds RFC 7627
// extended_master_secret, RFC 7366 encrypt-then-MAC, RFC 5746 secure
// renegotiation and session-ID resumption with a bounded in-memory
// server cache. Standalone; shares no code with net.tls.
module tls12

import crypto.hmac
import crypto.rand
import crypto.sha256
import crypto.subtle
import time

// Handshake message types (RFC 5246 §7.4).
const hstype_hello_request = u8(0)
const hstype_client_hello = u8(1)
const hstype_server_hello = u8(2)
const hstype_certificate = u8(11)
const hstype_server_key_exchange = u8(12)
const hstype_server_hello_done = u8(14)
const hstype_client_key_exchange = u8(16)
const hstype_finished = u8(20)

// Alert descriptions used here.
const alert_close_notify = u8(0)
const alert_unexpected_message = u8(10)
const alert_bad_record_mac = u8(20)
const alert_handshake_failure = u8(40)
const alert_illegal_parameter = u8(47)
const alert_decode_error = u8(50)
const alert_decrypt_error = u8(51)

// Extension IDs.
const ext_server_name = u16(0)
const ext_supported_groups = u16(10)
const ext_ec_point_formats = u16(11)
const ext_signature_algorithms = u16(13)
const ext_encrypt_then_mac = u16(0x0016)
const ext_extended_master_secret = u16(0x0017)
const ext_renegotiation_info = u16(0xff01)

// Only secp256r1 is implemented.
const group_secp256r1 = u16(23)

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

// get_u16 reads a big-endian u16. SAFETY: callers must bounds-check first
// (all current call sites verify length before slicing); an unchecked call
// panics on short input instead of failing closed.
fn get_u16(b []u8) u16 {
	return (u16(b[0]) << 8) | u16(b[1])
}

// get_u24 reads a big-endian u24. SAFETY: same bounds-check contract as
// get_u16 above.
fn get_u24(b []u8) int {
	return (int(b[0]) << 16) | (int(b[1]) << 8) | int(b[2])
}

// is_ip_literal reports whether name is an IPv4 or IPv6 address literal
// (TLS callers use it to skip the SNI extension, which must not carry
// literal IPs — RFC 6066 §3). Local helper keeps this module free of the
// x509 dependency; mirrors x509.is_ip_literal semantics (any colon means
// IPv6 literal, four all-numeric dot parts mean IPv4).
fn is_ip_literal(name string) bool {
	if name.contains(':') {
		return true
	}
	parts := name.split('.')
	if parts.len != 4 {
		return false
	}
	for p in parts {
		if p.len == 0 || p.len > 3 {
			return false
		}
		for c in p.bytes() {
			if c < `0` || c > `9` {
				return false
			}
		}
		if p.int() > 255 {
			return false
		}
	}
	return true
}

// hs_msg frames a handshake message (type + u24 length + body).
fn hs_msg(typ u8, body []u8) []u8 {
	mut out := []u8{cap: 4 + body.len}
	out << typ
	put_u24(mut out, body.len)
	out << body
	return out
}

// prf_sha256 is the TLS 1.2 PRF with SHA256 (RFC 5246 §5): PRF(secret,
// label, seed) = P_hash(secret, label + seed).
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

// extended_master_secret derives the master via RFC 7627 §4:
// PRF(premaster, "extended master secret", session_hash).
fn extended_master_secret(premaster []u8, session_hash []u8) []u8 {
	return prf_sha256(premaster, 'extended master secret'.bytes(), session_hash, 48)
}

// key_expansion derives the key block (RFC 5246 §6.3): the seed is
// server_random + client_random (server first).
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

// put_ext appends one extension (type + len + data).
fn put_ext(mut out []u8, typ u16, data []u8) {
	put_u16(mut out, typ)
	put_u16(mut out, u16(data.len))
	out << data
}

// build_reneg_ext builds the renegotiation_info extension body for the
// extension data (without the outer type/len): len byte + connection bytes.
fn build_reneg_ext_body(reneg_data []u8) []u8 {
	mut b := []u8{cap: 1 + reneg_data.len}
	b << u8(reneg_data.len)
	b << reneg_data
	return b
}

// build_client_hello assembles a full ClientHello handshake message with
// empty session ID and empty renegotiation_info (wrapper for tests and
// simple callers; always offers EMS + EtM).
fn build_client_hello(client_random []u8, suites []u16, server_name string) []u8 {
	return build_client_hello_full(client_random, suites, server_name, []u8{}, []u8{})
}

// build_client_hello_full assembles a ClientHello with explicit session ID
// and renegotiation data (empty for initial, client_verify for re-handshake).
// EMS (RFC 7627) and EtM (RFC 7366) are always offered.
fn build_client_hello_full(client_random []u8, suites []u16, server_name string, session_id []u8, reneg_verify []u8) []u8 {
	mut body := []u8{cap: 160}
	put_u16(mut body, version_tls12)
	body << client_random
	body << u8(session_id.len)
	body << session_id
	mut cs := []u8{}
	for s in suites {
		put_u16(mut cs, s)
	}
	put_u16(mut body, u16(cs.len))
	body << cs
	body << u8(1)
	body << u8(0)
	mut exts := []u8{}
	// SNI carries DNS names only: literal IPs must not be sent (RFC 6066
	// §3) and an empty name is omitted. Local check keeps this module
	// free of the x509 dependency (mirrors x509.is_ip_literal).
	if server_name.len > 0 && !is_ip_literal(server_name) {
		mut name_entry := []u8{}
		name_entry << u8(0)
		put_u16(mut name_entry, u16(server_name.len))
		name_entry << server_name.bytes()
		mut sni := []u8{}
		put_u16(mut sni, u16(name_entry.len))
		sni << name_entry
		put_ext(mut exts, ext_server_name, sni)
	}
	mut groups := []u8{}
	put_u16(mut groups, 2)
	put_u16(mut groups, group_secp256r1)
	put_ext(mut exts, ext_supported_groups, groups)
	exts << [u8(ext_ec_point_formats >> 8), u8(ext_ec_point_formats & 0xff), 0x00, 0x02, 0x01,
		0x00]
	sigalgs := [u8(0x04), 0x01, 0x04, 0x03, 0x05, 0x01, 0x05, 0x03]
	mut sa := []u8{}
	put_u16(mut sa, u16(sigalgs.len))
	sa << sigalgs
	put_ext(mut exts, ext_signature_algorithms, sa)
	put_ext(mut exts, ext_renegotiation_info, build_reneg_ext_body(reneg_verify))
	put_ext(mut exts, ext_extended_master_secret, []u8{})
	put_ext(mut exts, ext_encrypt_then_mac, []u8{})
	put_u16(mut body, u16(exts.len))
	body << exts
	return hs_msg(hstype_client_hello, body)
}

// ClientHelloInfo is a parsed ClientHello.
struct ClientHelloInfo {
	client_random []u8
	suites        []u16
	server_name   string
	session_id    []u8
	ems_offered   bool
	etm_offered   bool
	reneg_info    []u8
}

// ServerHelloInfo is a parsed ServerHello.
struct ServerHelloInfo {
	server_random []u8
	suite         u16
	session_id    []u8
	ems           bool
	etm           bool
	reneg_info    []u8
}

// parse_client_hello parses a ClientHello body.
fn parse_client_hello(body []u8) !ClientHelloInfo {
	if body.len < 38 {
		return error('tls12: short ClientHello')
	}
	ver := get_u16(body[..2])
	if ver != version_tls12 {
		return error('tls12: only TLS 1.2 (0x0303) supported')
	}
	client_random := body[2..34].clone()
	mut pos := 34
	sess_len := int(body[pos])
	pos++
	if pos + sess_len > body.len {
		return error('tls12: bad ClientHello session id')
	}
	session_id := body[pos..pos + sess_len].clone()
	pos += sess_len
	if pos + 2 > body.len {
		return error('tls12: bad ClientHello suites')
	}
	cs_len := int(get_u16(body[pos..pos + 2]))
	pos += 2
	if cs_len % 2 != 0 || pos + cs_len > body.len {
		return error('tls12: bad ClientHello suites length')
	}
	mut suites := []u16{}
	for i := 0; i < cs_len; i += 2 {
		suites << get_u16(body[pos + i..pos + i + 2])
	}
	pos += cs_len
	if pos + 1 > body.len {
		return error('tls12: bad ClientHello compression')
	}
	comp_len := int(body[pos])
	pos++
	if pos + comp_len > body.len {
		return error('tls12: bad ClientHello compression list')
	}
	pos += comp_len
	mut server_name := ''
	mut ems_offered := false
	mut etm_offered := false
	mut reneg_info := []u8{}
	if pos < body.len {
		if pos + 2 > body.len {
			return error('tls12: bad ClientHello extensions')
		}
		ext_total := int(get_u16(body[pos..pos + 2]))
		pos += 2
		if pos + ext_total > body.len {
			return error('tls12: bad ClientHello extensions length')
		}
		ext_end := pos + ext_total
		for pos + 4 <= ext_end {
			ext_id := get_u16(body[pos..pos + 2])
			ext_len := int(get_u16(body[pos + 2..pos + 4]))
			pos += 4
			if pos + ext_len > ext_end {
				return error('tls12: bad ClientHello extension')
			}
			ext := body[pos..pos + ext_len]
			if ext_id == ext_server_name && ext.len >= 5 {
				name_len := int(get_u16(ext[3..5]))
				if 5 + name_len <= ext.len && ext[2] == 0 {
					server_name = ext[5..5 + name_len].bytestr()
				}
			} else if ext_id == ext_extended_master_secret {
				if ext_len == 0 {
					ems_offered = true
				}
			} else if ext_id == ext_encrypt_then_mac {
				if ext_len == 0 {
					etm_offered = true
				}
			} else if ext_id == ext_renegotiation_info {
				if ext.len < 1 {
					return error('tls12: bad renegotiation_info')
				}
				inner_len := int(ext[0])
				if inner_len != ext.len - 1 {
					return error('tls12: bad renegotiation_info length')
				}
				reneg_info = ext[1..].clone()
			}
			pos += ext_len
		}
	}
	return ClientHelloInfo{
		client_random: client_random
		suites:        suites
		server_name:   server_name
		session_id:    session_id
		ems_offered:   ems_offered
		etm_offered:   etm_offered
		reneg_info:    reneg_info
	}
}

// build_server_hello assembles a ServerHello handshake message (wrapper:
// empty session ID, empty renegotiation_info, no EMS/EtM).
fn build_server_hello(server_random []u8, suite u16) []u8 {
	body := build_server_hello_body_full(server_random, suite, []u8{}, false, false, []u8{})
	return hs_msg(hstype_server_hello, body)
}

// parse_server_hello parses a ServerHello body; session IDs and trailing
// extensions are accepted. Returns the full ServerHelloInfo.
fn parse_server_hello(body []u8) !ServerHelloInfo {
	if body.len < 38 {
		return error('tls12: bad ServerHello length')
	}
	if get_u16(body[..2]) != version_tls12 {
		return error('tls12: server negotiated non-TLS1.2 version')
	}
	server_random := body[2..34].clone()
	mut pos := 34
	sess_len := int(body[pos])
	pos++
	if pos + sess_len + 3 > body.len {
		return error('tls12: bad ServerHello session id')
	}
	session_id := body[pos..pos + sess_len].clone()
	pos += sess_len
	suite := get_u16(body[pos..pos + 2])
	pos += 2
	if body[pos] != 0 {
		return error('tls12: non-null compression not supported')
	}
	pos++
	mut ems := false
	mut etm := false
	mut reneg_info := []u8{}
	mut have_reneg := false
	if pos < body.len {
		if pos + 2 > body.len {
			return error('tls12: bad ServerHello extensions')
		}
		ext_total := int(get_u16(body[pos..pos + 2]))
		pos += 2
		if pos + ext_total > body.len {
			return error('tls12: bad ServerHello extensions length')
		}
		ext_end := pos + ext_total
		for pos + 4 <= ext_end {
			ext_id := get_u16(body[pos..pos + 2])
			ext_len := int(get_u16(body[pos + 2..pos + 4]))
			pos += 4
			if pos + ext_len > ext_end {
				return error('tls12: bad ServerHello extension')
			}
			ext := body[pos..pos + ext_len]
			if ext_id == ext_extended_master_secret {
				if ext_len == 0 {
					ems = true
				}
			} else if ext_id == ext_encrypt_then_mac {
				if ext_len == 0 {
					etm = true
				}
			} else if ext_id == ext_renegotiation_info {
				if ext.len < 1 {
					return error('tls12: bad renegotiation_info')
				}
				inner_len := int(ext[0])
				if inner_len != ext.len - 1 {
					return error('tls12: bad renegotiation_info length')
				}
				reneg_info = ext[1..].clone()
				have_reneg = true
			}
			pos += ext_len
		}
		if pos != ext_end {
			return error('tls12: bad ServerHello extensions padding')
		}
	}
	if !have_reneg {
		return error('tls12: server missing renegotiation_info (RFC 5746)')
	}
	return ServerHelloInfo{
		server_random: server_random
		suite:         suite
		session_id:    session_id
		ems:           ems
		etm:           etm
		reneg_info:    reneg_info
	}
}

// build_certificate assembles a Certificate handshake message from a blob
// of concatenated raw DER certificates.
fn build_certificate(chain []u8) []u8 {
	mut list := []u8{}
	mut r := DerReader{ data: chain }
	for !r.at_end() {
		tag, body := r.read_tlv() or { break }
		if tag != 0x30 {
			break
		}
		entry := der_tlv(0x30, body)
		put_u24(mut list, entry.len)
		list << entry
		if r.at_end() {
			break
		}
	}
	mut body := []u8{}
	put_u24(mut body, list.len)
	body << list
	return hs_msg(hstype_certificate, body)
}

// parse_certificate_chain splits a Certificate body into raw DER certs.
fn parse_certificate_chain(body []u8) ![][]u8 {
	if body.len < 3 {
		return error('tls12: short Certificate message')
	}
	total := get_u24(body[..3])
	if total != body.len - 3 {
		return error('tls12: bad Certificate length')
	}
	mut out := [][]u8{}
	mut pos := 3
	for pos < body.len {
		if pos + 3 > body.len {
			return error('tls12: bad Certificate entry')
		}
		clen := get_u24(body[pos..pos + 3])
		pos += 3
		if pos + clen > body.len {
			return error('tls12: bad Certificate entry length')
		}
		out << body[pos..pos + clen].clone()
		pos += clen
	}
	if out.len == 0 {
		return error('tls12: empty certificate chain')
	}
	return out
}

// ParsedSke is a parsed ServerKeyExchange message.
struct ParsedSke {
	params_x []u8
	params_y []u8
	hash_alg u8
	sig_alg  u8
	sig      []u8
}

// parse_server_key_exchange parses an ECDHE ServerKeyExchange body.
fn parse_server_key_exchange(body []u8) !ParsedSke {
	if body.len < 8 || body[0] != 3 {
		return error('tls12: only named-curve ServerKeyExchange supported')
	}
	if get_u16(body[1..3]) != group_secp256r1 {
		return error('tls12: only secp256r1 ServerKeyExchange supported')
	}
	plen := int(body[3])
	if 4 + plen + 4 > body.len {
		return error('tls12: bad ServerKeyExchange pubkey')
	}
	point := body[4..4 + plen]
	if point.len != 65 || point[0] != 0x04 {
		return error('tls12: only uncompressed ServerKeyExchange points supported')
	}
	sig_off := 4 + plen
	hash_alg := body[sig_off]
	sig_alg := body[sig_off + 1]
	sig_len := int(get_u16(body[sig_off + 2..sig_off + 4]))
	if sig_off + 4 + sig_len != body.len {
		return error('tls12: bad ServerKeyExchange signature length')
	}
	return ParsedSke{
		params_x: point[1..33].clone()
		params_y: point[33..65].clone()
		hash_alg: hash_alg
		sig_alg:  sig_alg
		sig:      body[sig_off + 4..].clone()
	}
}

// ske_sig_oid maps (hash, signature) algorithm bytes to an OID.
fn ske_sig_oid(hash_alg u8, sig_alg u8) !string {
	if hash_alg == 4 && sig_alg == 1 {
		return oid_sha256_rsa
	}
	if hash_alg == 5 && sig_alg == 1 {
		return oid_sha384_rsa
	}
	if hash_alg == 6 && sig_alg == 1 {
		return oid_sha512_rsa
	}
	if hash_alg == 4 && sig_alg == 3 {
		return oid_ecdsa_sha256
	}
	return error('tls12: unsupported ServerKeyExchange signature scheme')
}

// verify_server_key_exchange verifies the SKE signature over
// client_random + server_random + params against the leaf public key.
fn verify_server_key_exchange(ske ParsedSke, params []u8, client_random []u8, server_random []u8, leaf Certificate) ! {
	oid := ske_sig_oid(ske.hash_alg, ske.sig_alg)!
	mut signed := []u8{cap: 64 + params.len}
	signed << client_random
	signed << server_random
	signed << params
	match leaf.pubkey {
		RsaPublicKey {
			if ske.sig_alg != 1 {
				return error('tls12: RSA cert with non-RSA ServerKeyExchange signature')
			}
			rsa_verify_pkcs1_v15(leaf.pubkey, ske.sig, signed, oid)!
		}
		EcPublicKey {
			if ske.sig_alg != 3 {
				return error('tls12: EC cert with non-ECDSA ServerKeyExchange signature')
			}
			hash := sha256.sum256(signed)
			r, s := ecdsa_sig_from_der(ske.sig)!
			if !p256_ecdsa_verify(leaf.pubkey.x, leaf.pubkey.y, hash, r, s) {
				return error('tls12: bad ServerKeyExchange ECDSA signature')
			}
		}
	}
	p := new_p256().p
	if !(os2ip(ske.params_x) < p) || !(os2ip(ske.params_y) < p) {
		return error('tls12: ServerKeyExchange point out of range')
	}
}

// parse_client_key_exchange extracts the peer point.
fn parse_client_key_exchange(body []u8) !([]u8, []u8) {
	if body.len < 2 || int(body[0]) + 1 != body.len {
		return error('tls12: bad ClientKeyExchange length')
	}
	point := body[1..]
	if point.len != 65 || point[0] != 0x04 {
		return error('tls12: only uncompressed ClientKeyExchange points supported')
	}
	return point[1..33].clone(), point[33..65].clone()
}

// client_suites returns the suites the client offers.
fn client_suites(cfg Config) []u16 {
	if cfg.cipher_suites.len > 0 {
		mut out := []u16{}
		for s in cfg.cipher_suites {
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

// pick_suite returns the first client suite the server supports with its key.
fn pick_suite(offered []u16, key ServerKey) !u16 {
	for s in offered {
		if !suite_is_supported(s) {
			continue
		}
		match key {
			RsaServerKey {
				if suite_is_rsa(s) {
					return s
				}
			}
			EcServerKey {
				if suite_is_ecdsa(s) {
					return s
				}
			}
		}
	}
	return error('tls12: no mutually supported cipher suite')
}

// verify_leaf_strict checks dates and the chain against pinned roots
// (or a self-signed leaf). Skipped entirely when skip_verify is set.
fn verify_leaf_strict(leaf Certificate, cfg Config) ! {
	now := time.utc()
	check_validity(leaf, now)!
	verify_cert_signature(leaf, leaf.pubkey) or {
		for root_der in cfg.root_ders {
			root := parse_certificate(root_der)!
			verify_cert_signature(leaf, root.pubkey) or { continue }
			return
		}
		return error('tls12: certificate not signed by a trusted root (use skip_verify for tests)')
	}
}

// new_random returns n random bytes.
fn new_random(n int) ![]u8 {
	return rand.bytes(n)
}

// new_session_id returns a fresh 32-byte session ID.
fn new_session_id() ![]u8 {
	return rand.bytes(32)
}

// check_client_reneg verifies the ClientHello renegotiation_info against the
// stored client verify_data. Initial handshakes require empty info;
// re-handshakes require equality (RFC 5746 §3.1). Mismatch aborts.
fn check_client_reneg(offered []u8, stored_client_verify []u8, is_initial bool) ! {
	if is_initial {
		if offered.len != 0 {
			return error('tls12: unexpected renegotiation_info on initial handshake')
		}
		return
	}
	if offered.len != stored_client_verify.len {
		return error('tls12: renegotiation_info length mismatch')
	}
	if subtle.constant_time_compare(offered, stored_client_verify) != 1 {
		return error('tls12: renegotiation_info mismatch (forged Finished?)')
	}
}

// check_server_reneg verifies the ServerHello renegotiation_info on the
// client. Initial handshakes require empty info; re-handshakes require
// client_verify + server_verify (RFC 5746 §3.2).
fn check_server_reneg(offered []u8, stored_client_verify []u8, stored_server_verify []u8, is_initial bool) ! {
	if is_initial {
		if offered.len != 0 {
			return error('tls12: server renegotiation_info on initial handshake')
		}
		return
	}
	mut want := []u8{cap: stored_client_verify.len + stored_server_verify.len}
	want << stored_client_verify
	want << stored_server_verify
	if offered.len != want.len {
		return error('tls12: server renegotiation_info length mismatch')
	}
	if subtle.constant_time_compare(offered, want) != 1 {
		return error('tls12: server renegotiation_info mismatch (forged Finished?)')
	}
}

// client_handshake runs the TLS 1.2 client flow (full or abbreviated).
fn client_handshake(mut c Conn) ! {
	is_reneg := c.client_verify.len != 0 || c.server_verify.len != 0
	if is_reneg && c.did_renegotiate {
		return error('tls12: only a single re-handshake is allowed')
	}
	client_random := new_random(32)!
	suites := client_suites(c.cfg)
	offer_session := c.cfg.resume.id.clone()
	mut reneg_offer := []u8{}
	if is_reneg {
		reneg_offer = c.client_verify.clone()
	}
	hello := build_client_hello_full(client_random, suites, c.cfg.server_name, offer_session, reneg_offer)
	write_record(mut c.tcp, rectype_handshake, hello) or {
		c.send_fatal(alert_handshake_failure)
		return error('tls12: send ClientHello: ${err}')
	}
	c.transcript << hello
	typ, body := c.read_hs_msg()!
	if typ != hstype_server_hello {
		c.send_fatal(alert_unexpected_message)
		return error('tls12: expected ServerHello')
	}
	sh := parse_server_hello(body)!
	if sh.suite !in suites {
		c.send_fatal(alert_handshake_failure)
		return error('tls12: server picked unoffered suite')
	}
	// Renegotiation binding check before any crypto.
	check_server_reneg(sh.reneg_info, c.client_verify, c.server_verify, !is_reneg) or {
		c.send_fatal(alert_handshake_failure)
		return error('tls12: ${err}')
	}
	c.suite = sh.suite
	c.is_gcm = suite_is_gcm(sh.suite)
	c.is_ccm = suite_is_ccm(sh.suite)
	c.ems_negotiated = sh.ems
	// EtM only applies to CBC suites; ignore the flag for AEAD.
	c.etm_negotiated = sh.etm && suite_is_cbc(sh.suite)
	c.client_random = client_random
	c.server_random = sh.server_random
	c.session_id = sh.session_id.clone()
	// Abbreviated handshake: server echoed our session ID.
	if offer_session.len != 0 && sh.session_id.len != 0 && sh.session_id == offer_session {
		if c.cfg.resume.suite != 0 && c.cfg.resume.suite != sh.suite {
			c.send_fatal(alert_handshake_failure)
			return error('tls12: resumption suite mismatch')
		}
		if c.cfg.resume.master.len == 0 {
			c.send_fatal(alert_handshake_failure)
			return error('tls12: no resumption master')
		}
		c.master = c.cfg.resume.master.clone()
		c.suite = if c.cfg.resume.suite != 0 { c.cfg.resume.suite } else { sh.suite }
		c.is_gcm = suite_is_gcm(c.suite)
		c.is_ccm = suite_is_ccm(c.suite)
		c.resumed = true
		c.derive_record_keys()!
		// Abbreviated handshake: client sends CCS+Finished first (RFC 5246
		// §7.3, same order as the full handshake), then reads the server
		// flight. Reversing this deadlocks (both sides waiting).
		c.write_encrypted = true
		write_record(mut c.tcp, rectype_change_cipher_spec, [u8(1)])!
		thash0 := sha256.sum256(c.transcript)
		client_vd0 := finished_verify_data(c.master, 'client finished', thash0)
		c.write_hs_msg(hstype_finished, client_vd0)!
		c.client_verify = client_vd0.clone()
		c.read_peer_ccs_finished('server finished')!
		if is_reneg {
			c.did_renegotiate = true
		}
		return
	}
	c.resumed = false
	typ2, body2 := c.read_hs_msg()!
	if typ2 != hstype_certificate {
		c.send_fatal(alert_unexpected_message)
		return error('tls12: expected Certificate')
	}
	chain := parse_certificate_chain(body2)!
	leaf := parse_certificate(chain[0])!
	if !c.cfg.skip_verify {
		verify_leaf_strict(leaf, c.cfg)!
	}
	if suite_is_rsa(sh.suite) {
		if leaf.pubkey !is RsaPublicKey {
			c.send_fatal(alert_handshake_failure)
			return error('tls12: RSA suite with non-RSA cert')
		}
	} else {
		if leaf.pubkey !is EcPublicKey {
			c.send_fatal(alert_handshake_failure)
			return error('tls12: ECDSA suite with non-EC cert')
		}
	}
	typ3, body3 := c.read_hs_msg()!
	if typ3 != hstype_server_key_exchange {
		c.send_fatal(alert_unexpected_message)
		return error('tls12: expected ServerKeyExchange')
	}
	ske := parse_server_key_exchange(body3)!
	params := body3[..body3.len - 4 - ske.sig.len].clone()
	verify_server_key_exchange(ske, params, client_random, sh.server_random, leaf) or {
		c.send_fatal(alert_handshake_failure)
		return error('tls12: bad ServerKeyExchange signature: ${err}')
	}
	typ4, _ := c.read_hs_msg()!
	if typ4 != hstype_server_hello_done {
		c.send_fatal(alert_unexpected_message)
		return error('tls12: expected ServerHelloDone')
	}
	priv, pub_x, pub_y := p256_keygen()!
	premaster := p256_ecdh(priv, ske.params_x, ske.params_y)!
	if c.ems_negotiated {
		// Session hash covers CH..CKE inclusive; CKE is not yet in the
		// transcript, so hash transcript + framed CKE body.
		cke_body := build_client_key_exchange_body(pub_x, pub_y)
		cke_msg := hs_msg(hstype_client_key_exchange, cke_body)
		mut th := c.transcript.clone()
		th << cke_msg
		sess_hash := sha256.sum256(th)
		c.master = extended_master_secret(premaster, sess_hash)
	} else {
		c.master = master_secret(premaster, client_random, sh.server_random)
	}
	c.write_hs_msg(hstype_client_key_exchange, build_client_key_exchange_body(pub_x, pub_y))!
	c.derive_record_keys()!
	c.write_encrypted = true
	write_record(mut c.tcp, rectype_change_cipher_spec, [u8(1)])!
	thash := sha256.sum256(c.transcript)
	client_vd := finished_verify_data(c.master, 'client finished', thash)
	c.write_hs_msg(hstype_finished, client_vd)!
	c.client_verify = client_vd.clone()
	// Capture server verify_data inside read_peer_ccs_finished.
	c.read_peer_ccs_finished('server finished')!
	if is_reneg {
		c.did_renegotiate = true
	}
}

// build_client_key_exchange_body returns the CKE body (without framing).
fn build_client_key_exchange_body(pub_x []u8, pub_y []u8) []u8 {
	mut point := [u8(0x04)]
	point << pub_x
	point << pub_y
	mut body := [u8(point.len)]
	body << point
	return body
}

// server_handshake runs the TLS 1.2 server flow (full or abbreviated).
fn server_handshake(mut c Conn) ! {
	if c.cfg.cert_der.len == 0 || c.cfg.key_der.len == 0 {
		return error('tls12: server needs cert_der and key_der')
	}
	key := parse_server_key_pkcs8(c.cfg.key_der)!
	is_reneg := c.client_verify.len != 0 || c.server_verify.len != 0
	if is_reneg && c.did_renegotiate {
		return error('tls12: only a single re-handshake is allowed')
	}
	typ, body := c.read_hs_msg()!
	if typ != hstype_client_hello {
		c.send_fatal(alert_unexpected_message)
		return error('tls12: expected ClientHello')
	}
	hello := parse_client_hello(body)!
	check_client_reneg(hello.reneg_info, c.client_verify, !is_reneg) or {
		c.send_fatal(alert_handshake_failure)
		return error('tls12: ${err}')
	}
	// Abbreviated handshake on session-ID hit.
	if hello.session_id.len != 0 {
		if entry := session_cache_lookup(hello.session_id) {
			suite := entry.suite
			master := entry.master
			mut pick_ok := false
			for s in hello.suites {
				if s == suite {
					pick_ok = true
					break
				}
			}
			mut usable := pick_ok
			if usable {
				match key {
					RsaServerKey {
						usable = suite_is_rsa(suite)
					}
					EcServerKey {
						usable = suite_is_ecdsa(suite)
					}
				}
			}
			if usable {
				c.suite = suite
				c.is_gcm = suite_is_gcm(suite)
				c.is_ccm = suite_is_ccm(suite)
				c.ems_negotiated = hello.ems_offered
				c.etm_negotiated = hello.etm_offered && suite_is_cbc(suite)
				c.client_random = hello.client_random
				server_random := new_random(32)!
				c.server_random = server_random
				c.session_id = hello.session_id.clone()
				c.master = master.clone()
				c.resumed = true
				mut reneg_data := []u8{}
				if is_reneg {
					reneg_data << c.client_verify
					reneg_data << c.server_verify
				}
				c.write_hs_msg(hstype_server_hello, build_server_hello_body_full(server_random,
					suite, c.session_id, c.ems_negotiated, c.etm_negotiated, reneg_data))!
				c.derive_record_keys()!
				c.read_peer_ccs_finished('client finished')!
				// Recover client verify for renegotiation binding: it is the
				// Finished we just verified (last 12 bytes of transcript tail
				// before our flight). read_peer_ccs_finished stored it.
				c.write_encrypted = true
				write_record(mut c.tcp, rectype_change_cipher_spec, [u8(1)])!
				thash := sha256.sum256(c.transcript)
				server_vd := finished_verify_data(c.master, 'server finished', thash)
				c.write_hs_msg(hstype_finished, server_vd)!
				c.server_verify = server_vd.clone()
				if is_reneg {
					c.did_renegotiate = true
				}
				return
			}
		}
	}
	suite := pick_suite(hello.suites, key) or {
		c.send_fatal(alert_handshake_failure)
		return error('tls12: ${err}')
	}
	c.suite = suite
	c.is_gcm = suite_is_gcm(suite)
	c.is_ccm = suite_is_ccm(suite)
	c.ems_negotiated = hello.ems_offered
	c.etm_negotiated = hello.etm_offered && suite_is_cbc(suite)
	c.client_random = hello.client_random
	server_random := new_random(32)!
	c.server_random = server_random
	new_id := new_session_id()!
	c.session_id = new_id.clone()
	c.resumed = false
	priv, pub_x, pub_y := p256_keygen()!
	c.ecdhe_priv = priv
	mut reneg_data := []u8{}
	if is_reneg {
		reneg_data << c.client_verify
		reneg_data << c.server_verify
	}
	c.write_hs_msg(hstype_server_hello, build_server_hello_body_full(server_random, suite,
		new_id, c.ems_negotiated, c.etm_negotiated, reneg_data))!
	chain := frame_chain_der(c.cfg.cert_der, c.cfg.chain_ders)!
	c.write_hs_msg(hstype_certificate, build_certificate_body(chain)!)!
	c.send_server_key_exchange(hello.client_random, server_random, pub_x, pub_y, key)!
	c.write_hs_msg(hstype_server_hello_done, []u8{})!
	typ2, body2 := c.read_hs_msg()!
	if typ2 != hstype_client_key_exchange {
		c.send_fatal(alert_unexpected_message)
		return error('tls12: expected ClientKeyExchange')
	}
	peer_x, peer_y := parse_client_key_exchange(body2)!
	premaster := p256_ecdh(c.ecdhe_priv, peer_x, peer_y)!
	if c.ems_negotiated {
		// Transcript already includes CKE (read_hs_msg appended it).
		sess_hash := sha256.sum256(c.transcript)
		c.master = extended_master_secret(premaster, sess_hash)
	} else {
		c.master = master_secret(premaster, c.client_random, server_random)
	}
	session_cache_store(new_id, c.master, suite)
	c.derive_record_keys()!
	c.read_peer_ccs_finished('client finished')!
	c.write_encrypted = true
	write_record(mut c.tcp, rectype_change_cipher_spec, [u8(1)])!
	thash := sha256.sum256(c.transcript)
	server_vd := finished_verify_data(c.master, 'server finished', thash)
	c.write_hs_msg(hstype_finished, server_vd)!
	c.server_verify = server_vd.clone()
	if is_reneg {
		c.did_renegotiate = true
	}
}

// build_server_hello_body returns the ServerHello body (without framing).
// It always carries a renegotiation_info extension (RFC 5746): modern peers
// abort the handshake when it is missing.
fn build_server_hello_body(server_random []u8, suite u16) []u8 {
	return build_server_hello_body_full(server_random, suite, []u8{}, false, false, []u8{})
}

// build_server_hello_body_full returns the ServerHello body with explicit
// session ID, EMS/EtM flags and renegotiation data (empty for initial,
// client_verify+server_verify for re-handshake).
fn build_server_hello_body_full(server_random []u8, suite u16, session_id []u8, ems bool, etm bool, reneg_data []u8) []u8 {
	mut body := []u8{cap: 64}
	put_u16(mut body, version_tls12)
	body << server_random
	body << u8(session_id.len)
	body << session_id
	put_u16(mut body, suite)
	body << u8(0)
	mut exts := []u8{}
	put_ext(mut exts, ext_renegotiation_info, build_reneg_ext_body(reneg_data))
	if ems {
		put_ext(mut exts, ext_extended_master_secret, []u8{})
	}
	if etm {
		put_ext(mut exts, ext_encrypt_then_mac, []u8{})
	}
	put_u16(mut body, u16(exts.len))
	body << exts
	return body
}

// build_certificate_body returns the Certificate body for a DER blob.
fn build_certificate_body(chain []u8) ![]u8 {
	mut list := []u8{}
	mut r := DerReader{ data: chain }
	for !r.at_end() {
		tag, cert_body := r.read_tlv()!
		if tag != 0x30 {
			return error('tls12: bad chain DER')
		}
		entry := der_tlv(0x30, cert_body)
		put_u24(mut list, entry.len)
		list << entry
		if r.at_end() {
			break
		}
	}
	if list.len == 0 {
		return error('tls12: empty certificate chain')
	}
	mut body := []u8{}
	put_u24(mut body, list.len)
	body << list
	return body
}

// frame_chain_der concatenates leaf + extras into one DER blob.
fn frame_chain_der(leaf []u8, extras [][]u8) ![]u8 {
	mut out := leaf.clone()
	for e in extras {
		out << e
	}
	return out
}

// send_server_key_exchange builds, signs and sends ServerKeyExchange.
fn (mut c Conn) send_server_key_exchange(client_random []u8, server_random []u8, pub_x []u8, pub_y []u8, key ServerKey) ! {
	mut params := []u8{}
	params << u8(3)
	put_u16(mut params, group_secp256r1)
	mut point := [u8(0x04)]
	point << pub_x
	point << pub_y
	params << u8(point.len)
	params << point
	mut signed := []u8{cap: 64 + params.len}
	signed << client_random
	signed << server_random
	signed << params
	mut body := []u8{}
	body << params
	match key {
		RsaServerKey {
			body << [u8(0x04), 0x01]
			sig := rsa_sign_pkcs1_v15(key.n, key.d, signed, oid_sha256_rsa)!
			put_u16(mut body, u16(sig.len))
			body << sig
		}
		EcServerKey {
			body << [u8(0x04), 0x03]
			hash := sha256.sum256(signed)
			r, s := p256_ecdsa_sign(key.priv, hash)!
			sig := ecdsa_sig_to_der(r, s)
			put_u16(mut body, u16(sig.len))
			body << sig
		}
	}
	c.write_hs_msg(hstype_server_key_exchange, body)!
}

// read_peer_ccs_finished reads ChangeCipherSpec + Finished and verifies it
// against label ('client finished' / 'server finished'). On success the
// peer verify_data is stashed for renegotiation binding.
fn (mut c Conn) read_peer_ccs_finished(label string) ! {
	was_encrypted := c.read_encrypted
	typ, body := c.read_hs_msg()!
	if typ != hstype_finished {
		c.send_fatal(alert_unexpected_message)
		return error('tls12: expected Finished')
	}
	if c.read_encrypted == was_encrypted {
		c.send_fatal(alert_unexpected_message)
		return error('tls12: missing ChangeCipherSpec before Finished')
	}
	if body.len != 12 {
		c.send_fatal(alert_decode_error)
		return error('tls12: bad Finished length')
	}
	if c.transcript.len < 16 {
		c.send_fatal(alert_decode_error)
		return error('tls12: short transcript')
	}
	thash := sha256.sum256(c.transcript[..c.transcript.len - 16])
	want := finished_verify_data(c.master, label, thash)
	if subtle.constant_time_compare(body, want) != 1 {
		c.send_fatal(alert_decrypt_error)
		return error('tls12: bad Finished verify_data')
	}
	if label == 'server finished' {
		c.server_verify = body.clone()
	} else {
		c.client_verify = body.clone()
	}
}

// renegotiate runs a single secure re-handshake on an established
// connection (RFC 5746). Both sides call it concurrently (client sends the
// new ClientHello, server reads it), like the initial handshake. The new
// handshake is bound to the previous Finished verify_data via
// renegotiation_info; forged bytes abort. Only one re-handshake is allowed.
pub fn (mut c Conn) renegotiate() ! {
	if c.suite == 0 {
		return error('tls12: renegotiate before handshake')
	}
	if c.did_renegotiate {
		return error('tls12: only a single re-handshake is allowed')
	}
	c.transcript = []u8{}
	c.hs_pending = []u8{}
	c.read_encrypted = false
	c.write_encrypted = false
	c.read_seq = 0
	c.write_seq = 0
	if c.is_client {
		client_handshake(mut c)!
	} else {
		server_handshake(mut c)!
	}
}
