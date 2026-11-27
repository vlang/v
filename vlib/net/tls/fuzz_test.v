module tls

import crypto.pem
import crypto.x509
import encoding.hex
import math.big
import os

// Deterministic bounded-iteration fuzz-shape tests for the TLS 1.3 codec,
// record layer, ticket/PSK codecs, RSA/X.509/PEM parsers and the AEAD
// seal/open paths.
//
// The generator is a fixed-seed xorshift32: every run feeds the same byte
// streams, so failures reproduce exactly. Each case mixes structured seeds
// (valid encodings, then mutated) with pure-random buffers and asserts
// fail-closed behaviour: parsers either return a typed error with a
// non-empty message or a value satisfying structural invariants. Nothing
// here may panic. Total work is bounded (a few thousand small inputs) and
// completes in seconds, well under the 60 s budget.

// FuzzRng is a tiny deterministic xorshift32 generator (fixed seed per test).
struct FuzzRng {
mut:
	state u32
}

fn new_fuzz_rng(seed u32) FuzzRng {
	mut s := seed
	if s == 0 {
		s = 0x243f6a88
	}
	return FuzzRng{ state: s }
}

fn (mut r FuzzRng) next_u32() u32 {
	mut x := r.state
	x ^= x << 13
	x ^= x >> 17
	x ^= x << 5
	r.state = x
	return x
}

fn (mut r FuzzRng) next_int(n int) int {
	if n <= 0 {
		return 0
	}
	return int(r.next_u32() % u32(n))
}

fn (mut r FuzzRng) next_bytes(n int) []u8 {
	mut out := []u8{len: n}
	for i in 0 .. n {
		out[i] = u8(r.next_u32())
	}
	return out
}

// fuzz_mutate clones seed, applies 0..4 random byte substitutions, sometimes
// truncates, and sometimes appends up to max_extra random bytes.
fn fuzz_mutate(mut r FuzzRng, seed []u8, max_extra int) []u8 {
	extra := r.next_int(max_extra + 1)
	mut out := []u8{cap: seed.len + extra}
	out << seed
	if extra > 0 {
		out << r.next_bytes(extra)
	}
	if out.len > 0 && r.next_int(8) == 0 {
		out = out[..r.next_int(out.len)].clone()
	}
	nmut := r.next_int(5)
	for _ in 0 .. nmut {
		if out.len == 0 {
			break
		}
		out[r.next_int(out.len)] = u8(r.next_u32())
	}
	return out
}

fn fuzz_valid_ch_body() []u8 {
	ch := ClientHello{
		legacy_version:      tls_version_1_3
		random:              []u8{len: 32, init: 7}
		session_id:          [u8(1), 2]
		cipher_suites:       [.tls_aes_128_gcm_sha256]
		compression_methods: [u8(0)]
		extensions:          [Extension{ typ: ext_supported_versions, data: [u8(2), 3, 4] }]
	}
	return encode_client_hello(ch) or { []u8{} }
}

fn fuzz_valid_sh_body() []u8 {
	sh := ServerHello{
		legacy_version:     tls_version_1_3
		random:             []u8{len: 32, init: 9}
		session_id:         [u8(3)]
		cipher_suite:       .tls_aes_128_gcm_sha256
		compression_method: 0
		extensions:         [Extension{ typ: ext_supported_versions, data: [u8(3), 4] }]
	}
	return encode_server_hello(sh) or { []u8{} }
}

fn fuzz_one_handshake_msg(buf []u8) {
	msg, n := parse_handshake_message(buf) or {
		assert err.msg().len > 0
		return
	}
	assert n >= 4 && n <= buf.len
	assert msg.body.len + 4 == n
}

fn test_fuzz_handshake_message_no_panic() {
	mut r := new_fuzz_rng(0x12345678)
	ch_body := fuzz_valid_ch_body()
	assert ch_body.len > 0
	ch_msg := encode_handshake_message(.client_hello, ch_body) or { panic(err) }
	// the valid seed must parse
	fuzz_one_handshake_msg(ch_msg)
	for _ in 0 .. 1500 {
		mut buf := if r.next_int(2) == 0 {
			fuzz_mutate(mut r, ch_msg, 12)
		} else {
			r.next_bytes(r.next_int(266))
		}
		fuzz_one_handshake_msg(buf)
	}
}

fn test_fuzz_client_hello_no_panic() {
	mut r := new_fuzz_rng(0x0badf00d)
	valid := fuzz_valid_ch_body()
	assert valid.len > 0
	got := parse_client_hello(valid) or { panic(err) }
	assert got.random.len == 32
	assert got.legacy_version == tls_version_1_3
	for _ in 0 .. 1200 {
		buf := if r.next_int(4) == 0 {
			r.next_bytes(r.next_int(301))
		} else {
			fuzz_mutate(mut r, valid, 24)
		}
		parsed := parse_client_hello(buf) or {
			assert err.msg().len > 0
			continue
		}
		assert parsed.random.len == 32
		assert parsed.legacy_version == tls_version_1_3
	}
}

fn test_fuzz_server_hello_no_panic() {
	mut r := new_fuzz_rng(0x5eed600d)
	valid := fuzz_valid_sh_body()
	assert valid.len > 0
	got := parse_server_hello(valid) or { panic(err) }
	assert got.random.len == 32
	for _ in 0 .. 1200 {
		buf := if r.next_int(4) == 0 {
			r.next_bytes(r.next_int(301))
		} else {
			fuzz_mutate(mut r, valid, 24)
		}
		parsed := parse_server_hello(buf) or {
			assert err.msg().len > 0
			continue
		}
		assert parsed.random.len == 32
	}
}

fn fuzz_one_plain_record(data []u8) {
	rec, n := decode_plain_record(data) or {
		assert err.msg().len > 0
		return
	}
	assert n == 5 + rec.fragment.len
	assert n <= data.len
	assert rec.version == tls_version_1_3 || rec.version == 0x0301 || rec.version == 0x0302
}

fn fuzz_one_cipher_record(data []u8) {
	rec, n := decode_cipher_record(data) or {
		assert err.msg().len > 0
		return
	}
	assert n == 5 + rec.fragment.len
	assert rec.content_type == .application_data
}

fn fuzz_one_inner(data []u8) {
	ip := decode_inner(data) or {
		assert err.msg().len > 0
		return
	}
	assert ip.padding_len >= 0
	assert ip.padding_len + 1 + ip.content.len == data.len
}

fn fuzz_one_alert(data []u8) {
	lvl, desc := parse_alert(data) or {
		assert err.msg().len > 0
		return
	}
	assert lvl == .warning || lvl == .fatal
	_ = desc
}

fn test_fuzz_records_no_panic() {
	mut r := new_fuzz_rng(0xdec0ded1)
	ch_body := fuzz_valid_ch_body()
	ch_msg := encode_handshake_message(.client_hello, ch_body) or { panic(err) }
	valid_plain := encode_plain_record(PlainRecord{
		content_type: .handshake
		version:      tls_version_1_3
		fragment:     ch_msg
	})
	fuzz_one_plain_record(valid_plain)
	valid_inner := encode_inner(InnerPlaintext{
		content:      [u8(10), 20, 30]
		content_type: .application_data
		padding_len:  3
	})
	fuzz_one_inner(valid_inner)
	fuzz_one_alert([u8(2), 40])
	for _ in 0 .. 1500 {
		fuzz_one_plain_record(r.next_bytes(r.next_int(300)))
		fuzz_one_cipher_record(r.next_bytes(r.next_int(300)))
		fuzz_one_inner(r.next_bytes(r.next_int(120)))
		fuzz_one_alert(r.next_bytes(r.next_int(12)))
		if r.next_int(4) == 0 {
			fuzz_one_plain_record(fuzz_mutate(mut r, valid_plain, 16))
			fuzz_one_inner(fuzz_mutate(mut r, valid_inner, 8))
		}
	}
}

fn fuzz_one_nst(body []u8) {
	nst := parse_new_session_ticket(body) or {
		assert err.msg().len > 0
		return
	}
	assert nst.nonce.len <= 255
	assert nst.ticket.len <= 0xffff
}

fn fuzz_one_psk_offer(data []u8) {
	ids, binders := parse_psk_offer(data) or {
		assert err.msg().len > 0
		return
	}
	assert ids.len >= 0
	assert binders.len >= 0
}

fn test_fuzz_ticket_psk_ee_no_panic() {
	mut r := new_fuzz_rng(0x7e57ee57)
	valid_nst := encode_new_session_ticket(86400, 12345, [u8(9), 8, 7, 6, 5, 4, 3, 2], [
		u8(10),
		20,
		30,
	]) or { panic(err) }
	fuzz_one_nst(valid_nst)
	assert ee_accepts_early_data([u8(0), 4, 0, 42, 0, 0])
	assert !ee_accepts_early_data([]u8{})
	assert !ee_accepts_early_data([u8(0), 1, 0])
	valid_offer := encode_psk_offer([u8(1), 2, 3], 42, .tls_aes_128_gcm_sha256)
	fuzz_one_psk_offer(valid_offer)
	assert parse_psk_modes(encode_psk_modes())
	for _ in 0 .. 1200 {
		fuzz_one_nst(fuzz_mutate(mut r, valid_nst, 16))
		fuzz_one_nst(r.next_bytes(r.next_int(200)))
		fuzz_one_psk_offer(fuzz_mutate(mut r, valid_offer, 12))
		fuzz_one_psk_offer(r.next_bytes(r.next_int(160)))
		_ = ee_accepts_early_data(r.next_bytes(r.next_int(60)))
		_ = parse_psk_modes(r.next_bytes(r.next_int(20)))
		_ = verify_binder(r.next_bytes(32), r.next_bytes(32), r.next_bytes(r.next_int(64)),
			.tls_aes_128_gcm_sha256)
		sel := parse_psk_selected(r.next_bytes(r.next_int(8))) or {
			assert err.msg().len > 0
			continue
		}
		assert sel >= 0
	}
}

fn test_fuzz_aead_seal_open_no_panic() {
	mut r := new_fuzz_rng(0xae4d0101)
	key := []u8{len: 16, init: 0x2a}
	iv := []u8{len: 12, init: 0x11}
	pt := 'fuzz aead payload'.bytes()
	wire := seal_record(.tls_aes_128_gcm_sha256, key, iv, 7, .application_data, pt) or {
		panic(err)
	}
	rec, n := decode_cipher_record(wire) or { panic(err) }
	assert n == wire.len
	ct, content := open_record(.tls_aes_128_gcm_sha256, key, iv, 7, rec) or { panic(err) }
	assert ct == .application_data
	assert content == pt
	for i in 0 .. 300 {
		mut bad := wire.clone()
		pos := 5 + r.next_int(wire.len - 5)
		bad[pos] ^= u8(1 + r.next_int(255))
		bad_rec, _ := decode_cipher_record(bad) or {
			assert err.msg().len > 0
			continue
		}
		// Any ciphertext/tag mutation must fail authentication: fail
		// closed, never compare tampered plaintext.
		if _, _ := open_record(.tls_aes_128_gcm_sha256, key, iv, 7, bad_rec) {
			assert false, 'tampered record must fail authentication'
		} else {
			assert err.msg().len > 0
		}
		// wrong seq / wrong key must fail closed
		wct, wpt := open_record(.tls_aes_128_gcm_sha256, key, iv, u64(i + 1000), rec) or {
			assert err.msg().len > 0
			continue
		}
		_ = wct
		_ = wpt
		assert false, 'wrong seq must fail'
	}
	// chacha suite roundtrip with fixed material
	key32 := []u8{len: 32, init: 0x3b}
	wire2 := seal_record(.tls_chacha20_poly1305_sha256, key32, iv, 1, .handshake, pt) or {
		panic(err)
	}
	rec2, _ := decode_cipher_record(wire2) or { panic(err) }
	ct2, content2 := open_record(.tls_chacha20_poly1305_sha256, key32, iv, 1, rec2) or {
		panic(err)
	}
	assert ct2 == .handshake
	assert content2 == pt
}

// fuzz_rsa_pubkey parses the fixed 1024-bit KAT public key once; fuzzing then
// feeds random digests/signatures to the verify paths.
fn fuzz_rsa_pubkey() (big.Integer, u64) {
	n_hex := 'c3949ae5815f3feb91de6520701093c870fd657d3ba7c3dfd29fe5ebb019b3bf847a40b9a9707582d2a22b7429ec690cc77f9239984cc8685018a90fed9cf200aa678288f0bbb9d5322b3251fb37b260fcd36cc963c5d9b3955ae1676f7ee3deb05578ccf4988da48da16d197ddc612c7ef58e90063a32cfe666eaf6ef05cd63'
	n_bytes := hex.decode(n_hex) or { panic(err) }
	return big.integer_from_bytes(n_bytes, signum: 1), u64(65537)
}

fn test_fuzz_rsa_verify_no_panic() {
	n, e := fuzz_rsa_pubkey()
	mut r := new_fuzz_rng(0x851a851a)
	// the fixed KAT signature over 'assurance KAT message' must verify
	sig_hex := 'b66fa3e1fe139bc9f66c3b087c9825065a710d01991fd1e787e75c74b645314edfebed4126f86938051856228157225d37a643ff452d5d243e0af11c9f7fdc5fee23135ade33a30957b4ebc9bce03278ba3b4568e2d1caaa75ff5d7cb9c561a4eb113abc97d9796ace5e0b45302cd3f782463fd32d592d40708aaece4b0c1d49'
	sig := hex.decode(sig_hex) or { panic(err) }
	digest := hex.decode('f2b8d3ebee1863fd7e942275e546a2ca9ca771172318149dff3b5550b78488a8') or {
		panic(err)
	}
	rsa_pkcs1_verify_digest(digest, sig, n, e, .sha256) or { panic(err) }
	mut bad := sig.clone()
	bad[10] ^= 0xff
	if _ := rsa_pkcs1_verify_digest(digest, bad, n, e, .sha256) {
		assert false, 'mutated PKCS#1 v1.5 signature must fail'
	}
	k := (n.bit_len() + 7) / 8
	for _ in 0 .. 400 {
		choice := r.next_int(3)
		ds := if choice == 0 { r.next_bytes(k) } else { r.next_bytes(r.next_int(300)) }
		ss := if choice == 1 { r.next_bytes(k) } else { r.next_bytes(r.next_int(300)) }
		if _ := rsa_pkcs1_verify_digest(ds, ss, n, e, .sha256) {
		} else {
			assert err.msg().len > 0
		}
		msg := r.next_bytes(r.next_int(96))
		if _ := rsa_pss_verify(u16(0x0804), msg, r.next_bytes(r.next_int(300)), e, n) {
		} else {
			assert err.msg().len > 0
		}
	}
	// PSS roundtrip with fresh randomness must verify; tampering must not
	d_hex := '42c0959d7f6a6306a3e2a3e14e5e5a122f7e4d0551371143733fdfd971615458297f10584ddc743f1265072fbe9f51bb6adbbfaca8ae8c380e1e5c957323fdf46369a7c76007f7d46fd6dffda1996c9e111fbe897684b8d84a177e5a1b1015fc554d7e2a5b382d94e8f81dc7d4769dfeb4458cface55f3654fa8f71ef0578d01'
	d_bytes := hex.decode(d_hex) or { panic(err) }
	d := big.integer_from_bytes(d_bytes, signum: 1)
	msg := 'fuzz pss roundtrip'.bytes()
	pss_sig := rsa_pss_sign(u16(0x0804), msg, d, n) or { panic(err) }
	rsa_pss_verify(u16(0x0804), msg, pss_sig, e, n) or { panic(err) }
	mut bad_pss := pss_sig.clone()
	bad_pss[0] ^= 0x01
	if _ := rsa_pss_verify(u16(0x0804), msg, bad_pss, e, n) {
		assert false, 'mutated PSS signature must fail'
	}
}

fn fuzz_one_privkey(der []u8) {
	priv := parse_rsa_private_key(der) or {
		assert err.msg().len > 0
		return
	}
	assert priv.e != 0
}

fn test_fuzz_rsa_privkey_no_panic() {
	mut r := new_fuzz_rng(0x9a11e9a1)
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	pkcs8_pem := os.read_file(os.join_path(td, 'rsa_leaf_pkcs8_key.pem')) or { panic(err) }
	block, _ := pem.decode(pkcs8_pem) or { panic('no pem block') }
	fuzz_one_privkey(block.data)
	pkcs1_pem := os.read_file(os.join_path(td, 'rsa_leaf_rsa_key.pem')) or { panic(err) }
	block1, _ := pem.decode(pkcs1_pem) or { panic('no pkcs1 pem block') }
	fuzz_one_privkey(block1.data)
	for _ in 0 .. 600 {
		fuzz_one_privkey(fuzz_mutate(mut r, block.data, 24))
		fuzz_one_privkey(r.next_bytes(r.next_int(400)))
	}
}

fn fuzz_one_pem(s string) {
	_, _ := pem.decode(s) or { return }
}

fn fuzz_one_der(der []u8) {
	cert := x509.parse_der(der) or {
		return
	}
	_ = cert.subject
}

fn test_fuzz_pem_x509_cert_no_panic() {
	mut r := new_fuzz_rng(0xce8715ce)
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	leaf_pem := os.read_file(os.join_path(td, 'rsa_leaf_cert.pem')) or { panic(err) }
	fuzz_one_pem(leaf_pem)
	pem_seed := os.read_file(os.join_path(td, 'fuzz/pem_seed.pem')) or { panic(err) }
	seed_block, _ := pem.decode(pem_seed) or { panic('pem seed must decode') }
	assert seed_block.block_type == 'CERTIFICATE'
	block, _ := pem.decode(leaf_pem) or { panic('no pem block') }
	fuzz_one_der(block.data)
	// Certificate message body path: context(0) + empty chain is valid
	empty_body := encode_certificate_body([]u8{}, [][]u8{}) or { panic(err) }
	chain, ctx := parse_client_cert_chain(empty_body) or { panic(err) }
	assert chain.len == 0
	assert ctx.len == 0
	printables := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/= \n-'
	for _ in 0 .. 800 {
		mut s := ''
		n := r.next_int(200)
		for _ in 0 .. n {
			s += printables[r.next_int(printables.len)].ascii_str()
		}
		if r.next_int(4) == 0 {
			s = '-----BEGIN CERTIFICATE-----\n' + s + '\n-----END CERTIFICATE-----\n'
		}
		fuzz_one_pem(s)
		fuzz_one_der(fuzz_mutate(mut r, block.data, 24))
		fuzz_one_der(r.next_bytes(r.next_int(600)))
		if _ := extract_cert_der(r.next_bytes(r.next_int(600))) {
		} else {
			assert err.msg().len > 0
		}
		if cv_scheme, cv_sig := parse_cert_verify_sig(r.next_bytes(r.next_int(200))) {
			_ = cv_scheme
			_ = cv_sig
		} else {
			assert err.msg().len > 0
		}
	}
}

fn test_fuzz_corpus_no_panic() {
	dir := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata/fuzz')
	for name in ['client_hello_seed.hex', 'server_hello_seed.hex', 'plain_record_seed.hex',
		'nst_seed.hex', 'ee_seed.hex', 'handshake_ch_seed.hex', 'handshake_sh_seed.hex'] {
		text := os.read_file(os.join_path(dir, name)) or { panic('missing corpus ${name}') }
		raw := hex.decode(text.trim_space()) or { panic('bad corpus hex ${name}') }
		assert raw.len > 0, 'empty corpus ${name}'
		match name {
			'client_hello_seed.hex' {
				parsed := parse_client_hello(raw) or { panic('valid corpus must parse: ${name}') }
				assert parsed.random.len == 32
			}
			'server_hello_seed.hex' {
				parsed := parse_server_hello(raw) or { panic('valid corpus must parse: ${name}') }
				assert parsed.random.len == 32
			}
			'plain_record_seed.hex' {
				_, n := decode_plain_record(raw) or { panic('valid corpus must parse: ${name}') }
				assert n == raw.len
			}
			'nst_seed.hex' {
				nst := parse_new_session_ticket(raw) or {
					panic('valid corpus must parse: ${name}')
				}
				assert nst.ticket.len > 0
			}
			'ee_seed.hex' {
				assert ee_accepts_early_data(raw)
			}
			else {
				fuzz_one_handshake_msg(raw)
			}
		}
		// every corpus seed must also survive single-byte mutations fail-closed
		mut r := new_fuzz_rng(0xf00d5eed)
		for _ in 0 .. 60 {
			mutated := fuzz_mutate(mut r, raw, 4)
			match name {
				'client_hello_seed.hex' {
					if _ := parse_client_hello(mutated) {
					} else {
						assert err.msg().len > 0
					}
				}
				'server_hello_seed.hex' {
					if _ := parse_server_hello(mutated) {
					} else {
						assert err.msg().len > 0
					}
				}
				'plain_record_seed.hex' {
					fuzz_one_plain_record(mutated)
				}
				'nst_seed.hex' {
					fuzz_one_nst(mutated)
				}
				'ee_seed.hex' {
					_ = ee_accepts_early_data(mutated)
				}
				else {
					fuzz_one_handshake_msg(mutated)
				}
			}
		}
	}
}

// test_fuzz_crafted_invalid_inputs locks fail-closed behavior on
// hand-picked invalid buffers for the early-data and PSK-mode codecs.
fn test_fuzz_crafted_invalid_inputs() {
	// ee_accepts_early_data negatives: empty, truncated, wrong extension
	// type, wrong extension length, trailing garbage.
	assert !ee_accepts_early_data([]u8{})
	assert !ee_accepts_early_data([u8(0)])
	assert !ee_accepts_early_data([u8(0), 4, 0, 42])
	assert !ee_accepts_early_data([u8(0), 4, 0, 43, 0, 0])
	assert !ee_accepts_early_data([u8(0), 4, 0, 42, 0, 1])
	assert !ee_accepts_early_data([u8(0), 4, 0, 42, 0, 0, 0x99])
	// parse_psk_modes negatives: empty, truncated, no dhe_ke, trailing.
	assert !parse_psk_modes([]u8{})
	assert !parse_psk_modes([u8(1)])
	assert !parse_psk_modes([u8(1), 0])
	assert !parse_psk_modes([u8(2), 0, 0])
	assert !parse_psk_modes([u8(1), 1, 9])
	assert parse_psk_modes(encode_psk_modes())
	// parse_psk_selected negatives: wrong lengths.
	if _ := parse_psk_selected([]u8{}) {
		assert false, 'empty selected identity must fail'
	} else {
		assert err.msg().len > 0
	}
	if _ := parse_psk_selected([u8(0)]) {
		assert false, '1-byte selected identity must fail'
	} else {
		assert err.msg().len > 0
	}
	if _ := parse_psk_selected([u8(0), 0, 0]) {
		assert false, '3-byte selected identity must fail'
	} else {
		assert err.msg().len > 0
	}
	assert parse_psk_selected([u8(0), 1])! == 1
	// verify_binder negatives: length mismatch and wrong key never verify.
	assert !verify_binder([]u8{len: 32}, []u8{len: 32}, []u8{len: 16},
		.tls_aes_128_gcm_sha256)
	// ee rejects a body whose extension length overruns the buffer.
	assert !ee_accepts_early_data([u8(0), 8, 0, 42, 0, 4, 1, 2])
}

// test_fuzz_rejection_rates asserts all-0xFF and empty blobs fail every
// codec (rejection, never panic, never accept).
fn test_fuzz_rejection_rates() {
	ff := []u8{len: 64, init: 0xff}
	if _ := parse_client_hello(ff) {
		assert false, 'all-0xFF ClientHello must fail'
	} else {
		assert err.msg().len > 0
	}
	if _ := parse_server_hello(ff) {
		assert false, 'all-0xFF ServerHello must fail'
	} else {
		assert err.msg().len > 0
	}
	if _, _ := parse_handshake_message(ff) {
		assert false, 'all-0xFF handshake message must fail'
	} else {
		assert err.msg().len > 0
	}
	if _, _ := decode_plain_record(ff) {
		assert false, 'all-0xFF record must fail'
	} else {
		assert err.msg().len > 0
	}
	if _, _ := decode_cipher_record(ff) {
		assert false, 'all-0xFF cipher record must fail'
	} else {
		assert err.msg().len > 0
	}
	if _, _ := parse_psk_offer(ff) {
		assert false, 'all-0xFF PSK offer must fail'
	} else {
		assert err.msg().len > 0
	}
	empty := []u8{}
	if _ := parse_client_hello(empty) {
		assert false, 'empty ClientHello must fail'
	} else {
		assert err.msg().len > 0
	}
	if _ := parse_server_hello(empty) {
		assert false, 'empty ServerHello must fail'
	} else {
		assert err.msg().len > 0
	}
	if _, _ := parse_handshake_message(empty) {
		assert false, 'empty handshake message must fail'
	} else {
		assert err.msg().len > 0
	}
	if _, _ := decode_plain_record(empty) {
		assert false, 'empty record must fail'
	} else {
		assert err.msg().len > 0
	}
	if _, _ := decode_cipher_record(empty) {
		assert false, 'empty cipher record must fail'
	} else {
		assert err.msg().len > 0
	}
	if _, _ := parse_psk_offer(empty) {
		assert false, 'empty PSK offer must fail'
	} else {
		assert err.msg().len > 0
	}
}

// test_fuzz_oversize_length_prefix_no_hang proves absurd length prefixes
// fail fast with small bounded inputs (no hang, no OOM).
fn test_fuzz_oversize_length_prefix_no_hang() {
	// 0xFFFFFF handshake length with a 1-byte body: truncated, not a 16MB
	// allocation.
	if _, _ := parse_handshake_message([u8(1), 0xff, 0xff, 0xff, 0xaa]) {
		assert false, '0xFFFFFF handshake prefix must fail'
	} else {
		assert err.msg().len > 0
	}
	// Record headers naming >16K fragments are rejected before allocation.
	if _, _ := decode_plain_record([u8(0x16), 0x03, 0x03, 0xff, 0xff]) {
		assert false, 'oversize plaintext record must fail'
	} else {
		assert err.msg().contains('too large')
	}
	if _, _ := decode_cipher_record([u8(0x17), 0x03, 0x03, 0xff, 0xff]) {
		assert false, 'oversize cipher record must fail'
	} else {
		assert err.msg().contains('too large')
	}
}
