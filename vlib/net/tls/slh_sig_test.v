module tls

import crypto.pem
import crypto.slh_dsa
import crypto.x509
import os
import time

fn slh_read(name string) []u8 {
	return os.read_file(os.join_path(@VEXEROOT, 'vlib/net/tls/testdata/' + name)) or {
		panic('read fixture ${name}: ${err}')
	}.bytes()
}

fn slh_pem_block(pem_text []u8, typ string) []u8 {
	block, _ := pem.decode(pem_text.bytestr()) or { panic('no ${typ} pem block') }
	assert block.block_type == typ
	return block.data.clone()
}

fn slh_store(ca_pem_name string) CertStore {
	ca_der := slh_pem_block(slh_read(ca_pem_name), 'CERTIFICATE')
	ca_cert := x509.parse_der(ca_der) or { panic('parse ca: ${err}') }
	return CertStore{
		roots: [ca_cert]
	}
}

fn slh_leaf(leaf_pem_name string) ([]u8, x509.PublicKey) {
	leaf_der := slh_pem_block(slh_read(leaf_pem_name), 'CERTIFICATE')
	leaf := x509.parse_der(leaf_der) or { panic('parse leaf: ${err}') }
	pk := leaf.public_key() or { panic('leaf pubkey: ${err}') }
	return leaf_der, pk
}

// slh_openssl_bin locates an OpenSSL CLI: PATH first, then the common Windows
// install path. Returns '' when unavailable; callers must skip gracefully.
fn slh_openssl_bin() string {
	res := os.exec(['openssl', 'version'])
	if res.exit_code == 0 {
		return 'openssl'
	}
	win := 'C:\\Program Files\\OpenSSL-Win64\\bin\\openssl.exe'
	res2 := os.exec([win, 'version'])
	if res2.exit_code == 0 {
		return win
	}
	return ''
}

// slh_case bundles one SLH-DSA SHAKE parameter set's TLS parameters.
struct SlhCase {
	params  slh_dsa.ParameterSet
	scheme  u16
	pk_len  int
	sig_len int
	ca      string
	leaf    string
	key     string
}

fn slh_cases() []SlhCase {
	return [
		SlhCase{
			params:  .shake_128s
			scheme:  u16(0x0917)
			pk_len:  32
			sig_len: 7856
			ca:      'slhshake128s_ca_cert.pem'
			leaf:    'slhshake128s_leaf_cert.pem'
			key:     'slhshake128s_leaf_key.pem'
		},
		SlhCase{
			params:  .shake_128f
			scheme:  u16(0x0918)
			pk_len:  32
			sig_len: 17088
			ca:      'slhshake128f_ca_cert.pem'
			leaf:    'slhshake128f_leaf_cert.pem'
			key:     'slhshake128f_leaf_key.pem'
		},
		SlhCase{
			params:  .shake_192s
			scheme:  u16(0x0919)
			pk_len:  48
			sig_len: 16224
			ca:      'slhshake192s_ca_cert.pem'
			leaf:    'slhshake192s_leaf_cert.pem'
			key:     'slhshake192s_leaf_key.pem'
		},
		SlhCase{
			params:  .shake_192f
			scheme:  u16(0x091a)
			pk_len:  48
			sig_len: 35664
			ca:      'slhshake192f_ca_cert.pem'
			leaf:    'slhshake192f_leaf_cert.pem'
			key:     'slhshake192f_leaf_key.pem'
		},
		SlhCase{
			params:  .shake_256s
			scheme:  u16(0x091b)
			pk_len:  64
			sig_len: 29792
			ca:      'slhshake256s_ca_cert.pem'
			leaf:    'slhshake256s_leaf_cert.pem'
			key:     'slhshake256s_leaf_key.pem'
		},
		SlhCase{
			params:  .shake_256f
			scheme:  u16(0x091c)
			pk_len:  64
			sig_len: 49856
			ca:      'slhshake256f_ca_cert.pem'
			leaf:    'slhshake256f_leaf_cert.pem'
			key:     'slhshake256f_leaf_key.pem'
		},
	]
}

// slh_sha2_cases mirrors slh_cases for the SHA2 instantiation (TLS schemes
// 0x0911-0x0916, openssl-generated fixtures).
fn slh_sha2_cases() []SlhCase {
	return [
		SlhCase{
			params:  .sha2_128s
			scheme:  u16(0x0911)
			pk_len:  32
			sig_len: 7856
			ca:      'slhsha2_128s_ca_cert.pem'
			leaf:    'slhsha2_128s_leaf_cert.pem'
			key:     'slhsha2_128s_leaf_key.pem'
		},
		SlhCase{
			params:  .sha2_128f
			scheme:  u16(0x0912)
			pk_len:  32
			sig_len: 17088
			ca:      'slhsha2_128f_ca_cert.pem'
			leaf:    'slhsha2_128f_leaf_cert.pem'
			key:     'slhsha2_128f_leaf_key.pem'
		},
		SlhCase{
			params:  .sha2_192s
			scheme:  u16(0x0913)
			pk_len:  48
			sig_len: 16224
			ca:      'slhsha2_192s_ca_cert.pem'
			leaf:    'slhsha2_192s_leaf_cert.pem'
			key:     'slhsha2_192s_leaf_key.pem'
		},
		SlhCase{
			params:  .sha2_192f
			scheme:  u16(0x0914)
			pk_len:  48
			sig_len: 35664
			ca:      'slhsha2_192f_ca_cert.pem'
			leaf:    'slhsha2_192f_leaf_cert.pem'
			key:     'slhsha2_192f_leaf_key.pem'
		},
		SlhCase{
			params:  .sha2_256s
			scheme:  u16(0x0915)
			pk_len:  64
			sig_len: 29792
			ca:      'slhsha2_256s_ca_cert.pem'
			leaf:    'slhsha2_256s_leaf_cert.pem'
			key:     'slhsha2_256s_leaf_key.pem'
		},
		SlhCase{
			params:  .sha2_256f
			scheme:  u16(0x0916)
			pk_len:  64
			sig_len: 49856
			ca:      'slhsha2_256f_ca_cert.pem'
			leaf:    'slhsha2_256f_leaf_cert.pem'
			key:     'slhsha2_256f_leaf_key.pem'
		},
	]
}

// slh_all_cases covers every wired SLH-DSA set (SHAKE + SHA2).
fn slh_all_cases() []SlhCase {
	mut out := slh_cases()
	out << slh_sha2_cases()
	return out
}

// test_slh_chain verifies the openssl-generated SLH-DSA leaves against
// their CAs through verify_cert_signature (the openssl -> V direction).
fn test_slh_chain() {
	for cs in slh_all_cases() {
		store := slh_store(cs.ca)
		leaf_der, pk := slh_leaf(cs.leaf)
		_ = pk
		mut trusted := false
		for r in store.roots {
			rpk := r.public_key() or { panic(err) }
			verify_cert_signature(leaf_der, rpk) or {
				panic('slh chain verify ${cs.scheme:04x}: ${err}')
			}
			trusted = true
			break
		}
		assert trusted
	}
}

// test_slh_chain_tampered locks rejection of a damaged SLH-DSA signature,
// for every wired parameter set (a per-set stub that always accepts would
// pass a single-set test).
fn test_slh_chain_tampered() {
	for cs in slh_all_cases() {
		store := slh_store(cs.ca)
		leaf_der, _ := slh_leaf(cs.leaf)
		mut bad := leaf_der.clone()
		bad[bad.len - 1] ^= u8(0x01)
		mut failed := false
		for r in store.roots {
			rpk := r.public_key() or { panic(err) }
			verify_cert_signature(bad, rpk) or {
				assert err.msg().contains('signature') || err.msg().contains('verify')
					|| err.msg().contains('mismatch')
				failed = true
				break
			}
		}
		assert failed, 'tampered SLH-DSA chain must fail for ${cs.scheme:04x}'
	}
}

// test_slh_vgen_chains covers V-generated SLH-DSA CA/leaf pairs.
fn test_slh_vgen_chains() {
	now := time.now()
	for cs in slh_all_cases() {
		ca_priv, _ := slh_dsa.keygen(cs.params) or { panic('ca keygen') }
		ca_cert, _ := x509.generate_self_signed_slh_dsa(x509.CertGenOptions{
			common_name: 'Test SLH-DSA CA'
			not_before:  now
			not_after:   now.add_days(3650)
		}, ca_priv, cs.params) or { panic('ca gen') }
		leaf_priv, _ := slh_dsa.keygen(cs.params) or { panic('leaf keygen') }
		_, leaf_der := x509.generate_ca_signed_slh_dsa(x509.CertGenOptions{
			common_name: 'leaf.example.com'
			dns_names:   ['leaf.example.com']
			not_before:  now
			not_after:   now.add_days(365)
		}, leaf_priv, cs.params, &ca_cert, ca_priv, cs.params) or { panic('leaf gen') }
		ca_pk := ca_cert.public_key() or { panic('ca pubkey') }
		verify_cert_signature(leaf_der, ca_pk) or {
			panic('vgen slh chain ${cs.scheme:04x}: ${err}')
		}
		// Cross-variant: same-n s/f pairs must not cross-verify (a per-set
		// stub or OID-tail mixup would pass a single-pair test). The 128
		// pair uses V-generated certs; the slower 192/256 pairs use the
		// openssl fixtures at the verify layer (verify is fast for all
		// sets; only signing is slow).
		if cs.params == .shake_128s {
			other_priv, _ := slh_dsa.keygen(.shake_128f) or { panic('other keygen') }
			other_ca, _ := x509.generate_self_signed_slh_dsa(x509.CertGenOptions{
				common_name: 'Other CA'
				not_before:  now
				not_after:   now.add_days(3650)
			}, other_priv, .shake_128f) or { panic('other ca gen') }
			other_pk := other_ca.public_key() or { panic('other pubkey') }
			verify_cert_signature(leaf_der, other_pk) or {
				assert err.msg().contains('mismatch')
				continue
			}
			assert false, 'cross-variant SLH-DSA chain must fail'
		}
	}
	for pair in [['slhshake192s_leaf_cert.pem', 'slhshake192f_ca_cert.pem'],
		['slhshake192f_leaf_cert.pem', 'slhshake192s_ca_cert.pem'],
		['slhshake256s_leaf_cert.pem', 'slhshake256f_ca_cert.pem'],
		['slhshake256f_leaf_cert.pem', 'slhshake256s_ca_cert.pem'],
		['slhsha2_192s_leaf_cert.pem', 'slhsha2_192f_ca_cert.pem'],
		['slhsha2_192f_leaf_cert.pem', 'slhsha2_192s_ca_cert.pem'],
		['slhsha2_256s_leaf_cert.pem', 'slhsha2_256f_ca_cert.pem'],
		['slhsha2_256f_leaf_cert.pem', 'slhsha2_256s_ca_cert.pem']] {
		store := slh_store(pair[1])
		leaf_der, _ := slh_leaf(pair[0])
		mut failed := false
		for r in store.roots {
			rpk := r.public_key() or { panic(err) }
			verify_cert_signature(leaf_der, rpk) or {
				assert err.msg().contains('mismatch')
				failed = true
				break
			}
		}
		assert failed, 'cross-variant SLH-DSA chain must fail for ${pair[0]}'
	}
}

// test_slh_key_params_mismatch locks the key/params agreement gate: a key
// passed with the wrong parameter set must fail generation, not emit an
// unverifiable certificate.
fn test_slh_key_params_mismatch() {
	now := time.now()
	priv128f, _ := slh_dsa.keygen(.shake_128f) or { panic('keygen') }
	x509.generate_self_signed_slh_dsa(x509.CertGenOptions{
		common_name: 'Mismatch CA'
		not_before:  now
		not_after:   now.add_days(30)
	}, priv128f, .shake_128s) or {
		assert err.msg().contains('mismatch')
		return
	}
	assert false, 'wrong-params SLH-DSA generation must fail'
}

// test_slh_sha2_supported locks SHA2 instantiation end to end: real
// keygen, self-signed issuance and chain verification all succeed now
// that the pure-V primitive implements the SHA2 sets.
fn test_slh_sha2_supported() {
	now := time.now()
	priv, _ := slh_dsa.keygen(.sha2_128s) or { panic('sha2 keygen: ${err}') }
	ca, _ := x509.generate_self_signed_slh_dsa(x509.CertGenOptions{
		common_name: 'SHA2 CA'
		not_before:  now
		not_after:   now.add_days(30)
	}, priv, .sha2_128s) or { panic('sha2 ca gen: ${err}') }
	ca_pk := ca.public_key() or { panic('sha2 ca pubkey: ${err}') }
	leaf_priv, _ := slh_dsa.keygen(.sha2_128s) or { panic('sha2 leaf keygen: ${err}') }
	_, leaf_der := x509.generate_ca_signed_slh_dsa(x509.CertGenOptions{
		common_name: 'sha2.example.com'
		dns_names:   ['sha2.example.com']
		not_before:  now
		not_after:   now.add_days(30)
	}, leaf_priv, .sha2_128s, &ca, priv, .sha2_128s) or { panic('sha2 leaf gen: ${err}') }
	verify_cert_signature(leaf_der, ca_pk) or { panic('sha2 chain: ${err}') }
	// A SHAKE leaf still must not verify under a SHA2 CA key.
	shake_leaf_der, _ := slh_leaf('slhshake128s_leaf_cert.pem')
	verify_cert_signature(shake_leaf_der, ca_pk) or {
		assert err.msg().contains('mismatch')
		return
	}
	assert false, 'shake leaf under sha2 CA must fail'
}

// test_slh_sigalg_params_present locks fail-closed sigAlg dispatch: an
// SLH-DSA sigAlg with parameters (NULL or a trailing OID) is rejected
// outright, never read as a primary-OID win.
fn test_slh_sigalg_params_present() {
	// Every SLH-DSA tail (0x14-0x1f, SHA2 then SHAKE in OID order) with NULL
	// parameters is rejected outright, never read as a primary-OID win.
	for tail in [u8(0x14), 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f] {
		with_null := [u8(0x06), 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, tail, 0x05,
			0x00]
		slh_dsa_params_from_sigalg(with_null) or {
			assert err.msg().contains('SLH-DSA')
			continue
		}
		assert false, 'SLH-DSA sigAlg with params must fail for tail ${tail:02x}'
	}
	// SEQ { OID shake-128s, OID shake-128f }: the trailing OID counts as
	// parameters, which SLH-DSA rejects outright.
	with_trailing := [u8(0x06), 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, u8(0x1a),
		0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, u8(0x1b)]
	slh_dsa_params_from_sigalg(with_trailing) or {
		assert err.msg().contains('SLH-DSA')
		return
	}
	assert false, 'SLH-DSA sigAlg with trailing OID must fail'
}

// test_slh_cv_roundtrips exercises CertificateVerify for every wired
// SLH-DSA parameter set: raw API sign, CV framing, verify, tamper rejection.
fn test_slh_cv_roundtrips() {
	content := 'slh certificate verify'.bytes()
	for cs in slh_all_cases() {
		priv, _ := slh_dsa.keygen(cs.params) or { panic('keygen') }
		raw := priv.public_key().bytes()
		assert raw.len == cs.pk_len
		sig := slh_dsa.sign_deterministic(content, [], priv) or { panic('sign') }
		assert sig.len == cs.sig_len
		mut body := []u8{}
		body << u8(cs.scheme >> 8)
		body << u8(cs.scheme)
		body << u8(sig.len >> 8)
		body << u8(sig.len)
		body << sig
		pk := match cs.params {
			.shake_128s { x509.PublicKey(x509.SlhDsaShake128sPublicKey{ raw: raw }) }
			.shake_128f { x509.PublicKey(x509.SlhDsaShake128fPublicKey{ raw: raw }) }
			.shake_192s { x509.PublicKey(x509.SlhDsaShake192sPublicKey{ raw: raw }) }
			.shake_192f { x509.PublicKey(x509.SlhDsaShake192fPublicKey{ raw: raw }) }
			.shake_256s { x509.PublicKey(x509.SlhDsaShake256sPublicKey{ raw: raw }) }
			.shake_256f { x509.PublicKey(x509.SlhDsaShake256fPublicKey{ raw: raw }) }
			.sha2_128s { x509.PublicKey(x509.SlhDsaSha2128sPublicKey{ raw: raw }) }
			.sha2_128f { x509.PublicKey(x509.SlhDsaSha2128fPublicKey{ raw: raw }) }
			.sha2_192s { x509.PublicKey(x509.SlhDsaSha2192sPublicKey{ raw: raw }) }
			.sha2_192f { x509.PublicKey(x509.SlhDsaSha2192fPublicKey{ raw: raw }) }
			.sha2_256s { x509.PublicKey(x509.SlhDsaSha2256sPublicKey{ raw: raw }) }
			else { x509.PublicKey(x509.SlhDsaSha2256fPublicKey{ raw: raw }) }
		}
		verify_certificate_verify(body, pk, content) or {
			panic('slh cv verify ${cs.scheme:04x}: ${err}')
		}
		mut bad_sig := sig.clone()
		bad_sig[0] ^= u8(0x01)
		mut bad_body := []u8{}
		bad_body << u8(cs.scheme >> 8)
		bad_body << u8(cs.scheme)
		bad_body << u8(bad_sig.len >> 8)
		bad_body << u8(bad_sig.len)
		bad_body << bad_sig
		verify_certificate_verify(bad_body, pk, content) or { continue }
		assert false, 'tampered slh cv sig must fail for ${cs.scheme:04x}'
	}
}

// test_slh_cv_mismatch locks scheme/key agreement for SLH-DSA.
fn test_slh_cv_mismatch() {
	priv, _ := slh_dsa.keygen(.shake_128s) or { panic('keygen') }
	raw := priv.public_key().bytes()
	content := 'mismatch'.bytes()
	sig := slh_dsa.sign_deterministic(content, [], priv) or { panic('sign') }
	// Right key, wrong scheme (128f instead of 128s).
	mut body := []u8{}
	body << u8(0x09)
	body << u8(0x18)
	body << u8(sig.len >> 8)
	body << u8(sig.len)
	body << sig
	pk128s := x509.PublicKey(x509.SlhDsaShake128sPublicKey{ raw: raw })
	verify_certificate_verify(body, pk128s, content) or {
		assert err.msg().contains('mismatch')
		// Wrong key type: an SLH-DSA signature presented as ECDSA must fail.
		mut ebody := []u8{}
		ebody << u8(0x04)
		ebody << u8(0x03)
		ebody << u8(sig.len >> 8)
		ebody << u8(sig.len)
		ebody << sig
		verify_certificate_verify(ebody, pk128s, content) or {
			assert err.msg().contains('mismatch') || err.msg().contains('scheme')
			// SHA2 direction: a SHA2-128f key presented under a SHAKE scheme.
			priv2, _ := slh_dsa.keygen(.sha2_128f) or { panic('keygen') }
			raw2 := priv2.public_key().bytes()
			sig2 := slh_dsa.sign_deterministic(content, [], priv2) or { panic('sign') }
			mut sbody := []u8{}
			sbody << u8(0x09)
			sbody << u8(0x17)
			sbody << u8(sig2.len >> 8)
			sbody << u8(sig2.len)
			sbody << sig2
			pk2 := x509.PublicKey(x509.SlhDsaSha2128fPublicKey{ raw: raw2 })
			verify_certificate_verify(sbody, pk2, content) or {
				assert err.msg().contains('mismatch')
				return
			}
			assert false, 'sha2 key with shake scheme must fail'
		}
		assert false, 'ecdsa scheme with slh key must fail'
	}
	assert false, 'cross-scheme SLH-DSA must fail'
}

// test_slh_select_cv_scheme locks server scheme selection for SLH-DSA.
fn test_slh_select_cv_scheme() {
	assert select_cv_scheme([u16(0x0917), 0x0403], .slh_shake_128s)! == u16(0x0917)
	assert select_cv_scheme([u16(0x0918)], .slh_shake_128f)! == u16(0x0918)
	assert select_cv_scheme([u16(0x0919)], .slh_shake_192s)! == u16(0x0919)
	assert select_cv_scheme([u16(0x091a)], .slh_shake_192f)! == u16(0x091a)
	assert select_cv_scheme([u16(0x091b)], .slh_shake_256s)! == u16(0x091b)
	assert select_cv_scheme([u16(0x091c)], .slh_shake_256f)! == u16(0x091c)
	assert select_cv_scheme([u16(0x0911)], .slh_sha2_128s)! == u16(0x0911)
	assert select_cv_scheme([u16(0x0912)], .slh_sha2_128f)! == u16(0x0912)
	assert select_cv_scheme([u16(0x0913)], .slh_sha2_192s)! == u16(0x0913)
	assert select_cv_scheme([u16(0x0914)], .slh_sha2_192f)! == u16(0x0914)
	assert select_cv_scheme([u16(0x0915)], .slh_sha2_256s)! == u16(0x0915)
	assert select_cv_scheme([u16(0x0916)], .slh_sha2_256f)! == u16(0x0916)
	select_cv_scheme([u16(0x0918)], .slh_shake_128s) or {
		assert err.msg().contains('scheme')
		return
	}
	assert false, 'SLH-DSA-128s without 0917 must fail'
}

// test_slh_server_key_loading locks SLH-DSA PKCS#8 credential loading.
fn test_slh_server_key_loading() {
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	for cs in slh_all_cases() {
		kind := match cs.params {
			.shake_128s { ServerKeyKind.slh_shake_128s }
			.shake_128f { ServerKeyKind.slh_shake_128f }
			.shake_192s { ServerKeyKind.slh_shake_192s }
			.shake_192f { ServerKeyKind.slh_shake_192f }
			.shake_256s { ServerKeyKind.slh_shake_256s }
			.shake_256f { ServerKeyKind.slh_shake_256f }
			.sha2_128s { ServerKeyKind.slh_sha2_128s }
			.sha2_128f { ServerKeyKind.slh_sha2_128f }
			.sha2_192s { ServerKeyKind.slh_sha2_192s }
			.sha2_192f { ServerKeyKind.slh_sha2_192f }
			.sha2_256s { ServerKeyKind.slh_sha2_256s }
			else { ServerKeyKind.slh_sha2_256f }
		}
		mut s := new_server_handshake_with_config(.off, Config{
			cert: os.join_path(td, cs.leaf)
			key:  os.join_path(td, cs.key)
		}) or { panic('slh load ${cs.scheme:04x}: ${err}') }
		assert s.key_kind == kind
	}
	new_server_handshake_with_config(.off, Config{
		cert: os.join_path(td, 'slhshake128s_leaf_cert.pem')
		key:  os.join_path(td, 'slhshake128f_leaf_key.pem')
	}) or {
		assert err.msg().contains('mismatch')
		return
	}
	assert false, 'mismatched slh cert/key must fail'
}

// test_slh_server_key_loading_match locks the cert/key pubkey comparison:
// a different key of the same parameter set must fail at the match check,
// not at an earlier parse gate.
fn test_slh_server_key_loading_match() {
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	other_priv, _ := slh_dsa.keygen(.shake_128f) or { panic('keygen') }
	priv_pem := slh_pkcs8_pem(other_priv, .shake_128f)
	new_server_handshake_with_config(.off, Config{
		cert:    os.join_path(td, 'slhshake128f_leaf_cert.pem')
		key_pem: priv_pem.bytes()
	}) or {
		assert err.msg().contains('match')
		return
	}
	assert false, 'same-params different slh key must fail the match check'
}

// slh_der_tlv wraps content in a DER tag+length header (test-only helper
// with a unique name so single-file and directory test runs agree).
fn slh_der_tlv(tag u8, content []u8) []u8 {
	mut out := []u8{}
	out << tag
	mut v := content.len
	mut len_bytes := []u8{}
	if v < 0x80 {
		len_bytes << u8(v)
	} else {
		mut tmp := []u8{}
		for v > 0 {
			tmp << u8(v & 0xff)
			v >>= 8
		}
		len_bytes << u8(0x80 | tmp.len)
		for i := tmp.len - 1; i >= 0; i-- {
			len_bytes << tmp[i]
		}
	}
	for b in len_bytes {
		out << b
	}
	for b in content {
		out << b
	}
	return out
}

// slh_pkcs8_pem encodes an SLH-DSA private key as PKCS#8 PEM (test-only):
// the privateKey OCTET STRING holds the raw 4n-byte key, matching what
// `openssl genpkey -algorithm SLH-DSA-...` emits.
fn slh_pkcs8_pem(priv slh_dsa.PrivateKey, params slh_dsa.ParameterSet) string {
	tail := slh_dsa_sigalg_tail(params)
	oid := slh_der_tlv(0x06, [u8(0x60), 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, tail])
	alg := slh_der_tlv(0x30, oid)
	mut body := slh_der_tlv(0x02, [u8(0)])
	body << alg
	body << slh_der_tlv(0x04, priv.bytes())
	der := slh_der_tlv(0x30, body)
	return pem.Block{
		block_type: 'PRIVATE KEY'
		data:       der
	}.encode(pem.EncodeConfig{}) or { panic('pem encode') }
}

// test_slh_loopback runs a full verified handshake against an SLH-DSA
// server for every set: record-layer fragmentation (RFC 8446 §5.1) carries
// the multi-record Certificate/CertificateVerify flights of the larger
// parameter sets.
fn test_slh_loopback() {
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	for cs in slh_all_cases() {
		server_cfg := Config{
			cert: os.join_path(td, cs.leaf)
			key:  os.join_path(td, cs.key)
		}
		mut l := listen(server_cfg) or { panic('listen: ${err}') }
		port := l.addr() or { panic(err) }.port() or { panic(err) }
		server := spawn fn (mut l Listener) {
			mut conn := l.accept() or { return }
			defer { conn.close() or {} }
			mut buf := []u8{len: 4}
			n := conn.read(mut buf) or { return }
			if n > 0 {
				conn.write('pong'.bytes()) or {}
			}
		}(mut l)
		ca_der := slh_pem_block(slh_read(cs.ca), 'CERTIFICATE')
		ca_cert := x509.parse_der(ca_der) or { panic(err) }
		client_cfg := Config{
			server_name:  'localhost'
			verify:       true
			verify_chain: true
			ca_store:     CertStore{
				roots: [ca_cert]
			}
		}
		mut c := dial('127.0.0.1', port, client_cfg) or { panic('dial: ${err}') }
		c.handshake() or { panic('client hs ${cs.scheme:04x}: ${err}') }
		c.write('ping'.bytes()) or { panic(err) }
		mut buf := []u8{len: 4}
		n := c.read(mut buf) or { panic(err) }
		assert n == 4
		assert buf == 'pong'.bytes()
		c.close() or {}
		l.close() or {}
		_ = server
	}
}

// test_slh_loopback_wrong_ca locks chain verification in the loopback: a
// client that trusts only the 128f CA must reject the 128s leaf. A server
// that skipped verification would complete the handshake and echo.
fn test_slh_loopback_wrong_ca() {
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	server_cfg := Config{
		cert: os.join_path(td, 'slhshake128s_leaf_cert.pem')
		key:  os.join_path(td, 'slhshake128s_leaf_key.pem')
	}
	mut l := listen(server_cfg) or { panic('listen: ${err}') }
	port := l.addr() or { panic(err) }.port() or { panic(err) }
	server := spawn fn (mut l Listener) {
		mut conn := l.accept() or { return }
		defer { conn.close() or {} }
		mut buf := []u8{len: 4}
		n := conn.read(mut buf) or { return }
		if n > 0 {
			conn.write('pong'.bytes()) or {}
		}
	}(mut l)
	ca_der := slh_pem_block(slh_read('slhshake128f_ca_cert.pem'), 'CERTIFICATE')
	ca_cert := x509.parse_der(ca_der) or { panic(err) }
	client_cfg := Config{
		server_name:  'localhost'
		verify:       true
		verify_chain: true
		ca_store:     CertStore{
			roots: [ca_cert]
		}
	}
	mut c := dial('127.0.0.1', port, client_cfg) or { panic('dial: ${err}') }
	c.handshake() or {
		c.close() or {}
		l.close() or {}
		_ = server
		return
	}
	c.write('ping'.bytes()) or {
		c.close() or {}
		l.close() or {}
		_ = server
		return
	}
	mut buf := []u8{len: 4}
	c.read(mut buf) or {
		c.close() or {}
		l.close() or {}
		_ = server
		return
	}
	assert false, 'client accepted an untrusted SLH-DSA leaf'
}

// test_slh_openssl_interop checks V-generated SLH-DSA certificates under
// openssl (the V -> openssl direction; the reverse is covered without skips
// by test_slh_chain). Skips without openssl.
fn test_slh_openssl_interop() {
	bin := slh_openssl_bin()
	if bin == '' {
		return
	}
	ca_priv, _ := slh_dsa.keygen(.shake_128s) or { panic('ca keygen') }
	now := time.now()
	ca_cert, ca_der := x509.generate_self_signed_slh_dsa(x509.CertGenOptions{
		common_name: 'Test SLH-DSA CA'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(3650)
		is_ca:       true
	}, ca_priv, .shake_128s) or { panic('ca gen') }
	leaf_priv, _ := slh_dsa.keygen(.shake_128s) or { panic('leaf keygen') }
	_, leaf_der := x509.generate_ca_signed_slh_dsa(x509.CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, leaf_priv, .shake_128s, &ca_cert, ca_priv, .shake_128s) or { panic('leaf gen') }
	dir := os.temp_dir()
	ca_pem := dir + '/tls_slh_ca.pem'
	leaf_pem := dir + '/tls_slh_leaf.pem'
	leaf_der_file := dir + '/tls_slh_leaf.der'
	ca_text := pem.Block{
		block_type: 'CERTIFICATE'
		data:       ca_der
	}.encode(pem.EncodeConfig{}) or { panic('pem encode') }
	leaf_text := pem.Block{
		block_type: 'CERTIFICATE'
		data:       leaf_der
	}.encode(pem.EncodeConfig{}) or { panic('pem encode') }
	os.write_file(ca_pem, ca_text) or { panic('write ca') }
	os.write_file(leaf_pem, leaf_text) or { panic('write leaf') }
	os.write_file_array(leaf_der_file, leaf_der) or { panic('write leaf der') }
	defer {
		os.rm(ca_pem) or {}
		os.rm(leaf_pem) or {}
		os.rm(leaf_der_file) or {}
	}
	res := os.exec([bin, 'verify', '-CAfile', ca_pem, leaf_pem])
	assert res.exit_code == 0, 'openssl verify failed: ${res.output}'
	text := os.exec([bin, 'x509', '-inform', 'DER', '-in', leaf_der_file, '-text', '-noout'])
	assert text.exit_code == 0
	assert text.output.contains('SLH-DSA-SHAKE-128s')
	assert text.output.contains('leaf.example.com')
}
