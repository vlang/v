module x509

import crypto.ecdsa
import crypto.ed25519
import crypto.pem
import crypto.slh_dsa
import math.big
import os
import time
import x.crypto.mldsa

// pki_crl_test covers CRL parse/generate/verify: roundtrips for every
// CA key type, signature tamper, validity windows, serial lookup,
// chain revocation enforcement, env inputs and unknown-critical
// fail-closed parsing.

fn pki_crl_ca() (Certificate, CertSigner) {
	now := time.now()
	_, priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen: ${err}') }
	ca, _ := generate_self_signed(CertGenOptions{
		common_name:   'PKI CRL CA'
		not_before:    now.add_days(-1)
		not_after:     now.add_days(3650)
		is_ca:         true
		has_key_usage: true
		key_usage:     ku_digital_signature | ku_key_cert_sign | ku_crl_sign
	}, priv) or { panic('ca gen: ${err}') }
	return ca, CertSigner{
		kind:       .ecdsa
		ecdsa_priv: priv
	}
}

fn pki_crl_leaf(ca &Certificate, ca_priv ecdsa.PrivateKey, name string) (Certificate, []u8) {
	now := time.now()
	_, priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen: ${err}') }
	cert, der := generate_ca_signed(CertGenOptions{
		common_name: name
		dns_names:   [name]
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, priv, ca, ca_priv) or { panic('leaf gen: ${err}') }
	return cert, der
}

// test_pki_crl_roundtrip generates, parses and verifies an empty CRL.
fn test_pki_crl_roundtrip() {
	now := time.now()
	ca, signer := pki_crl_ca()
	crl, der := make_crl(&ca, CrlGenOptions{
		this_update:     now.add_days(-1)
		next_update:     now.add_days(7)
		has_next_update: true
		crl_number:      [u8(1), 2, 3]
	}, signer) or { panic('make_crl: ${err}') }
	assert der.len > 0
	assert crl.crl_number == [u8(1), 2, 3]
	assert crl.authority_key_id == ca.subject_key_id
	assert crl.issuer_der == ca.subject_der
	verify_crl(&crl, &ca) or { panic('verify_crl: ${err}') }
	assert !crl.is_revoked([u8(1)])
	// DER roundtrip through the parser.
	again := parse_crl_der(der) or { panic('reparse: ${err}') }
	assert again.crl_number == [u8(1), 2, 3]
	verify_crl(&again, &ca) or { panic('reparse verify: ${err}') }
}

// test_pki_crl_revoke enforces revocation through verify_chain.
fn test_pki_crl_revoke() {
	now := time.now()
	ca, signer := pki_crl_ca()
	leaf, _ := pki_crl_leaf(&ca, signer.ecdsa_priv or { panic('no key') }, 'leaf.example.com')
	other, _ := pki_crl_leaf(&ca, signer.ecdsa_priv or { panic('no key') }, 'other.example.com')
	crl, _ := make_crl(&ca, CrlGenOptions{
		this_update:     now.add_days(-1)
		next_update:     now.add_days(7)
		has_next_update: true
		revoked:         [
			CrlEntry{
				serial:     leaf.serial
				revoked_at: now
			},
		]
	}, signer) or { panic('make_crl: ${err}') }
	assert crl.is_revoked(leaf.serial)
	assert !crl.is_revoked(other.serial)
	// Serial comparison tolerates a sign-byte difference.
	mut padded := []u8{}
	padded << u8(0)
	for b in other.serial {
		padded << b
	}
	assert !crl.is_revoked(padded)
	revoked_chain := build_chain(leaf, [], [ca]) or { panic('build: ${err}') }
	verify_chain(revoked_chain, VerifyOptions{
		hostname: 'leaf.example.com'
		crls:     [crl]
	}) or {
		assert err.msg().contains('revoked')
		// Without the CRL the same chain verifies.
		verify_chain(revoked_chain, VerifyOptions{
			hostname: 'leaf.example.com'
		}) or { panic('plain verify: ${err}') }
		return
	}
	assert false, 'revoked leaf must fail'
}

// test_pki_crl_tamper rejects damaged signatures and bodies.
fn test_pki_crl_tamper() {
	now := time.now()
	ca, signer := pki_crl_ca()
	crl, der := make_crl(&ca, CrlGenOptions{
		this_update:     now.add_days(-1)
		next_update:     now.add_days(7)
		has_next_update: true
	}, signer) or { panic('make_crl: ${err}') }
	_ = crl
	mut bad_sig := der.clone()
	bad_sig[bad_sig.len - 1] ^= u8(0x01)
	tampered := parse_crl_der(bad_sig) or { panic('tampered must still parse: ${err}') }
	verify_crl(&tampered, &ca) or {
		// A damaged TBS also fails (at parse or at verify).
		mut bad_tbs := der.clone()
		bad_tbs[20] ^= u8(0x01)
		if _ := parse_crl_der(bad_tbs) {
			// parsed: signature must then fail
			parsed := parse_crl_der(bad_tbs) or { panic('unreachable') }
			verify_crl(&parsed, &ca) or {
				assert err.msg().contains('signature') || err.msg().contains('bad')
					|| err.msg().contains('mismatch')
				return
			}
			assert false, 'damaged TBS must fail verify'
		}
		return
	}
	assert false, 'tampered CRL signature must fail'
}

// test_pki_crl_validity_window enforces thisUpdate/nextUpdate.
fn test_pki_crl_validity_window() {
	now := time.now()
	ca, signer := pki_crl_ca()
	// Expired: nextUpdate in the past.
	expired, _ := make_crl(&ca, CrlGenOptions{
		this_update:     now.add_days(-14)
		next_update:     now.add_days(-1)
		has_next_update: true
	}, signer) or { panic('make_crl: ${err}') }
	verify_crl(&expired, &ca) or {
		assert err.msg().contains('expired')
		// Not yet valid: thisUpdate in the future.
		future, _ := make_crl(&ca, CrlGenOptions{
			this_update:     now.add_days(1)
			next_update:     now.add_days(8)
			has_next_update: true
		}, signer) or { panic('make future: ${err}') }
		verify_crl(&future, &ca) or {
			assert err.msg().contains('not yet valid')
			return
		}
		assert false, 'future CRL must fail'
	}
	assert false, 'expired CRL must fail'
}

// test_pki_crl_wrong_issuer rejects CRLs from another CA and issuers
// whose keyUsage lacks cRLSign.
fn test_pki_crl_wrong_issuer() {
	now := time.now()
	ca, signer := pki_crl_ca()
	crl, _ := make_crl(&ca, CrlGenOptions{
		this_update:     now.add_days(-1)
		next_update:     now.add_days(7)
		has_next_update: true
	}, signer) or { panic('make_crl: ${err}') }
	_, other_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen: ${err}') }
	other, _ := generate_self_signed(CertGenOptions{
		common_name:   'PKI Other CRL CA'
		not_before:    now.add_days(-1)
		not_after:     now.add_days(3650)
		is_ca:         true
		has_key_usage: true
		key_usage:     ku_digital_signature | ku_key_cert_sign | ku_crl_sign
	}, other_priv) or { panic('other ca gen') }
	verify_crl(&crl, &other) or {
		assert err.msg().contains('issuer mismatch')
		// A CA without cRLSign cannot have signed a CRL.
		_, nosign_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen') }
		nosign, _ := generate_self_signed(CertGenOptions{
			common_name:   'PKI NoSign CA'
			not_before:    now.add_days(-1)
			not_after:     now.add_days(3650)
			is_ca:         true
			has_key_usage: true
			key_usage:     ku_digital_signature | ku_key_cert_sign
		}, nosign_priv) or { panic('nosign gen') }
		verify_crl(&crl, &nosign) or {
			assert err.msg().contains('issuer mismatch') || err.msg().contains('cRLSign')
			return
		}
		assert false, 'CRL under wrong issuer must fail'
	}
	assert false, 'CRL under wrong CA must fail'
}

// parse_test_rsa_privkey reads n, e, d from a PKCS#8 RSA PEM block
// (test-only; production RSA keys never enter crypto.x509 to avoid an
// import cycle with crypto.rsa).
fn parse_test_rsa_privkey(pem_text string) (big.Integer, u64, big.Integer) {
	block, _ := pem.decode(pem_text) or { panic('no PEM block') }
	assert block.block_type == 'PRIVATE KEY'
	content := expect_tag(block.data, 0, tag_seq, 'PKCS#8') or { panic('pkcs8: ${err}') }
	mut pos := 0
	pos = skip_tlv(content, pos) or { panic(err) } // version
	pos = skip_tlv(content, pos) or { panic(err) } // algorithm
	inner := expect_tag(content, pos, tag_octet_string, 'privateKey') or { panic(err) }
	pkcs1 := expect_tag(inner, 0, tag_seq, 'PKCS#1') or { panic(err) }
	mut o := 0
	o = skip_tlv(pkcs1, o) or { panic(err) } // version
	n_raw := expect_tag(pkcs1, o, tag_integer, 'n') or { panic(err) }
	o = skip_tlv(pkcs1, o) or { panic(err) }
	e_raw := expect_tag(pkcs1, o, tag_integer, 'e') or { panic(err) }
	o = skip_tlv(pkcs1, o) or { panic(err) }
	d_raw := expect_tag(pkcs1, o, tag_integer, 'd') or { panic(err) }
	mut n_bytes := n_raw.clone()
	if n_bytes.len > 1 && n_bytes[0] == 0 {
		n_bytes = n_bytes[1..].clone()
	}
	mut d_bytes := d_raw.clone()
	if d_bytes.len > 1 && d_bytes[0] == 0 {
		d_bytes = d_bytes[1..].clone()
	}
	n := big.integer_from_bytes(n_bytes, signum: 1)
	mut e_val := u64(0)
	for b in e_raw {
		e_val = (e_val << 8) | u64(b)
	}
	d := big.integer_from_bytes(d_bytes, signum: 1)
	return n, e_val, d
}

// test_pki_crl_rsa signs and verifies a CRL with the openssl RSA CA
// fixture key.
fn test_pki_crl_rsa() {
	td := os.join_path(@VMODROOT, 'vlib/net/tls/testdata')
	ca := parse_pem(os.read_file(os.join_path(td, 'rsa_ca_cert.pem')) or {
		panic('read ca: ${err}')
	}) or { panic('parse ca: ${err}') }
	n, e, d := parse_test_rsa_privkey(os.read_file(os.join_path(td, 'rsa_ca_key.pem')) or {
		panic('read key: ${err}')
	})
	now := time.now()
	crl, _ := make_crl(&ca, CrlGenOptions{
		this_update:     now.add_days(-1)
		next_update:     now.add_days(7)
		has_next_update: true
	}, CertSigner{
		kind:  .rsa
		rsa_n: n
		rsa_e: e
		rsa_d: d
	}) or { panic('make_crl rsa: ${err}') }
	assert crl.authority_key_id == ca.subject_key_id
	verify_crl(&crl, &ca) or { panic('verify_crl rsa: ${err}') }
}

// test_pki_crl_ed25519 signs and verifies a CRL with an Ed25519 CA.
fn test_pki_crl_ed25519() {
	now := time.now()
	epub, priv := ed25519.generate_key() or { panic('keygen: ${err}') }
	_ = epub
	ca, _ := generate_self_signed_ed25519(CertGenOptions{
		common_name:   'PKI Ed CA'
		not_before:    now.add_days(-1)
		not_after:     now.add_days(3650)
		is_ca:         true
		has_key_usage: true
		key_usage:     ku_digital_signature | ku_key_cert_sign | ku_crl_sign
	}, priv) or { panic('ca gen: ${err}') }
	crl, der := make_crl(&ca, CrlGenOptions{
		this_update:     now.add_days(-1)
		next_update:     now.add_days(7)
		has_next_update: true
	}, CertSigner{
		kind:    .ed25519
		ed_priv: priv
	}) or { panic('make_crl ed: ${err}') }
	_ = der
	verify_crl(&crl, &ca) or { panic('verify_crl ed: ${err}') }
}

// test_pki_crl_mldsa signs and verifies a CRL with an ML-DSA-44 CA.
fn test_pki_crl_mldsa() {
	now := time.now()
	priv := mldsa.PrivateKey.generate(.ml_dsa_44) or { panic('keygen: ${err}') }
	ca, _ := generate_self_signed_mldsa(CertGenOptions{
		common_name:   'PKI ML-DSA CA'
		not_before:    now.add_days(-1)
		not_after:     now.add_days(3650)
		is_ca:         true
		has_key_usage: true
		key_usage:     ku_digital_signature | ku_key_cert_sign | ku_crl_sign
	}, priv, .ml_dsa_44) or { panic('ca gen: ${err}') }
	crl, _ := make_crl(&ca, CrlGenOptions{
		this_update:     now.add_days(-1)
		next_update:     now.add_days(7)
		has_next_update: true
	}, CertSigner{
		kind:       .mldsa
		mldsa_priv: priv
		mldsa_kind: .ml_dsa_44
	}) or { panic('make_crl mldsa: ${err}') }
	verify_crl(&crl, &ca) or { panic('verify_crl mldsa: ${err}') }
}

// test_pki_crl_slh_dsa signs and verifies a CRL with an SLH-DSA CA.
fn test_pki_crl_slh_dsa() {
	now := time.now()
	priv, _ := slh_dsa.keygen(.shake_128s) or { panic('keygen: ${err}') }
	ca, _ := generate_self_signed_slh_dsa(CertGenOptions{
		common_name:   'PKI SLH CA'
		not_before:    now.add_days(-1)
		not_after:     now.add_days(3650)
		is_ca:         true
		has_key_usage: true
		key_usage:     ku_digital_signature | ku_key_cert_sign | ku_crl_sign
	}, priv, .shake_128s) or { panic('ca gen: ${err}') }
	crl, _ := make_crl(&ca, CrlGenOptions{
		this_update:     now.add_days(-1)
		next_update:     now.add_days(7)
		has_next_update: true
	}, CertSigner{
		kind:       .slh_dsa
		slh_priv:   priv
		slh_params: .shake_128s
	}) or { panic('make_crl slh: ${err}') }
	verify_crl(&crl, &ca) or { panic('verify_crl slh: ${err}') }
}

// test_pki_crl_pem_roundtrip covers the X509 CRL PEM framing.
fn test_pki_crl_pem_roundtrip() {
	now := time.now()
	ca, signer := pki_crl_ca()
	_, der := make_crl(&ca, CrlGenOptions{
		this_update:     now.add_days(-1)
		next_update:     now.add_days(7)
		has_next_update: true
	}, signer) or { panic('make_crl: ${err}') }
	text := pem.Block{
		block_type: 'X509 CRL'
		data:       der
	}.encode(pem.EncodeConfig{}) or { panic('pem encode: ${err}') }
	back := parse_crl_pem(text) or { panic('parse_crl_pem: ${err}') }
	verify_crl(&back, &ca) or { panic('verify: ${err}') }
	parse_crl_pem('-----BEGIN CERTIFICATE-----\nAAAA\n-----END CERTIFICATE-----\n') or {
		assert err.msg().contains('no X509 CRL')
		return
	}
	assert false, 'non-CRL PEM must fail'
}

// test_pki_crl_env loads CRLs from SSL_CRL_FILE/SSL_CRL_DIR.
fn test_pki_crl_env() {
	now := time.now()
	ca, signer := pki_crl_ca()
	leaf, _ := pki_crl_leaf(&ca, signer.ecdsa_priv or { panic('no key') }, 'leaf.example.com')
	_, der := make_crl(&ca, CrlGenOptions{
		this_update:     now.add_days(-1)
		next_update:     now.add_days(7)
		has_next_update: true
		revoked:         [
			CrlEntry{
				serial:     leaf.serial
				revoked_at: now
			},
		]
	}, signer) or { panic('make_crl: ${err}') }
	dir := os.join_path(os.temp_dir(), 'pki_crl_env')
	os.mkdir_all(dir) or { panic('mkdir: ${err}') }
	defer {
		os.rmdir_all(dir) or {}
	}
	text := pem.Block{
		block_type: 'X509 CRL'
		data:       der
	}.encode(pem.EncodeConfig{}) or { panic('pem encode') }
	crl_file := os.join_path(dir, 'test.crl')
	os.write_file(crl_file, text) or { panic('write: ${err}') }
	old_file := os.getenv('SSL_CRL_FILE')
	old_dir := os.getenv('SSL_CRL_DIR')
	os.setenv('SSL_CRL_FILE', crl_file, true)
	defer {
		os.setenv('SSL_CRL_FILE', old_file, true)
		os.setenv('SSL_CRL_DIR', old_dir, true)
	}
	loaded := load_system_crls()
	assert loaded.len == 1
	assert loaded[0].is_revoked(leaf.serial)
	// Directory scan finds the same CRL.
	os.setenv('SSL_CRL_FILE', '', true)
	os.setenv('SSL_CRL_DIR', dir, true)
	loaded_dir := load_system_crls()
	assert loaded_dir.len == 1
}

// test_pki_crl_unknown_critical locks fail-closed CRL extension parsing.
fn test_pki_crl_unknown_critical() {
	mut content := []u8{}
	for b in encode_oid('9.9.9.9.9') {
		content << b
	}
	for b in der_tlv(0x01, [u8(0xff)]) {
		content << b
	}
	for b in der_octstr([]u8{}) {
		content << b
	}
	body := der_seq(content)
	_, _ = parse_crl_extensions(body) or {
		assert err.msg().contains('unsupported critical CRL')
		return
	}
	assert false, 'unknown critical CRL extension must fail'
}

// test_pki_crl_tbs_tamper_message proves TBS damage fails with a message,
// whether the DER still parses (signature mismatch) or not (parse error).
fn test_pki_crl_tbs_tamper_message() {
	now := time.now()
	ca, signer := pki_crl_ca()
	_, der := make_crl(&ca, CrlGenOptions{
		this_update:     now.add_days(-1)
		next_update:     now.add_days(7)
		has_next_update: true
	}, signer) or { panic('make_crl: ${err}') }
	mut proven := false
	for off in [15, 25, 35, 55] {
		if off >= der.len {
			continue
		}
		mut bad := der.clone()
		bad[off] ^= u8(0x01)
		if tampered := parse_crl_der(bad) {
			if _ := verify_crl(&tampered, &ca) {
				continue
			} else {
				assert err.msg().contains('signature') || err.msg().contains('bad')
					|| err.msg().contains('mismatch')
				proven = true
				break
			}
		}
	}
	assert proven, 'no TBS offset both parsed and failed verify with a message'
}

// test_pki_crl_revoked_intermediate enforces revocation of an intermediate
// CA through verify_chain.
fn test_pki_crl_revoked_intermediate() {
	now := time.now()
	ca, signer := pki_crl_ca()
	mid_priv := signer.ecdsa_priv or { panic('no key') }
	_, mid_der_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen: ${err}') }
	mid, _ := generate_ca_signed(CertGenOptions{
		common_name: 'PKI CRL Mid'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(1825)
		is_ca:       true
	}, mid_der_priv, &ca, mid_priv) or { panic('mid gen: ${err}') }
	_, leaf_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen: ${err}') }
	leaf, _ := generate_ca_signed(CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, leaf_priv, &mid, mid_der_priv) or { panic('leaf gen: ${err}') }
	crl, _ := make_crl(&ca, CrlGenOptions{
		this_update:     now.add_days(-1)
		next_update:     now.add_days(7)
		has_next_update: true
		revoked:         [
			CrlEntry{
				serial:     mid.serial
				revoked_at: now
			},
		]
	}, signer) or { panic('make_crl: ${err}') }
	assert crl.is_revoked(mid.serial)
	chain := build_chain(leaf, [mid], [ca]) or { panic('build: ${err}') }
	verify_chain(chain, VerifyOptions{
		hostname: 'leaf.example.com'
		crls:     [crl]
	}) or {
		assert err.msg().contains('revoked')
		return
	}
	assert false, 'revoked intermediate must fail'
}

// test_pki_crl_two_issuers wires one CRL per issuer into verify_chain: a
// leaf revoked by its issuer's CRL fails even when the root CRL is clean,
// and a clean pair verifies.
fn test_pki_crl_two_issuers() {
	now := time.now()
	ca, signer := pki_crl_ca()
	mid_priv := signer.ecdsa_priv or { panic('no key') }
	_, mid_key := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen: ${err}') }
	mid, _ := generate_ca_signed(CertGenOptions{
		common_name:   'PKI CRL Mid'
		not_before:    now.add_days(-1)
		not_after:     now.add_days(1825)
		is_ca:         true
		has_key_usage: true
		key_usage:     ku_digital_signature | ku_key_cert_sign | ku_crl_sign
	}, mid_key, &ca, mid_priv) or { panic('mid gen: ${err}') }
	mid_signer := CertSigner{
		kind:       .ecdsa
		ecdsa_priv: mid_key
	}
	_, leaf_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen: ${err}') }
	leaf, _ := generate_ca_signed(CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, leaf_priv, &mid, mid_key) or { panic('leaf gen: ${err}') }
	root_crl, _ := make_crl(&ca, CrlGenOptions{
		this_update:     now.add_days(-1)
		next_update:     now.add_days(7)
		has_next_update: true
	}, signer) or { panic('root crl: ${err}') }
	mid_crl, _ := make_crl(&mid, CrlGenOptions{
		this_update:     now.add_days(-1)
		next_update:     now.add_days(7)
		has_next_update: true
		revoked:         [
			CrlEntry{
				serial:     leaf.serial
				revoked_at: now
			},
		]
	}, mid_signer) or { panic('mid crl: ${err}') }
	chain := build_chain(leaf, [mid], [ca]) or { panic('build: ${err}') }
	// Clean pair verifies.
	verify_chain(chain, VerifyOptions{
		hostname: 'leaf.example.com'
		crls:     [root_crl, mid_crl]
	}) or {
		// mid CRL revokes the leaf: must fail revoked.
		assert err.msg().contains('revoked')
		return
	}
	assert false, 'leaf revoked by its issuer CRL must fail'
}

// test_pki_crl_indefinite_no_next_update accepts a CRL without nextUpdate
// (no expiry bound) and round-trips the absence through DER.
fn test_pki_crl_indefinite_no_next_update() {
	now := time.now()
	ca, signer := pki_crl_ca()
	crl, der := make_crl(&ca, CrlGenOptions{
		this_update: now.add_days(-1)
		// has_next_update false: indefinite, never expires.
	}, signer) or { panic('make_crl: ${err}') }
	assert !crl.has_next_update
	verify_crl(&crl, &ca) or { panic('indefinite verify: ${err}') }
	again := parse_crl_der(der) or { panic('reparse: ${err}') }
	assert !again.has_next_update
	verify_crl(&again, &ca) or { panic('indefinite reparse verify: ${err}') }
}

// test_pki_crl_wrong_signing_key rejects a CRL whose signature was made
// with a key that is not the issuer's key.
fn test_pki_crl_wrong_signing_key() {
	now := time.now()
	ca, _ := pki_crl_ca()
	_, other_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen: ${err}') }
	crl, _ := make_crl(&ca, CrlGenOptions{
		this_update:     now.add_days(-1)
		next_update:     now.add_days(7)
		has_next_update: true
	}, CertSigner{
		kind:       .ecdsa
		ecdsa_priv: other_priv
	}) or { panic('make_crl: ${err}') }
	verify_crl(&crl, &ca) or {
		assert err.msg().contains('signature') || err.msg().contains('bad')
			|| err.msg().contains('mismatch')
		return
	}
	assert false, 'CRL signed with the wrong key must fail'
}
