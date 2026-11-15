module x509

import crypto.ecdsa
import crypto.ed25519
import crypto.pem
import os
import time

// pki_ed_test covers Ed25519 issuance and verification in the existing
// sig-test style: openssl fixture chain, tamper rejection, V-generated
// roundtrips, and key/sigAlg mismatch rejection.

fn ed_testdata(name string) []u8 {
	return os.read_file(os.join_path(@VMODROOT, 'vlib/crypto/x509/testdata/' + name)) or {
		panic('read fixture ${name}: ${err}')
	}.bytes()
}

// test_pki_ed_openssl_chain verifies the openssl-generated Ed25519 CA
// and leaf through build_chain + verify_chain.
fn test_pki_ed_openssl_chain() {
	ca := parse_pem(ed_testdata('ed25519_ca_cert.pem').bytestr()) or {
		panic('parse ca: ${err}')
	}
	leaf := parse_pem(ed_testdata('ed25519_leaf_cert.pem').bytestr()) or {
		panic('parse leaf: ${err}')
	}
	ca_pk := ca.public_key() or { panic('ca pubkey: ${err}') }
	match ca_pk {
		Ed25519PublicKey {
			assert ca_pk.raw.len == 32
		}
		else {
			panic('expected ed25519 CA')
		}
	}
	leaf_pk := leaf.public_key() or { panic('leaf pubkey: ${err}') }
	match leaf_pk {
		Ed25519PublicKey {
			assert leaf_pk.raw.len == 32
		}
		else {
			panic('expected ed25519 leaf')
		}
	}
	assert ca.is_ca
	assert leaf.ext_key_usage == [oid_eku_server_auth]
	chain := build_chain(leaf, [], [ca]) or { panic('build: ${err}') }
	assert chain.len == 2
	verify_chain(chain, VerifyOptions{
		hostname: 'leaf.example.com'
	}) or { panic('verify: ${err}') }
}

// test_pki_ed_chain_tampered locks rejection of a damaged Ed25519
// certificate signature.
fn test_pki_ed_chain_tampered() {
	ca := parse_pem(ed_testdata('ed25519_ca_cert.pem').bytestr()) or { panic('parse ca') }
	leaf := parse_pem(ed_testdata('ed25519_leaf_cert.pem').bytestr()) or {
		panic('parse leaf')
	}
	mut bad := leaf.raw.clone()
	bad[bad.len - 1] ^= u8(0x01)
	tampered := parse_der(bad) or { panic('tampered must still parse: ${err}') }
	build_chain(tampered, [], [ca]) or {
		assert err.msg().contains('signature') || err.msg().contains('bad')
			|| err.msg().contains('no valid chain')
		return
	}
	assert false, 'tampered Ed25519 chain must fail'
}

// test_pki_ed_vgen covers V-generated Ed25519 CA/leaf pairs end to end,
// including an openssl parse cross-check (skipped without openssl).
fn test_pki_ed_vgen() {
	now := time.now()
	_, ca_priv := ed25519.generate_key() or { panic('ca keygen: ${err}') }
	ca, _ := generate_self_signed_ed25519(CertGenOptions{
		common_name: 'Test Ed25519 CA'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(3650)
		is_ca:       true
	}, ca_priv) or { panic('ca gen: ${err}') }
	assert ca.is_ca
	_, leaf_priv := ed25519.generate_key() or { panic('leaf keygen: ${err}') }
	leaf, leaf_der := generate_ca_signed_ed25519(CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, leaf_priv, &ca, ca_priv) or { panic('leaf gen: ${err}') }
	chain := build_chain(leaf, [], [ca]) or { panic('build: ${err}') }
	assert chain.len == 2
	verify_chain(chain, VerifyOptions{
		hostname: 'leaf.example.com'
	}) or { panic('verify: ${err}') }
	// openssl must parse the V-generated leaf (stable fixture shape).
	bin := pki_openssl_bin()
	if bin == '' {
		return
	}
	tmp := os.join_path(os.temp_dir(), 'pki_ed_leaf.der')
	os.write_file_array(tmp, leaf_der) or { panic('write: ${err}') }
	defer {
		os.rm(tmp) or {}
	}
	res := os.exec([bin, 'x509', '-inform', 'DER', '-in', tmp, '-text', '-noout'])
	assert res.exit_code == 0, 'openssl parse failed: ${res.output}'
	assert res.output.contains('ED25519')
	assert res.output.contains('leaf.example.com')
}

// test_pki_ed_mismatch locks key/sigAlg agreement: a leaf verified under
// the wrong Ed25519 CA fails, and sigAlg-with-parameters is rejected.
fn test_pki_ed_mismatch() {
	now := time.now()
	_, ca_priv := ed25519.generate_key() or { panic('ca keygen') }
	ca, _ := generate_self_signed_ed25519(CertGenOptions{
		common_name: 'CA One'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(3650)
		is_ca:       true
	}, ca_priv) or { panic('ca gen') }
	_, other_priv := ed25519.generate_key() or { panic('other keygen') }
	other, _ := generate_self_signed_ed25519(CertGenOptions{
		common_name: 'CA Two'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(3650)
		is_ca:       true
	}, other_priv) or { panic('other gen') }
	_, leaf_priv := ed25519.generate_key() or { panic('leaf keygen') }
	leaf, _ := generate_ca_signed_ed25519(CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, leaf_priv, &ca, ca_priv) or { panic('leaf gen') }
	build_chain(leaf, [], [other]) or {
		assert err.msg().contains('no valid chain')
		// sigAlg encodings with parameters are never accepted.
		mut with_params := encode_oid(oid_ed25519)
		with_params << der_tlv(0x05, []u8{})
		assert !sigalg_is_exact(with_params, oid_ed25519)
		assert sigalg_is_exact(encode_oid(oid_ed25519), oid_ed25519)
		return
	}
	assert false, 'cross-CA Ed25519 chain must fail'
}

// test_pki_ed_openssl_verify checks the V-generated Ed25519 pair under
// openssl verify (CA:TRUE + SAN shape). Skips without openssl.
fn test_pki_ed_openssl_verify() {
	bin := pki_openssl_bin()
	if bin == '' {
		return
	}
	now := time.now()
	_, ca_priv := ed25519.generate_key() or { panic('ca keygen') }
	ca, ca_der := generate_self_signed_ed25519(CertGenOptions{
		common_name: 'Test Ed25519 CA'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(3650)
		is_ca:       true
	}, ca_priv) or { panic('ca gen') }
	_, leaf_priv := ed25519.generate_key() or { panic('leaf keygen') }
	_, leaf_der := generate_ca_signed_ed25519(CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, leaf_priv, &ca, ca_priv) or { panic('leaf gen') }
	dir := os.join_path(os.temp_dir(), 'pki_ed_ossl')
	os.mkdir_all(dir) or { panic('mkdir: ${err}') }
	defer {
		os.rmdir_all(dir) or {}
	}
	ca_pem := os.join_path(dir, 'ca.pem')
	leaf_pem := os.join_path(dir, 'leaf.pem')
	os.write_file(ca_pem, pem.Block{
		block_type: 'CERTIFICATE'
		data:       ca_der
	}.encode(pem.EncodeConfig{}) or { panic(err) }) or { panic(err) }
	os.write_file(leaf_pem, pem.Block{
		block_type: 'CERTIFICATE'
		data:       leaf_der
	}.encode(pem.EncodeConfig{}) or { panic(err) }) or { panic(err) }
	res := os.exec([bin, 'verify', '-CAfile', ca_pem, leaf_pem])
	assert res.exit_code == 0, 'openssl verify failed: ${res.output}'
}

// test_pki_ed_tbs_tamper_message proves TBS damage fails with a message:
// every DER offset that still parses must fail chain building, and offsets
// that no longer parse must fail parsing (no offset may silently pass).
fn test_pki_ed_tbs_tamper_message() {
	ca := parse_pem(ed_testdata('ed25519_ca_cert.pem').bytestr()) or { panic('parse ca') }
	leaf := parse_pem(ed_testdata('ed25519_leaf_cert.pem').bytestr()) or {
		panic('parse leaf')
	}
	for off in [15, 25, 35, 55, 100] {
		if off >= leaf.raw.len {
			continue
		}
		mut bad := leaf.raw.clone()
		bad[off] ^= u8(0x01)
		if tampered := parse_der(bad) {
			if _ := build_chain(tampered, [], [ca]) {
				assert false, 'tampered leaf at offset ${off} must not build'
			} else {
				assert err.msg().contains('signature') || err.msg().contains('bad')
					|| err.msg().contains('no valid chain')
			}
		} else {
			assert err.msg().len > 0, 'tampered leaf at offset ${off} must fail parse'
		}
	}
}

// test_pki_ed_sigalg_params_through_chain rejects a leaf whose outer
// signatureAlgorithm carries parameters (NULL) at chain-build time: the
// DER still parses (parameters are tolerated structurally) but the exact
// single-OID gate fails the chain.
fn test_pki_ed_sigalg_params_through_chain() {
	ca := parse_pem(ed_testdata('ed25519_ca_cert.pem').bytestr()) or { panic('parse ca') }
	leaf := parse_pem(ed_testdata('ed25519_leaf_cert.pem').bytestr()) or {
		panic('parse leaf')
	}
	cert_body := expect_tag(leaf.raw, 0, tag_seq, 'certificate') or { panic('outer: ${err}') }
	tbs_tlv, p1 := split_tlv(cert_body, 0) or { panic('tbs: ${err}') }
	sigalg_tlv, p2 := split_tlv(cert_body, p1) or { panic('sigalg: ${err}') }
	sig_tlv, p3 := split_tlv(cert_body, p2) or { panic('sig: ${err}') }
	assert p3 == cert_body.len
	sig_content := expect_tag(sigalg_tlv, 0, tag_seq, 'sigalg seq') or {
		panic('sigalg content: ${err}')
	}
	mut with_params := sig_content.clone()
	for b in der_tlv(0x05, []u8{}) {
		with_params << b
	}
	new_sigalg := der_tlv(0x30, with_params)
	mut new_body := tbs_tlv.clone()
	new_body << new_sigalg
	new_body << sig_tlv
	new_der := der_tlv(0x30, new_body)
	if cert := parse_der(new_der) {
		if _ := build_chain(cert, [], [ca]) {
			assert false, 'sigAlg-with-params leaf must not build'
		} else {
			assert err.msg().contains('no valid chain')
		}
	} else {
		assert err.msg().len > 0
	}
}

// test_pki_ed_key_confusion locks key/issuer-type agreement: an Ed25519
// leaf never chains to an ECDSA CA, and an ECDSA leaf never chains to an
// Ed25519 CA.
fn test_pki_ed_key_confusion() {
	now := time.now()
	_, ed_ca_priv := ed25519.generate_key() or { panic('ed keygen: ${err}') }
	ed_ca, _ := generate_self_signed_ed25519(CertGenOptions{
		common_name: 'Test Ed25519 CA'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(3650)
		is_ca:       true
	}, ed_ca_priv) or { panic('ed ca gen: ${err}') }
	_, ec_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('ec keygen: ${err}') }
	ec_ca, _ := generate_self_signed(CertGenOptions{
		common_name: 'Test EC CA'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(3650)
		is_ca:       true
	}, ec_priv) or { panic('ec ca gen: ${err}') }
	_, ed_leaf_priv := ed25519.generate_key() or { panic('ed leaf keygen: ${err}') }
	ed_leaf, _ := generate_ca_signed_ed25519(CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, ed_leaf_priv, &ed_ca, ed_ca_priv) or { panic('ed leaf gen: ${err}') }
	if _ := build_chain(ed_leaf, [], [ec_ca]) {
		assert false, 'Ed25519 leaf must not chain to an ECDSA CA'
	} else {
		assert err.msg().contains('no valid chain')
	}
	_, ec_leaf_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or {
		panic('ec leaf keygen: ${err}')
	}
	ec_leaf, _ := generate_ca_signed(CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, ec_leaf_priv, &ec_ca, ec_priv) or { panic('ec leaf gen: ${err}') }
	if _ := build_chain(ec_leaf, [], [ed_ca]) {
		assert false, 'ECDSA leaf must not chain to an Ed25519 CA'
	} else {
		assert err.msg().contains('no valid chain')
	}
}
