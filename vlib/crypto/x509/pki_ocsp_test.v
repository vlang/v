module x509

import crypto.ecdsa
import crypto.pem
import encoding.hex
import os
import time

// pki_ocsp_test covers OCSP stapling verification: V-generated
// responses (good/revoked/unknown/tampered/stale/wrong-issuer) and
// openssl-generated responses with the CA as responder.

// ocsp_test_ca builds an ECDSA CA plus signer and one leaf.
fn ocsp_test_ca() (Certificate, CertSigner, Certificate, []u8) {
	now := time.now()
	_, ca_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen: ${err}') }
	ca, _ := generate_self_signed(CertGenOptions{
		common_name:   'PKI OCSP CA'
		not_before:    now.add_days(-1)
		not_after:     now.add_days(3650)
		is_ca:         true
		has_key_usage: true
		key_usage:     ku_digital_signature | ku_key_cert_sign
	}, ca_priv) or { panic('ca gen: ${err}') }
	_, leaf_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen: ${err}') }
	leaf, leaf_der := generate_ca_signed(CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, leaf_priv, &ca, ca_priv) or { panic('leaf gen: ${err}') }
	return ca, CertSigner{
		kind:       .ecdsa
		ecdsa_priv: ca_priv
	}, leaf, leaf_der
}

// test_pki_ocsp_vgen_good verifies a fresh V-generated staple.
fn test_pki_ocsp_vgen_good() {
	now := time.now()
	ca, signer, leaf, leaf_der := ocsp_test_ca()
	_, resp_der := make_ocsp_response(&leaf, &ca, OcspGenOptions{
		produced_at:     now
		this_update:     now
		next_update:     now.add_days(7)
		has_next_update: true
	}, signer) or { panic('make_ocsp: ${err}') }
	verify_ocsp_staple(resp_der, leaf_der, ca.raw) or { panic('staple: ${err}') }
}

// test_pki_ocsp_vgen_revoked rejects a revoked staple.
fn test_pki_ocsp_vgen_revoked() {
	now := time.now()
	ca, signer, leaf, leaf_der := ocsp_test_ca()
	_, resp_der := make_ocsp_response(&leaf, &ca, OcspGenOptions{
		produced_at:     now
		this_update:     now
		next_update:     now.add_days(7)
		has_next_update: true
		status:          .revoked
		revoked_at:      now
	}, signer) or { panic('make_ocsp: ${err}') }
	verify_ocsp_staple(resp_der, leaf_der, ca.raw) or {
		assert err.msg().contains('revoked')
		return
	}
	assert false, 'revoked staple must fail'
}

// test_pki_ocsp_vgen_unknown rejects an unknown-status staple.
fn test_pki_ocsp_vgen_unknown() {
	now := time.now()
	ca, signer, leaf, leaf_der := ocsp_test_ca()
	_, resp_der := make_ocsp_response(&leaf, &ca, OcspGenOptions{
		produced_at:     now
		this_update:     now
		next_update:     now.add_days(7)
		has_next_update: true
		status:          .unknown
	}, signer) or { panic('make_ocsp: ${err}') }
	verify_ocsp_staple(resp_der, leaf_der, ca.raw) or {
		assert err.msg().contains('unknown')
		return
	}
	assert false, 'unknown staple must fail'
}

// test_pki_ocsp_vgen_tamper rejects a damaged staple signature.
fn test_pki_ocsp_vgen_tamper() {
	now := time.now()
	ca, signer, leaf, leaf_der := ocsp_test_ca()
	_, resp_der := make_ocsp_response(&leaf, &ca, OcspGenOptions{
		produced_at:     now
		this_update:     now
		next_update:     now.add_days(7)
		has_next_update: true
	}, signer) or { panic('make_ocsp: ${err}') }
	mut bad := resp_der.clone()
	bad[bad.len - 1] ^= u8(0x01)
	verify_ocsp_staple(bad, leaf_der, ca.raw) or {
		assert err.msg().contains('signature') || err.msg().contains('bad')
			|| err.msg().contains('mismatch')
		return
	}
	assert false, 'tampered staple must fail'
}

// test_pki_ocsp_vgen_wrong_serial rejects a staple for another leaf.
fn test_pki_ocsp_vgen_wrong_serial() {
	now := time.now()
	ca, signer, leaf, leaf_der := ocsp_test_ca()
	_, other_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen') }
	other, _ := generate_ca_signed(CertGenOptions{
		common_name: 'other.example.com'
		dns_names:   ['other.example.com']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(30)
	}, other_priv, &ca, signer.ecdsa_priv or { panic('no key') }) or {
		panic('other gen: ${err}')
	}
	_, resp_der := make_ocsp_response(&other, &ca, OcspGenOptions{
		produced_at:     now
		this_update:     now
		next_update:     now.add_days(7)
		has_next_update: true
	}, signer) or { panic('make_ocsp: ${err}') }
	verify_ocsp_staple(resp_der, leaf_der, ca.raw) or {
		assert err.msg().contains('missing leaf serial')
		return
	}
	assert false, 'staple for another leaf must fail'
}

// test_pki_ocsp_vgen_stale rejects out-of-window staples.
fn test_pki_ocsp_vgen_stale() {
	now := time.now()
	ca, signer, leaf, leaf_der := ocsp_test_ca()
	// Produced two days ago: outside the freshness window.
	_, old := make_ocsp_response(&leaf, &ca, OcspGenOptions{
		produced_at:     now.add_days(-2)
		this_update:     now.add_days(-2)
		next_update:     now.add_days(5)
		has_next_update: true
	}, signer) or { panic('make_ocsp: ${err}') }
	verify_ocsp_staple(old, leaf_der, ca.raw) or {
		assert err.msg().contains('freshness')
		// Fresh producedAt but a past nextUpdate: stale.
		_, stale := make_ocsp_response(&leaf, &ca, OcspGenOptions{
			produced_at:     now
			this_update:     now.add_days(-2)
			next_update:     now.add_days(-1)
			has_next_update: true
		}, signer) or { panic('make stale: ${err}') }
		verify_ocsp_staple(stale, leaf_der, ca.raw) or {
			assert err.msg().contains('stale')
			return
		}
		assert false, 'stale staple must fail'
	}
	assert false, 'old staple must fail freshness'
}

// test_pki_ocsp_vgen_wrong_issuer rejects staples under another CA.
fn test_pki_ocsp_vgen_wrong_issuer() {
	now := time.now()
	ca, signer, leaf, leaf_der := ocsp_test_ca()
	_, bad_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen') }
	bad_ca, _ := generate_self_signed(CertGenOptions{
		common_name: 'PKI Other CA'
		not_before:  now.add_days(-1)
		not_after:   now.add_days(3650)
		is_ca:       true
	}, bad_priv) or { panic('bad ca gen') }
	_, resp_der := make_ocsp_response(&leaf, &ca, OcspGenOptions{
		produced_at:     now
		this_update:     now
		next_update:     now.add_days(7)
		has_next_update: true
	}, signer) or { panic('make_ocsp: ${err}') }
	verify_ocsp_staple(resp_der, leaf_der, bad_ca.raw) or {
		assert err.msg().contains('responder') || err.msg().contains('mismatch')
			|| err.msg().contains('signature') || err.msg().contains('bad')
		return
	}
	assert false, 'staple under wrong issuer must fail'
}

// test_pki_ocsp_chain_staple wires a staple into verify_chain.
fn test_pki_ocsp_chain_staple() {
	now := time.now()
	ca, signer, leaf, _ := ocsp_test_ca()
	chain := build_chain(leaf, [], [ca]) or { panic('build: ${err}') }
	_, good := make_ocsp_response(&leaf, &ca, OcspGenOptions{
		produced_at:     now
		this_update:     now
		next_update:     now.add_days(7)
		has_next_update: true
	}, signer) or { panic('make_ocsp: ${err}') }
	verify_chain(chain, VerifyOptions{
		ocsp_resp: good
	}) or { panic('stapled verify: ${err}') }
	_, revoked := make_ocsp_response(&leaf, &ca, OcspGenOptions{
		produced_at:     now
		this_update:     now
		next_update:     now.add_days(7)
		has_next_update: true
		status:          .revoked
		revoked_at:      now
	}, signer) or { panic('make revoked: ${err}') }
	verify_chain(chain, VerifyOptions{
		ocsp_resp: revoked
	}) or {
		assert err.msg().contains('revoked')
		return
	}
	assert false, 'revoked staple in chain must fail'
}

// ocsp_openssl_setup creates a CA + leaf + index + request in dir,
// returning the leaf/issuer DER and the request path.
fn ocsp_openssl_setup(bin string, dir string) ([]u8, []u8, string) {
	ca_key := os.join_path(dir, 'ocsp_ca_key.pem')
	ca_pem := os.join_path(dir, 'ocsp_ca.pem')
	leaf_key := os.join_path(dir, 'ocsp_leaf_key.pem')
	leaf_csr := os.join_path(dir, 'ocsp_leaf.csr')
	leaf_pem := os.join_path(dir, 'ocsp_leaf.pem')
	req_der := os.join_path(dir, 'ocsp_req.der')
	assert os.exec([bin, 'req', '-x509', '-newkey', 'ec', '-pkeyopt', 'ec_paramgen_curve:P-256',
		'-keyout', ca_key, '-out', ca_pem, '-days', '30', '-nodes', '-subj', '/CN=OCSP-Test-CA',
		'-addext', 'basicConstraints=critical,CA:TRUE', '-addext',
		'keyUsage=critical,digitalSignature,keyCertSign,cRLSign']).exit_code == 0
	assert os.exec([bin, 'req', '-newkey', 'ec', '-pkeyopt', 'ec_paramgen_curve:P-256', '-keyout',
		leaf_key, '-out', leaf_csr, '-nodes', '-subj', '/CN=leaf.example.com', '-addext',
		'subjectAltName=DNS:leaf.example.com']).exit_code == 0
	assert os.exec([bin, 'x509', '-req', '-in', leaf_csr, '-CA', ca_pem, '-CAkey', ca_key,
		'-CAcreateserial', '-days', '30', '-out', leaf_pem]).exit_code == 0
	assert os.exec([bin, 'ocsp', '-issuer', ca_pem, '-cert', leaf_pem, '-reqout', req_der]).exit_code == 0
	ca_text := os.read_file(ca_pem) or { panic('read ca: ${err}') }
	leaf_text := os.read_file(leaf_pem) or { panic('read leaf: ${err}') }
	ca_block, _ := pem.decode(ca_text) or { panic('ca pem missing') }
	leaf_block, _ := pem.decode(leaf_text) or { panic('leaf pem missing') }
	return leaf_block.data, ca_block.data, req_der
}

// ocsp_openssl_respond answers req_der from index_txt with the CA itself
// as responder, returning the response DER.
fn ocsp_openssl_respond(bin string, dir string, index_txt string, req_der string) []u8 {
	ca_pem := os.join_path(dir, 'ocsp_ca.pem')
	ca_key := os.join_path(dir, 'ocsp_ca_key.pem')
	ca_both := os.join_path(dir, 'ocsp_ca_both.pem')
	index_file := os.join_path(dir, 'ocsp_index.txt')
	resp_der := os.join_path(dir, 'ocsp_resp.der')
	os.write_file(index_file, index_txt) or { panic('write index: ${err}') }
	both := os.read_file(ca_pem) or { panic(err) } + os.read_file(ca_key) or { panic(err) }
	os.write_file(ca_both, both) or { panic(err) }
	res := os.exec([bin, 'ocsp', '-index', index_file, '-CA', ca_pem, '-rsigner', ca_both, '-reqin',
		req_der, '-respout', resp_der, '-ndays', '7'])
	assert res.exit_code == 0, 'openssl ocsp failed: ${res.output}'
	return os.read_file(resp_der) or { panic('read resp: ${err}') }.bytes()
}

// test_pki_ocsp_openssl verifies an openssl-generated staple (CA as
// responder) and rejects its revoked counterpart. Skips without openssl.
fn test_pki_ocsp_openssl() {
	bin := pki_openssl_bin()
	if bin == '' {
		return
	}
	dir := os.join_path(os.temp_dir(), 'pki_ocsp_ossl')
	os.mkdir_all(dir) or { panic('mkdir: ${err}') }
	defer {
		os.rmdir_all(dir) or {}
	}
	leaf_der, ca_der, req_der := ocsp_openssl_setup(bin, dir)
	leaf := parse_der(leaf_der) or { panic('parse leaf: ${err}') }
	serial_hex := hex.encode(leaf.serial).to_upper()
	index_txt := 'V\t271231235959Z\t\t${serial_hex}\tunknown\t/CN=leaf.example.com\n'
	resp := ocsp_openssl_respond(bin, dir, index_txt, req_der)
	verify_ocsp_staple(resp, leaf_der, ca_der) or { panic('openssl staple: ${err}') }
	// Revoked entry: same serial marked R.
	rev_index := 'R\t271231235959Z\t261115120000Z\t${serial_hex}\tunknown\t/CN=leaf.example.com\n'
	rev_resp := ocsp_openssl_respond(bin, dir, rev_index, req_der)
	verify_ocsp_staple(rev_resp, leaf_der, ca_der) or {
		assert err.msg().contains('revoked')
		return
	}
	assert false, 'revoked openssl staple must fail'
}
