module x509

import crypto.ecdsa
import crypto.pem
import os
import time
import x.crypto.mldsa

// openssl_bin locates an OpenSSL CLI: PATH first, then the common Windows
// install path. Returns '' when unavailable; callers must skip gracefully.
fn openssl_bin() string {
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

fn test_our_der_parses_under_openssl() {
	bin := openssl_bin()
	if bin == '' {
		return
	}
	key, priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('keygen') }
	_ = key
	now := time.now()
	opts := CertGenOptions{
		common_name: 'crosscheck.example.com'
		dns_names:   ['crosscheck.example.com']
		not_before:  now
		not_after:   now.add_days(30)
	}
	_, der := generate_self_signed(opts, priv) or { panic('gen') }
	tmp := os.temp_dir() + '/tls_cc_self.der'
	os.write_file_array(tmp, der) or { panic('write') }
	defer {
		os.rm(tmp) or {}
	}
	res := os.exec([bin, 'x509', '-inform', 'DER', '-in', tmp, '-text', '-noout'])
	assert res.exit_code == 0
	assert res.output.contains('crosscheck.example.com')
}

fn test_our_ca_signed_der_parses_under_openssl() {
	bin := openssl_bin()
	if bin == '' {
		return
	}
	ca_key, ca_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('ca keygen') }
	_ = ca_key
	leaf_key, leaf_priv := ecdsa.generate_key(ecdsa.CurveOptions{}) or { panic('leaf keygen') }
	_ = leaf_key
	now := time.now()
	ca_opts := CertGenOptions{
		common_name: 'Test CA'
		not_before:  now
		not_after:   now.add_days(3650)
	}
	ca_cert, _ := generate_self_signed(ca_opts, ca_priv) or { panic('ca gen') }
	leaf_opts := CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now
		not_after:   now.add_days(365)
	}
	_, leaf_der := generate_ca_signed(leaf_opts, leaf_priv, &ca_cert, ca_priv) or {
		panic('leaf gen')
	}
	tmp := os.temp_dir() + '/tls_cc_leaf.der'
	os.write_file_array(tmp, leaf_der) or { panic('write') }
	defer {
		os.rm(tmp) or {}
	}
	res := os.exec([bin, 'x509', '-inform', 'DER', '-in', tmp, '-text', '-noout'])
	assert res.exit_code == 0
	assert res.output.contains('leaf.example.com')
}

fn test_openssl_cert_parses_under_v() {
	bin := openssl_bin()
	if bin == '' {
		return
	}
	dir := os.temp_dir()
	keyf := dir + '/tls_ossl_key.pem'
	certf := dir + '/tls_ossl_cert.pem'
	defer {
		os.rm(keyf) or {}
		os.rm(certf) or {}
	}
	gen := os.exec([bin, 'req', '-x509', '-newkey', 'ec', '-pkeyopt', 'ec_paramgen_curve:P-256',
		'-keyout', keyf, '-out', certf, '-days', '2', '-nodes', '-subj', '/CN=ossl-interop.example.com',
		'-addext', 'subjectAltName=DNS:ossl-interop.example.com'])
	assert gen.exit_code == 0
	pem_text := os.read_file(certf) or { panic('read cert') }
	assert pem_text.contains('BEGIN CERTIFICATE')
	block, _ := pem.decode(pem_text) or { panic('pem decode') }
	cert := parse_der(block.data) or { panic('parse openssl cert: ${err}') }
	cert.check_host('ossl-interop.example.com') or { panic('host mismatch') }
	cert.check_host('other.example.com') or { return }
	panic('expected hostname mismatch')
}

// test_our_mldsa_der_parses_under_openssl checks V-generated ML-DSA
// certificates parse under openssl for every variant.
// Skips without openssl; the same key material is covered without skips by
// test_sig_vgen_mldsa_chains and test_sig_mldsa_chain in net.tls.
fn test_our_mldsa_der_parses_under_openssl() {
	bin := openssl_bin()
	if bin == '' {
		return
	}
	kinds := [mldsa.Kind.ml_dsa_44, .ml_dsa_65, .ml_dsa_87]
	for kind in kinds {
		priv := mldsa.PrivateKey.generate(kind) or { panic('keygen') }
		now := time.now()
		_, der := generate_self_signed_mldsa(CertGenOptions{
			common_name: 'mldsa-crosscheck.example.com'
			dns_names:   ['mldsa-crosscheck.example.com']
			not_before:  now
			not_after:   now.add_days(30)
		}, priv, kind) or { panic('gen') }
		tmp := os.temp_dir() + '/tls_mldsa_self.der'
		os.write_file_array(tmp, der) or { panic('write') }
		defer {
			os.rm(tmp) or {}
		}
		res := os.exec([bin, 'x509', '-inform', 'DER', '-in', tmp, '-text', '-noout'])
		assert res.exit_code == 0
		assert res.output.contains('ML-DSA')
		assert res.output.contains('mldsa-crosscheck.example.com')
	}
}

// test_our_mldsa_ca_signed_verifies_under_openssl checks V-generated ML-DSA
// CA/leaf pairs verify under openssl for every variant.
// Skips without openssl; V-side verification of the same shapes is covered
// without skips by test_sig_mldsa_chain in net.tls.
fn test_our_mldsa_ca_signed_verifies_under_openssl() {
	bin := openssl_bin()
	if bin == '' {
		return
	}
	kinds := [mldsa.Kind.ml_dsa_44, .ml_dsa_65, .ml_dsa_87]
	for kind in kinds {
		ca_priv := mldsa.PrivateKey.generate(kind) or { panic('ca keygen') }
		now := time.now()
		ca_cert, ca_der := generate_self_signed_mldsa(CertGenOptions{
			common_name: 'Test ML-DSA CA'
			not_before:  now.add_days(-1)
			not_after:   now.add_days(3650)
			is_ca:       true
		}, ca_priv, kind) or { panic('ca gen') }
		leaf_priv := mldsa.PrivateKey.generate(kind) or { panic('leaf keygen') }
		leaf_cert, leaf_der := generate_ca_signed_mldsa(CertGenOptions{
			common_name: 'leaf.example.com'
			dns_names:   ['leaf.example.com']
			not_before:  now.add_days(-1)
			not_after:   now.add_days(30)
		}, leaf_priv, kind, &ca_cert, ca_priv, kind) or { panic('leaf gen') }
		_ = leaf_cert
		dir := os.temp_dir()
		ca_pem := dir + '/tls_mldsa_ca.pem'
		leaf_pem := dir + '/tls_mldsa_leaf.pem'
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
		defer {
			os.rm(ca_pem) or {}
			os.rm(leaf_pem) or {}
		}
		res := os.exec([bin, 'verify', '-CAfile', ca_pem, leaf_pem])
		assert res.exit_code == 0, 'openssl verify failed: ${res.output}'
	}
}
