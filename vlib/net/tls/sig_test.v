module tls

import crypto.ecdsa
import crypto.pem
import crypto.x509
import os
import time
import x.crypto.mldsa

fn sigtest_read(name string) []u8 {
	return os.read_file(os.join_path(@VEXEROOT, 'vlib/net/tls/testdata/' + name)) or {
		panic('read fixture ${name}: ${err}')
	}.bytes()
}

fn sigtest_pem_block(pem_text []u8, typ string) []u8 {
	block, _ := pem.decode(pem_text.bytestr()) or { panic('no ${typ} pem block') }
	assert block.block_type == typ
	return block.data.clone()
}

fn sigtest_store(ca_pem_name string) CertStore {
	ca_der := sigtest_pem_block(sigtest_read(ca_pem_name), 'CERTIFICATE')
	ca_cert := x509.parse_der(ca_der) or { panic('parse ca: ${err}') }
	return CertStore{
		roots: [ca_cert]
	}
}

fn sigtest_leaf(leaf_pem_name string) ([]u8, x509.PublicKey) {
	leaf_der := sigtest_pem_block(sigtest_read(leaf_pem_name), 'CERTIFICATE')
	leaf := x509.parse_der(leaf_der) or { panic('parse leaf: ${err}') }
	pk := leaf.public_key() or { panic('leaf pubkey: ${err}') }
	return leaf_der, pk
}

// test_sig_rsa_chain verifies the openssl-generated RSA leaf against its CA.
fn test_sig_rsa_chain() {
	store := sigtest_store('rsa_ca_cert.pem')
	leaf_der, _ := sigtest_leaf('rsa_leaf_cert.pem')
	mut trusted := store.contains(x509.parse_der(leaf_der) or { panic(err) })
	if !trusted {
		for r in store.roots {
			rpk := r.public_key() or { panic(err) }
			verify_cert_signature(leaf_der, rpk) or { panic('rsa chain verify: ${err}') }
			trusted = true
			break
		}
	}
	assert trusted
}

// test_sig_rsa_chain_tampered locks rejection of a damaged RSA signature.
fn test_sig_rsa_chain_tampered() {
	store := sigtest_store('rsa_ca_cert.pem')
	leaf_der, _ := sigtest_leaf('rsa_leaf_cert.pem')
	mut bad := leaf_der.clone()
	bad[bad.len - 1] ^= u8(0x01)
	for r in store.roots {
		rpk := r.public_key() or { panic(err) }
		verify_cert_signature(bad, rpk) or {
			assert err.msg().contains('signature')
			return
		}
	}
	assert false, 'tampered RSA chain must fail'
}

// test_sig_p384_chain verifies the openssl P-384 leaf (ecdsa-with-SHA256,
// i.e. a curve/hash cross-combo) against its CA.
fn test_sig_p384_chain() {
	store := sigtest_store('p384_ca_cert.pem')
	leaf_der, _ := sigtest_leaf('p384_leaf_cert.pem')
	mut trusted := false
	for r in store.roots {
		rpk := r.public_key() or { panic(err) }
		verify_cert_signature(leaf_der, rpk) or { panic('p384 chain verify: ${err}') }
		trusted = true
		break
	}
	assert trusted
}

// test_sig_p521_chain verifies the openssl P-521 leaf against its CA.
fn test_sig_p521_chain() {
	store := sigtest_store('p521_ca_cert.pem')
	leaf_der, _ := sigtest_leaf('p521_leaf_cert.pem')
	mut trusted := false
	for r in store.roots {
		rpk := r.public_key() or { panic(err) }
		verify_cert_signature(leaf_der, rpk) or { panic('p521 chain verify: ${err}') }
		trusted = true
		break
	}
	assert trusted
}

// test_sig_vgen_p384_chain covers V-generated P-384 (same-combo SHA-384).
fn test_sig_vgen_p384_chain() {
	_, ca_priv := ecdsa.generate_key(ecdsa.CurveOptions{ nid: .secp384r1 }) or {
		panic('ca keygen')
	}
	now := time.now()
	ca_cert, _ := x509.generate_self_signed(x509.CertGenOptions{
		common_name: 'Test P-384 CA'
		not_before:  now
		not_after:   now.add_days(3650)
	}, ca_priv) or { panic('ca gen') }
	_, leaf_priv := ecdsa.generate_key(ecdsa.CurveOptions{ nid: .secp384r1 }) or {
		panic('leaf keygen')
	}
	_, leaf_der := x509.generate_ca_signed(x509.CertGenOptions{
		common_name: 'leaf.example.com'
		dns_names:   ['leaf.example.com']
		not_before:  now
		not_after:   now.add_days(365)
	}, leaf_priv, &ca_cert, ca_priv) or { panic('leaf gen') }
	ca_pk := ca_cert.public_key() or { panic('ca pubkey') }
	verify_cert_signature(leaf_der, ca_pk) or { panic('vgen p384 chain: ${err}') }
}

// test_sig_rsa_cv_roundtrip exercises RSA CertificateVerify for every RSA scheme.
fn test_sig_rsa_cv_roundtrip() {
	leaf_der, pk := sigtest_leaf('rsa_leaf_cert.pem')
	_ = leaf_der
	rsa_pk := match pk {
		x509.RsaPublicKey { pk }
		else { panic('expected rsa leaf') }
	}
	n, e := parse_rsa_pubkey(rsa_pk.der) or { panic(err) }
	key_der := sigtest_pem_block(sigtest_read('rsa_leaf_rsa_key.pem'), 'RSA PRIVATE KEY')
	priv := parse_rsa_private_key(key_der) or { panic(err) }
	assert priv.n == n
	content := 'test certificate verify'.bytes()
	for scheme in [u16(0x0401), 0x0501, 0x0601, 0x0804, 0x0805, 0x0806] {
		mut sig := []u8{}
		if scheme in [u16(0x0401), 0x0501, 0x0601] {
			h := match scheme {
				u16(0x0401) { RsaHash.sha256 }
				u16(0x0501) { RsaHash.sha384 }
				else { RsaHash.sha512 }
			}
			sig = rsa_pkcs1_sign_digest(rsa_hash_bytes(h, content), priv.d, priv.n, h) or {
				panic('pkcs1 sign ${scheme:04x}: ${err}')
			}
		} else {
			sig = rsa_pss_sign(scheme, content, priv.d, priv.n) or {
				panic('pss sign ${scheme:04x}: ${err}')
			}
		}
		mut body := []u8{}
		body << u8(scheme >> 8)
		body << u8(scheme)
		body << u8(sig.len >> 8)
		body << u8(sig.len)
		body << sig
		verify_certificate_verify(body, pk, content) or {
			panic('cv verify ${scheme:04x}: ${err}')
		}
		// Tampered signatures must fail.
		mut bad_sig := sig.clone()
		bad_sig[0] ^= u8(0x01)
		mut bad_body := []u8{}
		bad_body << u8(scheme >> 8)
		bad_body << u8(scheme)
		bad_body << u8(bad_sig.len >> 8)
		bad_body << u8(bad_sig.len)
		bad_body << bad_sig
		verify_certificate_verify(bad_body, pk, content) or { continue }
		assert false, 'tampered cv sig must fail for ${scheme:04x}'
	}
}

// test_sig_ecdsa_cv_curves exercises ECDSA CertificateVerify for P-384/P-521.
fn test_sig_ecdsa_cv_curves() {
	content := 'curve certificate verify'.bytes()
	for cfg in [
		CvCurve{ nid: ecdsa.Nid.secp384r1, scheme: u16(0x0503), size: 97 },
		CvCurve{ nid: ecdsa.Nid.secp521r1, scheme: u16(0x0603), size: 133 },
	] {
		_, priv := ecdsa.generate_key(ecdsa.CurveOptions{ nid: cfg.nid }) or {
			panic('keygen')
		}
		epub := priv.public_key() or { panic('pubkey') }
		point := epub.uncompressed_bytes() or { panic('point') }
		assert point.len == cfg.size
		sig := priv.sign(content, ecdsa.SignerOpts{}) or { panic('sign') }
		mut body := []u8{}
		body << u8(cfg.scheme >> 8)
		body << u8(cfg.scheme)
		body << u8(sig.len >> 8)
		body << u8(sig.len)
		body << sig
		pk := if cfg.nid == ecdsa.Nid.secp384r1 {
			x509.PublicKey(x509.EcdsaP384PublicKey{ point: point })
		} else {
			x509.PublicKey(x509.EcdsaP521PublicKey{ point: point })
		}
		verify_certificate_verify(body, pk, content) or { panic('cv verify: ${err}') }
	}
}

struct CvCurve {
	nid    ecdsa.Nid
	scheme u16
	size   int
}

// test_sig_cv_mismatch locks scheme/key-type agreement enforcement.
fn test_sig_cv_mismatch() {
	_, priv := ecdsa.generate_key() or { panic('keygen') }
	epub := priv.public_key() or { panic('pubkey') }
	point := epub.uncompressed_bytes() or { panic('point') }
	content := 'mismatch'.bytes()
	sig := priv.sign(content, ecdsa.SignerOpts{}) or { panic('sign') }
	// P-256 signature presented as RSA-PSS must fail even though the bytes verify.
	mut body := []u8{}
	body << u8(0x08)
	body << u8(0x04)
	body << u8(sig.len >> 8)
	body << u8(sig.len)
	body << sig
	pk := x509.PublicKey(x509.EcdsaP256PublicKey{ point: point })
	verify_certificate_verify(body, pk, content) or {
		assert err.msg().contains('mismatch') || err.msg().contains('scheme')
		return
	}
	assert false, 'scheme/key mismatch must fail'
}

// test_sig_select_cv_scheme locks server scheme selection.
fn test_sig_select_cv_scheme() {
	assert select_cv_scheme([u16(0x0403), 0x0804], .ecdsa_p256)! == u16(0x0403)
	assert select_cv_scheme([u16(0x0503)], .ecdsa_p384)! == u16(0x0503)
	assert select_cv_scheme([u16(0x0603)], .ecdsa_p521)! == u16(0x0603)
	select_cv_scheme([u16(0x0804)], .ecdsa_p256) or {
		assert err.msg().contains('scheme')
		return
	}
	assert false, 'P-256 without 0403 must fail'
	assert select_cv_scheme([u16(0x0403), 0x0804, 0x0401], .rsa)! == u16(0x0804)
	assert select_cv_scheme([u16(0x0401), 0x0501], .rsa)! == u16(0x0401)
	select_cv_scheme([u16(0x0403)], .rsa) or {
		assert err.msg().contains('scheme')
		return
	}
	assert false, 'RSA without RSA scheme must fail'
}

// test_sig_server_key_loading locks RSA/ECDSA credential loading paths.
fn test_sig_server_key_loading() {
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	mut s := new_server_handshake_with_config(.off, Config{
		cert: os.join_path(td, 'rsa_leaf_cert.pem')
		key:  os.join_path(td, 'rsa_leaf_rsa_key.pem')
	}) or { panic('rsa pkcs1 load: ${err}') }
	assert s.key_kind == .rsa
	mut s8 := new_server_handshake_with_config(.off, Config{
		cert: os.join_path(td, 'rsa_leaf_cert.pem')
		key:  os.join_path(td, 'rsa_leaf_pkcs8_key.pem')
	}) or { panic('rsa pkcs8 load: ${err}') }
	assert s8.key_kind == .rsa
	mut s384 := new_server_handshake_with_config(.off, Config{
		cert: os.join_path(td, 'p384_leaf_cert.pem')
		key:  os.join_path(td, 'p384_leaf_key.pem')
	}) or { panic('p384 load: ${err}') }
	assert s384.key_kind == .ecdsa_p384
	new_server_handshake_with_config(.off, Config{
		cert: os.join_path(td, 'rsa_leaf_cert.pem')
		key:  os.join_path(td, 'rsa_ca_key.pem')
	}) or {
		assert err.msg().contains('match')
		return
	}
	assert false, 'mismatched cert/key must fail'
}

// test_sig_rsa_private_parsing locks PKCS#1/PKCS#8 parsing and n/e recovery.
fn test_sig_rsa_private_parsing() {
	key_der := sigtest_pem_block(sigtest_read('rsa_leaf_rsa_key.pem'), 'RSA PRIVATE KEY')
	priv := parse_rsa_private_key(key_der) or { panic(err) }
	_, pk := sigtest_leaf('rsa_leaf_cert.pem')
	rsa_pk := match pk {
		x509.RsaPublicKey { pk }
		else { panic('expected rsa leaf') }
	}
	n, e := parse_rsa_pubkey(rsa_pk.der) or { panic(err) }
	assert priv.n == n
	assert priv.e == e
	key8 := sigtest_pem_block(sigtest_read('rsa_leaf_pkcs8_key.pem'), 'PRIVATE KEY')
	priv8 := parse_rsa_private_key(key8) or { panic(err) }
	assert priv8.n == n
}

// test_sig_loopback_rsa runs a full verified handshake against an RSA server.
fn test_sig_loopback_rsa() {
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	server_cfg := Config{
		cert: os.join_path(td, 'rsa_leaf_cert.pem')
		key:  os.join_path(td, 'rsa_leaf_key.pem')
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
	ca_der := sigtest_pem_block(sigtest_read('rsa_ca_cert.pem'), 'CERTIFICATE')
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
	c.handshake() or { panic('client hs: ${err}') }
	c.write('ping'.bytes()) or { panic(err) }
	mut buf := []u8{len: 4}
	n := c.read(mut buf) or { panic(err) }
	assert n == 4
	assert buf == 'pong'.bytes()
	c.close() or {}
	l.close() or {}
	_ = server
}

// test_sig_loopback_p384 runs a full verified handshake against a P-384 server.
fn test_sig_loopback_p384() {
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	server_cfg := Config{
		cert: os.join_path(td, 'p384_leaf_cert.pem')
		key:  os.join_path(td, 'p384_leaf_key.pem')
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
	ca_der := sigtest_pem_block(sigtest_read('p384_ca_cert.pem'), 'CERTIFICATE')
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
	c.handshake() or { panic('client hs: ${err}') }
	c.write('ping'.bytes()) or { panic(err) }
	mut buf := []u8{len: 4}
	n := c.read(mut buf) or { panic(err) }
	assert n == 4
	assert buf == 'pong'.bytes()
	c.close() or {}
	l.close() or {}
	_ = server
}

// mldsa_case bundles one ML-DSA variant's TLS parameters.
struct MldsaCase {
	kind   mldsa.Kind
	scheme u16
	pk_len int
	ca     string
	leaf   string
	key    string
}

fn mldsa_cases() []MldsaCase {
	return [
		MldsaCase{
			kind:   .ml_dsa_44
			scheme: u16(0x0904)
			pk_len: 1312
			ca:     'mldsa44_ca_cert.pem'
			leaf:   'mldsa44_leaf_cert.pem'
			key:    'mldsa44_leaf_key.pem'
		},
		MldsaCase{
			kind:   .ml_dsa_65
			scheme: u16(0x0905)
			pk_len: 1952
			ca:     'mldsa65_ca_cert.pem'
			leaf:   'mldsa65_leaf_cert.pem'
			key:    'mldsa65_leaf_key.pem'
		},
		MldsaCase{
			kind:   .ml_dsa_87
			scheme: u16(0x0906)
			pk_len: 2592
			ca:     'mldsa87_ca_cert.pem'
			leaf:   'mldsa87_leaf_cert.pem'
			key:    'mldsa87_leaf_key.pem'
		},
	]
}

// test_sig_mldsa_chain verifies the openssl-generated ML-DSA leaves against
// their CAs through verify_cert_signature.
fn test_sig_mldsa_chain() {
	for cs in mldsa_cases() {
		store := sigtest_store(cs.ca)
		leaf_der, pk := sigtest_leaf(cs.leaf)
		_ = pk
		mut trusted := false
		for r in store.roots {
			rpk := r.public_key() or { panic(err) }
			verify_cert_signature(leaf_der, rpk) or { panic('mldsa chain verify ${cs.scheme:04x}: ${err}') }
			trusted = true
			break
		}
		assert trusted
	}
}

// test_sig_mldsa_chain_tampered locks rejection of a damaged ML-DSA signature.
fn test_sig_mldsa_chain_tampered() {
	store := sigtest_store('mldsa44_ca_cert.pem')
	leaf_der, _ := sigtest_leaf('mldsa44_leaf_cert.pem')
	mut bad := leaf_der.clone()
	bad[bad.len - 1] ^= u8(0x01)
	for r in store.roots {
		rpk := r.public_key() or { panic(err) }
		verify_cert_signature(bad, rpk) or {
			assert err.msg().contains('signature') || err.msg().contains('verify')
				|| err.msg().contains('mismatch')
			return
		}
	}
	assert false, 'tampered ML-DSA chain must fail'
}

// test_sig_vgen_mldsa_chains covers V-generated ML-DSA CA/leaf pairs.
fn test_sig_vgen_mldsa_chains() {
	now := time.now()
	for cs in mldsa_cases() {
		ca_priv := mldsa.PrivateKey.generate(cs.kind) or { panic('ca keygen') }
		ca_cert, _ := x509.generate_self_signed_mldsa(x509.CertGenOptions{
			common_name: 'Test ML-DSA CA'
			not_before:  now
			not_after:   now.add_days(3650)
		}, ca_priv, cs.kind) or { panic('ca gen') }
		leaf_priv := mldsa.PrivateKey.generate(cs.kind) or { panic('leaf keygen') }
		_, leaf_der := x509.generate_ca_signed_mldsa(x509.CertGenOptions{
			common_name: 'leaf.example.com'
			dns_names:   ['leaf.example.com']
			not_before:  now
			not_after:   now.add_days(365)
		}, leaf_priv, cs.kind, &ca_cert, ca_priv, cs.kind) or { panic('leaf gen') }
		ca_pk := ca_cert.public_key() or { panic('ca pubkey') }
		verify_cert_signature(leaf_der, ca_pk) or {
			panic('vgen mldsa chain ${cs.scheme:04x}: ${err}')
		}
		// Cross-variant: a 44-signed leaf must not verify under a 65 CA key,
		// and the sigAlg/key gate must be the reason.
		if cs.kind == .ml_dsa_44 {
			other := mldsa.PrivateKey.generate(.ml_dsa_65) or { panic('other keygen') }
			other_ca, _ := x509.generate_self_signed_mldsa(x509.CertGenOptions{
				common_name: 'Other CA'
				not_before:  now
				not_after:   now.add_days(3650)
			}, other, .ml_dsa_65) or { panic('other ca gen') }
			other_pk := other_ca.public_key() or { panic('other pubkey') }
			verify_cert_signature(leaf_der, other_pk) or {
				assert err.msg().contains('mismatch')
				continue
			}
			assert false, 'cross-variant ML-DSA chain must fail'
		}
	}
}

// test_sig_mldsa_key_kind_mismatch locks the key/kind agreement gate: a key
// passed with the wrong variant must fail generation, not emit an
// unverifiable certificate.
fn test_sig_mldsa_key_kind_mismatch() {
	now := time.now()
	priv65 := mldsa.PrivateKey.generate(.ml_dsa_65) or { panic('keygen') }
	x509.generate_self_signed_mldsa(x509.CertGenOptions{
		common_name: 'Mismatch CA'
		not_before:  now
		not_after:   now.add_days(30)
	}, priv65, .ml_dsa_44) or {
		assert err.msg().contains('mismatch')
		return
	}
	assert false, 'wrong-kind ML-DSA generation must fail'
}

// test_sigalg_primary_oid locks primary-OID dispatch: a trailing smuggled OID
// in the parameters position must never select the ECDSA hash, and an
// ML-DSA sigAlg with any parameters is rejected outright.
fn test_sigalg_primary_oid() {
	// SEQ { OID ecdsa-with-SHA384, OID ecdsa-with-SHA256 }: primary is SHA-384.
	sigalg := [u8(0x06), 0x08, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x04, 0x03, 0x03, u8(0x06), 0x08,
		0x2a, 0x86, 0x48, 0xce, 0x3d, 0x04, 0x03, 0x02]
	assert ecdsa_sigalg_hash(sigalg)! == .sha384
	// SEQ { OID ml-dsa-65, OID ml-dsa-44 }: the trailing OID counts as
	// parameters, which ML-DSA rejects outright (no silent primary-wins).
	mldsa_sig := [u8(0x06), 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, u8(0x12), 0x06,
		0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, 0x11]
	mldsa_kind_from_sigalg(mldsa_sig) or {
		assert err.msg().contains('ML-DSA')
		// ML-DSA sigAlg with NULL parameters is rejected outright.
		with_params := [u8(0x06), 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, u8(0x11),
			0x05, 0x00]
		mldsa_kind_from_sigalg(with_params) or {
			assert err.msg().contains('ML-DSA')
			return
		}
		assert false, 'ML-DSA sigAlg with params must fail'
	}
	assert false, 'ML-DSA sigAlg with trailing OID must fail'
}

// test_sig_mldsa_cv_roundtrips exercises CertificateVerify for every ML-DSA
// variant: raw API sign, CV framing, verify, tamper rejection.
fn test_sig_mldsa_cv_roundtrips() {
	content := 'mldsa certificate verify'.bytes()
	for cs in mldsa_cases() {
		priv := mldsa.PrivateKey.generate(cs.kind) or { panic('keygen') }
		raw := priv.public_key().bytes()
		assert raw.len == cs.pk_len
		sig := priv.sign(content, mldsa.SignerOpts{}) or { panic('sign') }
		assert sig.len == cs.kind.signature_size()
		mut body := []u8{}
		body << u8(cs.scheme >> 8)
		body << u8(cs.scheme)
		body << u8(sig.len >> 8)
		body << u8(sig.len)
		body << sig
		pk := match cs.kind {
			.ml_dsa_44 { x509.PublicKey(x509.MlDsa44PublicKey{ raw: raw }) }
			.ml_dsa_65 { x509.PublicKey(x509.MlDsa65PublicKey{ raw: raw }) }
			else { x509.PublicKey(x509.MlDsa87PublicKey{ raw: raw }) }
		}
		verify_certificate_verify(body, pk, content) or {
			panic('mldsa cv verify ${cs.scheme:04x}: ${err}')
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
		assert false, 'tampered mldsa cv sig must fail for ${cs.scheme:04x}'
	}
}

// test_sig_mldsa_cv_mismatch locks scheme/key agreement for ML-DSA.
fn test_sig_mldsa_cv_mismatch() {
	priv := mldsa.PrivateKey.generate(.ml_dsa_44) or { panic('keygen') }
	raw := priv.public_key().bytes()
	content := 'mismatch'.bytes()
	sig := priv.sign(content, mldsa.SignerOpts{}) or { panic('sign') }
	// Right key, wrong scheme (65 instead of 44).
	mut body := []u8{}
	body << u8(0x09)
	body << u8(0x05)
	body << u8(sig.len >> 8)
	body << u8(sig.len)
	body << sig
	pk44 := x509.PublicKey(x509.MlDsa44PublicKey{ raw: raw })
	verify_certificate_verify(body, pk44, content) or {
		assert err.msg().contains('mismatch')
		// Wrong key type: ML-DSA signature presented as ECDSA must fail.
		mut ebody := []u8{}
		ebody << u8(0x04)
		ebody << u8(0x03)
		ebody << u8(sig.len >> 8)
		ebody << u8(sig.len)
		ebody << sig
		verify_certificate_verify(ebody, pk44, content) or {
			assert err.msg().contains('mismatch') || err.msg().contains('scheme')
			return
		}
		assert false, 'ecdsa scheme with mldsa key must fail'
	}
	assert false, 'cross-scheme ML-DSA must fail'
}

// test_sig_select_cv_scheme_mldsa locks server scheme selection for ML-DSA.
fn test_sig_select_cv_scheme_mldsa() {
	assert select_cv_scheme([u16(0x0904), 0x0403], .mldsa_44)! == u16(0x0904)
	assert select_cv_scheme([u16(0x0905)], .mldsa_65)! == u16(0x0905)
	assert select_cv_scheme([u16(0x0906)], .mldsa_87)! == u16(0x0906)
	select_cv_scheme([u16(0x0905)], .mldsa_44) or {
		assert err.msg().contains('scheme')
		return
	}
	assert false, 'ML-DSA-44 without 0904 must fail'
}

// test_sig_server_key_loading_mldsa locks ML-DSA PKCS#8 credential loading.
fn test_sig_server_key_loading_mldsa() {
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	for cs in mldsa_cases() {
		kind := match cs.kind {
			.ml_dsa_44 { ServerKeyKind.mldsa_44 }
			.ml_dsa_65 { ServerKeyKind.mldsa_65 }
			else { ServerKeyKind.mldsa_87 }
		}
		mut s := new_server_handshake_with_config(.off, Config{
			cert: os.join_path(td, cs.leaf)
			key:  os.join_path(td, cs.key)
		}) or { panic('mldsa load ${cs.scheme:04x}: ${err}') }
		assert s.key_kind == kind
	}
	new_server_handshake_with_config(.off, Config{
		cert: os.join_path(td, 'mldsa44_leaf_cert.pem')
		key:  os.join_path(td, 'mldsa65_leaf_key.pem')
	}) or {
		assert err.msg().contains('match')
		return
	}
	assert false, 'mismatched mldsa cert/key must fail'
}

// mldsa_der_tlv wraps content in a DER tag+length header (test-only helper
// with a unique name so single-file and directory test runs agree).
fn mldsa_der_tlv(tag u8, content []u8) []u8 {
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

// mldsa_pkcs8_pem encodes an ML-DSA private key as PKCS#8 PEM (test-only).
fn mldsa_pkcs8_pem(priv mldsa.PrivateKey, kind mldsa.Kind) string {
	seed := priv.seed()
	tail := match kind {
		.ml_dsa_44 { u8(0x11) }
		.ml_dsa_65 { u8(0x12) }
		else { u8(0x13) }
	}
	oid := mldsa_der_tlv(0x06, [u8(0x60), 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, tail])
	alg := mldsa_der_tlv(0x30, oid)
	mut inner_content := mldsa_der_tlv(0x04, seed)
	inner_content << mldsa_der_tlv(0x04, priv.bytes())
	inner := mldsa_der_tlv(0x30, inner_content)
	mut body := mldsa_der_tlv(0x02, [u8(0)])
	body << alg
	body << mldsa_der_tlv(0x04, inner)
	der := mldsa_der_tlv(0x30, body)
	return pem.Block{
		block_type: 'PRIVATE KEY'
		data:       der
	}.encode(pem.EncodeConfig{}) or { panic('pem encode') }
}

// test_sig_server_key_loading_mldsa_match locks the cert/key pubkey comparison:
// a different key of the same variant must fail at the match check, not at an
// earlier parse gate.
fn test_sig_server_key_loading_mldsa_match() {
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	other := mldsa.PrivateKey.generate(.ml_dsa_44) or { panic('keygen') }
	new_server_handshake_with_config(.off, Config{
		cert:    os.join_path(td, 'mldsa44_leaf_cert.pem')
		key_pem: mldsa_pkcs8_pem(other, .ml_dsa_44).bytes()
	}) or {
		assert err.msg().contains('match')
		return
	}
	assert false, 'same-variant different mldsa key must fail the match check'
}

// test_sig_mldsa_degenerate locks rejection of malformed ML-DSA inputs:
// empty/truncated signatures, wrong-variant framing, trailing CV bytes,
// truncated chain DER, and wrong-length parent keys.
fn test_sig_mldsa_degenerate() {
	priv := mldsa.PrivateKey.generate(.ml_dsa_44) or { panic('keygen') }
	raw := priv.public_key().bytes()
	content := 'degenerate'.bytes()
	sig := priv.sign(content, mldsa.SignerOpts{}) or { panic('sign') }
	pk := x509.PublicKey(x509.MlDsa44PublicKey{ raw: raw })
	// Empty signature parses but never verifies.
	verify_certificate_verify([u8(0x09), 0x04, 0, 0], pk, content) or {
		assert err.msg().contains('verify')
		// Truncated signature.
		mut trunc := []u8{}
		trunc << u8(0x09)
		trunc << u8(0x04)
		trunc << u8(100 >> 8)
		trunc << u8(100 & 0xff)
		trunc << sig[..100]
		verify_certificate_verify(trunc, pk, content) or {
			assert err.msg().contains('verify')
			// Trailing bytes after a valid signature are rejected at parse.
			mut trailing := []u8{}
			trailing << u8(0x09)
			trailing << u8(0x04)
			trailing << u8(sig.len >> 8)
			trailing << u8(sig.len)
			trailing << sig
			trailing << u8(0)
			parse_cert_verify_sig(trailing) or {
				assert err.msg().contains('length')
				return
			}
			assert false, 'trailing CV bytes must fail'
		}
		assert false, 'truncated mldsa sig must fail'
	}
	assert false, 'empty mldsa sig must fail'
}

// test_sig_mldsa_cv_wrong_variant_length locks rejection of a valid 44
// signature framed as ML-DSA-65 against a genuine 65 key.
fn test_sig_mldsa_cv_wrong_variant_length() {
	priv := mldsa.PrivateKey.generate(.ml_dsa_44) or { panic('keygen') }
	priv65 := mldsa.PrivateKey.generate(.ml_dsa_65) or { panic('keygen65') }
	content := 'wrong variant'.bytes()
	sig := priv.sign(content, mldsa.SignerOpts{}) or { panic('sign') }
	mut body := []u8{}
	body << u8(0x09)
	body << u8(0x05)
	body << u8(sig.len >> 8)
	body << u8(sig.len)
	body << sig
	pk65 := x509.PublicKey(x509.MlDsa65PublicKey{ raw: priv65.public_key().bytes() })
	verify_certificate_verify(body, pk65, content) or {
		assert err.msg().contains('verify') || err.msg().contains('mismatch')
		return
	}
	assert false, '44-byte-length sig as 0905 must fail'
}

// test_sig_mldsa_chain_degenerate locks rejection of truncated chain DER and
// wrong-length parent keys at the chain layer.
fn test_sig_mldsa_chain_degenerate() {
	store := sigtest_store('mldsa44_ca_cert.pem')
	leaf_der, _ := sigtest_leaf('mldsa44_leaf_cert.pem')
	for r in store.roots {
		rpk := r.public_key() or { panic(err) }
		// Truncated child DER.
		verify_cert_signature(leaf_der[..leaf_der.len - 10], rpk) or {
			// Wrong-length parent raw (44 bytes presented as a 65 key).
			bad := mldsa.PublicKey.from_bytes(rpk_pubkey_bytes(rpk)[..64], .ml_dsa_65) or {
				return
			}
			_ = bad
			assert false, 'short parent key must not parse'
		}
		assert false, 'truncated mldsa chain must fail'
	}
}

// rpk_pubkey_bytes extracts raw key bytes from an ML-DSA public key.
fn rpk_pubkey_bytes(pk x509.PublicKey) []u8 {
	return match pk {
		x509.MlDsa44PublicKey { pk.raw }
		x509.MlDsa65PublicKey { pk.raw }
		x509.MlDsa87PublicKey { pk.raw }
		else { []u8{} }
	}
}

// test_sig_loopback_mldsa runs full verified handshakes against ML-DSA servers.
fn test_sig_loopback_mldsa() {
	td := os.join_path(@VEXEROOT, 'vlib/net/tls/testdata')
	for cs in mldsa_cases() {
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
		ca_der := sigtest_pem_block(sigtest_read(cs.ca), 'CERTIFICATE')
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
