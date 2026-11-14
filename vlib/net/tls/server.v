module tls

import crypto.ecdsa
import crypto.rand
import crypto.sha256
import crypto.sha512
import crypto.pem
import crypto.x509
import crypto.subtle
import x.crypto.curve25519
import x.crypto.mldsa
import x.crypto.mlkem
import crypto
import os
import time

// ServerKeyKind identifies the server credential type for CertificateVerify.
pub enum ServerKeyKind {
	ecdsa_p256
	ecdsa_p384
	ecdsa_p521
	rsa
	mldsa_44
	mldsa_65
	mldsa_87
}

// ServerHandshake holds state for a TLS 1.3 server handshake.
pub struct ServerHandshake {
pub mut:
	host           string
	pq_mode        PostQuantumMode
	suites         []CipherSuite
	suite          CipherSuite
	echd_priv      &curve25519.PrivateKey
	key_kind       ServerKeyKind
	ecdsa_priv     ecdsa.PrivateKey
	ecdsa_pub      ecdsa.PublicKey
	rsa_priv       RsaPrivateKey
	mldsa_priv     mldsa.PrivateKey
	cert_chain     [][]u8
	cv_scheme      u16
	client_random  []u8
	server_random  []u8
	has_mlkem      bool
	mlkem_ss       []u8
	handshake_keys HandshakeSecrets
	app_keys       ApplicationSecrets
	transcript     []u8
}

// new_server_handshake creates a server handshake with an ephemeral self-signed cert.
// new_server_handshake is a test/self-contained default; configure real certs via new_server_handshake_with_config.
pub fn new_server_handshake(pq_mode PostQuantumMode) !ServerHandshake {
	priv := curve25519.PrivateKey.new()!
	pb, pv := ecdsa.generate_key()!
	sr := crypto.rand.bytes(32)!
	now := time.now()
	_, cert_der := x509.generate_self_signed(x509.CertGenOptions{
		common_name: 'localhost'
		dns_names:   ['localhost']
		not_before:  now.add_days(-1)
		not_after:   now.add_days(365)
	}, pv)!
	mut h := ServerHandshake{
		pq_mode:       pq_mode
		echd_priv:     priv
		ecdsa_priv:    pv
		ecdsa_pub:     pb
		cert_chain:    [cert_der]
		server_random: sr
	}
	if pq_mode != .off {
		h.has_mlkem = true
	}
	return h
}

fn config_pem_bytes(inline []u8, value string, in_memory bool, label string) ![]u8 {
	if inline.len > 0 {
		return inline.clone()
	}
	if value.len == 0 {
		return error('tls: missing server ${label}')
	}
	if in_memory || value.contains('-----BEGIN ') {
		return value.bytes()
	}
	contents := os.read_file(value) or { return error('tls: cannot read server ${label}') }
	return contents.bytes()
}

// ecdsa_nid_for_scalar maps a private scalar length to its curve.
fn ecdsa_nid_for_scalar(scalar []u8) !ecdsa.Nid {
	return match scalar.len {
		32 { ecdsa.Nid.prime256v1 }
		48 { ecdsa.Nid.secp384r1 }
		66 { ecdsa.Nid.secp521r1 }
		else { return error('tls: unsupported EC private key size') }
	}
}

fn private_scalar_from_sec1(data []u8) ![]u8 {
	tag, content, consumed := der_tlv(data)!
	if tag != 0x30 || consumed != data.len {
		return error('tls: invalid EC private key')
	}
	mut off := 0
	vtag, _, vn := der_tlv(content[off..])!
	if vtag != 0x02 {
		return error('tls: invalid EC private key version')
	}
	off += vn
	ktag, scalar, _ := der_tlv(content[off..])!
	if ktag != 0x04 || (scalar.len != 32 && scalar.len != 48 && scalar.len != 66) {
		return error('tls: unsupported EC private key size')
	}
	return scalar
}

fn private_scalar_from_der(data []u8) ![]u8 {
	tag, content, consumed := der_tlv(data)!
	if tag != 0x30 || consumed != data.len {
		return error('tls: invalid private key DER')
	}
	mut off := 0
	vtag, _, vn := der_tlv(content[off..])!
	if vtag != 0x02 { return error('tls: invalid private key version') }
	off += vn
	next_tag, _, _ := der_tlv(content[off..])!
	if next_tag == 0x30 {
		// PKCS#8 PrivateKeyInfo: version, AlgorithmIdentifier, privateKey.
		_, _, alg_len := der_tlv(content[off..])!
		off += alg_len
		ktag, key_der, _ := der_tlv(content[off..])!
		if ktag != 0x04 { return error('tls: invalid PKCS#8 private key') }
		return private_scalar_from_sec1(key_der)
	}
	return private_scalar_from_sec1(data)
}

fn load_server_certificate_chain(cfg Config) ![][]u8 {
	data := config_pem_bytes(cfg.cert_pem, cfg.cert, cfg.in_memory, 'certificate')!
	mut rest := data.bytestr()
	mut certs := [][]u8{}
	for rest.len > 0 {
		block, next := pem.decode(rest) or { break }
		if block.block_type == 'CERTIFICATE' {
			_ := x509.parse_der(block.data) or { return error('tls: invalid server certificate') }
			certs << block.data.clone()
		}
		if next.len == rest.len { break }
		rest = next
	}
	if certs.len == 0 { return error('tls: no PEM certificate found') }
	return certs
}

fn new_server_handshake_with_config(pq_mode PostQuantumMode, cfg Config) !ServerHandshake {
	if cfg.cert.len == 0 && cfg.cert_pem.len == 0 && cfg.key.len == 0 && cfg.key_pem.len == 0 {
		return new_server_handshake(pq_mode)
	}
	if (cfg.cert.len == 0 && cfg.cert_pem.len == 0) || (cfg.key.len == 0 && cfg.key_pem.len == 0) {
		return error('tls: server certificate and private key must both be configured')
	}
	chain := load_server_certificate_chain(cfg)!
	key_data := config_pem_bytes(cfg.key_pem, cfg.key, cfg.in_memory, 'private key')!
	key_block, _ := pem.decode(key_data.bytestr()) or { return error('tls: invalid server private key PEM') }
	leaf := x509.parse_der(chain[0])!
	leaf_public := leaf.public_key() or { return error('tls: unsupported server certificate public key') }
	mut server := new_server_handshake(pq_mode)!
	server.suites = effective_cipher_suites(cfg)
	match leaf_public {
		x509.EcdsaP256PublicKey {
			if key_block.block_type !in ['EC PRIVATE KEY', 'PRIVATE KEY'] {
				return error('tls: ECDSA server key must be SEC1 or PKCS#8 PEM')
			}
			server.key_kind = .ecdsa_p256
			server.ecdsa_priv = load_ecdsa_key(key_block.data, 32)!
			server.ecdsa_pub = server.ecdsa_priv.public_key()!
			if subtle.constant_time_compare(server.ecdsa_pub.bytes()!, leaf_public.point) != 1 {
				return error('tls: server certificate and private key do not match')
			}
		}
		x509.EcdsaP384PublicKey {
			if key_block.block_type !in ['EC PRIVATE KEY', 'PRIVATE KEY'] {
				return error('tls: ECDSA server key must be SEC1 or PKCS#8 PEM')
			}
			server.key_kind = .ecdsa_p384
			server.ecdsa_priv = load_ecdsa_key(key_block.data, 48)!
			server.ecdsa_pub = server.ecdsa_priv.public_key()!
			if subtle.constant_time_compare(server.ecdsa_pub.bytes()!, leaf_public.point) != 1 {
				return error('tls: server certificate and private key do not match')
			}
		}
		x509.EcdsaP521PublicKey {
			if key_block.block_type !in ['EC PRIVATE KEY', 'PRIVATE KEY'] {
				return error('tls: ECDSA server key must be SEC1 or PKCS#8 PEM')
			}
			server.key_kind = .ecdsa_p521
			server.ecdsa_priv = load_ecdsa_key(key_block.data, 66)!
			server.ecdsa_pub = server.ecdsa_priv.public_key()!
			if subtle.constant_time_compare(server.ecdsa_pub.bytes()!, leaf_public.point) != 1 {
				return error('tls: server certificate and private key do not match')
			}
		}
		x509.RsaPublicKey {
			if key_block.block_type !in ['RSA PRIVATE KEY', 'PRIVATE KEY'] {
				return error('tls: RSA server key must be PKCS#1 or PKCS#8 PEM')
			}
			server.key_kind = .rsa
			server.rsa_priv = parse_rsa_private_key(key_block.data)!
			n, _ := parse_rsa_pubkey(leaf_public.der) or {
				return error('tls: invalid server certificate key')
			}
			if n != server.rsa_priv.n {
				return error('tls: server certificate and private key do not match')
			}
		}
		x509.MlDsa44PublicKey {
			if key_block.block_type != 'PRIVATE KEY' {
				return error('tls: ML-DSA server key must be PKCS#8 PEM')
			}
			server.key_kind = .mldsa_44
			server.mldsa_priv = parse_mldsa_private_key(key_block.data, .ml_dsa_44)!
			if subtle.constant_time_compare(server.mldsa_priv.public_key().bytes(), leaf_public.raw) != 1 {
				return error('tls: server certificate and private key do not match')
			}
		}
		x509.MlDsa65PublicKey {
			if key_block.block_type != 'PRIVATE KEY' {
				return error('tls: ML-DSA server key must be PKCS#8 PEM')
			}
			server.key_kind = .mldsa_65
			server.mldsa_priv = parse_mldsa_private_key(key_block.data, .ml_dsa_65)!
			if subtle.constant_time_compare(server.mldsa_priv.public_key().bytes(), leaf_public.raw) != 1 {
				return error('tls: server certificate and private key do not match')
			}
		}
		x509.MlDsa87PublicKey {
			if key_block.block_type != 'PRIVATE KEY' {
				return error('tls: ML-DSA server key must be PKCS#8 PEM')
			}
			server.key_kind = .mldsa_87
			server.mldsa_priv = parse_mldsa_private_key(key_block.data, .ml_dsa_87)!
			if subtle.constant_time_compare(server.mldsa_priv.public_key().bytes(), leaf_public.raw) != 1 {
				return error('tls: server certificate and private key do not match')
			}
		}
	}
	server.cert_chain = chain
	return server
}

// load_ecdsa_key loads a SEC1 or PKCS#8 EC private key with the expected size.
fn load_ecdsa_key(der []u8, size int) !ecdsa.PrivateKey {
	seed := private_scalar_from_der(der)!
	if seed.len != size {
		return error('tls: EC private key size does not match certificate curve')
	}
	return ecdsa.new_key_from_seed(seed, ecdsa.CurveOptions{
		nid: ecdsa_nid_for_scalar(seed)!
	})
}

// mldsa_alg_oid_matches reports whether an AlgorithmIdentifier value is
// exactly the OID for the expected ML-DSA variant with no parameters.
fn mldsa_alg_oid_matches(alg_val []u8, want mldsa.Kind) bool {
	return match want {
		.ml_dsa_44 {
			der_is_single_oid(alg_val, [u8(0x60), 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, 0x11])
		}
		.ml_dsa_65 {
			der_is_single_oid(alg_val, [u8(0x60), 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, 0x12])
		}
		.ml_dsa_87 {
			der_is_single_oid(alg_val, [u8(0x60), 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, 0x13])
		}
	}
}

// parse_mldsa_private_key loads an ML-DSA private key from PKCS#8 DER.
// OpenSSL encodes the privateKey OCTET STRING as SEQUENCE { OCTET STRING
// seed(32), OCTET STRING expanded }; the seed regenerates the key.
fn parse_mldsa_private_key(der []u8, want mldsa.Kind) !mldsa.PrivateKey {
	tag, content, consumed := der_tlv(der)!
	if tag != 0x30 || consumed != der.len {
		return error('tls: expected SEQUENCE for ML-DSA private key')
	}
	mut off := 0
	vtag, _, vn := der_tlv(content[off..])!
	if vtag != 0x02 {
		return error('tls: invalid PKCS#8 version')
	}
	off += vn
	atag, alg_val, an := der_tlv(content[off..])!
	if atag != 0x30 {
		return error('tls: invalid PKCS#8 algorithm')
	}
	if !mldsa_alg_oid_matches(alg_val, want) {
		return error('tls: ML-DSA key algorithm mismatch')
	}
	off += an
	ktag, inner, _ := der_tlv(content[off..])!
	if ktag != 0x04 {
		return error('tls: invalid PKCS#8 private key')
	}
	// Inner is usually SEQUENCE { seed, expanded }; accept a bare 32-byte
	// seed as well.
	if inner.len == 32 {
		return mldsa.PrivateKey.from_seed(inner, want)
	}
	itag, ival, _ := der_tlv(inner)!
	if itag != 0x30 {
		return error('tls: invalid ML-DSA private key encoding')
	}
	stag, seed, _ := der_tlv(ival)!
	if stag != 0x04 || seed.len != 32 {
		return error('tls: invalid ML-DSA seed')
	}
	return mldsa.PrivateKey.from_seed(seed, want)
}

// select_cv_scheme picks the CertificateVerify scheme for a server key.
// select_cv_scheme intersects the client's offer with the key type: ECDSA
// keys use their curve's scheme, ML-DSA keys use their variant's scheme,
// RSA prefers PSS over PKCS#1 v1.5.
pub fn select_cv_scheme(offered []u16, kind ServerKeyKind) !u16 {
	match kind {
		.ecdsa_p256 {
			if u16(0x0403) in offered {
				return u16(0x0403)
			}
		}
		.ecdsa_p384 {
			if u16(0x0503) in offered {
				return u16(0x0503)
			}
		}
		.ecdsa_p521 {
			if u16(0x0603) in offered {
				return u16(0x0603)
			}
		}
		.mldsa_44 {
			if u16(0x0904) in offered {
				return u16(0x0904)
			}
		}
		.mldsa_65 {
			if u16(0x0905) in offered {
				return u16(0x0905)
			}
		}
		.mldsa_87 {
			if u16(0x0906) in offered {
				return u16(0x0906)
			}
		}
		.rsa {
			for want in [u16(0x0804), 0x0805, 0x0806, 0x0401, 0x0501, 0x0601] {
				if want in offered {
					return want
				}
			}
		}
	}
	return error('tls: no supported signature scheme')
}

// process_client_hello ingests a ClientHello, negotiates parameters, returns the server flight.
// process_client_hello requires TLS 1.3 (supported_versions), a mutually
// supported cipher suite, a mutual key share, and a signature scheme the
// server key supports; only P-256 / hybrid key shares are accepted.
pub fn (mut s ServerHandshake) process_client_hello(ch_msg []u8) ![]u8 {
	msg, consumed := parse_handshake_message(ch_msg) or { return err }
	if consumed != ch_msg.len {
		return error('tls: trailing data after ClientHello')
	}
	if msg.typ != .client_hello {
		return error('tls: expected ClientHello')
	}
	ch := parse_client_hello(msg.body) or { return err }
	// Only TLS 1.3 is supported: the client must offer it via supported_versions
	// and share at least one implemented cipher suite. The server picks by
	// its own preference order (Config.cipher_suites, else the default);
	// parsed-but-unimplemented suites never match.
	server_prefs := if s.suites.len > 0 { s.suites.clone() } else { default_cipher_suites() }
	mut found_suite := false
	for want in server_prefs {
		if want in ch.cipher_suites && want in default_cipher_suites() {
			s.suite = want
			found_suite = true
			break
		}
	}
	if !found_suite {
		return error('tls: no supported cipher suite')
	}
	mut offered_tls13 := false
	for e in ch.extensions {
		if e.typ == ext_supported_versions {
			// Extension data is a length-prefixed vector of uint16 versions.
			if e.data.len < 1 {
				return error('tls: bad supported_versions')
			}
			vlen := int(e.data[0])
			if vlen + 1 != e.data.len || vlen % 2 != 0 {
				return error('tls: bad supported_versions')
			}
			mut vo := 1
			for vo + 1 < e.data.len {
				if (u16(e.data[vo]) << 8) | u16(e.data[vo + 1]) == supported_version_tls_1_3 {
					offered_tls13 = true
					break
				}
				vo += 2
			}
		}
	}
	if !offered_tls13 {
		return error('tls: TLS 1.3 not offered')
	}
	// Select the CertificateVerify scheme: the client's signature_algorithms
	// extension intersected with what the server key supports. ECDSA keys use
	// their curve's scheme, ML-DSA keys use their variant's scheme, RSA
	// prefers PSS, falling back to PKCS#1 v1.5.
	mut offered_sigs := []u16{}
	for e in ch.extensions {
		if e.typ == ext_signature_algorithms {
			if e.data.len < 1 {
				return error('tls: bad signature_algorithms')
			}
			slen := (int(e.data[0]) << 8) | int(e.data[1])
			if e.data.len < 2 || slen + 2 != e.data.len || slen % 2 != 0 {
				return error('tls: bad signature_algorithms')
			}
			mut so := 2
			for so + 1 < e.data.len {
				offered_sigs << (u16(e.data[so]) << 8) | u16(e.data[so + 1])
				so += 2
			}
		}
	}
	s.cv_scheme = select_cv_scheme(offered_sigs, s.key_kind) or { return err }
	s.client_random = ch.random.clone()
	s.transcript = ch_msg.clone()

	// find client key_share
	mut classical_pub := []u8{}
	mut hybrid_pub := []u8{}
	for e in ch.extensions {
		if e.typ == ext_key_share {
			if e.data.len < 2 {
				return error('tls: bad key_share')
			}
			entries_len := (int(e.data[0]) << 8) | int(e.data[1])
			if entries_len != e.data.len - 2 {
				return error('tls: invalid key_share vector length')
			}
			mut off := 2
			for off < e.data.len {
				if off + 4 > e.data.len { return error('tls: truncated key_share entry') }
				g := (u16(e.data[off]) << 8) | u16(e.data[off + 1])
				kl := (int(e.data[off + 2]) << 8) | int(e.data[off + 3])
				off += 4
				if off + kl > e.data.len { return error('tls: truncated key_share') }
				if g == named_group_x25519 { classical_pub = e.data[off..off + kl].clone() }
				if g == named_group_x25519_mlkem768 { hybrid_pub = e.data[off..off + kl].clone() }
				off += kl
			}
		}
	}
	mut client_pub := classical_pub
	mut client_group := named_group_x25519
	if s.has_mlkem && hybrid_pub.len > 0 {
		client_pub = hybrid_pub.clone()
		client_group = named_group_x25519_mlkem768
	} else if s.pq_mode == .hybrid_require {
		return error('tls: hybrid key exchange required')
	}
	if client_pub.len == 0 {
		return error('tls: missing client key_share')
	}

	mut shared := []u8{}
	mut srv_pub_bytes := []u8{}
	srv_pub := s.echd_priv.public_key()!
	x_pub_bytes := srv_pub.bytes()!

	if client_group == named_group_x25519_mlkem768 && s.has_mlkem {
		// hybrid
		if client_pub.len < 32 {
			return error('tls: bad hybrid key_share')
		}
		x_cpub := client_pub[..32].clone()
		ek_bytes := client_pub[32..].clone()
		x_shared := s.echd_priv.x25519(x_cpub) or { return err }
		// encapsulate to client's ek
		ct, ss := mlkem.encaps(mlkem.EncapsulationKey{ data: ek_bytes, variant: .ml_kem_768 }) or { return err }
		shared << x_shared
		shared << ss
		// server key_share = X25519 pub || ML-KEM ciphertext
		mut hybrid_key := []u8{}
		hybrid_key << x_pub_bytes
		hybrid_key << ct.data
		srv_pub_bytes << u8(named_group_x25519_mlkem768 >> 8)
		srv_pub_bytes << u8(named_group_x25519_mlkem768)
		srv_pub_bytes << u8(hybrid_key.len >> 8)
		srv_pub_bytes << u8(hybrid_key.len)
		srv_pub_bytes << hybrid_key
	} else {
		if s.pq_mode == .hybrid_require {
			return error('tls: hybrid key exchange required')
		}
		if client_group != named_group_x25519 {
			return error('tls: unsupported key_share group ${client_group}')
		}
		shared = s.echd_priv.x25519(client_pub) or { return err }
		mut entry := []u8{}
		entry << u8(named_group_x25519 >> 8)
		entry << u8(named_group_x25519)
		entry << u8(x_pub_bytes.len >> 8)
		entry << u8(x_pub_bytes.len)
		entry << x_pub_bytes
		srv_pub_bytes = entry.clone()
	}

	sh := ServerHello{
		legacy_version:     tls_version_1_3
		random:             s.server_random.clone()
		session_id:         []u8{}
		cipher_suite:       s.suite
		compression_method: 0
		extensions:         [
			Extension{
				typ:  ext_supported_versions
				data: [
					u8(supported_version_tls_1_3 >> 8),
					u8(supported_version_tls_1_3),
				]
			},
			Extension{ typ: ext_key_share, data: srv_pub_bytes },
		]
	}
	sh_body := encode_server_hello(sh)!
	sh_msg := encode_handshake_message(.server_hello, sh_body)!
	s.transcript << sh_msg

	es := derive_early_secret() or { return err }
	th := sha256.sum256(s.transcript)
	hs := derive_handshake_secrets(es, shared, th) or { return err }
	s.handshake_keys = hs

	ee_body := []u8{len: 2}
	ee_msg := encode_handshake_message(.encrypted_extensions, ee_body)!
	s.transcript << ee_msg

	mut cert_list := []u8{}
	for cert_der in s.cert_chain {
		if cert_der.len == 0 || cert_der.len > 0xffffff {
			return error('tls: server certificate size out of range')
		}
		cert_list << u8(cert_der.len >> 16)
		cert_list << u8(cert_der.len >> 8)
		cert_list << u8(cert_der.len)
		cert_list << cert_der
		cert_list << u8(0)
		cert_list << u8(0)
	}
	if cert_list.len > 0xffffff {
		return error('tls: server certificate chain too large')
	}
	mut cert_body := []u8{}
	cert_body << u8(0) // certificate_request_context
	cert_body << u8(cert_list.len >> 16)
	cert_body << u8(cert_list.len >> 8)
	cert_body << u8(cert_list.len)
	cert_body << cert_list
	cert_msg := encode_handshake_message(.certificate, cert_body)!
	s.transcript << cert_msg

	transcript_hash := sha256.sum256(s.transcript)
	content := cv_content(true, transcript_hash)
	sig := s.sign_certificate_verify(content) or { return err }
	mut cv_body := []u8{}
	cv_body << u8(s.cv_scheme >> 8)
	cv_body << u8(s.cv_scheme)
	cv_body << u8(sig.len >> 8)
	cv_body << u8(sig.len)
	cv_body << sig
	cv_msg := encode_handshake_message(.certificate_verify, cv_body)!
	s.transcript << cv_msg

	fin_vd := finished_verify_data(hs.server_secret, sha256.sum256(s.transcript)) or { return err }
	fin_msg := encode_handshake_message(.finished, encode_finished(Finished{ verify_data: fin_vd }))!
	s.transcript << fin_msg

	mut out := []u8{}
	out << sh_msg
	out << ee_msg
	out << cert_msg
	out << cv_msg
	out << fin_msg
	return out
}

// sign_certificate_verify signs CertificateVerify content with the server key.
// sign_certificate_verify dispatches on the negotiated scheme selected in
// process_client_hello: ECDSA signs directly, ML-DSA signs directly, RSA
// uses PKCS#1 v1.5 or PSS.
fn (s &ServerHandshake) sign_certificate_verify(content []u8) ![]u8 {
	match s.key_kind {
		.ecdsa_p256, .ecdsa_p384, .ecdsa_p521 {
			return s.ecdsa_priv.sign(content, ecdsa.SignerOpts{}) or { return err }
		}
		.mldsa_44, .mldsa_65, .mldsa_87 {
			return s.mldsa_priv.sign(content, mldsa.SignerOpts{}) or { return err }
		}
		.rsa {
			return match s.cv_scheme {
				u16(0x0401) {
					rsa_pkcs1_sign_digest(sha256.sum256(content), s.rsa_priv.d, s.rsa_priv.n, .sha256) or {
						return err
					}
				}
				u16(0x0501) {
					rsa_pkcs1_sign_digest(sha512.sum384(content), s.rsa_priv.d, s.rsa_priv.n, .sha384) or {
						return err
					}
				}
				u16(0x0601) {
					rsa_pkcs1_sign_digest(sha512.sum512(content), s.rsa_priv.d, s.rsa_priv.n, .sha512) or {
						return err
					}
				}
				u16(0x0804), u16(0x0805), u16(0x0806) {
					rsa_pss_sign(s.cv_scheme, content, s.rsa_priv.d, s.rsa_priv.n) or {
						return err
					}
				}
				else {
					return error('tls: no supported signature scheme')
				}
			}
		}
	}
}

// verify_client_finished verifies the client Finished against the handshake transcript.
// verify_client_finished derives the application secrets on success.
// verify_client_finished rejects trailing bytes after the Finished message.
pub fn (mut s ServerHandshake) verify_client_finished(fin_msg []u8) ! {
	parsed, n := parse_handshake_message(fin_msg) or { return err }
	if n != fin_msg.len {
		return error('tls: trailing data after Finished')
	}
	if parsed.typ != .finished {
		return error('tls: expected Finished')
	}
	th := sha256.sum256(s.transcript)
	expected := finished_verify_data(s.handshake_keys.client_secret, th) or { return err }
	fin := parse_finished(parsed.body, 32) or { return err }
	if subtle.constant_time_compare(fin.verify_data, expected) != 1 {
		return error('tls: client Finished mismatch')
	}
	as_ := derive_application_secrets(s.handshake_keys.handshake_secret, th) or { return err }
	s.app_keys = as_
	s.transcript << fin_msg
}

fn key_share_server_entry(pubkey []u8) []u8 {
	return key_share_server_entry_for(pubkey)
}

fn key_share_server_entry_for(pubkey []u8) []u8 {
	mut entry := []u8{}
	// server key_share is a single KeyShareEntry (no length prefix) for the hybrid?
	// Actually server's key_share extension contains a single KeyShareEntry.
	entry << pubkey // placeholder: caller must prepend group+len?
	_ = pubkey
	return entry
}
