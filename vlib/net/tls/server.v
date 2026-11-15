module tls

import crypto.ecdsa
import crypto.rand
import crypto.pem
import crypto.slh_dsa
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
	ed25519
	rsa
	mldsa_44
	mldsa_65
	mldsa_87
	slh_shake_128s
	slh_shake_128f
	slh_shake_192s
	slh_shake_192f
	slh_shake_256s
	slh_shake_256f
	slh_sha2_128s
	slh_sha2_128f
	slh_sha2_192s
	slh_sha2_192f
	slh_sha2_256s
	slh_sha2_256f
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
	slh_dsa_priv   slh_dsa.PrivateKey
	cert_chain     [][]u8
	cv_scheme      u16
	client_random  []u8
	server_random  []u8
	has_mlkem      bool
	mlkem_ss       []u8
	handshake_keys HandshakeSecrets
	app_keys       ApplicationSecrets
	transcript     []u8
	// ticket encryption key for stateless session tickets.
	ticket_key []u8
	// Rotation: extra ticket keys. [0] seals; all are tried on open.
	// Empty means single-key mode via ticket_key.
	ticket_keys [][]u8
	// SNI map + ALPN preference + external PSK + OCSP (from Config).
	cert_map        map[string]CertEntry
	alpn_protos     []string
	negotiated_alpn string
	psk_identity    []u8
	psk_key         []u8
	ocsp_response   []u8
	// HRR state: set when the first flight needed a retry.
	is_hrr         bool
	hrr_group      u16
	hrr_transcript []u8
	// Ephemeral ECDHE (P-256/P-384) and FFDHE/ML-KEM state per flight.
	ecdhe_nid      ecdsa.Nid
	ecdhe_priv     []u8
	ffdhe_priv     []u8
	ffdhe_group    u16
	mlkem_dk512    mlkem.DecapsulationKey
	mlkem_dk768    mlkem.DecapsulationKey
	mlkem_dk1024   mlkem.DecapsulationKey
	mlkem_variant  mlkem.MlKemVariant
	selected_group u16
	// Ed25519 server credential (set when the leaf is Ed25519; x509
	// support for the public half lives with the x509 agent).
	ed25519_priv []u8
	// status_request flag: client asked for OCSP.
	want_ocsp bool
	// PSK resumption state: set when the client offered a valid ticket.
	accepted_psk     bool
	accepted_psk_idx int
	early_secret_psk []u8
	// 0-RTT state (RFC 8446 §4.2.10, §7.5). offered_early_data mirrors the
	// ClientHello early_data extension; accepted_early_data is set only when
	// resumption was accepted and Config.max_early_data is non-zero, in
	// which case EncryptedExtensions carries early_data with the limit and
	// early_traffic decrypts the 0-RTT records before EndOfEarlyData.
	max_early_data      u32
	offered_early_data  bool
	accepted_early_data bool
	early_traffic       TrafficKeys
	has_early_traffic   bool
	// early_base_th captures the transcript hash through server Finished
	// before the client's EndOfEarlyData extends the transcript (same role
	// as auth_base_th for client auth): application secrets bind only the
	// flight through server Finished (RFC 8446 §7.1).
	early_base_th []u8
	// verify_client requests a client certificate (mTLS). Client certs are
	// verified against client_roots/system roots when verify_chain is set.
	verify_client bool
	verify_chain  bool
	client_roots  CertStore
	// auth_base_th is captured in verify_client_certificate before the
	// client auth messages are appended: application secrets bind the
	// transcript only through server Finished (RFC 8446 §7.1).
	auth_base_th []u8
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
		ticket_key:    crypto.rand.bytes(32)!
		verify_chain:  true
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
	if cfg.cert.len == 0 && cfg.cert_pem.len == 0 && cfg.key.len == 0 && cfg.key_pem.len == 0
		&& cfg.cert_map.len == 0 {
		// Ephemeral self-signed default (tests): still share the listener's
		// ticket key so resumption works across connections.
		mut server := new_server_handshake(pq_mode)!
		if cfg.ticket_key.len == 32 {
			server.ticket_key = cfg.ticket_key.clone()
		}
		server.ticket_keys = cfg.ticket_keys.clone()
		server.max_early_data = cfg.max_early_data
		server.verify_client = cfg.verify_client
		server.verify_chain = cfg.verify_chain
		server.client_roots = cfg.ca_store
		server.cert_map = cfg.cert_map.clone()
		server.alpn_protos = cfg.alpn_protocols.clone()
		server.psk_identity = cfg.psk_identity.clone()
		server.psk_key = cfg.psk_key.clone()
		server.ocsp_response = load_ocsp_bytes(cfg)!
		return server
	}
	if (cfg.cert.len == 0 && cfg.cert_pem.len == 0) || (cfg.key.len == 0 && cfg.key_pem.len == 0) {
		// SNI-only deployments still need the map even when no default
		// credential is configured; fail closed below if neither matches.
		if cfg.cert_map.len == 0 {
			return error('tls: server certificate and private key must both be configured')
		}
		mut server := new_server_handshake(pq_mode)!
		server.suites = effective_cipher_suites(cfg)
		server.ticket_keys = cfg.ticket_keys.clone()
		if cfg.ticket_key.len == 32 {
			server.ticket_key = cfg.ticket_key.clone()
		}
		server.max_early_data = cfg.max_early_data
		server.verify_client = cfg.verify_client
		server.verify_chain = cfg.verify_chain
		server.client_roots = cfg.ca_store
		server.cert_map = cfg.cert_map.clone()
		server.alpn_protos = cfg.alpn_protocols.clone()
		server.psk_identity = cfg.psk_identity.clone()
		server.psk_key = cfg.psk_key.clone()
		server.ocsp_response = load_ocsp_bytes(cfg)!
		server.cert_chain = [][]u8{}
		return server
	}
	chain := load_server_certificate_chain(cfg)!
	key_data := config_pem_bytes(cfg.key_pem, cfg.key, cfg.in_memory, 'private key')!
	key_block, _ := pem.decode(key_data.bytestr()) or { return error('tls: invalid server private key PEM') }
	leaf := x509.parse_der(chain[0])!
	leaf_public := leaf.public_key() or { return error('tls: unsupported server certificate public key') }
	mut server := new_server_handshake(pq_mode)!
	server.suites = effective_cipher_suites(cfg)
	keys := load_credential_keys(key_block, leaf_public)!
	server.key_kind = keys.kind
	server.rsa_priv = keys.rsa_priv
	server.mldsa_priv = keys.mldsa_priv
	server.slh_dsa_priv = keys.slh_dsa_priv
	server.ed25519_priv = keys.ed25519_priv.clone()
	if epriv := keys.ecdsa_priv {
		server.ecdsa_priv = epriv
		server.ecdsa_pub = epriv.public_key()!
	}
	server.cert_chain = chain
	if cfg.ticket_key.len == 32 {
		server.ticket_key = cfg.ticket_key.clone()
	}
	server.ticket_keys = cfg.ticket_keys.clone()
	server.max_early_data = cfg.max_early_data
	server.verify_client = cfg.verify_client
	server.verify_chain = cfg.verify_chain
	server.client_roots = cfg.ca_store
	server.cert_map = cfg.cert_map.clone()
	server.alpn_protos = cfg.alpn_protocols.clone()
	server.psk_identity = cfg.psk_identity.clone()
	server.psk_key = cfg.psk_key.clone()
	server.ocsp_response = load_ocsp_bytes(cfg)!
	return server
}

// load_ocsp_bytes loads the stapled OCSP response (opaque, never verified).
// Empty when unconfigured; accepts a file path or inline PEM/DER via
// ocsp_response, or raw bytes via ocsp_response_pem.
fn load_ocsp_bytes(cfg Config) ![]u8 {
	if cfg.ocsp_response_pem.len > 0 {
		return cfg.ocsp_response_pem.clone()
	}
	if cfg.ocsp_response.len == 0 {
		return []u8{}
	}
	if cfg.in_memory || cfg.ocsp_response.contains('-----BEGIN ') {
		return cfg.ocsp_response.bytes()
	}
	contents := os.read_file(cfg.ocsp_response) or { return error('tls: cannot read ocsp response') }
	return contents.bytes()
}

// ticket_seal_key returns the key used to seal new tickets ([0] when
// rotating, else ticket_key).
fn (s &ServerHandshake) ticket_seal_key() []u8 {
	if s.ticket_keys.len > 0 {
		return s.ticket_keys[0].clone()
	}
	return s.ticket_key.clone()
}

// ticket_open_keys returns every key tried when opening a ticket.
fn (s &ServerHandshake) ticket_open_keys() [][]u8 {
	if s.ticket_keys.len > 0 {
		return s.ticket_keys.clone()
	}
	if s.ticket_key.len == 32 {
		return [s.ticket_key.clone()]
	}
	return [][]u8{}
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

// slh_dsa_alg_oid_matches reports whether an AlgorithmIdentifier value is
// exactly the OID for the expected SLH-DSA parameter set with no parameters.
fn slh_dsa_alg_oid_matches(alg_val []u8, want slh_dsa.ParameterSet) bool {
	return der_is_single_oid(alg_val, slh_dsa_sigalg_content(want))
}

// parse_slh_dsa_private_key loads an SLH-DSA private key from PKCS#8 DER.
// OpenSSL encodes the privateKey OCTET STRING as the raw 4n-byte private
// key (SK.seed || SK.prf || PK.seed || PK.root); there is no seed-only
// form, so only the full-length encoding is accepted.
fn parse_slh_dsa_private_key(der []u8, want slh_dsa.ParameterSet) !slh_dsa.PrivateKey {
	p := slh_dsa.get_params(want)
	tag, content, consumed := der_tlv(der)!
	if tag != 0x30 || consumed != der.len {
		return error('tls: expected SEQUENCE for SLH-DSA private key')
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
	if !slh_dsa_alg_oid_matches(alg_val, want) {
		return error('tls: SLH-DSA key algorithm mismatch')
	}
	off += an
	ktag, inner, _ := der_tlv(content[off..])!
	if ktag != 0x04 {
		return error('tls: invalid PKCS#8 private key')
	}
	if inner.len != p.sk_len() {
		return error('tls: invalid SLH-DSA private key length')
	}
	return slh_dsa.private_key_from_bytes(want, inner)
}

// select_cv_scheme picks the CertificateVerify scheme for a server key.
// select_cv_scheme intersects the client's offer with the key type: ECDSA
// keys use their curve's scheme, ML-DSA keys use their variant's scheme,
// SLH-DSA keys use their parameter set's scheme, RSA prefers PSS over
// PKCS#1 v1.5.
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
		.slh_shake_128s {
			if u16(0x0917) in offered {
				return u16(0x0917)
			}
		}
		.slh_shake_128f {
			if u16(0x0918) in offered {
				return u16(0x0918)
			}
		}
		.slh_shake_192s {
			if u16(0x0919) in offered {
				return u16(0x0919)
			}
		}
		.slh_shake_192f {
			if u16(0x091a) in offered {
				return u16(0x091a)
			}
		}
		.slh_shake_256s {
			if u16(0x091b) in offered {
				return u16(0x091b)
			}
		}
		.slh_shake_256f {
			if u16(0x091c) in offered {
				return u16(0x091c)
			}
		}
		.slh_sha2_128s {
			if u16(0x0911) in offered {
				return u16(0x0911)
			}
		}
		.slh_sha2_128f {
			if u16(0x0912) in offered {
				return u16(0x0912)
			}
		}
		.slh_sha2_192s {
			if u16(0x0913) in offered {
				return u16(0x0913)
			}
		}
		.slh_sha2_192f {
			if u16(0x0914) in offered {
				return u16(0x0914)
			}
		}
		.slh_sha2_256s {
			if u16(0x0915) in offered {
				return u16(0x0915)
			}
		}
		.slh_sha2_256f {
			if u16(0x0916) in offered {
				return u16(0x0916)
			}
		}
		.rsa {
			for want in [u16(0x0804), 0x0805, 0x0806, 0x0401, 0x0501, 0x0601] {
				if want in offered {
					return want
				}
			}
		}
		.ed25519 {
			// RFC 8446 §4.2.3, scheme 0x0807. Offered by OpenSSL 4.1.0
			// clients (ClientHello capture) and by this stack;VERIFY
			// side lives in cert_verify_sig.v (separate owner) — see note.
			if u16(0x0807) in offered {
				return u16(0x0807)
			}
		}
	}
	return error('tls: no supported signature scheme')
}

// select_sni_credential switches the handshake credential when the
// ClientHello server_name matches cert_map. No match keeps the default
// credential; an SNI-only server (no default chain) with no match aborts.
fn (mut s ServerHandshake) select_sni_credential(ch ClientHello) ! {
	mut hostname := ''
	for e in ch.extensions {
		if e.typ == ext_server_name {
			name := parse_sni_hostname(e.data) or { continue }
			if name.len > 0 {
				hostname = name
				break
			}
		}
	}
	if hostname.len == 0 || hostname !in s.cert_map {
		if s.cert_chain.len == 0 && s.cert_map.len > 0 {
			return error('tls: no certificate for server name')
		}
		return
	}
	entry := s.cert_map[hostname]
	mut cert_data := entry.cert_pem.clone()
	if cert_data.len == 0 {
		if entry.cert.contains('-----BEGIN ') {
			cert_data = entry.cert.bytes()
		} else if entry.cert.len > 0 {
			contents := os.read_file(entry.cert) or {
				return error('tls: cannot read server certificate')
			}
			cert_data = contents.bytes()
		}
	}
	mut key_data := entry.key_pem.clone()
	if key_data.len == 0 {
		if entry.key.contains('-----BEGIN ') {
			key_data = entry.key.bytes()
		} else if entry.key.len > 0 {
			contents := os.read_file(entry.key) or {
				return error('tls: cannot read server private key')
			}
			contents2 := contents
			key_data = contents2.bytes()
		}
	}
	if cert_data.len == 0 || key_data.len == 0 {
		return error('tls: incomplete SNI credential')
	}
	mut rest := cert_data.bytestr()
	mut chain := [][]u8{}
	for rest.len > 0 {
		block, next := pem.decode(rest) or { break }
		if block.block_type == 'CERTIFICATE' {
			_ := x509.parse_der(block.data) or { return error('tls: invalid server certificate') }
			chain << block.data.clone()
		}
		if next.len == rest.len {
			break
		}
		rest = next
	}
	if chain.len == 0 {
		return error('tls: no PEM certificate found')
	}
	key_block, _ := pem.decode(key_data.bytestr()) or {
		return error('tls: invalid server private key PEM')
	}
	leaf := x509.parse_der(chain[0])!
	leaf_public := leaf.public_key() or { return error('tls: unsupported server certificate public key') }
	keys := load_credential_keys(key_block, leaf_public)!
	s.key_kind = keys.kind
	s.rsa_priv = keys.rsa_priv
	s.mldsa_priv = keys.mldsa_priv
	s.slh_dsa_priv = keys.slh_dsa_priv
	s.ed25519_priv = keys.ed25519_priv.clone()
	if epriv := keys.ecdsa_priv {
		s.ecdsa_priv = epriv
		s.ecdsa_pub = epriv.public_key()!
	}
	s.cert_chain = chain
}

// server_group_prefs lists the server's key-share groups in preference
// order. X25519 stays first so defaults negotiate exactly as before;
// hybrid_require restricts to the hybrid group.
fn (s &ServerHandshake) server_group_prefs() []u16 {
	if s.pq_mode == .hybrid_require {
		return [named_group_x25519_mlkem768]
	}
	return [
		named_group_x25519,
		named_group_secp256r1,
		named_group_secp384r1,
		named_group_ffdhe2048,
		named_group_ffdhe3072,
		named_group_x25519_mlkem768,
		named_group_mlkem768,
		named_group_mlkem1024,
	]
}

// hrr_selected_group encodes a HelloRetryRequest selected_group value
// (the key_share extension carrying just the group, RFC 8446 §4.1.2).
fn hrr_selected_group(group u16) []u8 {
	return [u8(group >> 8), u8(group)]
}

// mlkem_variant_for_group maps a pure ML-KEM group to its variant.
fn mlkem_variant_for_group(group u16) !mlkem.MlKemVariant {
	if group == named_group_mlkem512 {
		return .ml_kem_512
	}
	if group == named_group_mlkem768 {
		return .ml_kem_768
	}
	if group == named_group_mlkem1024 {
		return .ml_kem_1024
	}
	return error('tls: not a pure ML-KEM group')
}

// do_server_share generates the server share for group and derives the
// (EC)DHE shared secret. Returns (shared_secret, key_share_entry_bytes
// for the ServerHello extension, without the outer extension header).
fn (mut s ServerHandshake) do_server_share(group u16, client_pub []u8) !([]u8, []u8) {
	if group == named_group_x25519 {
		if client_pub.len != 32 {
			return error('tls: bad X25519 key_share')
		}
		shared := s.echd_priv.x25519(client_pub)!
		srv_pub := s.echd_priv.public_key()!
		xb := srv_pub.bytes()!
		mut entry := []u8{}
		entry << u8(group >> 8)
		entry << u8(group)
		entry << u8(xb.len >> 8)
		entry << u8(xb.len)
		entry << xb
		return shared, entry
	}
	if group == named_group_secp256r1 || group == named_group_secp384r1 {
		nid := if group == named_group_secp256r1 {
			ecdsa.Nid.prime256v1
		} else {
			ecdsa.Nid.secp384r1
		}
		want_len := if group == named_group_secp256r1 { 65 } else { 97 }
		if client_pub.len != want_len {
			return error('tls: bad ECDHE key_share length')
		}
		_, epriv := ecdsa.generate_key(ecdsa.CurveOptions{ nid: nid })!
		epub := epriv.public_key()!
		pub_bytes := epub.bytes()!
		peer := ecdsa.PublicKey.from_uncompressed_bytes(client_pub, ecdsa.CurveOptions{
			nid: nid
		})!
		shared := epriv.derive_shared_secret(peer)!
		mut entry := []u8{}
		entry << u8(group >> 8)
		entry << u8(group)
		entry << u8(pub_bytes.len >> 8)
		entry << u8(pub_bytes.len)
		entry << pub_bytes
		return shared, entry
	}
	if group == named_group_ffdhe2048 || group == named_group_ffdhe3072 {
		if client_pub.len == 0 {
			return error('tls: bad FFDHE key_share')
		}
		priv, pub_bytes := ffdhe_generate(group)!
		shared := ffdhe_compute(priv, client_pub, group)!
		mut entry := []u8{}
		entry << u8(group >> 8)
		entry << u8(group)
		entry << u8(pub_bytes.len >> 8)
		entry << u8(pub_bytes.len)
		entry << pub_bytes
		return shared, entry
	}
	if group == named_group_x25519_mlkem768 {
		if !s.has_mlkem {
			return error('tls: hybrid key exchange unavailable')
		}
		if client_pub.len < 32 {
			return error('tls: bad hybrid key_share')
		}
		srv_pub := s.echd_priv.public_key()!
		xb := srv_pub.bytes()!
		x_cpub := client_pub[..32].clone()
		ek_bytes := client_pub[32..].clone()
		x_shared := s.echd_priv.x25519(x_cpub)!
		ct, ss := mlkem.encaps(mlkem.EncapsulationKey{ data: ek_bytes, variant: .ml_kem_768 })!
		mut shared := []u8{}
		shared << x_shared
		shared << ss
		mut hybrid_key := []u8{}
		hybrid_key << xb
		hybrid_key << ct.data
		mut entry := []u8{}
		entry << u8(group >> 8)
		entry << u8(group)
		entry << u8(hybrid_key.len >> 8)
		entry << u8(hybrid_key.len)
		entry << hybrid_key
		return shared, entry
	}
	if group == named_group_mlkem512 || group == named_group_mlkem768
		|| group == named_group_mlkem1024 {
		variant := mlkem_variant_for_group(group)!
		want := mlkem.params_of(variant).ek_size
		if client_pub.len != want {
			return error('tls: bad ML-KEM key_share length')
		}
		ct, ss := mlkem.encaps(mlkem.EncapsulationKey{ data: client_pub.clone(), variant: variant })!
		mut entry := []u8{}
		entry << u8(group >> 8)
		entry << u8(group)
		entry << u8(ct.data.len >> 8)
		entry << u8(ct.data.len)
		entry << ct.data
		return ss, entry
	}
	return error('tls: unsupported key_share group ${group}')
}

// process_client_hello ingests a ClientHello, negotiates parameters, returns the server flight.
// process_client_hello requires TLS 1.3 (supported_versions), a mutually
// supported cipher suite, a mutual key share (X25519, P-256/P-384 ECDHE,
// FFDHE 2048/3072, pure ML-KEM-768/1024 or hybrid), and a signature scheme
// the server key supports. With no mutual share it returns a cookie-less
// HelloRetryRequest for its most-preferred group (message_hash transcript);
// the caller must feed the second ClientHello back into this function.
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
	// PSK evaluation (RFC 8446 §4.2.11): pre_shared_key must be the last
	// extension, paired with psk_key_exchange_modes. A valid ticket moves
	// the handshake to PSK authentication (no server certificate flight).
	mut psk_data := []u8{}
	mut psk_modes_ok := false
	mut has_psk_ext := false
	for i, e in ch.extensions {
		if e.typ == ext_psk_key_exchange_modes {
			psk_modes_ok = parse_psk_modes(e.data)
		}
		if e.typ == ext_pre_shared_key {
			if i != ch.extensions.len - 1 {
				return error('tls: pre_shared_key must be last')
			}
			has_psk_ext = true
			psk_data = e.data.clone()
		}
	}
	if has_psk_ext && psk_modes_ok
		&& (s.ticket_key.len == 32 || s.ticket_keys.len > 0 || (s.psk_identity.len > 0 && s.psk_key.len > 0)) {
		ok, es_psk := s.try_accept_psk(ch_msg, psk_data)
		if ok {
			s.accepted_psk = true
			s.early_secret_psk = es_psk
		}
	}
	// 0-RTT: note the early_data offer; acceptance is decided below once the
	// PSK outcome is known (replay check included).
	s.offered_early_data = ch_offers_early_data(ch)
	if s.accepted_psk && s.offered_early_data {
		// Derive the 0-RTT keys from the full ClientHello (with binders),
		// the same transcript the client used — including the HRR prefix
		// on a retried flight (RFC 8446 §7.1).
		mut early_input := ch_msg.clone()
		if s.is_hrr {
			mut prefixed := s.hrr_transcript.clone()
			prefixed << ch_msg
			early_input = prefixed.clone()
		}
		ch_hash := transcript_hash_for_suite(early_input, s.suite)
		early_secret := derive_client_early_traffic_secret(s.early_secret_psk, ch_hash,
			s.suite) or { return err }
		s.early_traffic = derive_traffic_keys(early_secret, s.suite) or { return err }
		s.has_early_traffic = true
		if s.max_early_data > 0 {
			s.accepted_early_data = true
		}
	}
	// SNI: select a mapped credential when the ClientHello names one.
	// No match falls back to the default credential (fail closed, no
	// downgrade); an SNI-only server with no match aborts.
	s.select_sni_credential(ch) or { return err }
	// ALPN: remember the client offer; the selection is encoded in EE.
	mut alpn_offer := []u8{}
	for e in ch.extensions {
		if e.typ == ext_alpn {
			alpn_offer = e.data.clone()
			break
		}
	}
	s.negotiated_alpn = select_alpn(s.alpn_protos, alpn_offer)
	// OCSP: note the client's status_request for the CertificateStatus flight.
	s.want_ocsp = offers_status_request(ch.extensions) && s.ocsp_response.len > 0
	// Select the CertificateVerify scheme: the client's signature_algorithms
	// extension intersected with what the server key supports. ECDSA keys use
	// their curve's scheme, ML-DSA keys use their variant's scheme, RSA
	// prefers PSS, falling back to PKCS#1 v1.5.
	mut offered_sigs := []u16{}
	for e in ch.extensions {
		if e.typ == ext_signature_algorithms {
			if e.data.len < 2 {
				return error('tls: bad signature_algorithms')
			}
			slen := (int(e.data[0]) << 8) | int(e.data[1])
			if slen + 2 != e.data.len || slen % 2 != 0 {
				return error('tls: bad signature_algorithms')
			}
			mut so := 2
			for so + 1 < e.data.len {
				offered_sigs << (u16(e.data[so]) << 8) | u16(e.data[so + 1])
				so += 2
			}
		}
	}
	if !s.accepted_psk {
		s.cv_scheme = select_cv_scheme(offered_sigs, s.key_kind) or { return err }
	}
	// Key shares: parse every entry, then select the server's most
	// preferred mutual group. With no mutual share the server answers
	// HelloRetryRequest (cookie-less, message_hash transcript) for its
	// most-preferred group instead of failing.
	mut share_by_group := map[u16][]u8{}
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
				if off + 4 > e.data.len {
					return error('tls: truncated key_share entry')
				}
				g := (u16(e.data[off]) << 8) | u16(e.data[off + 1])
				kl := (int(e.data[off + 2]) << 8) | int(e.data[off + 3])
				off += 4
				if off + kl > e.data.len {
					return error('tls: truncated key_share')
				}
				if g !in share_by_group {
					share_by_group[g] = e.data[off..off + kl].clone()
				}
				off += kl
			}
		}
	}
	// Second flight after HRR: the transcript prefix is already stored
	// and the client must offer the requested group.
	if s.is_hrr {
		if s.hrr_group !in share_by_group {
			return error('tls: missing HRR key_share')
		}
		s.transcript << ch_msg
	} else {
		s.client_random = ch.random.clone()
		s.transcript = ch_msg.clone()
	}
	prefs := s.server_group_prefs()
	mut client_group := u16(0)
	for g in prefs {
		if g in share_by_group {
			client_group = g
			break
		}
	}
	if client_group == u16(0) {
		// No mutual share: HelloRetryRequest for the top preference.
		want := prefs[0]
		sh_exts_hrr := [
			Extension{
				typ:  ext_supported_versions
				data: [
					u8(supported_version_tls_1_3 >> 8),
					u8(supported_version_tls_1_3),
				]
			},
			Extension{ typ: ext_key_share, data: hrr_selected_group(want) },
		]
		hrr_sh := ServerHello{
			legacy_version:     tls_version_1_3
			random:             hrr_random()
			session_id:         ch.session_id.clone()
			cipher_suite:       s.suite
			compression_method: 0
			extensions:         sh_exts_hrr
		}
		hrr_body := encode_server_hello(hrr_sh)!
		hrr_msg := encode_handshake_message(.server_hello, hrr_body)!
		// Transcript becomes message_hash(CH1) || HRR (RFC 8446 §4.1.2).
		ch_hash := transcript_hash_for_suite(ch_msg, s.suite)
		prefix := encode_handshake_message(.message_hash, ch_hash)!
		s.transcript = prefix.clone()
		s.transcript << hrr_msg
		s.is_hrr = true
		s.hrr_group = want
		s.hrr_transcript = s.transcript.clone()
		return hrr_msg
	}
	if s.is_hrr && client_group != s.hrr_group {
		return error('tls: wrong HRR key_share group')
	}
	s.selected_group = client_group
	client_pub := share_by_group[client_group].clone()

	shared, srv_share_entry := s.do_server_share(client_group, client_pub) or { return err }
	srv_pub_bytes := srv_share_entry

	mut sh_exts := [
		Extension{
			typ:  ext_supported_versions
			data: [
				u8(supported_version_tls_1_3 >> 8),
				u8(supported_version_tls_1_3),
			]
		},
		Extension{ typ: ext_key_share, data: srv_pub_bytes },
	]
	if s.accepted_psk {
		sh_exts << Extension{ typ: ext_pre_shared_key, data: encode_psk_selected_idx(s.accepted_psk_idx) }
	}
	sh := ServerHello{
		legacy_version:     tls_version_1_3
		random:             s.server_random.clone()
		session_id:         ch.session_id.clone()
		cipher_suite:       s.suite
		compression_method: 0
		extensions:         sh_exts
	}
	sh_body := encode_server_hello(sh)!
	sh_msg := encode_handshake_message(.server_hello, sh_body)!
	s.transcript << sh_msg

	mut es := derive_early_secret_for_suite(s.suite) or { return err }
	if s.accepted_psk {
		es = s.early_secret_psk.clone()
	}
	th := transcript_hash_for_suite(s.transcript, s.suite)
	hs := derive_handshake_secrets_for_suite(es, shared, th, s.suite) or { return err }
	s.handshake_keys = hs

	ee_body := if s.accepted_early_data || s.negotiated_alpn.len > 0 {
		mut ee_exts := []u8{}
		if s.accepted_early_data {
			size_data := encode_early_data_size(s.max_early_data)
			ee_exts << u8(ext_early_data >> 8)
			ee_exts << u8(ext_early_data)
			ee_exts << u8(size_data.len >> 8)
			ee_exts << u8(size_data.len)
			ee_exts << size_data
		}
		if s.negotiated_alpn.len > 0 {
			alpn_ext := encode_alpn([s.negotiated_alpn]) or { return err }
			// encode_alpn returns a full extension; strip its 4-byte header
			// to embed the body in the EE extension list.
			if alpn_ext.len < 4 {
				return error('tls: bad ALPN encoding')
			}
			alpn_body := alpn_ext[4..].clone()
			ee_exts << u8(ext_alpn >> 8)
			ee_exts << u8(ext_alpn)
			ee_exts << u8(alpn_body.len >> 8)
			ee_exts << u8(alpn_body.len)
			ee_exts << alpn_body
		}
		mut body := []u8{}
		body << u8(ee_exts.len >> 8)
		body << u8(ee_exts.len)
		body << ee_exts
		body
	} else {
		[]u8{len: 2}
	}
	ee_msg := encode_handshake_message(.encrypted_extensions, ee_body)!
	s.transcript << ee_msg

	// mTLS: request a client certificate (never on a resumed handshake).
	cr_msg := if s.verify_client && !s.accepted_psk {
		cr_body := encode_certificate_request(signature_algorithms_data())!
		m := encode_handshake_message(.certificate_request, cr_body)!
		s.transcript << m
		m
	} else {
		[]u8{}
	}

	mut out := []u8{}
	out << sh_msg
	out << ee_msg
	if cr_msg.len > 0 {
		out << cr_msg
	}
	if s.accepted_psk {
		// PSK authentication replaces the certificate flight.
		fin_vd := finished_verify_data_for_suite(hs.server_secret, transcript_hash_for_suite(s.transcript,
			s.suite), s.suite) or { return err }
		fin_msg := encode_handshake_message(.finished, encode_finished(Finished{
			verify_data: fin_vd
		}))!
		s.transcript << fin_msg
		out << fin_msg
		return out
	}

	cert_msg := encode_handshake_message(.certificate, encode_certificate_body([]u8{},
		s.cert_chain)!)!
	s.transcript << cert_msg

	// OCSP staple (transport only, never verified here): a
	// CertificateStatus message carrying the opaque response, sent only
	// when the client offered status_request and one is configured. It
	// sits between Certificate and CertificateVerify so the transcript
	// (and CV signature) covers it.
	mut cs_msg := []u8{}
	if s.want_ocsp && s.ocsp_response.len > 0 {
		cs_msg = encode_handshake_message(.certificate_status, s.ocsp_response.clone())!
		s.transcript << cs_msg
	}

	transcript_hash := transcript_hash_for_suite(s.transcript, s.suite)
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

	fin_vd := finished_verify_data_for_suite(hs.server_secret, transcript_hash_for_suite(s.transcript,
		s.suite), s.suite) or { return err }
	fin_msg := encode_handshake_message(.finished, encode_finished(Finished{ verify_data: fin_vd }))!
	s.transcript << fin_msg

	out = []u8{}
	out << sh_msg
	out << ee_msg
	if cr_msg.len > 0 {
		out << cr_msg
	}
	out << cert_msg
	// OCSP staple (transport only): CertificateStatus between Certificate
	// and CertificateVerify, covered by the transcript/CV signature.
	if cs_msg.len > 0 {
		out << cs_msg
	}
	out << cv_msg
	out << fin_msg
	return out
}

// try_accept_psk evaluates a PSK offer against the server ticket keys and
// the configured external PSK. It returns the PSK early secret on success;
// any failure returns false so the handshake falls back to certificates.
// ch_msg is the raw ClientHello wire bytes: binders cover the raw prefix.
// Supports one or two identities (ticket resumption and/or external PSK);
// each identity is tried in order with its own binder.
// try_accept_psk evaluates a PSK offer against the server ticket keys and
// the configured external PSK. It returns the PSK early secret on success;
// any failure returns false so the handshake falls back to certificates.
// ch_msg is the raw ClientHello wire bytes: binders cover the raw prefix.
// Supports one or two identities (ticket resumption and/or external PSK);
// each identity is tried in order with its own binder.
fn (mut s ServerHandshake) try_accept_psk(ch_msg []u8, psk_data []u8) (bool, []u8) {
	ids, binders := parse_psk_offer(psk_data) or { return false, []u8{} }
	if ids.len < 1 || ids.len > 2 || binders.len != ids.len {
		return false, []u8{}
	}
	// External PSK identity match (constant-time) is evaluated per
	// identity alongside ticket open attempts.
	for idx in 0 .. ids.len {
		id := ids[idx]
		// External PSK: identity bytes match and binder verifies under
		// the configured key with the negotiated suite hash.
		if s.psk_identity.len > 0 && s.psk_key.len > 0 {
			if id.ticket.len == s.psk_identity.len
				&& subtle.constant_time_compare(id.ticket, s.psk_identity) == 1 {
				es_psk := derive_early_secret_with_psk(s.psk_key, s.suite) or { continue }
				bk := binder_key_for(es_psk, s.suite) or { continue }
				trunc := binder_prefix(ch_msg, psk_data) or { continue }
				// After HRR the binder covers message_hash(CH1) || HRR
				// plus the truncated second flight (RFC 8446 §4.2.11.2).
				mut th_input := trunc.clone()
				if s.is_hrr {
					mut prefixed := s.hrr_transcript.clone()
					prefixed << trunc
					th_input = prefixed.clone()
				}
				th := transcript_hash_for_suite(th_input, s.suite)
				if verify_binder(bk, th, binders[idx], s.suite) {
					s.accepted_psk_idx = idx
					return true, es_psk
				}
				continue
			}
		}
		// Ticket resumption: try every ticket key in order.
		for tkey in s.ticket_open_keys() {
			master, tnonce, tsuite, created, tage := open_ticket(id.ticket, tkey) or {
				continue
			}
			if tsuite != s.suite {
				continue
			}
			if is_ticket_used(tnonce) {
				continue
			}
			now := time.now().unix()
			if now < created - 10 || now - created > i64(ticket_lifetime) + 10 {
				continue
			}
			psk := derive_psk(master, tnonce, tsuite) or { continue }
			es_psk := derive_early_secret_with_psk(psk, tsuite) or { continue }
			bk := binder_key_for(es_psk, tsuite) or { continue }
			trunc := binder_prefix(ch_msg, psk_data) or { continue }
			// Same HRR prefix rule as the external-PSK arm above.
			mut th_input := trunc.clone()
			if s.is_hrr {
				mut prefixed := s.hrr_transcript.clone()
				prefixed << trunc
				th_input = prefixed.clone()
			}
			th := transcript_hash_for_suite(th_input, tsuite)
			if !verify_binder(bk, th, binders[idx], tsuite) {
				continue
			}
			expected_obf := u32((now - created) * 1000) + tage
			d := expected_obf - id.obfuscated_age
			if d > 10000 && d < u32(0xffffffff) - 10000 {
				continue
			}
			mark_ticket_used(tnonce)
			s.accepted_psk_idx = idx
			return true, es_psk
		}
	}
	return false, []u8{}
}

// sign_certificate_verify signs CertificateVerify content with the server key.
// sign_certificate_verify dispatches on the negotiated scheme selected in
// process_client_hello (see sign_cv_with).
fn (s &ServerHandshake) sign_certificate_verify(content []u8) ![]u8 {
	return sign_cv_with_ed(s.key_kind, s.cv_scheme, s.ecdsa_priv, s.rsa_priv, s.mldsa_priv,
		s.slh_dsa_priv, s.ed25519_priv, content)
}

// verify_client_finished verifies the client Finished against the handshake transcript.
// verify_client_finished derives the application secrets on success (bound
// through server Finished; see auth_base_th).
// verify_client_finished rejects trailing bytes after the Finished message.
pub fn (mut s ServerHandshake) verify_client_finished(fin_msg []u8) ! {
	parsed, n := parse_handshake_message(fin_msg) or { return err }
	if n != fin_msg.len {
		return error('tls: trailing data after Finished')
	}
	if parsed.typ != .finished {
		return error('tls: expected Finished')
	}
	th := transcript_hash_for_suite(s.transcript, s.suite)
	expected := finished_verify_data_for_suite(s.handshake_keys.client_secret, th,
		s.suite) or { return err }
	fin := parse_finished(parsed.body, cipher_suite_hash_len(s.suite)) or { return err }
	if subtle.constant_time_compare(fin.verify_data, expected) != 1 {
		return error('tls: client Finished mismatch')
	}
	// Application secrets bind the transcript only through server Finished
	// (RFC 8446 §7.1); with client auth the transcript has grown past that,
	// and with accepted 0-RTT the client's EndOfEarlyData sits between.
	app_th := if s.auth_base_th.len > 0 {
		s.auth_base_th
	} else if s.early_base_th.len > 0 {
		s.early_base_th
	} else {
		th
	}
	as_ := derive_application_secrets_for_suite(s.handshake_keys.handshake_secret, app_th,
		s.suite) or { return err }
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
