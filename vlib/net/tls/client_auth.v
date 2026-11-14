module tls

import crypto.ecdsa
import crypto.pem
import crypto.sha256
import crypto.sha512
import crypto.subtle
import crypto.x509
import time
import x.crypto.mldsa

// Mutual TLS (RFC 8446 §4.3.2, §4.4.2).
//
// When Config.verify_client is set, the server sends a CertificateRequest
// after EncryptedExtensions (never on a resumed/PSK handshake) and the
// client answers with its Certificate + CertificateVerify before its
// Finished. The client credential comes from Config.client_cert/client_key
// (PEM bytes or file paths, same convention as the server cert/key). A
// client with no credential sends an empty Certificate, which the server
// rejects when verify_client is set. Client certificates are verified
// against ca_store/system roots when verify_chain is set; no hostname
// check applies to client certificates.

// CredentialKeys is a loaded private key of any supported kind, shared by
// the server credential and the mTLS client credential. The EC key is
// optional so key kinds without one can still be constructed (V requires
// reference fields initialised in every struct literal).
pub struct CredentialKeys {
pub mut:
	kind       ServerKeyKind
	ecdsa_priv ?ecdsa.PrivateKey
	rsa_priv   RsaPrivateKey
	mldsa_priv mldsa.PrivateKey
}

// ClientCreds is the mTLS client credential: chain + private key.
pub struct ClientCreds {
pub mut:
	chain [][]u8
	keys  CredentialKeys
}

// load_credential_keys loads a private key PEM block, requiring it to match
// the leaf public key. Used for both server and client credentials.
fn load_credential_keys(key_block pem.Block, leaf_public x509.PublicKey) !CredentialKeys {
	match leaf_public {
		x509.EcdsaP256PublicKey {
			if key_block.block_type !in ['EC PRIVATE KEY', 'PRIVATE KEY'] {
				return error('tls: ECDSA key must be SEC1 or PKCS#8 PEM')
			}
			priv := load_ecdsa_key(key_block.data, 32)!
			pb := priv.public_key()!
			if subtle.constant_time_compare(pb.bytes()!, leaf_public.point) != 1 {
				return error('tls: certificate and private key do not match')
			}
			return CredentialKeys{
				kind:       .ecdsa_p256
				ecdsa_priv: priv
			}
		}
		x509.EcdsaP384PublicKey {
			if key_block.block_type !in ['EC PRIVATE KEY', 'PRIVATE KEY'] {
				return error('tls: ECDSA key must be SEC1 or PKCS#8 PEM')
			}
			priv := load_ecdsa_key(key_block.data, 48)!
			pb := priv.public_key()!
			if subtle.constant_time_compare(pb.bytes()!, leaf_public.point) != 1 {
				return error('tls: certificate and private key do not match')
			}
			return CredentialKeys{
				kind:       .ecdsa_p384
				ecdsa_priv: priv
			}
		}
		x509.EcdsaP521PublicKey {
			if key_block.block_type !in ['EC PRIVATE KEY', 'PRIVATE KEY'] {
				return error('tls: ECDSA key must be SEC1 or PKCS#8 PEM')
			}
			priv := load_ecdsa_key(key_block.data, 66)!
			pb := priv.public_key()!
			if subtle.constant_time_compare(pb.bytes()!, leaf_public.point) != 1 {
				return error('tls: certificate and private key do not match')
			}
			return CredentialKeys{
				kind:       .ecdsa_p521
				ecdsa_priv: priv
			}
		}
		x509.RsaPublicKey {
			if key_block.block_type !in ['RSA PRIVATE KEY', 'PRIVATE KEY'] {
				return error('tls: RSA key must be PKCS#1 or PKCS#8 PEM')
			}
			priv := parse_rsa_private_key(key_block.data)!
			n, _ := parse_rsa_pubkey(leaf_public.der) or {
				return error('tls: invalid certificate key')
			}
			if n != priv.n {
				return error('tls: certificate and private key do not match')
			}
			return CredentialKeys{
				kind:     .rsa
				rsa_priv: priv
			}
		}
		x509.MlDsa44PublicKey {
			if key_block.block_type != 'PRIVATE KEY' {
				return error('tls: ML-DSA key must be PKCS#8 PEM')
			}
			priv := parse_mldsa_private_key(key_block.data, .ml_dsa_44)!
			if subtle.constant_time_compare(priv.public_key().bytes(), leaf_public.raw) != 1 {
				return error('tls: certificate and private key do not match')
			}
			return CredentialKeys{
				kind:       .mldsa_44
				mldsa_priv: priv
			}
		}
		x509.MlDsa65PublicKey {
			if key_block.block_type != 'PRIVATE KEY' {
				return error('tls: ML-DSA key must be PKCS#8 PEM')
			}
			priv := parse_mldsa_private_key(key_block.data, .ml_dsa_65)!
			if subtle.constant_time_compare(priv.public_key().bytes(), leaf_public.raw) != 1 {
				return error('tls: certificate and private key do not match')
			}
			return CredentialKeys{
				kind:       .mldsa_65
				mldsa_priv: priv
			}
		}
		x509.MlDsa87PublicKey {
			if key_block.block_type != 'PRIVATE KEY' {
				return error('tls: ML-DSA key must be PKCS#8 PEM')
			}
			priv := parse_mldsa_private_key(key_block.data, .ml_dsa_87)!
			if subtle.constant_time_compare(priv.public_key().bytes(), leaf_public.raw) != 1 {
				return error('tls: certificate and private key do not match')
			}
			return CredentialKeys{
				kind:       .mldsa_87
				mldsa_priv: priv
			}
		}
	}
}

// sign_cv_with signs CertificateVerify content with any loaded credential.
// sign_cv_with dispatches on the negotiated scheme: ECDSA signs directly,
// ML-DSA signs directly, RSA uses PKCS#1 v1.5 or PSS.
pub fn sign_cv_with(kind ServerKeyKind, scheme u16, ecdsa_priv ?ecdsa.PrivateKey, rsa_priv RsaPrivateKey, mldsa_priv mldsa.PrivateKey, content []u8) ![]u8 {
	match kind {
		.ecdsa_p256, .ecdsa_p384, .ecdsa_p521 {
			priv := ecdsa_priv or { return error('tls: missing EC private key') }
			return priv.sign(content, ecdsa.SignerOpts{}) or { return err }
		}
		.mldsa_44, .mldsa_65, .mldsa_87 {
			return mldsa_priv.sign(content, mldsa.SignerOpts{}) or { return err }
		}
		.rsa {
			return match scheme {
				u16(0x0401) {
					rsa_pkcs1_sign_digest(sha256.sum256(content), rsa_priv.d, rsa_priv.n,
						.sha256) or { return err }
				}
				u16(0x0501) {
					rsa_pkcs1_sign_digest(sha512.sum384(content), rsa_priv.d, rsa_priv.n,
						.sha384) or { return err }
				}
				u16(0x0601) {
					rsa_pkcs1_sign_digest(sha512.sum512(content), rsa_priv.d, rsa_priv.n,
						.sha512) or { return err }
				}
				u16(0x0804), u16(0x0805), u16(0x0806) {
					rsa_pss_sign(scheme, content, rsa_priv.d, rsa_priv.n) or { return err }
				}
				else {
					return error('tls: no supported signature scheme')
				}
			}
		}
	}
}

// encode_certificate_request encodes a CertificateRequest body (RFC 8446
// §4.3.2) with an empty request context and a signature_algorithms
// extension listing the schemes the server accepts.
pub fn encode_certificate_request(schemes_data []u8) ![]u8 {
	mut exts := []u8{}
	exts << encode_extension(ext_signature_algorithms, schemes_data)
	mut out := []u8{}
	out << u8(0) // empty request context
	out << u8((exts.len >> 8) & 0xff)
	out << u8(exts.len & 0xff)
	out << exts
	return out
}

// parse_certificate_request parses a CertificateRequest body, returning the
// accepted signature schemes and the request context (echoed in Certificate).
pub fn parse_certificate_request(body []u8) !([]u16, []u8) {
	if body.len < 3 {
		return error('tls: truncated CertificateRequest')
	}
	ctx_len := int(body[0])
	if 1 + ctx_len + 2 > body.len {
		return error('tls: truncated CertificateRequest context')
	}
	ctx := body[1..1 + ctx_len].clone()
	mut off := 1 + ctx_len
	ext_len := (int(body[off]) << 8) | int(body[off + 1])
	off += 2
	if off + ext_len != body.len {
		return error('tls: truncated CertificateRequest extensions')
	}
	mut schemes := []u16{}
	for off < body.len {
		if off + 4 > body.len {
			return error('tls: truncated CertificateRequest extension')
		}
		etyp := (u16(body[off]) << 8) | u16(body[off + 1])
		elen := (int(body[off + 2]) << 8) | int(body[off + 3])
		off += 4
		if off + elen > body.len {
			return error('tls: truncated CertificateRequest extension data')
		}
		if etyp == ext_signature_algorithms {
			edata := body[off..off + elen]
			slen := ((int(edata[0]) << 8) | int(edata[1])) + 2
			if edata.len < 2 || slen != edata.len || slen % 2 != 0 {
				return error('tls: bad signature_algorithms')
			}
			mut so := 2
			for so + 1 < edata.len {
				schemes << (u16(edata[so]) << 8) | u16(edata[so + 1])
				so += 2
			}
		}
		off += elen
	}
	if schemes.len == 0 {
		return error('tls: no signature schemes in CertificateRequest')
	}
	return schemes, ctx
}

// encode_certificate_body encodes a Certificate body (RFC 8446 §4.4.2) with
// the given request context and chain. An empty chain is a legal encoding
// (a client with no suitable certificate sends it); callers decide whether
// to accept it.
pub fn encode_certificate_body(context []u8, chain [][]u8) ![]u8 {
	if context.len > 255 {
		return error('tls: certificate request context too long')
	}
	mut cert_list := []u8{}
	for cert_der in chain {
		if cert_der.len == 0 || cert_der.len > 0xffffff {
			return error('tls: certificate size out of range')
		}
		cert_list << u8(cert_der.len >> 16)
		cert_list << u8(cert_der.len >> 8)
		cert_list << u8(cert_der.len)
		cert_list << cert_der
		cert_list << u8(0)
		cert_list << u8(0)
	}
	if cert_list.len > 0xffffff {
		return error('tls: certificate chain too large')
	}
	mut out := []u8{}
	out << u8(context.len)
	out << context
	out << u8(cert_list.len >> 16)
	out << u8(cert_list.len >> 8)
	out << u8(cert_list.len)
	out << cert_list
	return out
}

// parse_client_cert_chain parses a Certificate body into its chain (possibly
// empty) and request context.
pub fn parse_client_cert_chain(body []u8) !([][]u8, []u8) {
	if body.len < 4 {
		return error('tls: truncated Certificate')
	}
	mut off := 0
	ctx_len := int(body[off])
	off += 1
	if off + ctx_len + 3 > body.len {
		return error('tls: truncated Certificate')
	}
	ctx := body[off..off + ctx_len].clone()
	off += ctx_len
	list_len := (int(body[off]) << 16) | (int(body[off + 1]) << 8) | int(body[off + 2])
	off += 3
	if off + list_len != body.len {
		return error('tls: truncated certificate list')
	}
	end := off + list_len
	mut chain := [][]u8{}
	for off < end {
		if off + 3 > end {
			return error('tls: truncated cert entry')
		}
		cert_len := (int(body[off]) << 16) | (int(body[off + 1]) << 8) | int(body[off + 2])
		off += 3
		if off + cert_len + 2 > end {
			return error('tls: truncated cert')
		}
		chain << body[off..off + cert_len].clone()
		off += cert_len
		ext_len := (int(body[off]) << 8) | int(body[off + 1])
		off += 2
		if off + ext_len > end {
			return error('tls: truncated cert extensions')
		}
		off += ext_len
	}
	return chain, ctx
}

// load_client_creds loads the mTLS client credential from Config.client_cert
// /client_key (PEM bytes or file paths). It returns an error when no client
// credential is configured; the caller then sends an empty Certificate.
fn load_client_creds(cfg Config) !ClientCreds {
	if cfg.client_cert.len == 0 && cfg.client_key.len == 0 {
		return error('tls: no client certificate configured')
	}
	cert_data := config_pem_bytes([]u8{}, cfg.client_cert, cfg.in_memory, 'client certificate')!
	key_data := config_pem_bytes([]u8{}, cfg.client_key, cfg.in_memory, 'client private key')!
	mut rest := cert_data.bytestr()
	mut chain := [][]u8{}
	for rest.len > 0 {
		block, next := pem.decode(rest) or { break }
		if block.block_type == 'CERTIFICATE' {
			_ := x509.parse_der(block.data) or {
				return error('tls: invalid client certificate')
			}
			chain << block.data.clone()
		}
		if next.len == rest.len {
			break
		}
		rest = next
	}
	if chain.len == 0 {
		return error('tls: no PEM client certificate found')
	}
	key_block, _ := pem.decode(key_data.bytestr()) or {
		return error('tls: invalid client private key PEM')
	}
	leaf := x509.parse_der(chain[0])!
	leaf_public := leaf.public_key() or { return error('tls: unsupported client certificate public key') }
	keys := load_credential_keys(key_block, leaf_public)!
	return ClientCreds{
		chain: chain
		keys:  keys
	}
}

// build_client_auth_messages builds the client's Certificate (+
// CertificateVerify) flight answering a CertificateRequest. Without a
// configured credential, or when no scheme intersects, it returns a single
// empty Certificate; the server then rejects the handshake when
// verify_client is set.
pub fn build_client_auth_messages(cfg Config, req_context []u8, req_schemes []u16, suite CipherSuite, transcript []u8) ![][]u8 {
	creds := load_client_creds(cfg) or {
		empty := encode_handshake_message(.certificate, encode_certificate_body(req_context,
			[][]u8{})!)!
		return [empty]
	}
	scheme := select_cv_scheme(req_schemes, creds.keys.kind) or {
		empty := encode_handshake_message(.certificate, encode_certificate_body(req_context,
			[][]u8{})!)!
		return [empty]
	}
	cert_msg := encode_handshake_message(.certificate, encode_certificate_body(req_context,
		creds.chain)!)!
	mut th_input := transcript.clone()
	th_input << cert_msg
	th := transcript_hash_for_suite(th_input, suite)
	content := cv_content(false, th)
	sig := sign_cv_with(creds.keys.kind, scheme, creds.keys.ecdsa_priv, creds.keys.rsa_priv,
		creds.keys.mldsa_priv, content)!
	mut cv_body := []u8{}
	cv_body << u8(scheme >> 8)
	cv_body << u8(scheme)
	cv_body << u8(sig.len >> 8)
	cv_body << u8(sig.len)
	cv_body << sig
	cv_msg := encode_handshake_message(.certificate_verify, cv_body)!
	return [cert_msg, cv_msg]
}

// verify_client_certificate verifies a client's Certificate + CertificateVerify
// against the handshake transcript, appending both messages on success.
// verify_client_certificate rejects empty chains; the leaf is checked for
// validity dates and verified against client_roots/system roots when
// verify_chain is set (no hostname check applies to client certificates).
pub fn (mut s ServerHandshake) verify_client_certificate(cert_msg_body []u8, cv_msg_body []u8) ! {
	chain, _ := parse_client_cert_chain(cert_msg_body) or { return err }
	if chain.len == 0 {
		return error('tls: client certificate required')
	}
	cert_der := chain[0]
	leaf := x509.parse_der(cert_der) or { return error(error_bad_signature) }
	pk := leaf.public_key() or { return error(error_bad_signature) }
	if s.verify_chain {
		now := time.now()
		if now.unix() < leaf.not_before.unix() {
			return error(error_chain_untrusted)
		}
		if now.unix() > leaf.not_after.unix() {
			return error(error_chain_expired)
		}
		store := if s.client_roots.roots.len > 0 {
			s.client_roots
		} else {
			load_system_roots() or { return error(error_chain_untrusted) }
		}
		mut trusted := store.contains(leaf)
		if !trusted {
			for r in store.roots {
				rpk := r.public_key() or { continue }
				verify_cert_signature(cert_der, rpk) or { continue }
				trusted = true
				break
			}
		}
		if !trusted {
			return error(error_chain_untrusted)
		}
	}
	cert_msg := encode_handshake_message(.certificate, cert_msg_body)!
	// Capture the application-secret transcript (through server Finished)
	// before the client auth messages extend the transcript for Finished.
	s.auth_base_th = transcript_hash_for_suite(s.transcript, s.suite)
	s.transcript << cert_msg
	th := transcript_hash_for_suite(s.transcript, s.suite)
	content := cv_content(false, th)
	verify_certificate_verify(cv_msg_body, pk, content) or { return err }
	cv_msg := encode_handshake_message(.certificate_verify, cv_msg_body)!
	s.transcript << cv_msg
}
