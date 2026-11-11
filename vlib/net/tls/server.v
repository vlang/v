module tls

import crypto.ecdsa
import crypto.rand
import crypto.sha256
import crypto.pem
import crypto.x509
import crypto.subtle
import x.crypto.curve25519
import x.crypto.mlkem
import crypto
import os
import time

// ServerHandshake holds state for a TLS 1.3 server handshake.
pub struct ServerHandshake {
pub mut:
	host           string
	pq_mode        PostQuantumMode
	echd_priv      &curve25519.PrivateKey
	ecdsa_priv     ecdsa.PrivateKey
	ecdsa_pub      ecdsa.PublicKey
	cert_chain     [][]u8
	client_random  []u8
	server_random  []u8
	has_mlkem      bool
	mlkem_ss       []u8
	handshake_keys HandshakeSecrets
	app_keys       ApplicationSecrets
	transcript     []u8
}

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

fn private_scalar_from_sec1(data []u8) ![]u8 {
	tag, content, consumed := der_tlv(data)!
	if tag != 0x30 || consumed != data.len {
		return error('tls: invalid EC private key')
	}
	mut off := 0
	vtag, _, vn := der_tlv(content[off..])!
	if vtag != 0x02 { return error('tls: invalid EC private key version') }
	off += vn
	ktag, scalar, _ := der_tlv(content[off..])!
	if ktag != 0x04 || scalar.len != 32 {
		return error('tls: only P-256 EC private keys are supported')
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
	if key_block.block_type !in ['EC PRIVATE KEY', 'PRIVATE KEY'] {
		return error('tls: only P-256 ECDSA server keys are supported')
	}
	seed := private_scalar_from_der(key_block.data)!
	private_key := ecdsa.new_key_from_seed(seed, ecdsa.CurveOptions{})!
	public_key := private_key.public_key()!
	leaf := x509.parse_der(chain[0])!
	leaf_public := leaf.public_key() or { return error('tls: unsupported server certificate public key') }
	match leaf_public {
		x509.EcdsaP256PublicKey {
			if subtle.constant_time_compare(public_key.bytes()!, leaf_public.point) != 1 {
				return error('tls: server certificate and private key do not match')
			}
		}
		else { return error('tls: only P-256 ECDSA server certificates are supported') }
	}
	mut server := new_server_handshake(pq_mode)!
	server.ecdsa_priv = private_key
	server.ecdsa_pub = public_key
	server.cert_chain = chain
	return server
}

// process_client_hello ingests a ClientHello, computes shared secret, returns server flight.
pub fn (mut s ServerHandshake) process_client_hello(ch_msg []u8) ![]u8 {
	msg, _ := parse_handshake_message(ch_msg) or { return err }
	if msg.typ != .client_hello {
		return error('tls: expected ClientHello')
	}
	ch := parse_client_hello(msg.body) or { return err }
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
		x_cpub := client_pub[..32]
		ek_bytes := client_pub[32..]
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
		cipher_suite:       .tls_aes_128_gcm_sha256
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
	mut content := []u8{}
	content << 'TLS 1.3, server CertificateVerify'.bytes()
	for _ in 0 .. 64 {
		content << u8(0)
	}
	content << transcript_hash
	sig := s.ecdsa_priv.sign(content, ecdsa.SignerOpts{}) or { return err }
	mut cv_body := []u8{}
	cv_body << u8(0x04)
	cv_body << u8(0x03)
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

pub fn (mut s ServerHandshake) verify_client_finished(fin_msg []u8) ! {
	parsed, _ := parse_handshake_message(fin_msg) or { return err }
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
