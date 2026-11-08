module tls

import crypto.ecdsa
import crypto.rand
import crypto.sha256
import x.crypto.curve25519
import x.crypto.mlkem
import crypto

// ServerHandshake holds state for a TLS 1.3 server handshake.
pub struct ServerHandshake {
pub mut:
	host           string
	pq_mode        PostQuantumMode
	echd_priv      &curve25519.PrivateKey
	ecdsa_priv     ecdsa.PrivateKey
	ecdsa_pub      ecdsa.PublicKey
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
	mut h := ServerHandshake{
		pq_mode:       pq_mode
		echd_priv:     priv
		ecdsa_priv:    pv
		ecdsa_pub:     pb
		server_random: sr
	}
	if pq_mode != .off {
		h.has_mlkem = true
	}
	return h
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
	mut client_pub := []u8{}
	mut client_group := named_group_x25519
	for e in ch.extensions {
		if e.typ == ext_key_share {
			if e.data.len < 7 {
				return error('tls: bad key_share')
			}
			// e.data = outer_len(2) + KeyShareEntry: group(2)+len(2)+key
			g := (u16(e.data[2]) << 8) | u16(e.data[3])
			client_group = g
			kl := (int(e.data[4]) << 8) | int(e.data[5])
			if 6 + kl > e.data.len {
				return error('tls: truncated key_share')
			}
			client_pub = e.data[6..6 + kl].clone()
		}
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
		ct, ss := mlkem.encaps(mlkem.EncapsulationKey{ data: ek_bytes }) or { return err }
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
			Extension{ typ: ext_supported_versions, data: [
				u8(supported_version_tls_1_3 >> 8),
				u8(supported_version_tls_1_3),
			] },
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

	cert_body := []u8{len: 3}
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
	if fin.verify_data != expected {
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
