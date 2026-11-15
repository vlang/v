module tls

import crypto.rand
import crypto.x509
import time
import x.crypto.curve25519
import x.crypto.mlkem
import crypto

// ClientHandshake holds state for a TLS 1.3 client handshake.
pub struct ClientHandshake {
pub mut:
	host          string
	alpn          []string
	pq_mode       PostQuantumMode
	offered       []CipherSuite
	suite         CipherSuite
	echd_priv     &curve25519.PrivateKey
	client_random []u8
	// ML-KEM state
	mlkem_dk  mlkem.DecapsulationKey
	mlkem_ek  mlkem.EncapsulationKey
	has_mlkem bool
	// derived
	handshake_keys     HandshakeSecrets
	app_keys           ApplicationSecrets
	client_hs_traffic  TrafficKeys
	server_hs_traffic  TrafficKeys
	client_app_traffic TrafficKeys
	server_app_traffic TrafficKeys
	transcript         []u8
	server_cert_key    ?x509.PublicKey
	// hybrid shared secret components
	mlkem_ss []u8
	// PSK resumption: ticket offered on this handshake (if any).
	resume_offer ?SessionState
	// accepted_psk is set when the server selected our PSK identity.
	accepted_psk bool
	// max_early_data is the client's 0-RTT allowance (Config.max_early_data).
	// The client offers early data only when resuming with a non-zero limit.
	max_early_data u32
	// offered_early_data is set when the ClientHello carried early_data.
	offered_early_data bool
	// accepted_early_data is set when EncryptedExtensions carried early_data.
	accepted_early_data bool
	// early_traffic holds the 0-RTT keys (client_early_traffic_secret) used
	// to seal early records before the ServerHello arrives.
	early_traffic     TrafficKeys
	has_early_traffic bool
	early_suite       CipherSuite
	// requested_cert is set when the server sent CertificateRequest (mTLS).
	requested_cert bool
	req_schemes    []u16
	req_context    []u8
}

// new_client_handshake creates a client handshake for host with optional ALPN and PQ mode.
// new_client_handshake generates an ephemeral X25519 key and, unless pq_mode is off, an ML-KEM keypair.
// suites lists the cipher suites to offer; empty means the implemented default.
pub fn new_client_handshake(host string, alpn []string, pq_mode PostQuantumMode, suites []CipherSuite) !ClientHandshake {
	priv := curve25519.PrivateKey.new()!
	rnd := crypto.rand.bytes(32)!
	offered := if suites.len > 0 { suites.clone() } else { default_cipher_suites() }
	mut h := ClientHandshake{
		host:          host
		alpn:          alpn
		pq_mode:       pq_mode
		offered:       offered
		suite:         offered[0]
		echd_priv:     priv
		client_random: rnd
	}
	if pq_mode != .off {
		ek, dk := mlkem.keygen()!
		h.mlkem_ek = ek
		h.mlkem_dk = dk
		h.has_mlkem = true
	}
	return h
}

// build_client_hello returns the encoded ClientHello handshake message bytes.
pub fn (mut h ClientHandshake) build_client_hello() ![]u8 {
	pubkey := h.echd_priv.public_key()!
	pk_bytes := pubkey.bytes()!

	mut groups := if h.pq_mode == .hybrid_require {
		[named_group_x25519_mlkem768]
	} else {
		[named_group_x25519]
	}
	if h.has_mlkem && h.pq_mode != .hybrid_require {
		groups << named_group_x25519_mlkem768
	}
	sg_data := supported_groups_data_for(groups)

	mut ks_data := []u8{}
	if h.has_mlkem {
		mut hybrid_key := []u8{}
		hybrid_key << pk_bytes
		hybrid_key << h.mlkem_ek.data
		if h.pq_mode == .hybrid_prefer {
			mut entries := []u8{}
			entries << key_share_entry(named_group_x25519, pk_bytes)
			entries << key_share_entry(named_group_x25519_mlkem768, hybrid_key)
			ks_data = key_share_client_entries_data(entries)
		} else {
			ks_data = key_share_client_data_for(named_group_x25519_mlkem768, hybrid_key)
		}
	} else {
		ks_data = key_share_client_data(pk_bytes)
	}

	mut exts := [
		Extension{ typ: ext_supported_versions, data: supported_versions_client_data() },
		Extension{ typ: ext_supported_groups, data: sg_data },
		Extension{ typ: ext_signature_algorithms, data: signature_algorithms_data() },
		Extension{ typ: ext_key_share, data: ks_data },
		Extension{ typ: ext_server_name, data: sni_data(h.host)! },
	]
	if h.alpn.len > 0 {
		exts << Extension{ typ: ext_alpn, data: alpn_data(h.alpn)! }
	}
	// PSK offer goes last (RFC 8446 §4.2.11.2). Only offer when the ticket
	// suite is in our offer list (binder hash must match the negotiated hash).
	mut offered_psk := false
	mut offered_early := false
	if st := h.resume_offer {
		if st.suite in h.offered && st.ticket.len > 0 && st.psk.len > 0 {
			if h.max_early_data > 0 {
				exts << Extension{ typ: ext_early_data, data: encode_early_data_offer() }
				offered_early = true
			}
			exts << Extension{ typ: ext_psk_key_exchange_modes, data: encode_psk_modes() }
			age_ms := u32((time.now().unix() - st.created_unix) * 1000) & 0xffffffff
			obfuscated := age_ms + st.age_add
			exts << Extension{
				typ:  ext_pre_shared_key
				data: encode_psk_offer(st.ticket, obfuscated, st.suite)
			}
			offered_psk = true
		}
	}
	mut ch := ClientHello{
		legacy_version:      tls_version_1_3
		random:              h.client_random.clone()
		session_id:          []u8{}
		cipher_suites:       h.offered.clone()
		compression_methods: [u8(0)]
		extensions:          exts
	}
	body := encode_client_hello(ch)!
	mut msg := encode_handshake_message(.client_hello, body)!
	h.transcript = msg.clone()
	if offered_psk {
		st := h.resume_offer or { return error('tls: missing psk offer') }
		// Binder key from the PSK early secret ("res binder").
		es_psk := derive_early_secret_with_psk(st.psk, st.suite) or { return err }
		bk := binder_key_for(es_psk, st.suite) or { return err }
		trunc := binder_prefix(msg, exts[exts.len - 1].data) or { return err }
		th := transcript_hash_for_suite(trunc, st.suite)
		binder := compute_binder(bk, th, st.suite) or { return err }
		// Patch the trailing pre_shared_key extension with the real binder.
		mut psk_data := exts[exts.len - 1].data.clone()
		psk_data = patch_psk_binder(psk_data, binder)!
		exts[exts.len - 1] = Extension{
			typ:  ext_pre_shared_key
			data: psk_data
		}
		ch2 := ClientHello{
			legacy_version:      tls_version_1_3
			random:              h.client_random.clone()
			session_id:          []u8{}
			cipher_suites:       h.offered.clone()
			compression_methods: [u8(0)]
			extensions:          exts
		}
		body2 := encode_client_hello(ch2)!
		msg = encode_handshake_message(.client_hello, body2)!
		h.transcript = msg.clone()
	}
	if offered_early {
		// Derive the 0-RTT keys now: early records are sealed before the
		// ServerHello arrives, from the full (binder-patched) ClientHello.
		st := h.resume_offer or { return error('tls: missing psk offer') }
		es := derive_early_secret_with_psk(st.psk, st.suite) or { return err }
		ch_hash := transcript_hash_for_suite(msg, st.suite)
		early_secret := derive_client_early_traffic_secret(es, ch_hash, st.suite) or {
			return err
		}
		h.early_traffic = derive_traffic_keys(early_secret, st.suite) or { return err }
		h.has_early_traffic = true
		h.early_suite = st.suite
		h.offered_early_data = true
	}
	return msg
}

// process_server_hello ingests a ServerHello, computes shared secret, derives keys.
pub fn (mut h ClientHandshake) process_server_hello(sh_msg []u8) ! {
	msg, n := parse_handshake_message(sh_msg) or { return err }
	if n != sh_msg.len {
		return error('tls: trailing data after ServerHello')
	}
	if msg.typ != .server_hello {
		return error('tls: expected ServerHello')
	}
	sh := parse_server_hello(msg.body) or { return err }
	if sh.legacy_version != tls_version_1_3 {
		return error('tls: invalid ServerHello version')
	}
	if sh.cipher_suite !in h.offered {
		return error('tls: unsupported cipher suite')
	}
	h.suite = sh.cipher_suite
	// RFC 8446 §4.1.3: the server must confirm TLS 1.3 via supported_versions.
	mut confirmed_tls13 := false
	for e in sh.extensions {
		if e.typ == ext_supported_versions && e.data.len == 2
			&& (u16(e.data[0]) << 8) | u16(e.data[1]) == supported_version_tls_1_3 {
			confirmed_tls13 = true
			break
		}
	}
	if !confirmed_tls13 {
		return error('tls: server did not negotiate TLS 1.3')
	}
	h.transcript << sh_msg

	// find server key_share
	mut server_pub := []u8{}
	mut server_group := named_group_x25519
	for e in sh.extensions {
		if e.typ == ext_key_share {
			if e.data.len < 4 {
				return error('tls: bad key_share')
			}
			g := (u16(e.data[0]) << 8) | u16(e.data[1])
			server_group = g
			kl := (int(e.data[2]) << 8) | int(e.data[3])
			if 4 + kl > e.data.len {
				return error('tls: truncated key_share')
			}
			server_pub = e.data[4..4 + kl].clone()
			if 4 + kl != e.data.len {
				return error('tls: trailing data in key_share')
			}
		}
	}
	if server_pub.len == 0 {
		return error('tls: missing key_share')
	}

	mut shared := []u8{}
	if server_group == named_group_x25519_mlkem768 && h.has_mlkem {
		// hybrid: server_pub = X25519 pub (32) || ML-KEM ciphertext (1088)
		if server_pub.len < 32 {
			return error('tls: bad hybrid key_share')
		}
		x_pub := server_pub[..32].clone()
		ct_bytes := server_pub[32..].clone()
		// X25519 shared
		x_shared := h.echd_priv.x25519(x_pub) or { return err }
		// ML-KEM decaps
		mlkem_ss := mlkem.decaps(h.mlkem_dk, mlkem.Ciphertext{ data: ct_bytes }) or { return err }
		shared << x_shared
		shared << mlkem_ss
	} else {
		if h.pq_mode == .hybrid_require {
			return error('tls: hybrid key exchange required')
		}
		if server_group != named_group_x25519 {
			return error('tls: unsupported key_share group ${server_group}')
		}
		shared = h.echd_priv.x25519(server_pub) or { return err }
	}

	// Server PSK acceptance comes as a pre_shared_key extension carrying the
	// selected identity (we only ever offer one).
	for e in sh.extensions {
		if e.typ == ext_pre_shared_key {
			sel := parse_psk_selected(e.data) or { return err }
			if sel != 0 {
				return error('tls: bad psk selected identity')
			}
			h.accepted_psk = true
		}
	}

	mut es := derive_early_secret_for_suite(h.suite) or { return err }
	if h.accepted_psk {
		st := h.resume_offer or { return error('tls: server accepted unoffered psk') }
		if st.suite != h.suite {
			return error('tls: psk suite mismatch')
		}
		es = derive_early_secret_with_psk(st.psk, h.suite) or { return err }
	}
	th := transcript_hash_for_suite(h.transcript, h.suite)
	hs := derive_handshake_secrets_for_suite(es, shared, th, h.suite) or { return err }
	h.handshake_keys = hs
	h.client_hs_traffic = derive_traffic_keys(hs.client_secret, h.suite) or { return err }
	h.server_hs_traffic = derive_traffic_keys(hs.server_secret, h.suite) or { return err }
}

// client_handshake_traffic returns the derived client handshake traffic keys.
pub fn (h ClientHandshake) client_handshake_traffic() TrafficKeys {
	return h.client_hs_traffic
}

// server_handshake_traffic returns the derived server handshake traffic keys.
pub fn (h ClientHandshake) server_handshake_traffic() TrafficKeys {
	return h.server_hs_traffic
}

// ---------- extension helpers ----------

fn supported_versions_client_data() []u8 {
	mut data := []u8{}
	data << u8(2)
	data << u8(supported_version_tls_1_3 >> 8)
	data << u8(supported_version_tls_1_3)
	return data
}

fn supported_groups_data_for(groups []u16) []u8 {
	mut list := []u8{}
	for g in groups {
		list << u8(g >> 8)
		list << u8(g)
	}
	mut data := []u8{}
	data << u8(list.len >> 8)
	data << u8(list.len)
	data << list
	return data
}

fn supported_groups_data() []u8 {
	return supported_groups_data_for([named_group_x25519])
}

fn signature_algorithms_data() []u8 {
	schemes := [
		u16(0x0403),
		u16(0x0503),
		u16(0x0603),
		u16(0x0904),
		u16(0x0905),
		u16(0x0906),
		u16(0x0917),
		u16(0x0918),
		u16(0x0919),
		u16(0x091a),
		u16(0x091b),
		u16(0x091c),
		u16(0x0804),
		u16(0x0805),
		u16(0x0806),
		u16(0x0401),
		u16(0x0501),
		u16(0x0601),
	]
	mut list := []u8{}
	for s in schemes {
		list << u8(s >> 8)
		list << u8(s)
	}
	mut data := []u8{}
	data << u8(list.len >> 8)
	data << u8(list.len)
	data << list
	return data
}

fn key_share_client_data(pubkey []u8) []u8 {
	return key_share_client_data_for(named_group_x25519, pubkey)
}

fn key_share_client_data_for(group u16, pubkey []u8) []u8 {
	return key_share_client_entries_data(key_share_entry(group, pubkey))
}

fn key_share_entry(group u16, pubkey []u8) []u8 {
	mut entry := []u8{}
	entry << u8(group >> 8)
	entry << u8(group)
	entry << u8(pubkey.len >> 8)
	entry << u8(pubkey.len)
	entry << pubkey
	return entry
}

fn key_share_client_entries_data(entries []u8) []u8 {
	mut data := []u8{}
	data << u8(entries.len >> 8)
	data << u8(entries.len)
	data << entries
	return data
}

fn sni_data(host string) ![]u8 {
	if host.len == 0 || host.len > 255 {
		return error('tls: invalid server name length')
	}
	mut sni := []u8{}
	sni << u8(0)
	sni << u8(host.len >> 8)
	sni << u8(host.len)
	sni << host.bytes()
	mut data := []u8{}
	data << u8(sni.len >> 8)
	data << u8(sni.len)
	data << sni
	return data
}

fn alpn_data(protocols []string) ![]u8 {
	mut proto_list := []u8{}
	for p in protocols {
		if p.len == 0 || p.len > 255 {
			return error('tls: invalid ALPN protocol length')
		}
		proto_list << u8(p.len)
		proto_list << p.bytes()
	}
	if proto_list.len > 0xffff {
		return error('tls: ALPN extension too large')
	}
	mut data := []u8{}
	data << u8(proto_list.len >> 8)
	data << u8(proto_list.len)
	data << proto_list
	return data
}
