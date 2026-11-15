module tls

import crypto.ecdsa
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
	// External PSK (client side): offered as an extra identity alongside
	// (or instead of) the resumption ticket.
	ext_psk_identity []u8
	ext_psk_key      []u8
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
	// HRR state: set when the server answered HelloRetryRequest.
	is_hrr       bool
	hrr_group    u16
	first_ch_msg []u8
	// Extra offered groups beyond the default (tests trigger HRR and
	// negotiate ECDHE/FFDHE/ML-KEM via these). Empty preserves the
	// default wire bytes (X25519, plus hybrid when pq_mode needs it).
	offer_groups []u16
	// Negotiated ALPN protocol and stapled OCSP response, filled from
	// EncryptedExtensions / CertificateStatus.
	negotiated_alpn string
	ocsp_response   []u8
	// Server random from the ServerHello (for key logging).
	server_random []u8
	// Ephemeral key material per offered group (parallel to offer_groups;
	// X25519 uses echd_priv, ML-KEM uses mlkem_dk/ek below).
	ecdhe_priv_p256 ?ecdsa.PrivateKey
	ecdhe_priv_p384 ?ecdsa.PrivateKey
	has_p256        bool
	has_p384        bool
	ffdhe_priv      map[u16][]u8
	mlkem_dks       map[u16]mlkem.DecapsulationKey
	mlkem_eks       map[u16]mlkem.EncapsulationKey
	selected_group  u16
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
		ffdhe_priv:    map[u16][]u8{}
		mlkem_dks:     map[u16]mlkem.DecapsulationKey{}
		mlkem_eks:     map[u16]mlkem.EncapsulationKey{}
	}
	if pq_mode != .off {
		ek, dk := mlkem.keygen()!
		h.mlkem_ek = ek
		h.mlkem_dk = dk
		h.has_mlkem = true
	}
	return h
}

// client_share_for generates an ephemeral share for group, caching the
// private material on the handshake for the later ServerHello processing.
fn (mut h ClientHandshake) client_share_for(group u16) ![]u8 {
	if group == named_group_x25519 {
		pubkey := h.echd_priv.public_key()!
		return key_share_entry(group, pubkey.bytes()!)
	}
	if group == named_group_x25519_mlkem768 {
		pubkey := h.echd_priv.public_key()!
		pk_bytes := pubkey.bytes()!
		mut ek := h.mlkem_ek
		mut have := h.has_mlkem
		if !have {
			new_ek, new_dk := mlkem.generate_keypair(.ml_kem_768)!
			h.mlkem_eks[group] = new_ek
			h.mlkem_dks[group] = new_dk
			ek = new_ek
			have = true
		}
		mut hybrid_key := []u8{}
		hybrid_key << pk_bytes
		hybrid_key << ek.data
		return key_share_entry(group, hybrid_key)
	}
	if group == named_group_secp256r1 {
		_, priv := ecdsa.generate_key(ecdsa.CurveOptions{ nid: .prime256v1 })!
		pub_bytes := priv.public_key()!.bytes()!
		h.ecdhe_priv_p256 = priv
		h.has_p256 = true
		return key_share_entry(group, pub_bytes)
	}
	if group == named_group_secp384r1 {
		_, priv := ecdsa.generate_key(ecdsa.CurveOptions{ nid: .secp384r1 })!
		pub_bytes := priv.public_key()!.bytes()!
		h.ecdhe_priv_p384 = priv
		h.has_p384 = true
		return key_share_entry(group, pub_bytes)
	}
	if group == named_group_ffdhe2048 || group == named_group_ffdhe3072 {
		priv, pub_bytes := ffdhe_generate(group)!
		h.ffdhe_priv[group] = priv
		return key_share_entry(group, pub_bytes)
	}
	if group == named_group_mlkem512 || group == named_group_mlkem768
		|| group == named_group_mlkem1024 {
		variant := mlkem_variant_for_group(group)!
		ek, dk := mlkem.generate_keypair(variant)!
		h.mlkem_eks[group] = ek
		h.mlkem_dks[group] = dk
		return key_share_entry(group, ek.data)
	}
	return error('tls: unsupported client group ${group}')
}

// build_client_hello returns the encoded ClientHello handshake message bytes.
pub fn (mut h ClientHandshake) build_client_hello() ![]u8 {
	pubkey := h.echd_priv.public_key()!
	pk_bytes := pubkey.bytes()!

	mut groups := []u16{}
	mut entries := []u8{}
	if h.offer_groups.len > 0 {
		// Explicit group set (HRR/group tests): one share per group.
		for g in h.offer_groups {
			if g in groups {
				continue
			}
			groups << g
			entries << h.client_share_for(g)!
		}
	} else {
		if h.pq_mode == .hybrid_require {
			groups = [named_group_x25519_mlkem768]
		} else {
			groups = [named_group_x25519]
		}
		if h.has_mlkem && h.pq_mode != .hybrid_require {
			groups << named_group_x25519_mlkem768
		}
		sg_tmp := supported_groups_data_for(groups)
		_ = sg_tmp
		if h.has_mlkem {
			mut hybrid_key := []u8{}
			hybrid_key << pk_bytes
			hybrid_key << h.mlkem_ek.data
			if h.pq_mode == .hybrid_prefer {
				entries << key_share_entry(named_group_x25519, pk_bytes)
				entries << key_share_entry(named_group_x25519_mlkem768, hybrid_key)
			} else {
				entries << key_share_entry(named_group_x25519_mlkem768, hybrid_key)
			}
		} else {
			entries << key_share_entry(named_group_x25519, pk_bytes)
		}
	}
	sg_data := supported_groups_data_for(groups)
	ks_data := key_share_client_entries_data(entries)

	mut exts := [
		Extension{ typ: ext_supported_versions, data: supported_versions_client_data() },
		Extension{ typ: ext_supported_groups, data: sg_data },
		Extension{ typ: ext_signature_algorithms, data: signature_algorithms_data() },
		Extension{ typ: ext_key_share, data: ks_data },
		Extension{ typ: ext_server_name, data: sni_data(h.host)! },
		Extension{ typ: ext_status_request, data: encode_status_request() },
	]
	if h.alpn.len > 0 {
		exts << Extension{ typ: ext_alpn, data: alpn_data(h.alpn)! }
	}
	// PSK offers go last (RFC 8446 §4.2.11.2). The resumption ticket (if
	// its suite is offered) and/or the configured external PSK are
	// offered as identities with psk_dhe_ke; binders are patched below.
	mut offered_psk := false
	mut offered_early := false
	mut psk_ids := []PskIdentity{}
	mut psk_psks := [][]u8{}
	mut psk_suites := []CipherSuite{}
	if st := h.resume_offer {
		if st.suite in h.offered && st.ticket.len > 0 && st.psk.len > 0 {
			if h.max_early_data > 0 {
				exts << Extension{ typ: ext_early_data, data: encode_early_data_offer() }
				offered_early = true
			}
			age_ms := u32((time.now().unix() - st.created_unix) * 1000) & 0xffffffff
			obfuscated := age_ms + st.age_add
			psk_ids << PskIdentity{ ticket: st.ticket.clone(), obfuscated_age: obfuscated }
			psk_psks << st.psk.clone()
			psk_suites << st.suite
			offered_psk = true
		}
	}
	if h.ext_psk_identity.len > 0 && h.ext_psk_key.len > 0 {
		ext_suite := h.offered[0]
		psk_ids << PskIdentity{ ticket: h.ext_psk_identity.clone(), obfuscated_age: 0 }
		psk_psks << h.ext_psk_key.clone()
		psk_suites << ext_suite
		offered_psk = true
	}
	if offered_psk {
		exts << Extension{ typ: ext_psk_key_exchange_modes, data: encode_psk_modes() }
		exts << Extension{
			typ:  ext_pre_shared_key
			data: encode_psk_offer_multi(psk_ids, psk_suites)
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
		// One binder per identity, each under its own suite hash.
		mut binders := [][]u8{}
		for i in 0 .. psk_ids.len {
			es_psk := derive_early_secret_with_psk(psk_psks[i], psk_suites[i]) or {
				return err
			}
			bk := binder_key_for(es_psk, psk_suites[i]) or { return err }
			trunc := binder_prefix(msg, exts[exts.len - 1].data) or { return err }
			th := transcript_hash_for_suite(trunc, psk_suites[i])
			binders << compute_binder(bk, th, psk_suites[i]) or { return err }
		}
		mut psk_data := exts[exts.len - 1].data.clone()
		psk_data = patch_psk_binders(psk_data, binders)!
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

// build_second_client_hello rebuilds the ClientHello after a
// HelloRetryRequest for h.hrr_group. Everything stays identical except
// the key_share (now just the requested group); PSK binders are
// recomputed over the new flight. The transcript becomes
// message_hash(CH1) || HRR || CH2.
pub fn (mut h ClientHandshake) build_second_client_hello() ![]u8 {
	if !h.is_hrr || h.hrr_group == u16(0) {
		return error('tls: no HelloRetryRequest to answer')
	}
	prefix := h.transcript.clone()
	h.offer_groups = [h.hrr_group]
	ch2 := h.build_client_hello()!
	mut full := prefix.clone()
	full << ch2
	h.transcript = full
	return ch2
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
	// HelloRetryRequest: magic random means the server wants another
	// share. The transcript becomes message_hash(CH1) || HRR; the caller
	// rebuilds the ClientHello with the requested group and calls
	// build_second_client_hello + process_server_hello again.
	if is_hello_retry_request(msg.body) {
		mut selected := u16(0)
		for e in sh.extensions {
			if e.typ == ext_key_share {
				if e.data.len != 2 {
					return error('tls: bad HRR key_share')
				}
				selected = (u16(e.data[0]) << 8) | u16(e.data[1])
			}
		}
		if selected == u16(0) {
			return error('tls: HRR missing selected group')
		}
		ch1 := h.transcript.clone()
		prefix := encode_handshake_message(.message_hash, transcript_hash_for_suite(ch1,
			h.suite))!
		h.transcript = prefix.clone()
		h.transcript << sh_msg
		h.is_hrr = true
		h.hrr_group = selected
		h.first_ch_msg = ch1.clone()
		return
	}
	// A non-HRR ServerHello clears retry state (the second flight after
	// HRR, or the only flight when no retry happened).
	h.is_hrr = false
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
	h.server_random = sh.random.clone()
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
	if server_group == named_group_x25519_mlkem768 {
		// hybrid: server_pub = X25519 pub (32) || ML-KEM ciphertext.
		dk := h.mlkem_dks[server_group] or { h.mlkem_dk }
		if server_pub.len < 32 {
			return error('tls: bad hybrid key_share')
		}
		x_pub := server_pub[..32].clone()
		ct_bytes := server_pub[32..].clone()
		x_shared := h.echd_priv.x25519(x_pub) or { return err }
		mlkem_ss := mlkem.decaps(dk, mlkem.Ciphertext{ data: ct_bytes }) or { return err }
		shared << x_shared
		shared << mlkem_ss
	} else if server_group == named_group_x25519 {
		if h.pq_mode == .hybrid_require {
			return error('tls: hybrid key exchange required')
		}
		shared = h.echd_priv.x25519(server_pub) or { return err }
	} else if server_group == named_group_secp256r1 {
		priv := h.ecdhe_priv_p256 or { return error('tls: missing P-256 share') }
		if server_pub.len != 65 {
			return error('tls: bad P-256 key_share')
		}
		peer := ecdsa.PublicKey.from_uncompressed_bytes(server_pub, ecdsa.CurveOptions{
			nid: .prime256v1
		}) or { return err }
		shared = priv.derive_shared_secret(peer) or { return err }
	} else if server_group == named_group_secp384r1 {
		priv := h.ecdhe_priv_p384 or { return error('tls: missing P-384 share') }
		if server_pub.len != 97 {
			return error('tls: bad P-384 key_share')
		}
		peer := ecdsa.PublicKey.from_uncompressed_bytes(server_pub, ecdsa.CurveOptions{
			nid: .secp384r1
		}) or { return err }
		shared = priv.derive_shared_secret(peer) or { return err }
	} else if server_group == named_group_ffdhe2048 || server_group == named_group_ffdhe3072 {
		priv := h.ffdhe_priv[server_group] or { return error('tls: missing FFDHE share') }
		shared = ffdhe_compute(priv, server_pub, server_group) or { return err }
	} else if server_group == named_group_mlkem512 || server_group == named_group_mlkem768
		|| server_group == named_group_mlkem1024 {
		dk := h.mlkem_dks[server_group] or { return error('tls: missing ML-KEM share') }
		shared = mlkem.decaps(dk, mlkem.Ciphertext{ data: server_pub }) or { return err }
	} else {
		if h.pq_mode == .hybrid_require {
			return error('tls: hybrid key exchange required')
		}
		return error('tls: unsupported key_share group ${server_group}')
	}
	h.selected_group = server_group

	// Server PSK acceptance comes as a pre_shared_key extension carrying
	// the selected identity (index into our offered list).
	mut accepted_idx := -1
	for e in sh.extensions {
		if e.typ == ext_pre_shared_key {
			sel := parse_psk_selected(e.data) or { return err }
			accepted_idx = sel
			h.accepted_psk = true
		}
	}

	mut es := derive_early_secret_for_suite(h.suite) or { return err }
	if h.accepted_psk {
		// Resolve the accepted identity in offer order: ticket first,
		// then the external PSK.
		mut idx := accepted_idx
		mut psk := []u8{}
		mut suite := h.suite
		mut n_ids := 0
		mut ticket_ok := false
		if st := h.resume_offer {
			if st.suite in h.offered && st.ticket.len > 0 && st.psk.len > 0 {
				if idx == 0 {
					psk = st.psk.clone()
					suite = st.suite
					ticket_ok = true
				}
				n_ids++
				idx--
			}
		}
		if !ticket_ok {
			if h.ext_psk_identity.len > 0 && h.ext_psk_key.len > 0 {
				if idx == 0 {
					psk = h.ext_psk_key.clone()
					suite = h.offered[0]
				}
				n_ids++
				idx--
			}
		}
		if psk.len == 0 || idx != -1 {
			return error('tls: server accepted unoffered psk')
		}
		_ = n_ids
		if suite != h.suite {
			return error('tls: psk suite mismatch')
		}
		es = derive_early_secret_with_psk(psk, h.suite) or { return err }
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
		u16(0x0807),
		u16(0x0904),
		u16(0x0905),
		u16(0x0906),
		u16(0x0911),
		u16(0x0912),
		u16(0x0913),
		u16(0x0914),
		u16(0x0915),
		u16(0x0916),
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
