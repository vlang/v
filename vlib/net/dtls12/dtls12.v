// Copyright (c) 2026 V project. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// dtls12 is a minimal DTLS 1.2 (RFC 6347) profile over UDP (epoch 0/1).
// File-disjoint from net.tls and net.tls12: own Config/Conn, own record and
// handshake codecs over the TLS 1.2 PRF with the GCM + CBC+HMAC suite subset.
// The handshake does ECDHE (P-256): the server sends ServerKeyExchange with
// an ephemeral share signed by its ECDSA key, the client answers with
// ClientKeyExchange, and the premaster is the ECDH shared secret. Both
// Finished messages are encrypted under epoch-1 handshake keys after a
// ChangeCipherSpec flight (epoch bit set, sequence reset per epoch); epoch 0
// carries only the plaintext handshake flights.
// Cookie exchange (HelloVerifyRequest) is optional and disabled by default:
// enable it with Config.enable_cookie to challenge the first ClientHello.
// The cookie is a stateless HMAC (keyed per socket at bind/listen time) over
// client address + client_random; no per-handshake state is stored.
module dtls12

import crypto.rand
import net
import time

// Config holds DTLS 1.2 settings.
@[params]
pub struct Config {
pub mut:
	cipher_suites []u16
	// enable_cookie challenges the first ClientHello with HelloVerifyRequest.
	// Disabled by default for loopback; enable to mitigate spoofed-CH
	// amplification. The cookie is a stateless HMAC (per-socket key) over
	// client address + client_random; no per-handshake state is stored.
	enable_cookie bool
	// ecdsa_sign_priv is the optional 32-byte P-256 long-term signing key
	// the server uses to sign ServerKeyExchange. When empty, the server
	// generates a per-handshake signing key, which provides no
	// authentication (anonymous ECDHE: passive confidentiality and forward
	// secrecy only). Configure it together with ecdsa_sign_peer pinning on
	// the client to authenticate the server.
	ecdsa_sign_priv []u8
	// ecdsa_sign_peer is the optional pinned 65-byte uncompressed P-256
	// point the client uses to verify the ServerKeyExchange signature.
	// When empty, the client skips verification (anonymous ECDHE: an active
	// man-in-the-middle is NOT prevented).
	ecdsa_sign_peer   []u8
	read_timeout      time.Duration
	write_timeout     time.Duration
	handshake_timeout time.Duration
	initial_rto_ms    int
	max_rto_ms        int
	max_retries       int
	// drop_tx_first_n drops the first N outbound datagrams (loss simulation).
	drop_tx_first_n int
	// drop_rx_first_n drops the first N inbound datagrams (loss simulation).
	drop_rx_first_n int
}

// default_config returns usable defaults for loopback tests.
pub fn default_config() Config {
	return Config{
		read_timeout:      5 * time.second
		write_timeout:     5 * time.second
		handshake_timeout: 10 * time.second
		initial_rto_ms:    100
		max_rto_ms:        1000
		max_retries:       12
	}
}

// normalized fills zero timeouts/retries with defaults.
fn (cfg Config) normalized() Config {
	mut c := cfg
	if c.read_timeout == 0 {
		c.read_timeout = 5 * time.second
	}
	if c.write_timeout == 0 {
		c.write_timeout = 5 * time.second
	}
	if c.handshake_timeout == 0 {
		c.handshake_timeout = 10 * time.second
	}
	if c.initial_rto_ms <= 0 {
		c.initial_rto_ms = 100
	}
	if c.max_rto_ms <= 0 {
		c.max_rto_ms = 1000
	}
	if c.max_retries <= 0 {
		c.max_retries = 12
	}
	return c
}

// TrafficKeys holds the directional record keys for one epoch.
struct TrafficKeys {
	key []u8
	iv  []u8
	mac []u8
}

// Conn is a DTLS 1.2 connection over UDP.
pub struct Conn {
mut:
	udp           net.UdpConn
	peer          net.Addr
	has_peer      bool
	is_server     bool
	cfg           Config
	suite         u16
	is_gcm        bool
	client_random []u8
	server_random []u8
	master        []u8
	// cookie_key is the per-socket HMAC key for stateless cookies.
	cookie_key        []u8
	client_keys       TrafficKeys
	server_keys       TrafficKeys
	handshake_done    bool
	app_write_seq     u64
	app_replay        ReplayWindow
	tx_drop_remaining int
	rx_drop_remaining int
	closed            bool
	read_buf          []u8
	transcript        []u8
}

// Listener is a single-client DTLS listener (minimal: one accept per socket).
pub struct Listener {
mut:
	udp        net.UdpConn
	cfg        Config
	cookie_key []u8
	moved      bool
}

// dial connects a DTLS 1.2 client to raddr (e.g. '127.0.0.1:4433').
pub fn dial(raddr string, cfg Config) !&Conn {
	ncfg := cfg.normalized()
	mut u := net.dial_udp(raddr)!
	u.set_read_timeout(ncfg.read_timeout)
	u.set_write_timeout(ncfg.write_timeout)
	peer := u.sock.remote() or { return error('dtls12: no udp remote') }
	return &Conn{
		udp:               u
		peer:              peer
		has_peer:          true
		is_server:         false
		cfg:               ncfg
		tx_drop_remaining: ncfg.drop_tx_first_n
		rx_drop_remaining: ncfg.drop_rx_first_n
	}
}

// bind creates an unconnected server-side DTLS Conn bound to laddr.
pub fn bind(laddr string, cfg Config) !&Conn {
	ncfg := cfg.normalized()
	mut u := net.listen_udp(laddr)!
	u.set_read_timeout(ncfg.read_timeout)
	u.set_write_timeout(ncfg.write_timeout)
	return &Conn{
		udp:               u
		is_server:         true
		cfg:               ncfg
		cookie_key:        rand.bytes(32)!
		tx_drop_remaining: ncfg.drop_tx_first_n
		rx_drop_remaining: ncfg.drop_rx_first_n
	}
}

// listen binds a DTLS listener to laddr.
pub fn listen(laddr string, cfg Config) !&Listener {
	ncfg := cfg.normalized()
	mut u := net.listen_udp(laddr)!
	u.set_read_timeout(ncfg.read_timeout)
	u.set_write_timeout(ncfg.write_timeout)
	return &Listener{
		udp:        u
		cfg:        ncfg
		cookie_key: rand.bytes(32)!
	}
}

// addr returns the local address of the listener.
pub fn (l &Listener) addr() !net.Addr {
	return l.udp.sock.address()
}

// local_addr returns the local address of the Conn.
pub fn (c &Conn) local_addr() !net.Addr {
	return c.udp.sock.address()
}

// close closes the listener unless it was moved into an accepted Conn.
pub fn (mut l Listener) close() ! {
	if l.moved {
		return
	}
	l.moved = true
	mut u := l.udp
	u.close()!
	l.udp = u
}

// accept waits for a ClientHello and completes the server handshake.
pub fn (mut l Listener) accept() !&Conn {
	if l.moved {
		return error('dtls12: listener already accepted')
	}
	mut c := &Conn{
		udp:               l.udp
		is_server:         true
		cfg:               l.cfg
		cookie_key:        l.cookie_key.clone()
		tx_drop_remaining: l.cfg.drop_tx_first_n
		rx_drop_remaining: l.cfg.drop_rx_first_n
	}
	l.moved = true
	c.handshake_server()!
	return c
}

// handshake runs the DTLS 1.2 handshake (client or server by role).
pub fn (mut c Conn) handshake() ! {
	if c.is_server {
		c.handshake_server()!
	} else {
		c.handshake_client()!
	}
}

// is_handshake_done reports whether the handshake completed.
pub fn (c &Conn) is_handshake_done() bool {
	return c.handshake_done
}

// cipher_suite returns the negotiated suite ID (0 before handshake).
pub fn (c &Conn) cipher_suite() u16 {
	return c.suite
}

// close closes the underlying UDP socket.
pub fn (mut c Conn) close() ! {
	if c.closed {
		return
	}
	c.closed = true
	mut u := c.udp
	u.close()!
	c.udp = u
}

// send_record encodes and transmits one DTLS 1.2 record to peer.
fn (mut c Conn) send_record(rec Dtls12Record) ! {
	if !c.has_peer {
		return error('dtls12: no peer')
	}
	data := encode_dtls12_record(rec)!
	if c.tx_drop_remaining > 0 {
		c.tx_drop_remaining--
		return
	}
	mut u := c.udp
	u.write_to(c.peer, data)!
	c.udp = u
}

// recv_record receives one DTLS 1.2 record with a read timeout.
fn (mut c Conn) recv_record(timeout time.Duration) !(Dtls12Record, net.Addr) {
	mut u := c.udp
	u.set_read_timeout(timeout)
	c.udp = u
	mut buf := []u8{len: dtls12_max_datagram}
	mut uu := c.udp
	n, addr := uu.read(mut buf)!
	c.udp = uu
	if c.rx_drop_remaining > 0 {
		c.rx_drop_remaining--
		return error('dtls12: simulated rx loss')
	}
	rec := decode_dtls12_record(buf[..n])!
	return rec, addr
}

// same_peer reports whether addr matches the known peer.
fn (c &Conn) same_peer(addr net.Addr) bool {
	if !c.has_peer {
		return true
	}
	return c.peer.str() == addr.str()
}

// derive_traffic splits the key block into directional keys (RFC 5246 §6.3
// with DTLS epoch/seq AAD): client_write_MAC, server_write_MAC,
// client_write_key, server_write_key, client_write_IV, server_write_IV.
fn derive_traffic(suite u16, master []u8, server_random []u8, client_random []u8) !(TrafficKeys, TrafficKeys) {
	mac_len, key_len, iv_len := suite_key_lengths(suite)
	total := 2 * mac_len + 2 * key_len + 2 * iv_len
	block := key_expansion(master, server_random, client_random, total)
	mut off := 0
	client_mac := block[off..off + mac_len].clone()
	off += mac_len
	server_mac := block[off..off + mac_len].clone()
	off += mac_len
	client_key := block[off..off + key_len].clone()
	off += key_len
	server_key := block[off..off + key_len].clone()
	off += key_len
	client_iv := block[off..off + iv_len].clone()
	off += iv_len
	server_iv := block[off..off + iv_len].clone()
	return TrafficKeys{
		key: client_key
		iv:  client_iv
		mac: client_mac
	}, TrafficKeys{
		key: server_key
		iv:  server_iv
		mac: server_mac
	}
}

// seal_app protects application data with the write keys.
fn (c &Conn) seal_app(epoch u16, seq u64, plaintext []u8) ![]u8 {
	return c.seal_for(epoch, seq, .application_data, plaintext)
}

// open_app unprotects application data with the read keys.
fn (c &Conn) open_app(epoch u16, seq u64, sealed []u8) ![]u8 {
	return c.open_for(epoch, seq, .application_data, sealed)
}

// handshake_client runs the client side: CH [cookie] -> SH/SKE/SHD, then
// CKE + ChangeCipherSpec + encrypted Finished (epoch 1), then peer
// ChangeCipherSpec + encrypted Finished. Epoch 0 carries only the plaintext
// handshake flights; Finished messages never appear in epoch-0 plaintext.
fn (mut c Conn) handshake_client() ! {
	cfg := c.cfg
	suites := effective_suites(cfg.cipher_suites)
	client_random := rand.bytes(32)!
	c.client_random = client_random.clone()
	mut cookie := []u8{}
	mut ch_msg := build_client_hello_msg(client_random, suites, cookie)!
	mut ch_rec := Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_handshake
		sequence_number: 0
		fragment:        ch_msg
	}
	// Phase 1: collect the server flight SH(0)/SKE(1)/SHD(2) in epoch 0.
	mut sh_msg := []u8{}
	mut ske_msg := []u8{}
	mut shd_msg := []u8{}
	mut server_random := []u8{}
	mut suite := suites[0]
	mut timer := new_retransmit_timer(cfg.initial_rto_ms, cfg.max_rto_ms)
	mut attempts := 0
	for attempts <= cfg.max_retries {
		if sh_msg.len == 0 || ske_msg.len == 0 || shd_msg.len == 0 {
			c.send_record(ch_rec)!
		}
		rto := timer.next_timeout()
		rec, addr := c.recv_record(rto) or {
			if is_timeout(err) || err.msg().contains('simulated') || err.msg().contains('dtls12:') {
				attempts++
				continue
			}
			return err
		}
		if !c.same_peer(addr) {
			continue
		}
		if rec.epoch != dtls12_epoch_handshake
			|| rec.content_type != ContentType.handshake {
			continue
		}
		if rec.sequence_number == 0 {
			if sh_msg.len == 0 && ske_msg.len == 0 && shd_msg.len == 0 {
				// Optional cookie exchange: server challenges with
				// HelloVerifyRequest; client resends ClientHello with cookie.
				cookie_candidate := parse_hello_verify_request(rec.fragment) or { []u8{} }
				if cookie_candidate.len != 0 {
					cookie = cookie_candidate.clone()
					ch_msg = build_client_hello_msg(client_random, suites, cookie)!
					ch_rec = Dtls12Record{
						content_type:    .handshake
						epoch:           dtls12_epoch_handshake
						sequence_number: 0
						fragment:        ch_msg
					}
					c.send_record(ch_rec)!
					attempts++
					continue
				}
				sr, sel := parse_server_hello_msg(rec.fragment) or { continue }
				if sel !in suites {
					return error('dtls12: server selected unsupported suite')
				}
				server_random = sr.clone()
				suite = sel
				sh_msg = rec.fragment.clone()
			}
			continue
		}
		if rec.sequence_number == 1 {
			if sh_msg.len == 0 {
				continue
			}
			ske_typ, _ := parse_hs_msg(rec.fragment) or { continue }
			if ske_typ != hstype_server_key_exchange {
				continue
			}
			ske_msg = rec.fragment.clone()
			continue
		}
		if rec.sequence_number == 2 {
			if ske_msg.len == 0 {
				continue
			}
			shd_typ, _ := parse_hs_msg(rec.fragment) or { continue }
			if shd_typ != hstype_server_hello_done {
				continue
			}
			shd_msg = rec.fragment.clone()
			break
		}
	}
	if sh_msg.len == 0 || ske_msg.len == 0 || shd_msg.len == 0 {
		return error('dtls12: handshake timeout waiting for server flight')
	}
	ske_typ, ske_body := parse_hs_msg(ske_msg)!
	if ske_typ != hstype_server_key_exchange {
		return error('dtls12: expected ServerKeyExchange')
	}
	parsed_ske := parse_server_key_exchange(ske_body)!
	// Authenticate the ephemeral share when the server key is pinned;
	// otherwise proceed anonymously (no MITM protection, documented).
	if cfg.ecdsa_sign_peer.len != 0 {
		verify_server_key_exchange(parsed_ske, client_random, server_random, cfg.ecdsa_sign_peer)!
	}
	cli_priv, cli_x, cli_y := p256_keygen()!
	premaster := p256_ecdh(cli_priv, parsed_ske.eph_x, parsed_ske.eph_y)!
	master := derive_master(premaster, client_random, server_random)!
	c.master = master.clone()
	c.suite = suite
	c.is_gcm = suite_is_gcm(suite)
	c.server_random = server_random.clone()
	client_app, server_app := derive_traffic(suite, master, server_random, client_random)!
	c.client_keys = client_app
	c.server_keys = server_app
	mut transcript := ch_msg.clone()
	transcript << sh_msg
	transcript << ske_msg
	transcript << shd_msg
	cke_body := build_client_key_exchange(cli_x, cli_y)!
	cke_msg := hs_msg(hstype_client_key_exchange, cke_body)
	transcript << cke_msg
	// Phase 2: CKE (epoch 0) + ChangeCipherSpec + Finished (epoch 1).
	cke_rec := Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_handshake
		sequence_number: 1
		fragment:        cke_msg
	}
	ccs_rec := Dtls12Record{
		content_type:    .change_cipher_spec
		epoch:           dtls12_epoch_handshake
		sequence_number: 2
		fragment:        [u8(1)]
	}
	cfin_plain := build_finished_msg(master, 'client finished', transcript)!
	cfin_ct := c.seal_hs(dtls12_epoch_application, 0, cfin_plain)!
	cfin_rec := Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_application
		sequence_number: 0
		fragment:        cfin_ct
	}
	c.send_record(cke_rec)!
	c.send_record(ccs_rec)!
	c.send_record(cfin_rec)!
	// Phase 3: peer ChangeCipherSpec + encrypted Finished (epoch 1, seq 0).
	// CCS is advisory (the epoch is explicit on every record); the Finished
	// verify is what authenticates the handshake.
	mut sfin_plain := []u8{}
	mut timer2 := new_retransmit_timer(cfg.initial_rto_ms, cfg.max_rto_ms)
	mut attempts2 := 0
	for attempts2 <= cfg.max_retries {
		if sfin_plain.len != 0 {
			break
		}
		rto := timer2.next_timeout()
		rec, addr := c.recv_record(rto) or {
			if is_timeout(err) || err.msg().contains('simulated') || err.msg().contains('dtls12:') {
				c.send_record(cke_rec) or {}
				c.send_record(ccs_rec) or {}
				c.send_record(cfin_rec) or {}
				attempts2++
				continue
			}
			return err
		}
		if !c.same_peer(addr) {
			continue
		}
		if rec.content_type == ContentType.change_cipher_spec {
			continue
		}
		if rec.content_type != ContentType.handshake {
			continue
		}
		if rec.epoch == dtls12_epoch_handshake {
			// Server flight replay: resend our flight.
			if rec.sequence_number <= 2 {
				c.send_record(cke_rec) or {}
				c.send_record(ccs_rec) or {}
				c.send_record(cfin_rec) or {}
				continue
			}
			continue
		}
		if rec.epoch != dtls12_epoch_application || rec.sequence_number != 0 {
			continue
		}
		pt := c.open_hs(rec.epoch, rec.sequence_number, rec.fragment) or { continue }
		fin_typ, fin_body := parse_hs_msg(pt) or { continue }
		if fin_typ != hstype_finished || fin_body.len != 12 {
			continue
		}
		mut full := transcript.clone()
		full << cfin_plain
		verify_finished_msg(pt, master, 'server finished', full) or { continue }
		transcript << cfin_plain
		transcript << pt
		sfin_plain = pt.clone()
	}
	if sfin_plain.len == 0 {
		return error('dtls12: handshake timeout waiting for server Finished')
	}
	c.transcript = transcript.clone()
	c.handshake_done = true
	// Epoch-1 sequence space: Finished took 0 in each direction, so
	// application data starts at 1.
	c.app_write_seq = 1
	c.app_replay.reset()
	// Linger: answer server flight replays so a lost client flight still
	// converges (server resends its flight, we resend ours).
	mut linger := 0
	linger_rto := time.Duration(i64(cfg.initial_rto_ms) * 1_000_000)
	for linger < 3 {
		rec, _ := c.recv_record(linger_rto) or {
			if is_timeout(err) || err.msg().contains('simulated') {
				break
			}
			if err.msg().contains('dtls12:') {
				linger++
				continue
			}
			break
		}
		if rec.content_type == ContentType.handshake
			&& (rec.epoch == dtls12_epoch_handshake || rec.epoch == dtls12_epoch_application) {
			c.send_record(cke_rec) or {}
			c.send_record(ccs_rec) or {}
			c.send_record(cfin_rec) or {}
			linger++
			continue
		}
		linger++
	}
}

// handshake_server runs the server side: CH [cookie] -> SH/SKE/SHD, then
// CKE + peer ChangeCipherSpec + encrypted Finished, then ChangeCipherSpec +
// encrypted Finished. Epoch 0 carries only the plaintext flights.
fn (mut c Conn) handshake_server() ! {
	cfg := c.cfg
	server_prefs := effective_suites(cfg.cipher_suites)
	mut ch_msg := []u8{}
	mut client_random := []u8{}
	mut offered := []u16{}
	mut ch_addr := net.Addr{}
	mut got_ch := false
	mut u := c.udp
	u.set_read_timeout(cfg.handshake_timeout)
	c.udp = u
	for !got_ch {
		rec, addr := c.recv_record(cfg.handshake_timeout) or {
			if is_timeout(err) {
				return err
			}
			continue
		}
		if rec.epoch != dtls12_epoch_handshake
			|| rec.content_type != ContentType.handshake {
			continue
		}
		if rec.sequence_number != 0 {
			continue
		}
		// If this looks like a HelloVerifyRequest echo (should not happen on
		// the server), ignore it.
		hvr_probe := parse_hello_verify_request(rec.fragment) or { []u8{} }
		if hvr_probe.len != 0 {
			continue
		}
		cr, suites, ck := parse_client_hello_msg(rec.fragment) or { continue }
		if cfg.enable_cookie {
			if ck.len == 0 {
				// Challenge with a stateless HMAC cookie (no stored state).
				cookie := make_cookie(c.cookie_key, addr.str(), cr)!
				hvr := build_hello_verify_request(cookie)!
				c.peer = addr
				c.has_peer = true
				hvr_rec := Dtls12Record{
					content_type:    .handshake
					epoch:           dtls12_epoch_handshake
					sequence_number: 0
					fragment:        hvr
				}
				c.send_record(hvr_rec)!
				continue
			}
			if !check_cookie(ck, c.cookie_key, addr.str(), cr) {
				continue
			}
		}
		client_random = cr.clone()
		offered = suites.clone()
		ch_msg = rec.fragment.clone()
		ch_addr = addr
		got_ch = true
	}
	if !got_ch {
		return error('dtls12: handshake timeout waiting for ClientHello')
	}
	c.peer = ch_addr
	c.has_peer = true
	c.client_random = client_random.clone()
	suite := select_suite(offered, server_prefs)!
	c.suite = suite
	c.is_gcm = suite_is_gcm(suite)
	server_random := rand.bytes(32)!
	c.server_random = server_random.clone()
	// Ephemeral ECDHE share for this handshake.
	srv_priv, srv_x, srv_y := p256_keygen()!
	// Long-term (or per-handshake, when unconfigured) SKE signing key.
	mut sign_priv := cfg.ecdsa_sign_priv.clone()
	if sign_priv.len != 32 {
		sk, _, _ := p256_keygen()!
		sign_priv = sk.clone()
	}
	sh_msg := build_server_hello_msg(server_random, suite)!
	ske_body := build_server_key_exchange(client_random, server_random, srv_x, srv_y,
		sign_priv)!
	ske_msg := hs_msg(hstype_server_key_exchange, ske_body)
	shd_msg := build_server_hello_done()
	mut transcript := ch_msg.clone()
	transcript << sh_msg
	transcript << ske_msg
	transcript << shd_msg
	sh_rec := Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_handshake
		sequence_number: 0
		fragment:        sh_msg
	}
	ske_rec := Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_handshake
		sequence_number: 1
		fragment:        ske_msg
	}
	shd_rec := Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_handshake
		sequence_number: 2
		fragment:        shd_msg
	}
	c.send_record(sh_rec)!
	c.send_record(ske_rec)!
	c.send_record(shd_rec)!
	// Wait for ClientKeyExchange (epoch 0, seq 1); retransmit the flight
	// while waiting, and on duplicate ClientHello.
	mut cke_msg := []u8{}
	mut cke_body := []u8{}
	mut timer := new_retransmit_timer(cfg.initial_rto_ms, cfg.max_rto_ms)
	mut attempts := 0
	for attempts <= cfg.max_retries {
		if cke_msg.len != 0 {
			break
		}
		rto := timer.next_timeout()
		rec, addr := c.recv_record(rto) or {
			if is_timeout(err) || err.msg().contains('simulated') || err.msg().contains('dtls12:') {
				c.send_record(sh_rec) or {}
				c.send_record(ske_rec) or {}
				c.send_record(shd_rec) or {}
				attempts++
				continue
			}
			return err
		}
		if c.peer.str() != addr.str() {
			continue
		}
		if rec.epoch != dtls12_epoch_handshake
			|| rec.content_type != ContentType.handshake {
			continue
		}
		if rec.sequence_number == 0 {
			c.send_record(sh_rec) or {}
			c.send_record(ske_rec) or {}
			c.send_record(shd_rec) or {}
			continue
		}
		if rec.sequence_number != 1 {
			continue
		}
		cke_typ, parsed := parse_hs_msg(rec.fragment) or { continue }
		if cke_typ != hstype_client_key_exchange {
			continue
		}
		cke_msg = rec.fragment.clone()
		cke_body = parsed.clone()
		transcript << cke_msg
	}
	if cke_msg.len == 0 {
		return error('dtls12: handshake timeout waiting for ClientKeyExchange')
	}
	peer_x, peer_y := parse_client_key_exchange(cke_body)!
	premaster := p256_ecdh(srv_priv, peer_x, peer_y)!
	master := derive_master(premaster, client_random, server_random)!
	c.master = master.clone()
	client_app, server_app := derive_traffic(suite, master, server_random, client_random)!
	c.client_keys = client_app
	c.server_keys = server_app
	// Wait for peer ChangeCipherSpec + encrypted Finished (epoch 1, seq 0);
	// retransmit the first flight while waiting (CCS is advisory, the epoch
	// is explicit on every record). A plaintext epoch-0 Finished is ignored.
	mut cfin_plain := []u8{}
	mut timer2 := new_retransmit_timer(cfg.initial_rto_ms, cfg.max_rto_ms)
	mut attempts2 := 0
	for attempts2 <= cfg.max_retries {
		if cfin_plain.len != 0 {
			break
		}
		rto := timer2.next_timeout()
		rec, addr := c.recv_record(rto) or {
			if is_timeout(err) || err.msg().contains('simulated') || err.msg().contains('dtls12:') {
				c.send_record(sh_rec) or {}
				c.send_record(ske_rec) or {}
				c.send_record(shd_rec) or {}
				attempts2++
				continue
			}
			return err
		}
		if c.peer.str() != addr.str() {
			continue
		}
		if rec.content_type == ContentType.change_cipher_spec {
			continue
		}
		if rec.content_type != ContentType.handshake {
			continue
		}
		if rec.epoch == dtls12_epoch_handshake {
			if rec.sequence_number == 0 {
				c.send_record(sh_rec) or {}
				c.send_record(ske_rec) or {}
				c.send_record(shd_rec) or {}
				continue
			}
			if rec.sequence_number == 1 {
				// Duplicate CKE: harmless, still waiting for CCS/Finished.
				continue
			}
			continue
		}
		if rec.epoch != dtls12_epoch_application || rec.sequence_number != 0 {
			continue
		}
		pt := c.open_hs(rec.epoch, rec.sequence_number, rec.fragment) or { continue }
		fin_typ, fin_body := parse_hs_msg(pt) or { continue }
		if fin_typ != hstype_finished || fin_body.len != 12 {
			continue
		}
		verify_finished_msg(pt, master, 'client finished', transcript) or { continue }
		cfin_plain = pt.clone()
	}
	if cfin_plain.len == 0 {
		return error('dtls12: handshake timeout waiting for client Finished')
	}
	transcript << cfin_plain
	sfin_plain := build_finished_msg(master, 'server finished', transcript)!
	sfin_ct := c.seal_hs(dtls12_epoch_application, 0, sfin_plain)!
	transcript << sfin_plain
	c.transcript = transcript.clone()
	ccs_rec := Dtls12Record{
		content_type:    .change_cipher_spec
		epoch:           dtls12_epoch_handshake
		sequence_number: 3
		fragment:        [u8(1)]
	}
	sfin_rec := Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_application
		sequence_number: 0
		fragment:        sfin_ct
	}
	c.send_record(ccs_rec)!
	c.send_record(sfin_rec)!
	c.handshake_done = true
	// Epoch-1 sequence space: Finished took 0 in each direction, so
	// application data starts at 1.
	c.app_write_seq = 1
	c.app_replay.reset()
}

// write encrypts data as one epoch-1 application record.
pub fn (mut c Conn) write(data []u8) !int {
	if !c.handshake_done {
		return error('dtls12: handshake not complete')
	}
	if data.len == 0 {
		return error('dtls12: empty write')
	}
	if data.len > dtls12_max_fragment - 64 {
		return error('dtls12: plaintext too large')
	}
	sealed := c.seal_app(dtls12_epoch_application, c.app_write_seq, data)!
	rec := Dtls12Record{
		content_type:    .application_data
		epoch:           dtls12_epoch_application
		sequence_number: c.app_write_seq
		fragment:        sealed
	}
	c.send_record(rec)!
	c.app_write_seq++
	return data.len
}

// write_string encrypts s as application data.
pub fn (mut c Conn) write_string(s string) !int {
	return c.write(s.bytes())
}

// read decrypts one epoch-1 record into buf.
pub fn (mut c Conn) read(mut buf []u8) !int {
	if !c.handshake_done {
		return error('dtls12: handshake not complete')
	}
	for c.read_buf.len == 0 {
		rec, addr := c.recv_record(c.cfg.read_timeout) or { return err }
		if !c.same_peer(addr) {
			continue
		}
		if rec.epoch == dtls12_epoch_handshake {
			continue
		}
		if rec.epoch != dtls12_epoch_application {
			continue
		}
		if rec.content_type != ContentType.application_data {
			continue
		}
		if c.app_replay.is_replay(rec.sequence_number) {
			continue
		}
		plain := c.open_app(rec.epoch, rec.sequence_number, rec.fragment) or { continue }
		if !c.app_replay.mark(rec.sequence_number) {
			continue
		}
		c.read_buf = plain.clone()
		if c.read_buf.len == 0 {
			continue
		}
	}
	n := if buf.len < c.read_buf.len { buf.len } else { c.read_buf.len }
	for i in 0 .. n {
		buf[i] = c.read_buf[i]
	}
	c.read_buf = c.read_buf[n..].clone()
	return n
}
