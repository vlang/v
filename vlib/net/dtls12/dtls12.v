// Copyright (c) 2026 V project. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// dtls12 is a minimal DTLS 1.2 (RFC 6347) profile over UDP (epoch 0/1).
// File-disjoint from net.tls and net.tls12: own Config/Conn, own record and
// handshake codecs over the TLS 1.2 PRF with the GCM + CBC+HMAC suite subset.
// Cookie exchange (HelloVerifyRequest) is optional and disabled by default:
// enable it with Config.enable_cookie to challenge the first ClientHello.
// The cookie here is a random per-handshake value held in memory (not a
// stateless HMAC); the deployment is single-process like the TLS 1.2
// session-ID cache precedent.
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
	// amplification. The cookie is random per handshake (in-memory).
	enable_cookie     bool
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
	udp               net.UdpConn
	peer              net.Addr
	has_peer          bool
	is_server         bool
	cfg               Config
	suite             u16
	is_gcm            bool
	client_random     []u8
	server_random     []u8
	master            []u8
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
	udp   net.UdpConn
	cfg   Config
	moved bool
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
		udp: u
		cfg: ncfg
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
	typ := u8(ContentType.application_data)
	if c.is_server {
		if c.is_gcm {
			return gcm_seal(c.server_keys.key, c.server_keys.iv, epoch, seq, typ,
				plaintext)
		}
		return cbc_seal(c.server_keys.mac, c.server_keys.key, epoch, seq, typ, plaintext)
	}
	if c.is_gcm {
		return gcm_seal(c.client_keys.key, c.client_keys.iv, epoch, seq, typ, plaintext)
	}
	return cbc_seal(c.client_keys.mac, c.client_keys.key, epoch, seq, typ, plaintext)
}

// open_app unprotects application data with the read keys.
fn (c &Conn) open_app(epoch u16, seq u64, sealed []u8) ![]u8 {
	typ := u8(ContentType.application_data)
	if c.is_server {
		if c.is_gcm {
			return gcm_open(c.client_keys.key, c.client_keys.iv, epoch, seq, typ, sealed)
		}
		return cbc_open(c.client_keys.mac, c.client_keys.key, epoch, seq, typ, sealed)
	}
	if c.is_gcm {
		return gcm_open(c.server_keys.key, c.server_keys.iv, epoch, seq, typ, sealed)
	}
	return cbc_open(c.server_keys.mac, c.server_keys.key, epoch, seq, typ, sealed)
}

// handshake_client runs the client side (CH [cookie] -> SH/SFin -> CFin,
// with optional HelloVerifyRequest round-trip when the server enables it).
fn (mut c Conn) handshake_client() ! {
	cfg := c.cfg
	suites := effective_suites(cfg.cipher_suites)
	client_random := rand.bytes(32)!
	c.client_random = client_random.clone()
	mut cookie := []u8{}
	mut ch_msg := build_client_hello_msg(client_random, suites, cookie)!
	mut transcript := ch_msg.clone()
	mut ch_rec := Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_handshake
		sequence_number: 0
		fragment:        ch_msg
	}
	mut sh_msg := []u8{}
	mut sfin_msg := []u8{}
	mut server_random := []u8{}
	mut suite := suites[0]
	mut timer := new_retransmit_timer(cfg.initial_rto_ms, cfg.max_rto_ms)
	mut attempts := 0
	for attempts <= cfg.max_retries {
		if sh_msg.len == 0 || sfin_msg.len == 0 {
			c.send_record(ch_rec)!
		}
		rto := timer.next_timeout()
		if sh_msg.len == 0 {
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
			// Optional cookie exchange: server challenges with
			// HelloVerifyRequest; client resends ClientHello with cookie.
			if rec.sequence_number == 0 {
				cookie_candidate := parse_hello_verify_request(rec.fragment) or { []u8{} }
				if cookie_candidate.len != 0 {
					cookie = cookie_candidate.clone()
					ch_msg = build_client_hello_msg(client_random, suites, cookie)!
					transcript = ch_msg.clone()
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
			}
			if rec.sequence_number != 0 {
				continue
			}
			sr, sel := parse_server_hello_msg(rec.fragment) or { continue }
			if sel !in suites {
				return error('dtls12: server selected unsupported suite')
			}
			server_random = sr.clone()
			suite = sel
			sh_msg = rec.fragment.clone()
			transcript = ch_msg.clone()
			transcript << sh_msg
			rec2, addr2 := c.recv_record(rto) or {
				if is_timeout(err) || err.msg().contains('simulated') || err.msg().contains('dtls12:') {
					attempts++
					continue
				}
				return err
			}
			if !c.same_peer(addr2) {
				continue
			}
			if rec2.epoch != dtls12_epoch_handshake
				|| rec2.content_type != ContentType.handshake {
				continue
			}
			if rec2.sequence_number != 1 {
				continue
			}
			sfin_msg = rec2.fragment.clone()
			transcript << sfin_msg
			break
		} else {
			rec2, addr2 := c.recv_record(rto) or {
				if is_timeout(err) || err.msg().contains('simulated') || err.msg().contains('dtls12:') {
					attempts++
					continue
				}
				return err
			}
			if !c.same_peer(addr2) {
				continue
			}
			if rec2.epoch != dtls12_epoch_handshake
				|| rec2.content_type != ContentType.handshake {
				continue
			}
			if rec2.sequence_number != 1 {
				continue
			}
			sfin_msg = rec2.fragment.clone()
			transcript << sfin_msg
			break
		}
	}
	if sh_msg.len == 0 || sfin_msg.len == 0 {
		return error('dtls12: handshake timeout waiting for server flight')
	}
	master := derive_master(client_random, server_random)!
	c.master = master.clone()
	c.suite = suite
	c.is_gcm = suite_is_gcm(suite)
	c.server_random = server_random.clone()
	// Verify server Finished over CH+SH (transcript up to SFin exclusive of SFin?).
	// Our transcript currently is CH+SH+SFin; verify over CH+SH+SFin prefix?
	// For symmetry with the server, verify SFin over CH+SH.
	mut ch_sh := ch_msg.clone()
	ch_sh << sh_msg
	verify_finished_msg(sfin_msg, master, 'server finished', ch_sh)!
	client_app, server_app := derive_traffic(suite, master, server_random, client_random)!
	c.client_keys = client_app
	c.server_keys = server_app
	mut transcript_up_to_sfin := ch_msg.clone()
	transcript_up_to_sfin << sh_msg
	transcript_up_to_sfin << sfin_msg
	cfin_msg := build_finished_msg(master, 'client finished', transcript_up_to_sfin)!
	cfin_rec := Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_handshake
		sequence_number: 1
		fragment:        cfin_msg
	}
	c.transcript = transcript_up_to_sfin.clone()
	c.transcript << cfin_msg
	c.send_record(cfin_rec)!
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
		if rec.epoch == dtls12_epoch_handshake
			&& rec.content_type == ContentType.handshake {
			if rec.sequence_number == 0 || rec.sequence_number == 1 {
				c.send_record(cfin_rec) or {}
				linger++
				continue
			}
		}
		linger++
	}
	c.handshake_done = true
	c.app_replay.reset()
}

// handshake_server runs the server side (CH [cookie] -> SH/SFin -> CFin).
fn (mut c Conn) handshake_server() ! {
	cfg := c.cfg
	server_prefs := effective_suites(cfg.cipher_suites)
	mut ch_msg := []u8{}
	mut client_random := []u8{}
	mut offered := []u16{}
	mut cookie := []u8{}
	mut ch_addr := net.Addr{}
	mut got_ch := false
	mut u := c.udp
	u.set_read_timeout(cfg.handshake_timeout)
	c.udp = u
	// Optional cookie exchange state: challenge cookie for this handshake.
	mut challenge_cookie := []u8{}
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
		if cfg.enable_cookie && ck.len == 0 {
			// Challenge with a fresh random cookie (in-memory, per handshake).
			challenge_cookie = rand.bytes(16)!
			hvr := build_hello_verify_request(challenge_cookie)!
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
		if cfg.enable_cookie && ck.len != 0 {
			if challenge_cookie.len == 0 || ck != challenge_cookie {
				continue
			}
		}
		client_random = cr.clone()
		offered = suites.clone()
		cookie = ck.clone()
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
	sh_msg := build_server_hello_msg(server_random, suite)!
	mut transcript_ch_sh := ch_msg.clone()
	transcript_ch_sh << sh_msg
	master := derive_master(client_random, server_random)!
	c.master = master.clone()
	// Server Finished covers CH+SH.
	sfin_msg := build_finished_msg(master, 'server finished', transcript_ch_sh.clone())!
	mut transcript_up_to_sfin := ch_msg.clone()
	transcript_up_to_sfin << sh_msg
	transcript_up_to_sfin << sfin_msg
	_ = cookie
	c.transcript = transcript_up_to_sfin.clone()
	sh_rec := Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_handshake
		sequence_number: 0
		fragment:        sh_msg
	}
	sfin_rec := Dtls12Record{
		content_type:    .handshake
		epoch:           dtls12_epoch_handshake
		sequence_number: 1
		fragment:        sfin_msg
	}
	c.send_record(sh_rec)!
	c.send_record(sfin_rec)!
	mut timer := new_retransmit_timer(cfg.initial_rto_ms, cfg.max_rto_ms)
	mut attempts := 0
	for attempts <= cfg.max_retries {
		rto := timer.next_timeout()
		rec, addr := c.recv_record(rto) or {
			if is_timeout(err) || err.msg().contains('simulated') || err.msg().contains('dtls12:') {
				c.send_record(sh_rec) or {}
				c.send_record(sfin_rec) or {}
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
			c.send_record(sfin_rec) or {}
			continue
		}
		if rec.sequence_number != 1 {
			continue
		}
		verify_finished_msg(rec.fragment, master, 'client finished', transcript_up_to_sfin) or {
			continue
		}
		c.transcript << rec.fragment
		client_app, server_app := derive_traffic(suite, master, server_random, client_random)!
		c.client_keys = client_app
		c.server_keys = server_app
		c.handshake_done = true
		c.app_replay.reset()
		return
	}
	return error('dtls12: handshake timeout waiting for client Finished')
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
