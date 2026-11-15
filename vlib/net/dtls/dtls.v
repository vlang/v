// dtls is a minimal DTLS 1.3 (RFC 9147) profile over UDP (epoch 0/1).
// File-disjoint from net.tls: own Config/Conn, imports tls codecs only.
module dtls

import net
import net.tls
import time
import crypto.rand

// Config holds DTLS settings. Cipher suites default to TLS defaults.
@[params]
pub struct Config {
pub mut:
	server_name       string
	cipher_suites     []tls.CipherSuite
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

// Conn is a DTLS connection over UDP.
pub struct Conn {
mut:
	udp               net.UdpConn
	peer              net.Addr
	has_peer          bool
	is_server         bool
	cfg               Config
	suite             tls.CipherSuite
	app_write_seq     u64
	app_replay        ReplayWindow
	client_app        tls.TrafficKeys
	server_app        tls.TrafficKeys
	handshake_done    bool
	client_random     []u8
	server_random     []u8
	transcript        []u8
	tx_drop_remaining int
	rx_drop_remaining int
	closed            bool
	read_buf          []u8
}

// Listener is a single-client DTLS listener (minimal: one accept per socket).
pub struct Listener {
mut:
	udp   net.UdpConn
	cfg   Config
	moved bool
}

// dial connects a DTLS client to raddr (e.g. '127.0.0.1:4433').
pub fn dial(raddr string, cfg Config) !&Conn {
	ncfg := cfg.normalized()
	mut u := net.dial_udp(raddr)!
	u.set_read_timeout(ncfg.read_timeout)
	u.set_write_timeout(ncfg.write_timeout)
	peer := u.sock.remote() or { return error('dtls: no udp remote') }
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
// accept is single-client: a second call returns an error.
pub fn (mut l Listener) accept() !&Conn {
	if l.moved {
		return error('dtls: listener already accepted')
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

// handshake runs the DTLS handshake (client or server by role).
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

// send_record encodes and transmits one DTLS record to peer.
fn (mut c Conn) send_record(rec DtlsRecord) ! {
	if !c.has_peer {
		return error('dtls: no peer')
	}
	data := encode_dtls_record(rec)!
	if c.tx_drop_remaining > 0 {
		c.tx_drop_remaining--
		return
	}
	mut u := c.udp
	u.write_to(c.peer, data)!
	c.udp = u
}

// recv_record receives one DTLS record with a read timeout.
fn (mut c Conn) recv_record(timeout time.Duration) !(DtlsRecord, net.Addr) {
	mut u := c.udp
	u.set_read_timeout(timeout)
	c.udp = u
	mut buf := []u8{len: dtls_max_datagram}
	mut uu := c.udp
	n, addr := uu.read(mut buf)!
	c.udp = uu
	if c.rx_drop_remaining > 0 {
		c.rx_drop_remaining--
		return error('dtls: simulated rx loss')
	}
	rec := decode_dtls_record(buf[..n])!
	return rec, addr
}

// same_peer reports whether addr matches the known peer.
fn (c &Conn) same_peer(addr net.Addr) bool {
	if !c.has_peer {
		return true
	}
	return c.peer.str() == addr.str()
}

// handshake_client runs the client side (CH -> SH/SFin -> CFin).
fn (mut c Conn) handshake_client() ! {
	cfg := c.cfg
	suites := effective_suites(cfg)
	client_random := rand.bytes(32)!
	c.client_random = client_random.clone()
	ch_msg := build_client_hello_msg(client_random, suites)!
	mut transcript := ch_msg.clone()
	ch_rec := DtlsRecord{
		content_type:    tls.ContentType.handshake
		epoch:           dtls_epoch_handshake
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
		// collect SH if missing
		if sh_msg.len == 0 {
			rec, addr := c.recv_record(rto) or {
				if is_timeout(err) || err.msg().contains('simulated') || err.msg().contains('dtls:') {
					attempts++
					continue
				}
				return err
			}
			if !c.same_peer(addr) {
				continue
			}
			if rec.epoch != dtls_epoch_handshake || rec.content_type != tls.ContentType.handshake {
				continue
			}
			if rec.sequence_number != 0 {
				continue
			}
			parsed, _ := tls.parse_handshake_message(rec.fragment) or { continue }
			if parsed.typ != tls.HandshakeType.server_hello {
				continue
			}
			sr, sel := parse_server_hello_msg(rec.fragment) or { continue }
			if sel !in suites {
				return error('dtls: server selected unsupported suite')
			}
			server_random = sr.clone()
			suite = sel
			sh_msg = rec.fragment.clone()
			transcript = ch_msg.clone()
			transcript << sh_msg
			// fall through to wait for SFin with a fresh RTO
			rec2, addr2 := c.recv_record(rto) or {
				if is_timeout(err) || err.msg().contains('simulated') || err.msg().contains('dtls:') {
					attempts++
					continue
				}
				return err
			}
			if !c.same_peer(addr2) {
				continue
			}
			if rec2.epoch != dtls_epoch_handshake || rec2.content_type != tls.ContentType.handshake {
				continue
			}
			if rec2.sequence_number != 1 {
				continue
			}
			parsed2, _ := tls.parse_handshake_message(rec2.fragment) or { continue }
			if parsed2.typ != tls.HandshakeType.finished {
				continue
			}
			sfin_msg = rec2.fragment.clone()
			transcript << sfin_msg
			break
		} else {
			// SH already known, only SFin missing
			rec2, addr2 := c.recv_record(rto) or {
				if is_timeout(err) || err.msg().contains('simulated') || err.msg().contains('dtls:') {
					attempts++
					continue
				}
				return err
			}
			if !c.same_peer(addr2) {
				continue
			}
			if rec2.epoch != dtls_epoch_handshake || rec2.content_type != tls.ContentType.handshake {
				continue
			}
			if rec2.sequence_number != 1 {
				continue
			}
			parsed2, _ := tls.parse_handshake_message(rec2.fragment) or { continue }
			if parsed2.typ != tls.HandshakeType.finished {
				continue
			}
			sfin_msg = rec2.fragment.clone()
			transcript << sfin_msg
			break
		}
	}
	if sh_msg.len == 0 || sfin_msg.len == 0 {
		return error('dtls: handshake timeout waiting for server flight')
	}
	mut ch_sh := ch_msg.clone()
	ch_sh << sh_msg
	hs := derive_handshake_secrets(suite, ch_sh.clone(), client_random, server_random)!
	verify_finished_msg(sfin_msg, hs.server_secret, ch_sh.clone(), suite)!
	c.suite = suite
	c.server_random = server_random.clone()
	mut transcript_up_to_sfin := ch_msg.clone()
	transcript_up_to_sfin << sh_msg
	transcript_up_to_sfin << sfin_msg
	client_app, server_app := derive_app_traffic(suite, hs.handshake_secret, transcript_up_to_sfin)!
	c.client_app = client_app
	c.server_app = server_app
	cfin_msg := build_finished_msg(hs.client_secret, transcript_up_to_sfin, suite)!
	cfin_rec := DtlsRecord{
		content_type:    tls.ContentType.handshake
		epoch:           dtls_epoch_handshake
		sequence_number: 1
		fragment:        cfin_msg
	}
	c.transcript = transcript_up_to_sfin.clone()
	c.transcript << cfin_msg
	// send CFin and linger for duplicate SH/SFin (CFin loss indicator)
	c.send_record(cfin_rec)!
	mut linger := 0
	linger_rto := time.Duration(i64(cfg.initial_rto_ms) * 1_000_000)
	for linger < 3 {
		rec, _ := c.recv_record(linger_rto) or {
			if is_timeout(err) || err.msg().contains('simulated') {
				break
			}
			if err.msg().contains('dtls:') {
				linger++
				continue
			}
			break
		}
		if rec.epoch == dtls_epoch_handshake && rec.content_type == tls.ContentType.handshake {
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

// handshake_server runs the server side (CH -> SH/SFin -> CFin).
fn (mut c Conn) handshake_server() ! {
	cfg := c.cfg
	server_prefs := effective_suites(cfg)
	// wait for ClientHello
	mut ch_msg := []u8{}
	mut client_random := []u8{}
	mut offered := []tls.CipherSuite{}
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
		if rec.epoch != dtls_epoch_handshake || rec.content_type != tls.ContentType.handshake {
			continue
		}
		if rec.sequence_number != 0 {
			continue
		}
		parsed, _ := tls.parse_handshake_message(rec.fragment) or { continue }
		if parsed.typ != tls.HandshakeType.client_hello {
			continue
		}
		cr, suites := parse_client_hello_msg(rec.fragment) or { continue }
		client_random = cr.clone()
		offered = suites.clone()
		ch_msg = rec.fragment.clone()
		ch_addr = addr
		got_ch = true
	}
	if !got_ch {
		return error('dtls: handshake timeout waiting for ClientHello')
	}
	c.peer = ch_addr
	c.has_peer = true
	c.client_random = client_random.clone()
	suite := select_suite(offered, server_prefs)!
	c.suite = suite
	server_random := rand.bytes(32)!
	c.server_random = server_random.clone()
	sh_msg := build_server_hello_msg(server_random, suite)!
	mut transcript_ch_sh := ch_msg.clone()
	transcript_ch_sh << sh_msg
	hs := derive_handshake_secrets(suite, transcript_ch_sh.clone(), client_random, server_random)!
	sfin_msg := build_finished_msg(hs.server_secret, transcript_ch_sh.clone(), suite)!
	mut transcript_up_to_sfin := ch_msg.clone()
	transcript_up_to_sfin << sh_msg
	transcript_up_to_sfin << sfin_msg
	c.transcript = transcript_up_to_sfin.clone()
	sh_rec := DtlsRecord{
		content_type:    tls.ContentType.handshake
		epoch:           dtls_epoch_handshake
		sequence_number: 0
		fragment:        sh_msg
	}
	sfin_rec := DtlsRecord{
		content_type:    tls.ContentType.handshake
		epoch:           dtls_epoch_handshake
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
			if is_timeout(err) || err.msg().contains('simulated') || err.msg().contains('dtls:') {
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
		if rec.epoch != dtls_epoch_handshake || rec.content_type != tls.ContentType.handshake {
			continue
		}
		if rec.sequence_number == 0 {
			// duplicate CH: resend flight
			c.send_record(sh_rec) or {}
			c.send_record(sfin_rec) or {}
			continue
		}
		if rec.sequence_number != 1 {
			continue
		}
		parsed, _ := tls.parse_handshake_message(rec.fragment) or { continue }
		if parsed.typ != tls.HandshakeType.finished {
			continue
		}
		verify_finished_msg(rec.fragment, hs.client_secret, transcript_up_to_sfin, suite) or { continue }
		c.transcript << rec.fragment
		client_app, server_app := derive_app_traffic(suite, hs.handshake_secret, transcript_up_to_sfin)!
		c.client_app = client_app
		c.server_app = server_app
		c.handshake_done = true
		c.app_replay.reset()
		return
	}
	return error('dtls: handshake timeout waiting for client Finished')
}

// write encrypts data as one epoch-1 application record.
pub fn (mut c Conn) write(data []u8) !int {
	if !c.handshake_done {
		return error('dtls: handshake not complete')
	}
	if data.len == 0 {
		return error('dtls: empty write')
	}
	if data.len > dtls_max_fragment - 64 {
		return error('dtls: plaintext too large')
	}
	mut key := []u8{}
	mut iv := []u8{}
	if c.is_server {
		key = c.server_app.key.clone()
		iv = c.server_app.iv.clone()
	} else {
		key = c.client_app.key.clone()
		iv = c.client_app.iv.clone()
	}
	sealed := tls.seal_record(c.suite, key, iv, c.app_write_seq, tls.ContentType.application_data,
		data)!
	rec := DtlsRecord{
		content_type:    tls.ContentType.application_data
		epoch:           dtls_epoch_application
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
		return error('dtls: handshake not complete')
	}
	for c.read_buf.len == 0 {
		rec, addr := c.recv_record(c.cfg.read_timeout) or { return err }
		if !c.same_peer(addr) {
			continue
		}
		if rec.epoch == dtls_epoch_handshake {
			// ignore stale handshake retransmits after completion
			continue
		}
		if rec.epoch != dtls_epoch_application {
			continue
		}
		if rec.content_type != tls.ContentType.application_data {
			continue
		}
		if c.app_replay.is_replay(rec.sequence_number) {
			continue
		}
		crec, _ := tls.decode_cipher_record(rec.fragment) or { continue }
		mut key := []u8{}
		mut iv := []u8{}
		if c.is_server {
			key = c.client_app.key.clone()
			iv = c.client_app.iv.clone()
		} else {
			key = c.server_app.key.clone()
			iv = c.server_app.iv.clone()
		}
		_, plain := tls.open_record(c.suite, key, iv, rec.sequence_number, crec) or { continue }
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
