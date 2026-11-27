// Copyright (c) 2026 V project. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// TLS 1.2 client/server connections over TCP (module tls12).
// Standalone: does not import net.tls internals.
module tls12

import net

// Config configures a TLS 1.2 client or server endpoint.
pub struct Config {
pub mut:
	// server_name is sent as SNI and used for verification (client).
	// Empty means "verify against the dialed host" (filled by dial).
	server_name string
pub:
	// skip_verify disables certificate verification (tests only).
	skip_verify bool
	// root_ders holds pinned CA certificates (DER) for chain checks.
	root_ders [][]u8
	// cert_der is the server leaf certificate (DER); chain_ders extras follow.
	cert_der   []u8
	chain_ders [][]u8
	// key_der is the server private key (PKCS#8 DER: RSA or P-256 EC).
	key_der []u8
	// cipher_suites overrides the default offer list (client) or the
	// selectable set (server). Empty means all supported suites.
	cipher_suites []u16
	// resume carries a previous session for abbreviation (client): when
	// resume.id is non-empty the client offers it as the session ID. The
	// server looks it up in the in-memory session cache; on a hit the
	// handshake is abbreviated (no Certificate flight), otherwise the
	// server falls back to a full handshake with a fresh session ID.
	resume Session
}

// Session is a resumable TLS 1.2 session (client holder + server cache
// value): the session ID sent on the wire plus the master secret and the
// negotiated suite needed to derive fresh keys on resumption.
pub struct Session {
pub:
	id     []u8
	master []u8
	suite  u16
}

// Conn is a TLS 1.2 connection over a TCP socket.
pub struct Conn {
mut:
	tcp             net.TcpConn
	is_client       bool
	cfg             Config
	suite           u16
	is_gcm          bool
	is_ccm          bool
	etm_negotiated  bool
	ems_negotiated  bool
	client_random   []u8
	server_random   []u8
	master          []u8
	ecdhe_priv      []u8
	read_encrypted  bool
	write_encrypted bool
	read_seq        u64
	write_seq       u64
	read_gcm_key    []u8
	read_gcm_salt   []u8
	write_gcm_key   []u8
	write_gcm_salt  []u8
	read_mac        []u8
	read_enc        []u8
	write_mac       []u8
	write_enc       []u8
	hs_pending      []u8
	transcript      []u8
	pending         []u8
	closed          bool
	session_id      []u8
	resumed         bool
	client_verify   []u8
	server_verify   []u8
	did_renegotiate bool
}

// dial_tcp connects to host:port and returns the raw TCP connection.
fn dial_tcp(host string, port int) !net.TcpConn {
	t := net.dial_tcp('${host}:${port}')!
	return *t
}

// dial opens a client TLS 1.2 connection (handshake not yet run).
pub fn dial(host string, port int, cfg Config) !&Conn {
	tcp := dial_tcp(host, port)!
	mut ccfg := cfg
	// An empty server_name means "verify against the dialed host".
	if ccfg.server_name == '' {
		ccfg.server_name = host
	}
	return &Conn{
		tcp:       tcp
		is_client: true
		cfg:       ccfg
	}
}

// accept wraps an accepted TCP connection as a server TLS endpoint.
pub fn accept(tcp &net.TcpConn, cfg Config) !&Conn {
	return &Conn{
		tcp: *tcp
		cfg: cfg
	}
}

// handshake runs the TLS 1.2 handshake (client or server by endpoint kind).
pub fn (mut c Conn) handshake() ! {
	if c.is_client {
		client_handshake(mut c)!
	} else {
		server_handshake(mut c)!
	}
}

// cipher_suite returns the negotiated suite ID (0 before handshake).
pub fn (c &Conn) cipher_suite() u16 {
	return c.suite
}

// cipher_suite_name returns the IANA name of the negotiated suite.
pub fn (c &Conn) cipher_suite_name() string {
	return suite_name(c.suite)
}

// session returns the resumable session for this connection (ID, master,
// suite). Empty before a completed handshake. The client feeds it back as
// Config.resume on the next connection to attempt abbreviation.
pub fn (c &Conn) session() Session {
	return Session{
		id:     c.session_id.clone()
		master: c.master.clone()
		suite:  c.suite
	}
}

// did_resume reports whether the handshake was abbreviated (session-ID hit).
pub fn (c &Conn) did_resume() bool {
	return c.resumed
}

// uses_ems reports whether extended_master_secret was negotiated.
pub fn (c &Conn) uses_ems() bool {
	return c.ems_negotiated
}

// uses_etm reports whether encrypt-then-MAC was negotiated (CBC only).
pub fn (c &Conn) uses_etm() bool {
	return c.etm_negotiated
}

// send_fatal sends a fatal alert using the current write state (best effort).
fn (mut c Conn) send_fatal(desc u8) {
	payload := [u8(2), desc]
	if c.write_encrypted {
		sealed := c.seal_record(rectype_alert, payload) or { return }
		write_record(mut c.tcp, rectype_alert, sealed) or { return }
		return
	}
	write_record(mut c.tcp, rectype_alert, payload) or { return }
}

// seal_record protects a record payload with the active write keys.
fn (c &Conn) seal_record(typ u8, payload []u8) ![]u8 {
	if c.is_gcm {
		return gcm_seal(c.write_gcm_key, c.write_gcm_salt, c.write_seq, typ, payload)
	}
	if c.is_ccm {
		return ccm_seal(c.write_gcm_key, c.write_gcm_salt, c.write_seq, typ, payload)
	}
	if c.etm_negotiated {
		return cbc_etm_seal(c.write_mac, c.write_enc, c.write_seq, typ, payload)
	}
	return cbc_seal(c.write_mac, c.write_enc, c.write_seq, typ, payload)
}

// open_record unprotects a record payload with the active read keys.
fn (c &Conn) open_record(typ u8, payload []u8) ![]u8 {
	if c.is_gcm {
		return gcm_open(c.read_gcm_key, c.read_gcm_salt, c.read_seq, typ, payload)
	}
	if c.is_ccm {
		return ccm_open(c.read_gcm_key, c.read_gcm_salt, c.read_seq, typ, payload)
	}
	if c.etm_negotiated {
		return cbc_etm_open(c.read_mac, c.read_enc, c.read_seq, typ, payload)
	}
	return cbc_open(c.read_mac, c.read_enc, c.read_seq, typ, payload)
}

// derive_record_keys splits the key block into the directional keys
// (RFC 5246 §6.3): client_write_MAC, server_write_MAC, client_write_key,
// server_write_key, client_write_IV, server_write_IV.
fn (mut c Conn) derive_record_keys() ! {
	mac_len, key_len, iv_len := suite_key_lengths(c.suite)
	total := 2 * mac_len + 2 * key_len + 2 * iv_len
	block := key_expansion(c.master, c.server_random, c.client_random, total)
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
	if c.is_client {
		c.read_mac = server_mac
		c.read_enc = server_key
		c.read_gcm_key = server_key
		c.read_gcm_salt = server_iv
		c.write_mac = client_mac
		c.write_enc = client_key
		c.write_gcm_key = client_key
		c.write_gcm_salt = client_iv
	} else {
		c.read_mac = client_mac
		c.read_enc = client_key
		c.read_gcm_key = client_key
		c.read_gcm_salt = client_iv
		c.write_mac = server_mac
		c.write_enc = server_key
		c.write_gcm_key = server_key
		c.write_gcm_salt = server_iv
	}
	c.read_seq = 0
	c.write_seq = 0
}

// write_hs_msg frames a handshake message, appends it to the transcript
// and writes it (sealed when the write cipher is active).
fn (mut c Conn) write_hs_msg(typ u8, body []u8) ! {
	msg := hs_msg(typ, body)
	c.transcript << msg
	if c.write_encrypted {
		sealed := c.seal_record(rectype_handshake, msg)!
		write_record(mut c.tcp, rectype_handshake, sealed)!
		c.write_seq++
		return
	}
	write_record(mut c.tcp, rectype_handshake, msg)!
}

// read_hs_msg returns the next handshake message, reassembling split
// records, toggling the read cipher on ChangeCipherSpec, and surfacing
// peer alerts as errors. Returned messages are appended to the transcript.
fn (mut c Conn) read_hs_msg() !(u8, []u8) {
	for {
		if c.hs_pending.len >= 4 {
			ln := get_u24(c.hs_pending[1..4])
			if ln < 0 || ln > 1 << 20 {
				return error('tls12: bad handshake length')
			}
			if c.hs_pending.len >= 4 + ln {
				raw := c.hs_pending[..4 + ln].clone()
				c.hs_pending = c.hs_pending[4 + ln..].clone()
				c.transcript << raw
				return raw[0], raw[4..].clone()
			}
		}
		rtyp, payload := read_record(mut c.tcp)!
		if rtyp == rectype_alert {
			desc := if payload.len >= 2 { payload[1] } else { u8(0) }
			return error('tls12: peer alert ${desc}')
		}
		if rtyp == rectype_change_cipher_spec {
			if payload.len != 1 || payload[0] != 1 {
				return error('tls12: bad ChangeCipherSpec')
			}
			c.read_encrypted = true
			continue
		}
		if rtyp != rectype_handshake {
			return error('tls12: unexpected record ${rtyp} during handshake')
		}
		if c.read_encrypted {
			opened := c.open_record(rtyp, payload)!
			c.read_seq++
			c.hs_pending << opened
		} else {
			c.hs_pending << payload
		}
	}
}

// write sends application data (fragmented to 16 KiB records).
pub fn (mut c Conn) write(data []u8) !int {
	if c.closed {
		return error('tls12: write on closed connection')
	}
	if data.len == 0 {
		return 0
	}
	mut off := 0
	for off < data.len {
		end := if off + max_record_payload < data.len { off + max_record_payload } else { data.len }
		frag := data[off..end]
		sealed := c.seal_record(rectype_application_data, frag)!
		write_record(mut c.tcp, rectype_application_data, sealed)!
		c.write_seq++
		off = end
	}
	return data.len
}

// read reads decrypted application data into buf.
pub fn (mut c Conn) read(mut buf []u8) !int {
	if c.closed && c.pending.len == 0 {
		return error('tls12: read on closed connection')
	}
	for c.pending.len == 0 {
		rtyp, payload := read_record(mut c.tcp)!
		plain := c.open_record(rtyp, payload) or {
			c.send_fatal(alert_bad_record_mac)
			return error('tls12: record open failed: ${err}')
		}
		c.read_seq++
		if rtyp == rectype_alert {
			if plain.len >= 2 && plain[1] == alert_close_notify {
				c.closed = true
				if c.pending.len == 0 {
					return error('tls12: connection closed by peer')
				}
				break
			}
			return error('tls12: peer alert ${if plain.len >= 2 { plain[1] } else { u8(0) }}')
		}
		if rtyp != rectype_application_data {
			c.send_fatal(alert_unexpected_message)
			return error('tls12: unexpected record ${rtyp}')
		}
		c.pending << plain
	}
	n := if buf.len < c.pending.len { buf.len } else { c.pending.len }
	for i in 0 .. n {
		buf[i] = c.pending[i]
	}
	c.pending = c.pending[n..].clone()
	return n
}

// close sends close_notify and closes the TCP connection.
pub fn (mut c Conn) close() ! {
	if !c.closed {
		c.closed = true
		if c.suite != 0 {
			sealed := c.seal_record(rectype_alert, [u8(1), alert_close_notify]) or {
				c.tcp.close() or {}
				return
			}
			write_record(mut c.tcp, rectype_alert, sealed) or {}
		}
	}
	c.tcp.close() or {}
}
