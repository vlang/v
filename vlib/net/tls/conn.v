module tls

import net
import time
import crypto.sha256
import crypto.x509
import crypto.subtle

// Conn is a pure-V TLS 1.3 connection over TCP.
pub struct Conn {
mut:
	cfg       Config
	tcp       net.TcpConn
	is_server bool
	closed    bool

	// handshake state
	client_hs      ?ClientHandshake
	handshake_done bool

	// traffic keys
	client_hs_traffic TrafficKeys
	server_hs_traffic TrafficKeys
	client_app        TrafficKeys
	server_app        TrafficKeys

	// record sequence numbers
	write_seq u64
	read_seq  u64

	// read buffer for partial records
	read_buf []u8
}

// dial connects TCP to host:port and returns an unhandshaked TLS Conn.
// dial performs no TLS handshake; call handshake() before read/write.
pub fn dial(host string, port int, cfg Config) !&Conn {
	tcp := net.dial_tcp('${host}:${port}')!
	mut c := &Conn{
		cfg: cfg
		tcp: tcp
	}
	return c
}

// dial connects TCP to host:port on the existing Conn and runs the handshake.
// dial replaces the underlying TCP connection and completes the TLS handshake.
pub fn (mut c Conn) dial(host string, port int) ! {
	tcp := net.dial_tcp('${host}:${port}')!
	c.tcp = tcp
	c.handshake()!
}

// accept wraps an accepted TCP connection as an unhandshaked server-side TLS Conn.
// accept performs no TLS handshake; call handshake() before read/write.
pub fn accept(tcp &net.TcpConn, cfg Config) !&Conn {
	mut c := &Conn{
		cfg:       cfg
		tcp:       *tcp
		is_server: true
	}
	return c
}

// close shuts down the underlying TCP connection.
pub fn (mut c Conn) close() ! {
	if !c.closed {
		mut t := c.tcp
		t.close()!
		c.tcp = t
		c.closed = true
	}
}

// write_all writes all bytes.
fn (mut c Conn) write_all(data []u8) ! {
	mut off := 0
	for off < data.len {
		mut t := c.tcp
		n := t.write(data[off..])!
		c.tcp = t
		if n <= 0 {
			return error('tls: short write')
		}
		off += n
	}
}

// read_exact reads exactly n bytes.
fn (mut c Conn) read_exact(n int) ![]u8 {
	mut out := []u8{cap: n}
	for out.len < n {
		mut tmp := []u8{len: n - out.len}
		mut t := c.tcp
		rn := t.read(mut tmp)!
		c.tcp = t
		if rn <= 0 {
			return error('tls: unexpected EOF')
		}
		out << tmp[..rn]
	}
	return out
}

// read_record reads one TLS record. Returns content type + plaintext body.
// read_record enforces the RFC 8446 §5.1 size cap and the 0x0303 legacy version.
fn (mut c Conn) read_record() !(ContentType, []u8) {
	hdr := c.read_exact(5)!
	if hdr[0] != u8(ContentType.change_cipher_spec) && hdr[0] != u8(ContentType.alert)
		&& hdr[0] != u8(ContentType.handshake) && hdr[0] != u8(ContentType.application_data) {
		return error('tls: invalid outer content type')
	}
	ct := unsafe { ContentType(hdr[0]) }
	ver := (u16(hdr[1]) << 8) | u16(hdr[2])
	if ver != tls_version_1_3 {
		return error('tls: invalid legacy version')
	}
	ln := (int(hdr[3]) << 8) | int(hdr[4])
	if ct != .application_data {
		// RFC 8446 §5.1: TLSPlaintext fragments are capped at 2^14.
		if ln > max_plaintext_len {
			return error('tls: record too large')
		}
	} else if ln > max_ciphertext_len {
		return error('tls: record too large')
	}
	body := c.read_exact(ln)!
	if ct != .application_data {
		return ct, body
	}
	cr := CipherRecord{
		content_type:   .application_data
		legacy_version: tls_version_1_3
		fragment:       body
	}
	mut key := []u8{}
	mut iv := []u8{}
	if c.handshake_done {
		if c.is_server {
			key = c.client_app.key.clone()
			iv = c.client_app.iv.clone()
		} else {
			key = c.server_app.key.clone()
			iv = c.server_app.iv.clone()
		}
	} else {
		if c.is_server {
			key = c.client_hs_traffic.key.clone()
			iv = c.client_hs_traffic.iv.clone()
		} else {
			key = c.server_hs_traffic.key.clone()
			iv = c.server_hs_traffic.iv.clone()
		}
	}
	inner_ct, plain := open_record(key, iv, c.read_seq, cr) or { return err }
	c.read_seq++
	return inner_ct, plain
}

// write_plain_record writes a plaintext record.
fn (mut c Conn) write_plain_record(ct ContentType, data []u8) ! {
	rec := PlainRecord{
		content_type: ct
		version:      tls_version_1_3
		fragment:     data
	}
	c.write_all(encode_plain_record(rec))!
}

// write_cipher_record encrypts and writes a record.
fn (mut c Conn) write_cipher_record(ct ContentType, data []u8) ! {
	mut key := []u8{}
	mut iv := []u8{}
	if c.is_server {
		if c.handshake_done {
			key = c.server_app.key.clone()
			iv = c.server_app.iv.clone()
		} else {
			key = c.server_hs_traffic.key.clone()
			iv = c.server_hs_traffic.iv.clone()
		}
	} else {
		if c.handshake_done {
			key = c.client_app.key.clone()
			iv = c.client_app.iv.clone()
		} else {
			key = c.client_hs_traffic.key.clone()
			iv = c.client_hs_traffic.iv.clone()
		}
	}
	rec_bytes := seal_record(key, iv, c.write_seq, ct, data) or { return err }
	c.write_all(rec_bytes)!
	c.write_seq++
}

// shutdown closes the connection (net.ssl compatibility adapter).
// shutdown closes the TLS connection without sending close_notify.
pub fn (mut c Conn) shutdown() ! {
	c.close()!
}

// set_read_timeout is a no-op placeholder for net.ssl compatibility.
pub fn (mut c Conn) set_read_timeout(_ time.Duration) {
	// no-op for now
}

// read_timeout returns the configured read timeout (always 0: no timeout).
pub fn (c Conn) read_timeout() time.Duration {
	return 0
}

// negotiated_alpn returns the negotiated ALPN protocol (always empty in v1).
pub fn (c Conn) negotiated_alpn() string {
	return ''
}

// socket_read_into_ptr reads decrypted data into a raw pointer (net.ssl compatibility).
pub fn (mut c Conn) socket_read_into_ptr(buf &u8, bufsize int) !int {
	mut tmp := []u8{len: bufsize}
	n := c.read(mut tmp)!
	if n > 0 {
		for i in 0 .. n {
			unsafe {
				buf[i] = tmp[i]
			}
		}
	}
	return n
}

// handshake performs the TLS 1.3 handshake (client or server based on is_server).
// handshake must be called after dial/accept and before read/write.
pub fn (mut c Conn) handshake() ! {
	if c.is_server {
		mut shs := new_server_handshake_with_config(c.cfg.pq_mode, c.cfg) or { return err }

		ct, body := c.read_record() or { return err }
		if ct != .handshake {
			return error('tls: expected handshake, got ${ct}')
		}
		flight := shs.process_client_hello(body) or { return err }

		mut off := 0
		mut msgs := [][]u8{}
		for off < flight.len {
			parsed, n := parse_handshake_message(flight[off..]) or { return err }
			_ = parsed
			msgs << flight[off..off + n]
			off += n
		}
		c.server_hs_traffic = derive_traffic_keys(shs.handshake_keys.server_secret) or { return err }
		c.client_hs_traffic = derive_traffic_keys(shs.handshake_keys.client_secret) or { return err }

		c.write_plain_record(.handshake, msgs[0])!
		for i in 1 .. msgs.len {
			c.write_cipher_record(.handshake, msgs[i])!
		}

		fct, fbody := c.read_record() or { return err }
		if fct != .handshake {
			return error('tls: expected handshake, got ${fct}')
		}
		shs.verify_client_finished(fbody) or { return err }

		c.server_app = derive_traffic_keys(shs.app_keys.server_secret) or { return err }
		c.client_app = derive_traffic_keys(shs.app_keys.client_secret) or { return err }
		// Each traffic secret has an independent record sequence number.
		c.read_seq = 0
		c.write_seq = 0
		c.handshake_done = true
		return
	}
	mut chs := new_client_handshake(c.cfg.server_name, c.cfg.alpn_protocols, c.cfg.pq_mode) or { return err }

	ch_msg := chs.build_client_hello() or { return err }
	c.write_plain_record(.handshake, ch_msg)!

	ct, body := c.read_record() or { return err }
	if ct != .handshake {
		return error('tls: expected handshake, got ${ct}')
	}
	chs.process_server_hello(body) or { return err }
	c.client_hs_traffic = chs.client_hs_traffic
	c.server_hs_traffic = chs.server_hs_traffic

	mut server_finished := false
	mut hs_records := 0
	for !server_finished {
		hs_records++
		// Bound the handshake flight: a legitimate flight is a handful of
		// records, so a peer streaming endless handshake records is attacking.
		if hs_records > 32 {
			return error('tls: too many handshake records')
		}
		ict, ibody := c.read_record() or { return err }
		if ict != .handshake {
			return error('tls: expected handshake, got ${ict}')
		}
		// One record may carry several handshake messages (e.g. EE+Cert+CV
		// coalesced by the peer); walk them all so the transcript stays exact.
		mut ioff := 0
		for ioff < ibody.len {
			parsed, n := parse_handshake_message(ibody[ioff..]) or { return err }
			msg_raw := ibody[ioff..ioff + n]
			match parsed.typ {
				.encrypted_extensions {}
				.certificate {
					if c.cfg.verify {
						cert_der := extract_cert_der(parsed.body) or { return err }
						cert := x509.parse_der(cert_der) or { return error(error_bad_signature) }
						pk := cert.public_key() or { return error(error_bad_signature) }
						chs.server_cert_key = pk
						if c.cfg.verify_chain {
							now := time.now()
							if now.unix() < cert.not_before.unix() {
								return error(error_chain_untrusted)
							}
							if now.unix() > cert.not_after.unix() {
								return error(error_chain_expired)
							}
							cert.check_host(c.cfg.server_name) or {
								return error(error_hostname_mismatch)
							}
							store := if c.cfg.ca_store.roots.len > 0 {
								c.cfg.ca_store
							} else {
								load_system_roots() or { return error(error_chain_untrusted) }
							}
							mut trusted := store.contains(cert)
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
					}
				}
				.certificate_verify {
					if c.cfg.verify {
						key := chs.server_cert_key or { return error('tls: no server pubkey') }
						transcript_hash := sha256.sum256(chs.transcript)
						content := cv_content(true, transcript_hash)
						verify_certificate_verify(parsed.body, key, content) or { return err }
					}
				}
				.finished {
					fin := parse_finished(parsed.body, sha256.size) or { return err }
					expected := finished_verify_data(chs.handshake_keys.server_secret,
						sha256.sum256(chs.transcript)) or { return err }
					if subtle.constant_time_compare(fin.verify_data, expected) != 1 {
						return error('tls: server Finished mismatch')
					}
					chs.transcript << msg_raw
					server_finished = true
				}
				else {}
			}
			if parsed.typ != .finished {
				chs.transcript << msg_raw
			} else if ioff + n < ibody.len {
				return error('tls: trailing data after Finished')
			}
			ioff += n
		}
	}

	th := sha256.sum256(chs.transcript)
	as_ := derive_application_secrets(chs.handshake_keys.handshake_secret, th) or { return err }
	chs.app_keys = as_
	c.client_hs = chs
	c.client_hs_traffic = chs.client_hs_traffic
	c.server_hs_traffic = chs.server_hs_traffic
	c.client_app = derive_traffic_keys(as_.client_secret) or { return err }
	c.server_app = derive_traffic_keys(as_.server_secret) or { return err }

	cf_verified := finished_verify_data(chs.handshake_keys.client_secret, sha256.sum256(chs.transcript)) or { return err }
	fin_body := encode_handshake_message(.finished, encode_finished(Finished{ verify_data: cf_verified })) or { return err }
	c.write_cipher_record(.handshake, fin_body)!
	// Application traffic keys start a new record-number epoch.
	c.read_seq = 0
	c.write_seq = 0
	c.handshake_done = true
}

// read decrypts one application-data record into buf. The handshake must be done.
// read returns an error on alerts or unexpected handshake records.
pub fn (mut c Conn) read(mut buf []u8) !int {
	if !c.handshake_done {
		return error('tls: handshake not complete')
	}
	if c.read_buf.len == 0 {
		ct, body := c.read_record() or { return err }
		if ct != .application_data {
			if ct == .alert {
				return error('tls: peer alert')
			}
			return error('tls: unexpected record, got ${ct}')
		}
		c.read_buf = body.clone()
	}
	n := if buf.len < c.read_buf.len { buf.len } else { c.read_buf.len }
	for i in 0 .. n {
		buf[i] = c.read_buf[i]
	}
	c.read_buf = c.read_buf[n..].clone()
	return n
}

// write_string encrypts s as application data. The handshake must be done.
pub fn (mut c Conn) write_string(s string) !int {
	if !c.handshake_done {
		return error('tls: handshake not complete')
	}
	c.write_cipher_record(.application_data, s.bytes())!
	return s.len
}

// write encrypts data as application data. The handshake must be done.
pub fn (mut c Conn) write(data []u8) !int {
	if !c.handshake_done {
		return error('tls: handshake not complete')
	}
	c.write_cipher_record(.application_data, data)!
	return data.len
}
