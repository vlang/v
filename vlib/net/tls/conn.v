module tls

import crypto.rand
import crypto.subtle
import crypto.x509
import net
import os
import time

// Conn is a pure-V TLS 1.3 connection over TCP.
pub struct Conn {
mut:
	cfg       Config
	tcp       net.TcpConn
	is_server bool
	closed    bool
	// Negotiated cipher suite (zero value selects AES-128-GCM).
	suite CipherSuite

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

	// resumption state (client side): ticket received via NewSessionTicket.
	resumption SessionState
	has_ticket bool
	// resumed reports whether this handshake authenticated with a PSK.
	resumed bool
	// resumption_master is derived at handshake completion (transcript
	// including client Finished); each NewSessionTicket nonce derives a PSK.
	resumption_master     []u8
	has_resumption_master bool
	// early_buf holds 0-RTT plaintext accepted by the server, delivered to
	// the application before any post-handshake records.
	early_buf []u8
	// stashed holds one decrypted handshake flight pushed back by the
	// early-data drain (the client's Finished, or its auth flight), so the
	// regular handshake readers see an uninterrupted message stream.
	stashed     []u8
	has_stashed bool
	// Negotiated ALPN protocol (empty when none).
	negotiated_alpn_str string
	// Stapled OCSP response (raw, transport only, never verified here).
	ocsp_resp []u8
	// Application traffic secrets (for KeyUpdate) and exporter/master
	// secrets (for exporter() and SSLKEYLOGFILE).
	client_app_secret []u8
	server_app_secret []u8
	exporter_master   []u8
	master_secret     []u8
	client_random     []u8
	server_random     []u8
	// Full-handshake transcript through the client Finished (base for
	// post-handshake authentication).
	hs_transcript []u8
	// Post-handshake auth: server-side pending request context.
	pha_pending_ctx []u8
	has_pha_pending bool
	// PHA client Certificate body awaiting its CertificateVerify.
	pha_cert_body []u8
	has_pha_cert  bool
	// ku_count bounds post-handshake KeyUpdates per connection (DoS cap;
	// each update forces two HKDF schedules plus a reply when requested).
	ku_count int
}

// dial connects TCP to host:port and returns an unhandshaked TLS Conn.
// dial performs no TLS handshake; call handshake() before read/write.
pub fn dial(host string, port int, cfg Config) !&Conn {
	tcp := net.dial_tcp('${host}:${port}')!
	mut ccfg := cfg
	// An empty server_name means "verify against the dialed host": without
	// this default any valid chain (for any name) would be accepted.
	if ccfg.server_name == '' {
		ccfg.server_name = host
	}
	mut c := &Conn{
		cfg: ccfg
		tcp: tcp
	}
	return c
}

// dial connects TCP to host:port on the existing Conn and runs the handshake.
// dial replaces the underlying TCP connection and completes the TLS handshake.
pub fn (mut c Conn) dial(host string, port int) ! {
	tcp := net.dial_tcp('${host}:${port}')!
	c.tcp = tcp
	if c.cfg.server_name == '' {
		c.cfg.server_name = host
	}
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
// read_record enforces the RFC 8446 §5.1 size cap. Plaintext handshake records
// accept legacy versions 0x0301..0x0303: peers (e.g. OpenSSL) send the initial
// ClientHello with 0x0301, and RFC 8446 §5.1 allows either for that flight.
fn (mut c Conn) read_record() !(ContentType, []u8) {
	hdr := c.read_exact(5)!
	if hdr[0] != u8(ContentType.change_cipher_spec) && hdr[0] != u8(ContentType.alert)
		&& hdr[0] != u8(ContentType.handshake) && hdr[0] != u8(ContentType.application_data) {
		return error('tls: invalid outer content type')
	}
	ct := unsafe { ContentType(hdr[0]) }
	ver := (u16(hdr[1]) << 8) | u16(hdr[2])
	if ct == .application_data {
		if ver != tls_version_1_3 {
			return error('tls: invalid legacy version')
		}
	} else if ver != tls_version_1_3 && ver != 0x0301 && ver != 0x0302 {
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
	inner_ct, plain := open_record(c.suite, key, iv, c.read_seq, cr) or { return err }
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
	rec_bytes := seal_record(c.suite, key, iv, c.write_seq, ct, data) or { return err }
	c.write_all(rec_bytes)!
	c.write_seq++
}

// shutdown closes the connection (net.ssl compatibility adapter).
// shutdown closes the TLS connection without sending close_notify.
pub fn (mut c Conn) shutdown() ! {
	c.close()!
}

// send_close_notify transmits a warning close_notify alert without
// closing the socket. Call it before close() when the peer (e.g.
// OpenSSL) treats a bare TCP FIN as truncation.
pub fn (mut c Conn) send_close_notify() ! {
	if c.closed || !c.handshake_done {
		return
	}
	c.write_cipher_fragmented(.alert, alert_bytes(.warning, .close_notify))!
}

// set_read_timeout is a no-op placeholder for net.ssl compatibility.
pub fn (mut c Conn) set_read_timeout(_ time.Duration) {
	// no-op for now
}

// read_timeout returns the configured read timeout (always 0: no timeout).
pub fn (c Conn) read_timeout() time.Duration {
	return 0
}

// negotiated_alpn returns the negotiated ALPN protocol (empty when none).
pub fn (c Conn) negotiated_alpn() string {
	return c.negotiated_alpn_str
}

// ocsp_response returns the stapled OCSP response (raw bytes, transport
// only). Empty when the server sent none. Never verified here.
pub fn (c Conn) ocsp_response() []u8 {
	return c.ocsp_resp.clone()
}

// exporter derives application traffic keying material (RFC 8446 §7.5)
// from the exporter master secret: label + context hash + length.
pub fn (c &Conn) exporter(label string, context []u8, length int) ![]u8 {
	if !c.handshake_done {
		return error('tls: handshake not complete')
	}
	if c.exporter_master.len == 0 {
		return error('tls: no exporter secret')
	}
	return tls_exporter(c.exporter_master, label, context, length, c.suite)
}

// rekey initiates a KeyUpdate (RFC 8446 §4.6.3): a KeyUpdate
// (update_not_requested) is sent under the current sending keys, then the
// sending traffic secret is advanced and the sequence reset. The peer
// rotates its receiving keys on receipt (answering only if requested).
pub fn (mut c Conn) rekey() ! {
	if !c.handshake_done {
		return error('tls: handshake not complete')
	}
	ku := encode_handshake_message(.key_update, encode_key_update(false))!
	c.write_cipher_record(.handshake, ku)!
	if c.is_server {
		c.server_app_secret = update_traffic_secret(c.server_app_secret, c.suite)!
		c.server_app = derive_traffic_keys(c.server_app_secret, c.suite)!
		c.write_seq = 0
	} else {
		c.client_app_secret = update_traffic_secret(c.client_app_secret, c.suite)!
		c.client_app = derive_traffic_keys(c.client_app_secret, c.suite)!
		c.write_seq = 0
	}
}

// send_key_update_answer sends a KeyUpdate with update_not_requested and
// rotates our sending keys. Used by the KeyUpdate responder path.
fn (mut c Conn) send_key_update_answer() ! {
	ku := encode_handshake_message(.key_update, encode_key_update(false))!
	c.write_cipher_record(.handshake, ku)!
	if c.is_server {
		c.server_app_secret = update_traffic_secret(c.server_app_secret, c.suite)!
		c.server_app = derive_traffic_keys(c.server_app_secret, c.suite)!
		c.write_seq = 0
	} else {
		c.client_app_secret = update_traffic_secret(c.client_app_secret, c.suite)!
		c.client_app = derive_traffic_keys(c.client_app_secret, c.suite)!
		c.write_seq = 0
	}
}

// request_client_auth_post sends a post-handshake CertificateRequest
// (RFC 8446 §4.3.2). The client answers with Certificate(+Verify); the
// server verifies with the existing mTLS path on the next read. No
// traffic keys change.
pub fn (mut c Conn) request_client_auth_post() ! {
	if !c.handshake_done {
		return error('tls: handshake not complete')
	}
	if !c.is_server {
		return error('tls: only servers request post-handshake auth')
	}
	ctx := rand.bytes(32)!
	cr_body := encode_certificate_request_with_context(ctx, signature_algorithms_data())!
	cr_msg := encode_handshake_message(.certificate_request, cr_body)!
	c.hs_transcript << cr_msg
	c.pha_pending_ctx = ctx.clone()
	c.has_pha_pending = true
	c.write_cipher_record(.handshake, cr_msg)!
}

// session_state returns the resumption ticket received on this connection
// (client side only). Pass it as Config.resume_from on the next dial to
// attempt PSK resumption.
pub fn (c &Conn) session_state() ?SessionState {
	if c.is_server || !c.has_ticket {
		return none
	}
	return SessionState{
		ticket:         c.resumption.ticket.clone()
		psk:            c.resumption.psk.clone()
		suite:          c.resumption.suite
		created_unix:   c.resumption.created_unix
		lifetime:       c.resumption.lifetime
		age_add:        c.resumption.age_add
		max_early_data: c.resumption.max_early_data
	}
}

// resumed reports whether this connection resumed via a PSK.
pub fn (c &Conn) resumed() bool {
	return c.resumed
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

// read_handshake_body reads the next non-CCS handshake record body.
// Middlebox-compat peers (e.g. OpenSSL) send dummy ChangeCipherSpec records
// (RFC 8446 §5, appendix D.4); they carry no bytes that affect the transcript
// and must be ignored. A flight stashed by the early-data drain is returned
// first so the handshake sees one continuous message stream.
fn (mut c Conn) read_handshake_body() ![]u8 {
	if c.has_stashed {
		c.has_stashed = false
		out := c.stashed.clone()
		c.stashed = []u8{}
		return out
	}
	for i := 0; i < 8; i++ {
		ct, body := c.read_record() or { return err }
		if ct == .change_cipher_spec {
			continue
		}
		if ct != .handshake {
			return error('tls: expected handshake, got ${ct}')
		}
		return body
	}
	return error('tls: too many ChangeCipherSpec records')
}

// max_reassembled_flight caps handshake reassembly buffering (carry
// buffers in the fragmented-flight readers) so a malicious peer cannot
// grow connection memory without bound. Legitimate flights (even large
// SLH-DSA chains) stay two orders of magnitude below this.
const max_reassembled_flight = 1024 * 1024

// max_flight_certs caps certificates parsed from one Certificate flight
// (and intermediates fed to chain DFS) so a malicious flight cannot force
// unbounded parsing or combinatorial path search.
const max_flight_certs = 32

// max_key_updates caps post-handshake KeyUpdates per connection. Rekeying
// is cheap but unbounded updates force unbounded HKDF schedules.
const max_key_updates = 1024

// max_early_records caps the 0-RTT flight (early records plus EndOfEarlyData)
// so a malicious peer cannot make the server buffer or skip without bound.
const max_early_records = 32

// max_early_skip_bytes caps how many undecryptable bytes the server discards
// while resynchronising on a rejected early-data flight.
const max_early_skip_bytes = 65536

// try_open_record trial-decrypts one record, reporting success instead of failing.
fn try_open_record(suite CipherSuite, key []u8, iv []u8, seq u64, rec CipherRecord) (bool, ContentType, []u8) {
	ct, pt := open_record(suite, key, iv, seq, rec) or { return false, .application_data, []u8{} }
	return true, ct, pt
}

// drain_early_data consumes the client's optimistic 0-RTT flight after the
// server flight was sent. On acceptance it decrypts early records with the
// early traffic keys (failing closed on tamper: a record that verifies under
// neither the early nor the handshake keys aborts the handshake) and returns
// the accumulated plaintext; the client's EndOfEarlyData is appended to the
// handshake transcript. On rejection the early records are discarded and the
// first handshake-keys record is stashed for the regular readers, so the
// handshake continues as if no early data had been sent. Either way the
// record sequence accounting is left consistent for the Finished that
// follows.
fn (mut c Conn) drain_early_data(mut shs ServerHandshake) ![]u8 {
	mut out := []u8{}
	mut early_seq := u64(0)
	mut skipped := 0
	for _ in 0 .. max_early_records {
		hdr := c.read_exact(5)!
		if hdr[0] != u8(ContentType.application_data) {
			return error('tls: expected early application_data record')
		}
		if (u16(hdr[1]) << 8) | u16(hdr[2]) != tls_version_1_3 {
			return error('tls: invalid legacy version')
		}
		ln := (int(hdr[3]) << 8) | int(hdr[4])
		if ln > max_ciphertext_len {
			return error('tls: record too large')
		}
		frag := c.read_exact(ln)!
		rec := CipherRecord{
			content_type:   .application_data
			legacy_version: tls_version_1_3
			fragment:       frag
		}
		if shs.accepted_early_data {
			ok, ict, plain := try_open_record(c.suite, shs.early_traffic.key, shs.early_traffic.iv,
				early_seq, rec)
			if ok {
				if ict != .application_data {
					return error('tls: unexpected early content')
				}
				out << plain
				if out.len > int(shs.max_early_data) {
					return error('tls: early data exceeds limit')
				}
				early_seq++
				continue
			}
			// Not early data: must be EndOfEarlyData under the handshake keys.
			hok, hct, hplain := try_open_record(c.suite, c.client_hs_traffic.key,
				c.client_hs_traffic.iv, c.read_seq, rec)
			if !hok || hct != .handshake {
				return error('tls: bad early record')
			}
			parsed, n := parse_handshake_message(hplain) or { return err }
			if parsed.typ != .end_of_early_data || parsed.body.len != 0 {
				return error('tls: expected EndOfEarlyData')
			}
			// Application secrets bind the transcript through server
			// Finished only; capture that hash before EOED extends it.
			shs.early_base_th = transcript_hash_for_suite(shs.transcript, shs.suite)
			shs.transcript << hplain[..n]
			if n < hplain.len {
				c.stashed = hplain[n..].clone()
				c.has_stashed = true
			}
			c.read_seq++
			return out
		}
		// Rejected: the first handshake-keys record ends the early flight.
		hok, hct, hplain := try_open_record(c.suite, c.client_hs_traffic.key, c.client_hs_traffic.iv,
			c.read_seq, rec)
		if hok {
			if hct != .handshake {
				return error('tls: unexpected handshake content')
			}
			c.stashed = hplain.clone()
			c.has_stashed = true
			return out
		}
		if shs.has_early_traffic {
			// PSK accepted but 0-RTT rejected: discard verifiably.
			dok, dct, _ := try_open_record(c.suite, shs.early_traffic.key, shs.early_traffic.iv,
				early_seq, rec)
			if !dok || dct != .application_data {
				return error('tls: bad early record')
			}
			skipped += frag.len
			if skipped > max_early_skip_bytes {
				return error('tls: early data exceeds limit')
			}
			early_seq++
			continue
		}
		// PSK rejected too: no early keys exist, skip opaquely.
		skipped += frag.len
		if skipped > max_early_skip_bytes {
			return error('tls: early data exceeds limit')
		}
	}
	return error('tls: too many early records')
}

// write_cipher_fragmented encrypts and writes data chunked over 2^14-byte
// records (RFC 8446 §5.1). Handshake flights carrying large (e.g. SLH-DSA)
// certificates exceed one record; the peer reassembles the byte stream.
// Small payloads use a single record (coalesced fast path).
fn (mut c Conn) write_cipher_fragmented(ct ContentType, data []u8) ! {
	if data.len <= max_plaintext_len {
		c.write_cipher_record(ct, data)!
		return
	}
	mut off := 0
	for off < data.len {
		mut n := data.len - off
		if n > max_plaintext_len {
			n = max_plaintext_len
		}
		c.write_cipher_record(ct, data[off..off + n])!
		off += n
	}
}

// is_hrr_flight reports whether a server flight is a HelloRetryRequest
// (a single ServerHello carrying the magic random).
fn is_hrr_flight(flight []u8) bool {
	parsed, n := parse_handshake_message(flight) or { return false }
	if n != flight.len || parsed.typ != .server_hello {
		return false
	}
	return is_hello_retry_request(parsed.body)
}

// write_keylog appends a CLIENT_RANDOM line to $SSLKEYLOGFILE when set.
// Format: CLIENT_RANDOM <client_random_hex> <master_secret_hex>.
// Best-effort: never fails the handshake.
fn (c &Conn) write_keylog() ! {
	path := os.getenv('SSLKEYLOGFILE')
	if path.len == 0 {
		return
	}
	if c.client_random.len != 32 || c.master_secret.len == 0 {
		return
	}
	mut line := 'CLIENT_RANDOM '
	line += c.client_random.hex()
	line += ' '
	line += c.master_secret.hex()
	line += '\n'
	mut f := os.open_append(path) or { return }
	$if unix {
		// Secrets land here: restrict a newly created file even though
		// a pre-existing one keeps its own mode (best effort, like the
		// rest of key logging — never fail the handshake over it).
		os.chmod(path, 0o600) or {}
	}
	f.write_string(line) or {}
	f.close()
}

// handshake performs the TLS 1.3 handshake (client or server based on is_server).
// handshake must be called after dial/accept and before read/write.
pub fn (mut c Conn) handshake() ! {
	if c.is_server {
		mut shs := new_server_handshake_with_config(c.cfg.pq_mode, c.cfg) or { return err }

		body := c.read_handshake_body() or { return err }
		mut flight := shs.process_client_hello(body) or { return err }
		// HelloRetryRequest: answer cookie-less retries until the client
		// offers the requested share (bounded: one retry per group).
		mut hrr_rounds := 0
		for is_hrr_flight(flight) {
			hrr_rounds++
			if hrr_rounds > 4 {
				return error('tls: too many HelloRetryRequests')
			}
			c.write_plain_record(.handshake, flight)!
			body2 := c.read_handshake_body() or { return err }
			flight = shs.process_client_hello(body2) or { return err }
		}

		mut off := 0
		mut msgs := [][]u8{}
		for off < flight.len {
			parsed, n := parse_handshake_message(flight[off..]) or { return err }
			_ = parsed
			msgs << flight[off..off + n]
			off += n
		}
		c.server_hs_traffic = derive_traffic_keys(shs.handshake_keys.server_secret, shs.suite) or {
			return err
		}
		c.client_hs_traffic = derive_traffic_keys(shs.handshake_keys.client_secret, shs.suite) or {
			return err
		}
		c.suite = shs.suite

		c.write_plain_record(.handshake, msgs[0])!
		for i in 1 .. msgs.len {
			c.write_cipher_fragmented(.handshake, msgs[i])!
		}

		// 0-RTT: drain the client's optimistic early flight (early records
		// plus EndOfEarlyData, or just the resumed Finished when rejected),
		// leaving the stream positioned for the handshake readers below.
		if shs.offered_early_data {
			c.early_buf = c.drain_early_data(mut shs)!
		}

		// mTLS: collect Certificate [+ CertificateVerify] + Finished.
		// Records may fragment the flight (large client certificates),
		// so handshake messages are reassembled across record boundaries.
		if shs.verify_client && !shs.accepted_psk {
			mut cmsgs := [][]u8{}
			mut have_finished := false
			mut iters := 0
			mut carry := []u8{}
			for !have_finished {
				iters++
				if iters > 32 || cmsgs.len > 4 {
					return error('tls: unexpected client flight')
				}
				cbody := c.read_handshake_body() or { return err }
				carry << cbody
				if carry.len > max_reassembled_flight {
					return error('tls: handshake flight too large')
				}
				mut o := 0
				for o < carry.len {
					parsed, n := parse_handshake_message(carry[o..]) or {
						// Truncated tail: wait for the next record.
						if err.msg().contains('truncated') {
							break
						}
						return err
					}
					cmsgs << carry[o..o + n].clone()
					o += n
					if parsed.typ == .finished {
						if o != carry.len {
							return error('tls: trailing data after Finished')
						}
						have_finished = true
					}
					if cmsgs.len > 4 {
						return error('tls: unexpected client flight')
					}
				}
				// Keep any trailing fragment for the next record.
				if o < carry.len {
					carry = carry[o..].clone()
				} else {
					carry = []u8{}
				}
			}
			if cmsgs.len < 2 || cmsgs.len > 3 {
				return error('tls: unexpected client flight')
			}
			t0, _ := parse_handshake_message(cmsgs[0]) or { return err }
			if t0.typ != .certificate {
				return error('tls: expected client Certificate')
			}
			if cmsgs.len == 2 {
				t1, _ := parse_handshake_message(cmsgs[1]) or { return err }
				if t1.typ != .finished {
					return error('tls: expected client Finished')
				}
				chain, _ := parse_client_cert_chain(t0.body) or { return err }
				if chain.len == 0 {
					return error('tls: client certificate required')
				}
				return error('tls: expected client CertificateVerify')
			}
			t1, _ := parse_handshake_message(cmsgs[1]) or { return err }
			if t1.typ != .certificate_verify {
				return error('tls: expected client CertificateVerify')
			}
			shs.verify_client_certificate(t0.body, t1.body) or { return err }
			shs.verify_client_finished(cmsgs[2]) or { return err }
		} else {
			fct := c.read_handshake_body() or { return err }
			shs.verify_client_finished(fct) or { return err }
		}

		c.server_app = derive_traffic_keys(shs.app_keys.server_secret, c.suite) or { return err }
		c.client_app = derive_traffic_keys(shs.app_keys.client_secret, c.suite) or { return err }
		c.server_app_secret = shs.app_keys.server_secret.clone()
		c.client_app_secret = shs.app_keys.client_secret.clone()
		c.master_secret = shs.app_keys.master_secret.clone()
		c.client_random = shs.client_random.clone()
		c.server_random = shs.server_random.clone()
		c.hs_transcript = shs.transcript.clone()
		c.negotiated_alpn_str = shs.negotiated_alpn.clone()
		full_th := transcript_hash_for_suite(shs.transcript, c.suite)
		c.exporter_master = derive_exporter_master(shs.app_keys.master_secret, full_th,
			c.suite) or { []u8{} }
		c.write_keylog() or {}
		// Each traffic secret has an independent record sequence number.
		c.read_seq = 0
		c.write_seq = 0
		c.handshake_done = true
		c.resumed = shs.accepted_psk
		// Accepted 0-RTT plaintext is delivered before any post-handshake
		// application data.
		if c.early_buf.len > 0 {
			c.read_buf = c.early_buf.clone()
			c.early_buf = []u8{}
		}
		// Issue one session ticket under the application keys (best effort:
		// direct ServerHandshake users without a ticket key just skip it).
		// Rotation: seal with the first key.
		seal_key := shs.ticket_seal_key()
		if seal_key.len == 32 {
			nst := build_new_session_ticket(shs.app_keys.master_secret, transcript_hash_for_suite(shs.transcript,
				c.suite), c.suite, seal_key, c.cfg.max_early_data) or { []u8{} }
			if nst.len > 0 {
				c.write_cipher_record(.handshake, nst) or { return err }
			}
		}
		return
	}
	mut chs := new_client_handshake(c.cfg.server_name, c.cfg.alpn_protocols, c.cfg.pq_mode, effective_cipher_suites(c.cfg)) or {
		return err
	}
	chs.resume_offer = c.cfg.resume_from
	chs.max_early_data = c.cfg.max_early_data
	chs.ext_psk_identity = c.cfg.psk_identity.clone()
	chs.ext_psk_key = c.cfg.psk_key.clone()
	chs.request_ocsp = c.cfg.request_ocsp

	mut ch_msg := chs.build_client_hello() or { return err }
	c.write_plain_record(.handshake, ch_msg)!

	// 0-RTT: seal the early payload with the early traffic keys and send it
	// optimistically, before the ServerHello arrives. The record sequence
	// stays local so the handshake key epochs still start at zero.
	if chs.offered_early_data {
		if !chs.has_early_traffic {
			return error('tls: missing early traffic keys')
		}
		if c.cfg.early_data.len > int(c.cfg.max_early_data) {
			return error('tls: early data exceeds limit')
		}
		st := c.cfg.resume_from or { return error('tls: missing psk offer') }
		mut early_seq := u64(0)
		mut off := 0
		for off < c.cfg.early_data.len {
			mut n := c.cfg.early_data.len - off
			if n > max_plaintext_len {
				n = max_plaintext_len
			}
			rec := seal_record(st.suite, chs.early_traffic.key, chs.early_traffic.iv,
				early_seq, .application_data, c.cfg.early_data[off..off + n]) or {
				return err
			}
			c.write_all(rec)!
			early_seq++
			off += n
		}
	}

	body := c.read_handshake_body() or { return err }
	chs.process_server_hello(body) or { return err }
	// HelloRetryRequest: rebuild with the requested share and continue.
	// The transcript already covers message_hash(CH1) || HRR.
	mut hrr_rounds := 0
	for chs.is_hrr {
		hrr_rounds++
		if hrr_rounds > 4 {
			return error('tls: too many HelloRetryRequests')
		}
		ch_msg = chs.build_second_client_hello() or { return err }
		c.write_plain_record(.handshake, ch_msg)!
		body2 := c.read_handshake_body() or { return err }
		chs.process_server_hello(body2) or { return err }
	}
	c.suite = chs.suite
	c.client_hs_traffic = chs.client_hs_traffic
	c.server_hs_traffic = chs.server_hs_traffic

	mut server_finished := false
	mut hs_records := 0
	mut carry := []u8{}
	for !server_finished {
		hs_records++
		// Bound the handshake flight: a legitimate flight is a handful of
		// records, so a peer streaming endless handshake records is attacking.
		// Fragmented flights (large certificates) reuse records, so the
		// bound is generous.
		if hs_records > 64 {
			return error('tls: too many handshake records')
		}
		ict, ibody := c.read_record() or { return err }
		if ict == .change_cipher_spec {
			continue
		}
		if ict != .handshake {
			return error('tls: expected handshake, got ${ict}')
		}
		// Records may fragment the flight (large certificates) or
		// coalesce several messages; reassemble the byte stream and walk
		// every complete message so the transcript stays exact.
		carry << ibody
		if carry.len > max_reassembled_flight {
			return error('tls: handshake flight too large')
		}
		mut ioff := 0
		for ioff < carry.len {
			parsed, n := parse_handshake_message(carry[ioff..]) or {
				if err.msg().contains('truncated') {
					break
				}
				return err
			}
			msg_raw := carry[ioff..ioff + n].clone()
			match parsed.typ {
				.encrypted_extensions {
					if chs.offered_early_data && ee_accepts_early_data(parsed.body) {
						chs.accepted_early_data = true
					}
					alpn_sel := ee_alpn_selection(parsed.body) or { '' }
					if alpn_sel.len > 0 {
						chs.negotiated_alpn = alpn_sel
					}
				}
				.certificate_status {
					// Stapled OCSP (transport only): stash the raw bytes.
					chs.ocsp_response = parsed.body.clone()
				}
				.certificate {
					if c.cfg.verify {
						cert_ders := extract_chain_ders(parsed.body) or { return err }
						leaf0 := x509.parse_der(cert_ders[0]) or { return error(error_bad_signature) }
						mut pk := leaf0.public_key() or { return error(error_bad_signature) }
						if c.cfg.verify_chain {
							roots := explicit_roots(c.cfg)
							pk = verify_peer_chain(cert_ders, roots, .server_auth,
								c.cfg.server_name, chs.ocsp_response) or { return err }
						}
						chs.server_cert_key = pk
					}
				}
				.certificate_verify {
					if c.cfg.verify {
						key := chs.server_cert_key or { return error('tls: no server pubkey') }
						transcript_hash := transcript_hash_for_suite(chs.transcript, c.suite)
						content := cv_content(true, transcript_hash)
						verify_certificate_verify(parsed.body, key, content) or { return err }
					}
				}
				.finished {
					fin := parse_finished(parsed.body, cipher_suite_hash_len(c.suite)) or {
						return err
					}
					expected := finished_verify_data_for_suite(chs.handshake_keys.server_secret,
						transcript_hash_for_suite(chs.transcript, c.suite), c.suite) or {
						return err
					}
					if subtle.constant_time_compare(fin.verify_data, expected) != 1 {
						return error('tls: server Finished mismatch')
					}
					chs.transcript << msg_raw
					server_finished = true
				}
				.certificate_request {
					schemes, ctx := parse_certificate_request(parsed.body) or { return err }
					chs.requested_cert = true
					chs.req_schemes = schemes
					chs.req_context = ctx
				}
				else {}
			}
			if parsed.typ != .finished {
				chs.transcript << msg_raw
			} else if ioff + n < carry.len {
				return error('tls: trailing data after Finished')
			}
			ioff += n
		}
		// Keep any trailing fragment for the next record; the Finished
		// must end a record exactly, so a non-empty tail with
		// server_finished set is a protocol error (checked next loop).
		if ioff < carry.len {
			carry = carry[ioff..].clone()
		} else {
			carry = []u8{}
		}
	}

	th := transcript_hash_for_suite(chs.transcript, c.suite)
	as_ := derive_application_secrets_for_suite(chs.handshake_keys.handshake_secret, th,
		c.suite) or { return err }
	chs.app_keys = as_
	c.client_hs = chs
	c.client_hs_traffic = chs.client_hs_traffic
	c.server_hs_traffic = chs.server_hs_traffic
	c.client_app = derive_traffic_keys(as_.client_secret, c.suite) or { return err }
	c.server_app = derive_traffic_keys(as_.server_secret, c.suite) or { return err }
	c.client_app_secret = as_.client_secret.clone()
	c.server_app_secret = as_.server_secret.clone()
	c.master_secret = as_.master_secret.clone()
	c.client_random = chs.client_random.clone()
	c.server_random = chs.server_random.clone()
	c.negotiated_alpn_str = chs.negotiated_alpn.clone()
	c.ocsp_resp = chs.ocsp_response.clone()
	c.hs_transcript = chs.transcript.clone()

	// mTLS: answer a CertificateRequest before our Finished (still under the
	// handshake traffic keys).
	if chs.requested_cert {
		auth_msgs := build_client_auth_messages(c.cfg, chs.req_context, chs.req_schemes,
			c.suite, chs.transcript) or {
			[encode_handshake_message(.certificate, encode_certificate_body(chs.req_context,
				[][]u8{})!)!]
		}
		for m in auth_msgs {
			chs.transcript << m
			c.hs_transcript << m
			c.write_cipher_fragmented(.handshake, m)!
		}
		c.client_hs = chs
	}

	// 0-RTT: the server accepted early data, so close the early flight with
	// EndOfEarlyData before our Finished (same handshake key epoch, and
	// part of the transcript the Finished covers).
	if chs.offered_early_data && chs.accepted_early_data {
		eoe := encode_handshake_message(.end_of_early_data, []u8{}) or { return err }
		chs.transcript << eoe
		c.write_cipher_record(.handshake, eoe)!
	}

	cf_verified := finished_verify_data_for_suite(chs.handshake_keys.client_secret, transcript_hash_for_suite(chs.transcript,
		c.suite), c.suite) or { return err }
	fin_body := encode_handshake_message(.finished, encode_finished(Finished{ verify_data: cf_verified })) or { return err }
	chs.transcript << fin_body
	c.hs_transcript << fin_body
	c.write_cipher_record(.handshake, fin_body)!
	// Application traffic keys start a new record-number epoch.
	c.read_seq = 0
	c.write_seq = 0
	c.handshake_done = true
	c.resumed = chs.accepted_psk
	// Store the resumption master (transcript now includes client Finished)
	// so post-handshake NewSessionTickets can derive PSKs in read().
	full_th := transcript_hash_for_suite(chs.transcript, c.suite)
	c.resumption_master = derive_resumption_master(as_.master_secret, full_th,
		c.suite) or { []u8{} }
	c.has_resumption_master = c.resumption_master.len > 0
	c.exporter_master = derive_exporter_master(as_.master_secret, full_th, c.suite) or {
		[]u8{}
	}
	c.write_keylog() or {}
	c.client_hs = chs
}

// consume_post_handshake processes a decrypted post-handshake flight. Each
// NewSessionTicket derives and stores a SessionState; KeyUpdate rotates
// the receiving traffic secret (answering with our own update when
// requested); CertificateRequest triggers the client's auth flight;
// Certificate/CertificateVerify answer a pending post-handshake request on
// the server. Unknown message types are ignored so future extensions do
// not break reads.
// consume_post_handshake processes a decrypted post-handshake flight. Each
// NewSessionTicket derives and stores a SessionState; KeyUpdate rotates
// the receiving traffic secret (answering with our own update when
// requested); CertificateRequest triggers the client's auth flight;
// Certificate/CertificateVerify answer a pending post-handshake request on
// the server. Unknown message types are ignored so future extensions do
// not break reads.
fn (mut c Conn) consume_post_handshake(body []u8) ! {
	mut off := 0
	for off < body.len {
		parsed, n := parse_handshake_message(body[off..])!
		msg_raw := body[off..off + n].clone()
		match parsed.typ {
			.new_session_ticket {
				nst := parse_new_session_ticket(parsed.body)!
				if !c.is_server && c.has_resumption_master {
					psk := derive_psk(c.resumption_master, nst.nonce, c.suite) or {
						// Unusable ticket: skip it but keep walking the
						// flight (off must advance or the loop spins).
						off += n
						continue
					}
					c.resumption = SessionState{
						ticket:         nst.ticket.clone()
						psk:            psk
						suite:          c.suite
						created_unix:   time.now().unix()
						lifetime:       nst.lifetime
						age_add:        nst.age_add
						max_early_data: nst.max_early_data
					}
					c.has_ticket = true
				}
			}
			.key_update {
				requested := parse_key_update(parsed.body)!
				c.ku_count++
				if c.ku_count > max_key_updates {
					return error('tls: too many KeyUpdates')
				}
				// Rotate the receiving secret and reset its sequence.
				if c.is_server {
					c.client_app_secret = update_traffic_secret(c.client_app_secret,
						c.suite)!
					c.client_app = derive_traffic_keys(c.client_app_secret, c.suite)!
				} else {
					c.server_app_secret = update_traffic_secret(c.server_app_secret,
						c.suite)!
					c.server_app = derive_traffic_keys(c.server_app_secret, c.suite)!
				}
				c.read_seq = 0
				if requested {
					c.send_key_update_answer()!
				}
			}
			.certificate_status {
				// Stapled OCSP (transport only): stash the raw bytes.
				if !c.is_server {
					c.ocsp_resp = parsed.body.clone()
				}
			}
			.certificate_request {
				// Post-handshake auth request (client side): answer with
				// Certificate(+Verify) under the same handshake keys.
				if !c.is_server {
					schemes, ctx := parse_certificate_request(parsed.body) or {
						// Unparseable request: skip it but keep walking
						// the flight (off must advance or the loop spins).
						off += n
						continue
					}
					c.hs_transcript << msg_raw
					auth_msgs := build_client_auth_messages(c.cfg, ctx, schemes,
						c.suite, c.hs_transcript) or {
						[encode_handshake_message(.certificate, encode_certificate_body(ctx,
							[][]u8{})!)!]
					}
					for m in auth_msgs {
						c.hs_transcript << m
						c.write_cipher_record(.handshake, m)!
					}
				}
			}
			.certificate {
				// Post-handshake client auth answer (server side).
				if c.is_server && c.has_pha_pending {
					chain, ctx := parse_client_cert_chain(parsed.body) or {
						// Unparseable flight: skip it but keep walking
						// (off must advance or the loop spins).
						off += n
						continue
					}
					if ctx != c.pha_pending_ctx {
						return error('tls: bad post-handshake request context')
					}
					if chain.len == 0 {
						return error('tls: client certificate required')
					}
					cert_der := chain[0]
					leaf := x509.parse_der(cert_der) or { return error(error_bad_signature) }
					if c.cfg.verify_chain {
						now := time.now()
						if now.unix() < leaf.not_before.unix() {
							return error(error_chain_untrusted)
						}
						if now.unix() > leaf.not_after.unix() {
							return error(error_chain_expired)
						}
					}
					c.hs_transcript << msg_raw
					c.pha_cert_body = parsed.body.clone()
					c.has_pha_cert = true
				}
			}
			.certificate_verify {
				// Post-handshake client auth verify (server side).
				if c.is_server && c.has_pha_pending && c.has_pha_cert {
					cert_body := c.pha_cert_body.clone()
					c.has_pha_cert = false
					c.pha_cert_body = []u8{}
					chain, _ := parse_client_cert_chain(cert_body) or {
						// Unparseable stashed flight: skip it but keep
						// walking (off must advance or the loop spins).
						off += n
						continue
					}
					if chain.len == 0 {
						return error('tls: client certificate required')
					}
					leaf0 := x509.parse_der(chain[0]) or { return error(error_bad_signature) }
					mut pk := leaf0.public_key() or { return error(error_bad_signature) }
					if c.cfg.verify_chain {
						roots := explicit_roots(c.cfg)
						pk = verify_peer_chain(chain, roots, .client_auth, '', []u8{}) or {
							return err
						}
					}
					th := transcript_hash_for_suite(c.hs_transcript, c.suite)
					content := cv_content(false, th)
					verify_certificate_verify(parsed.body, pk, content) or { return err }
					c.hs_transcript << msg_raw
					c.has_pha_pending = false
					c.pha_pending_ctx = []u8{}
				}
			}
			else {}
		}
		off += n
	}
	if off != body.len {
		return error('tls: trailing data after post-handshake flight')
	}
}

// read decrypts one application-data record into buf. The handshake must be done.
// read opportunistically consumes post-handshake NewSessionTickets (stored for
// session_state()) so ticket-less peers never hang the handshake.
pub fn (mut c Conn) read(mut buf []u8) !int {
	if !c.handshake_done {
		return error('tls: handshake not complete')
	}
	for c.read_buf.len == 0 {
		ct, body := c.read_record() or { return err }
		if ct == .handshake {
			c.consume_post_handshake(body) or { return err }
			continue
		}
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
