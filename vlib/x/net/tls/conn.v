// Copyright (c) 2026 blackshirt.
//
// conn.v is the public entry point of the TLS stack: dial() runs the TLS 1.3
// handshake and Conn exposes the protected stream over TCP (RFC 8446 §5). The
// record-layer state and policy live in the RecordLayer facade (record_layer.v);
// Conn keeps transport, alert and lifecycle concerns. The handshake core
// (tls.handshake) stays transport-neutral; recordless QUIC shares it.
module tls

import net
import sync
import time
import x.net.tls.handshake
import x.net.tls.pki
import x.net.tls.wire

// max_stored_session_tickets bounds the NewSessionTicket backlog kept on the
// Conn; resumption is unimplemented, so the queue is only diagnostic
// (RFC 8446 §4.6.1).
const max_stored_session_tickets = 32

// recv_buf_compact_threshold is the consumed-prefix size at which try_reclaim_recv_buf
// shifts recv_buf's tail to the front, keeping the reclaim amortized O(1) per byte.
const recv_buf_compact_threshold = 16384

// recv_read_size is the number of bytes requested from the socket per read, sized
// to the largest plaintext record so a full record usually arrives in one read(2).
const recv_read_size = max_plaintext_record_len

// write_batch_threshold caps how much ciphertext Conn.write() accumulates before
// pushing it out, so a large write costs about one write(2) per 128 KiB.
const write_batch_threshold = 131072

// max_compat_ccs_records bounds the dummy ChangeCipherSpec records tolerated while
// awaiting a flight, so a peer cannot pin the handshake (RFC 8446 §5 / App. D.4).
const max_compat_ccs_records = 2

// handshake_deadline_multiplier bounds the whole handshake at this multiple of the
// per-operation socket timeout: per-read timeouts alone cannot stop a slow-loris
// peer that dribbles valid bytes just under each timeout and stalls the flight forever.
const handshake_deadline_multiplier = 4

// max_server_name_len bounds the SNI/verification name at the DNS ceiling (RFC 1035
// §2.3.4), so a hostile or mistyped name cannot overflow the u16 SNI vector encoding.
const max_server_name_len = 253

// max_handshake_recv_buf_len caps unconsumed framed bytes during the handshake: one
// maximal ciphertext record plus one socket-read slack (a compliant server's flight
// is fully consumed before the next read). Post-handshake streaming coalesces freely,
// so the cap is enforced only while dial() is handshaking (deadline_ms != 0).
const max_handshake_recv_buf_len = 2 * max_ciphertext_record_len + record_header_len + recv_read_size

// max_handshake_flight_records bounds every record do_handshake consumes. A
// compliant flight stays far below it; a peer flooding state-neutral but
// decryptable records meets a deterministic cap instead of only the monotonic
// handshake deadline (RFC 8446 §4 orders the flight; nothing legit repeats it).
const max_handshake_flight_records = 64

// max_tolerated_records_per_read bounds the records one Conn.read() may skip
// without delivering data — zero-length padding and post-handshake messages are
// legal (RFC 8446 §5.2/§4.6), but a verified flood must not starve the caller.
// The cap mirrors Go crypto/tls maxUserRecords.
const max_tolerated_records_per_read = 100

// max_post_handshake_messages_per_record bounds the complete messages one
// handshake-inner-type record may deliver to handle_post_handshake: a maximal
// record packs ~3300 eight-byte KeyUpdate or ticket bodies, each costing a
// rekey (plus a KeyUpdate response write), so the bound fires before that work.
const max_post_handshake_messages_per_record = 1000

// Conn is a TLS 1.3 client connection over TCP, built only by dial(). A single
// mutex serializes every public operation; see the lock discipline note below.
@[noinit]
pub struct Conn {
mut:
	// Transport and transport-neutral handshake adapter.
	sock &net.TcpConn            // underlying TCP transport
	hs   handshake.HandshakeCore // Unified handshake core in TLS-record mode (is_quic false)

	// Record facade: the four protection states, the post-handshake reassembly buffer and
	// the KeyUpdate mechanism (see record_layer.v). dial() must assign new_record_layer(),
	// because Conn is @[noinit] and an uninitialized nested struct is garbage, not zeros.
	rec RecordLayer

	// session_tickets holds the NewSessionTicket backlog the core parses but does
	// not keep; exceeding it drops the ticket, never the connection (RFC 8446 §4.6.1).
	session_tickets []handshake.NewSessionTicket

	// Read-side buffers: socket bytes are framed into records, then decrypted
	// application bytes are delivered to callers.
	// Fixed-size staging buffer filled directly by each sock.read; bytes land
	// here before recv_buf, avoiding a fresh allocation per socket read.
	sock_buf []u8
	// TCP is a byte stream, so reads may return part of a record or several;
	// read_raw_record parses one complete framed record at a time and keeps the
	// remaining bytes for the next call.
	recv_buf []u8
	// Index of the first unread byte in recv_buf; advancing it avoids shifting the
	// tail per record, and try_reclaim_recv_buf reclaims the prefix when appropriate.
	recv_pos int
	// Unread remainder of a decrypted application record when the caller's buffer
	// is smaller; Conn.read drains this before decrypting another, preserving
	// byte order.
	app_buf []u8
	// Index of the first undelivered byte in app_buf; advances without copying.
	app_pos int

	// Connection lifecycle and sticky read failure.
	// Terminal protocol state after a local close or peer alert; fatal alerts
	// surface through fatal_error rather than as clean EOF.
	closed bool
	// Set before attempting the local close_notify write, so a failed write or a
	// repeat close() never retries it.
	close_notify_sent bool
	// Whether close_tcp_socket() has run; separate from `closed` because
	// handle_alert closes the protocol but leaves the socket for close().
	sock_closed bool
	// Number of readers currently parked in sock.read() with guard released (see the
	// Conn lock discipline). While nonzero, close_tcp_socket() defers the physical fd
	// close so a parked recv() can never have its descriptor recycled under it.
	readers int
	// Set by close_tcp_socket() when it deferred the fd close behind a parked reader;
	// the last finish_parked_read() to drive `readers` back to 0 performs the close.
	sock_close_pending bool
	// Sticky fatal read error; returned again on subsequent read() calls instead
	// of appearing as a clean EOF.
	fatal_error string
	// Set by the first failure alert sent, so one failure emits at most one alert
	// (RFC 8446 §6): abort_connection and dial()'s teardown cannot double-send.
	failure_alert_sent bool

	// One exclusive mutex serializes read(), write() and teardown; helpers assume it held, so only
	// the public entry points and dial()'s do_handshake acquire it. The sole exception: guard is
	// released around the blocking sock.read() in read_raw_record(), the reader registering in
	// `readers` first so close() never closes/recycles the fd under a parked recv(). 1 reader+writer max.
	guard &sync.Mutex // serializes read(), write(), close() and every teardown

	// Negotiated connection identity and protocol.
	negotiated_alpn string // server's ALPN pick — always one of HandshakeOptions.alpn (the handshake aborts otherwise), '' if none
	server_name     string // SNI value sent and the name the certificate was checked against

	// Failure-alert plumbing, used only by send_failure_alert on a dead handshake.
	// Suppresses a fatal-alert response when the peer has already terminated.
	peer_terminated bool
	fail_alert      ?handshake.AlertDescription // adapter-classified alert; when unset, send_failure_alert falls back to the core's hs.failure_alert()

	// Absolute monotonic-ms deadline bounding the whole handshake; 0 after dial
	// succeeds (and in @[noinit] defaults) so Conn.read() is never deadline-bound.
	deadline_ms i64
	// Per-operation socket timeout applied to both directions at dial. The
	// handshake loop shrinks the read side toward the deadline; dial restores
	// this value on success so Conn.read() keeps the caller's own bound.
	sock_timeout time.Duration
}

// Config holds TLS client connection options, consumed by dial().
@[params]
pub struct Config {
pub:
	// server_name is used for SNI and certificate hostname verification.
	server_name string
	// hsk_options configures ALPN, certificate validation, and ClientHello preferences.
	hsk_options handshake.HandshakeOptions
	// insecure_skip_verify, when true, makes dial() use pki.new_insecure_validator and
	// skip all certificate chain and hostname validation (tests only).
	insecure_skip_verify bool
	// root_cas, when set, is used instead of the system trust store.
	root_cas ?pki.TrustStore
	// min_rsa_bits floors the accepted RSA leaf key size; 0 keeps the 2048-bit default.
	min_rsa_bits int
	// timeout sets the socket read and write timeouts; defaults to 30 seconds.
	timeout time.Duration = 30 * time.second
}

// default_config builds a Config for the given hostname that trusts the system
// root store (root_cas stays none, so dial loads it) and keeps verification on.
pub fn default_config(server_name string) Config {
	return Config{
		server_name:          server_name
		insecure_skip_verify: false
		root_cas:             none
		hsk_options:          handshake.HandshakeOptions{}
	}
}

// dial connects to address (host:port), performs a TLS 1.3 handshake, and returns
// Conn. server_name falls back to the address host so SNI and hostname checks work.
pub fn dial(address string, config Config) !&Conn {
	// Prefer an explicit server_name; otherwise derive it from the dial address.
	server_name := if config.server_name.len > 0 {
		config.server_name
	} else {
		host_from_address(address)
	}
	// A name with stray whitespace would travel verbatim into SNI and into hostname
	// verification — reject it before the socket opens rather than dial ambiguously.
	if server_name != server_name.trim_space() {
		return error('tls: server name has stray whitespace')
	}
	if server_name.len > max_server_name_len {
		return error('tls: server name exceeds ${max_server_name_len} characters')
	}

	// Open the TCP transport first: every later failure path must close it.
	mut sock := net.dial_tcp(address)!
	// Apply one bounded timeout to both directions (caller's value, else 30s).
	timeout := if config.timeout > 0 { config.timeout } else { 30 * time.second }
	sock.set_read_timeout(timeout)
	sock.set_write_timeout(timeout)
	// Absolute bound for the whole handshake (see handshake_deadline_multiplier).
	// The socket timeout is ns-based, so it is converted to ms to match mono_ms().
	deadline_ms := mono_ms() + i64(timeout) * handshake_deadline_multiplier / i64(time.millisecond)

	handshake_options := config.hsk_options
	mut validator := if configured := handshake_options.validator {
		configured
	} else if config.insecure_skip_verify {
		// The named escape hatch: no roots are consulted, verification is skipped.
		pki.new_insecure_validator()
	} else {
		// Trust anchors: an explicit root_cas wins, else the system bundle.
		mut store := pki.new_trust_store()
		if roots := config.root_cas {
			store = roots
		} else {
			// Fatal, but close the socket so a dial error leaks no fd.
			store = pki.system_root_cas() or {
				sock.close() or {}
				return err
			}
		}
		pki.new_validator(store)
	}
	if config.min_rsa_bits > 0 {
		validator.set_min_rsa_bits(config.min_rsa_bits)
	}
	options := handshake_options.with_validator(validator)

	mut hs := handshake.new_tls_handshake(server_name, options) or {
		sock.close() or {}
		return err
	}

	mut conn := &Conn{
		sock:            sock
		hs:              hs
		session_tickets: []handshake.NewSessionTicket{cap: 4}
		sock_buf:        []u8{len: recv_read_size} // reused by every socket read for this connection
		recv_buf:        []u8{cap: 2 * (max_ciphertext_record_len + record_header_len)} // one frame parsing, one arriving
		app_buf:         []u8{cap: max_plaintext_record_len} // one full plaintext record
		deadline_ms:     deadline_ms
		sock_timeout:    timeout
		guard:           sync.new_mutex()
		server_name:     server_name
		// The record facade's four protections are probed (send_failure_alert) before any
		// install, so they start uninitialized rather than carrying key material.
		rec:             new_record_layer()
	}

	// The single handshake-phase teardown choke-point: every do_handshake() failure —
	// whether it pinned an alert via classify_failure or is an internal `!` — funnels here
	// to send the classified fatal alert (§6), destroy secrets and close the socket. It stays
	// a safety net so no future `return` path can leak material.
	conn.do_handshake() or {
		// do_handshake released guard on return, and the Conn is not shared yet, so this
		// teardown runs single-threaded with no lock held.
		conn.send_failure_alert()
		// A handshake that failed never reached .connected, so destroy_all takes
		// its no-error branch and the swallow below cannot discard a real failure.
		conn.destroy_all() or {}
		sock.close() or {}
		return err
	}
	// The ALPN pick is exposed only after the handshake proved it was offered (RFC 7301 §3.1).
	conn.negotiated_alpn = conn.hs.negotiated_alpn()
	// Retire the handshake-only markers: the framing cap and deadline guard key
	// off deadline_ms, and steady-state reads must obey only the per-op timeout.
	conn.deadline_ms = 0
	// do_handshake shrinks the read timeout toward the handshake deadline; a
	// successful dial must hand the caller the full per-operation timeout again.
	sock.set_read_timeout(conn.sock_timeout)
	return conn
}

// read decrypts application data into buf. Returns bytes read (0 on clean close).
pub fn (mut c Conn) read(mut buf []u8) !int {
	// Serialize the whole read path: recv_buf, the read protection state and the sticky error.
	c.guard.lock()
	defer {
		c.guard.unlock()
	}
	// A fatal alert is sticky: report it on every later read, never 0. This is checked
	// before the empty-buffer shortcut so a 0-length liveness probe cannot masquerade a
	// truncated or fatally-errored stream as a clean EOF (RFC 8446 §6.1).
	if c.fatal_error != '' {
		return error(c.fatal_error)
	}
	// Only a healthy stream reads a 0-length buffer as EOF; it can receive no data anyway.
	if buf.len == 0 {
		return 0
	}
	// Drain leftovers first so no plaintext is dropped or reordered.
	if c.app_buf.len > c.app_pos {
		n := copy(mut buf, c.app_buf[c.app_pos..])
		c.app_pos += n
		if c.app_pos == c.app_buf.len {
			c.wipe_app_buf()
		}
		return n
	}
	if c.closed {
		return 0
	}

	mut tolerated_records := 0
	for {
		rec := c.read_raw_record() or {
			// A socket-level I/O failure (per-op timeout, EINTR, EAGAIN) is the
			// caller's condition, not a protocol fault: pass it through unpinned
			// and un-destroyed so a retry or close() still works. Everything else
			// is stream/protocol damage and stays terminal (RFC 8446 §6).
			if is_transport_error(err.msg()) {
				return err
			}
			alert := c.fail_alert or { handshake.AlertDescription.decode_error }
			return c.abort_connection(alert, err.msg())
		}
		match rec.content_type {
			.change_cipher_spec {
				// RFC 8446 §5: a CCS record is illegal once application keys are in use.
				return c.abort_connection(.unexpected_message, 'tls: received change_cipher_spec record after handshake (unexpected_message)')
			}
			.alert {
				// RFC 8446 §6: post-handshake alerts must be encrypted; a plaintext one is rejected.
				return c.abort_connection(.unexpected_message, 'tls: received unencrypted alert post-handshake (must be encrypted)')
			}
			.application_data {
				mut dec := c.rec.s_app_protection.decrypt_record(rec) or {
					return c.abort_connection(decrypt_failure_alert(err.msg()), err.msg())
				}
				match dec.content_type {
					.application_data {
						// Application traffic of any length breaks a KeyUpdate run (§4.5).
						c.rec.clear_key_update_run()
						if dec.plaintext.len == 0 {
							// Zero-length records are legal (padding only), but a verified flood
							// must not spin this loop forever (max_tolerated_records_per_read).
							tolerated_records++
							if tolerated_records > max_tolerated_records_per_read {
								return c.abort_connection(.unexpected_message, 'tls: too many skipped records in one read (unexpected_message)')
							}
							continue
						}
						n := copy(mut buf, dec.plaintext)
						if n < dec.plaintext.len {
							c.app_buf << dec.plaintext[n..]
						}
						// §7.5 spirit: scrub the decrypted staging copy on every exit path
						// (delivered prefix, alert, post-handshake, or error); the app_buf
						// remainder is scrubbed when it drains. Each arm scrubs explicitly
						// because a defer registered in this loop only fires at function exit.
						zero_bytes(mut dec.plaintext)
						return n
					}
					.alert {
						// Post-handshake alerts arrive encrypted as the inner type (RFC 8446 §5.1/§6).
						// handle_alert consumes the body synchronously; scrub the staging copy on
						// both the clean-0 and the error path before returning (§7.5 spirit).
						mut res := c.handle_alert(dec.plaintext) or {
							zero_bytes(mut dec.plaintext)
							return err
						}
						zero_bytes(mut dec.plaintext)
						return res
					}
					.handshake {
						// NewSessionTicket/KeyUpdate must not surface as application data, but a
						// record flood of them is the same starvation vector as empty padding.
						tolerated_records++
						if tolerated_records > max_tolerated_records_per_read {
							zero_bytes(mut dec.plaintext)
							return c.abort_connection(.unexpected_message, 'tls: too many skipped records in one read (unexpected_message)')
						}
						c.handle_post_handshake(dec.plaintext) or {
							zero_bytes(mut dec.plaintext)
							return err
						}
						// This record's plaintext is fully consumed; scrub before the next
						// iteration's dec would drop it unscrubbed (§7.5 spirit).
						zero_bytes(mut dec.plaintext)
						continue
					}
					else {
						zero_bytes(mut dec.plaintext)
						return c.abort_connection(.unexpected_message, 'tls: unexpected inner content type ${dec.content_type}')
					}
				}
			}
			else {
				return c.abort_connection(.unexpected_message, 'tls: unexpected record type ${rec.content_type} after handshake')
			}
		}
	}
	// The loop only exits through the explicit returns inside the match arms or an
	// error propagation; this fail-closed tail exists so no future path can fall out.
	return error('tls: unreachable')
}

// write encrypts and sends application data.
@[direct_array_access]
pub fn (mut c Conn) write(data []u8) !int {
	// Serialize against other writers and the read path's KeyUpdate response. Writes
	// stay under guard because sock.write is bounded by dial()'s write timeout, so a
	// held guard never pins close(); any failure is terminal and aborts in place.
	c.guard.lock()
	defer {
		c.guard.unlock()
	}
	// Refuse to encrypt onto a finished connection.
	if c.closed {
		return error('tls: connection closed')
	}
	// A sticky fatal error must surface at the public boundary too, not only
	// inside write_application_data(): a future caller could bypass it.
	if c.fatal_error != '' {
		return error(c.fatal_error)
	}
	// Application keys exist only once connected.
	if c.hs.state() != .connected {
		return error('tls: handshake not complete')
	}
	return c.write_application_data(data)
}

// close sends a close_notify alert and closes the underlying TCP connection.
pub fn (mut c Conn) close() ! {
	// Single-phase teardown under guard: emit close_notify at most once (it needs the write
	// keys alive, so it runs before destroy_all wipes them), mark closed, destroy every
	// secret and buffer, then release the socket. A parked reader holds no guard, so this
	// acquires it; the parked read surfaces the close itself (see the lock discipline).
	c.guard.lock()
	defer {
		c.guard.unlock()
	}
	if c.closed {
		c.close_tcp_socket()
		return
	}
	if !c.close_notify_sent {
		c.close_notify_sent = true
		c.send_close_notify()
	}
	c.closed = true
	// Destroy every secret, but never skip releasing the socket: a zeroization error
	// must not leak the fd, so close_tcp_socket runs on both the clean and error path.
	mut teardown_err := ''
	c.destroy_all() or {
		teardown_err = err.msg()
	}
	c.close_tcp_socket()
	if teardown_err != '' {
		return error('tls: secret zeroization failed during close: ${teardown_err}')
	}
}

// negotiated_alpn returns the negotiated ALPN protocol, if any.
pub fn (c Conn) negotiated_alpn() string {
	return c.negotiated_alpn
}

// do_handshake drives the full client flight over TCP: frame the ClientHello
// (+dummy CCS), then feed records into the core until it reports .connected (§5.1).
fn (mut c Conn) do_handshake() ! {
	// dial() runs this before the Conn is shared, so the lock is uncontended; holding
	// it keeps the "helpers run with guard held" invariant true on the handshake path
	// too (read_raw_record() releases/reacquires guard around each blocking socket read).
	c.guard.lock()
	defer {
		c.guard.unlock()
	}
	ch := c.hs.create_client_hello()!
	// The one-record bound (§5.1) is enforced inside send_plaintext_handshake_flight, the single
	// choke point shared with the retry ClientHello below.
	c.send_plaintext_handshake_flight(ch)!

	mut ccs_seen := 0
	mut flight_records := 0
	for c.hs.state() != .connected {
		// Cheap early check; the fill loop enforces the same deadline per read.
		_ := c.handshake_read_timeleft()!
		rec := c.read_raw_record()!
		flight_records++
		if flight_records > max_handshake_flight_records {
			return c.classify_failure(.unexpected_message, 'tls: too many records in the server flight (unexpected_message)')
		}

		match rec.content_type {
			.change_cipher_spec {
				// The peer's dummy CCS carries no meaning in TLS 1.3 — drop it (RFC 8446 §5).
				ccs_seen = validate_ccs_record(rec.payload, ccs_seen) or {
					return c.classify_failure(.unexpected_message, err.msg())
				}
				continue
			}
			.alert {
				// After ServerHello, RFC 8446 §6 requires alerts to be encrypted — reject a plaintext one.
				if c.hs.state() != .wait_sh {
					return c.classify_failure(.unexpected_message, 'tls: unencrypted alert after ServerHello (unexpected_message)')
				}
				peer_alert := handshake.parse_alert(rec.payload) or {
					// A malformed alert record is a decode error, not our internal_error default.
					return c.classify_failure(.decode_error, 'tls: malformed alert record (decode_error): ${err.msg()}')
				}
				// The peer terminated of its own accord: never alert back (RFC 8446 §6).
				c.peer_terminated = true
				return error('tls: received alert ${peer_alert.description} (level=${peer_alert.level})')
			}
			.handshake {
				// Plaintext handshake records are legal only while the ServerHello is outstanding (§5.1/§6).
				if c.hs.state() != .wait_sh {
					return c.classify_failure(.unexpected_message, 'tls: unencrypted handshake record after ServerHello (unexpected_message)')
				}
				// Feed the plaintext record into the core's initial-phase reassembly.
				// It parses one complete ServerHello (or, on a retry path, a HelloRetryRequest
				// and synthesizes the outbound retry ClientHello). `msgs` holds those
				// outbound messages — empty when this record only advanced state,
				// non-empty when a retry ClientHello must be sent below.
				msgs := c.hs.receive_initial_bytes(rec.payload)!
				// The record must have yielded exactly one complete message (§5.1).
				backlog := c.hs.reassembly_len()!
				if backlog != 0 {
					return c.classify_failure(.unexpected_message, 'tls: misframed ServerHello record: ${backlog} leftover byte(s) (unexpected_message)')
				}
				if msgs.len == 0 && c.hs.state() == .wait_sh {
					return c.classify_failure(.unexpected_message, 'tls: incomplete ServerHello at record boundary (unexpected_message)')
				}
				if c.hs.state() != .start && c.hs.state() != .wait_sh && c.hs.state() != .failed {
					// Install once the core actually holds handshake secrets, keyed on
					// the same readiness the QUIC engine derives from HandshakeState,
					// not on a separate readiness query.
					c.rec.install_traffic_protection(mut c.hs)!
				}
				// A retry ClientHello travels plaintext plus a fresh dummy CCS (App. D.4).
				// `msgs` is not the HRR we just received: receive_initial_bytes routed
				// the HRR to handle_hello_retry_request, which produced this newly
				// generated ClientHello (echoing the HRR cookie, possibly with a new
				// key-share group) and already hashed it into the transcript. We send
				// that outbound retry, never echo back the server's HRR.
				for m in msgs {
					c.send_plaintext_handshake_flight(m.msg())!
				}
			}
			.application_data {
				c.handle_encrypted_flight_record(rec)!
			}
			else {
				return c.classify_failure(.unexpected_message, 'tls: unexpected record type ${rec.content_type} during handshake')
			}
		}
	}
	// The core verified the server Finished; install this adapter's post-handshake protection.
	c.rec.install_traffic_protection(mut c.hs)!
	// §4.6.1 lets the server coalesce a NewSessionTicket behind its Finished; the
	// core stops consuming handshake messages at .connected, so take (and scrub) its
	// buffered bytes.
	coalesced := c.hs.take_buffered_handshake_bytes()!
	if coalesced.len > 0 {
		// handle_post_handshake failures already run abort_connection (which latches
		// the failure alert), so dial()'s teardown wrapper will not double-alert.
		c.handle_post_handshake(coalesced)!
	}
	// Nothing may protect or decrypt in the handshake phase now, so the core may
	// destroy its secrets (RFC 8446 §7.5). Handshake and application traffic secrets
	// are zeroized together at close via zeroize_traffic_secrets().
}

// validate_ccs_record checks one dummy ChangeCipherSpec record and enforces max_compat_ccs_records.
fn validate_ccs_record(payload []u8, seen int) !int {
	mut total := seen + 1
	if total > max_compat_ccs_records {
		return error('tls: too many change_cipher_spec records during handshake (unexpected_message)')
	}
	// Compliant peers send exactly the single byte 0x01 (App. D.4).
	if payload.len != 1 || payload[0] != 0x01 {
		return error('tls: malformed change_cipher_spec during handshake (must be the single byte 0x01)')
	}
	return total
}

// send_plaintext_handshake_flight emits one handshake flight — the initial or a retry
// ClientHello — as a plaintext record followed by the dummy CCS (§5/App. D.4).
fn (mut c Conn) send_plaintext_handshake_flight(msg []u8) ! {
	// §5.1: every plaintext flight (initial ClientHello or retry) must fit one record.
	// serialize_into encodes the length in two bytes without re-checking, so an oversized
	// local message would silently mis-frame. Pin internal_error and fail before framing;
	// dial()'s teardown wrapper then emits the pinned alert (the handshake-phase convention).
	if msg.len > max_plaintext_record_len {
		return c.classify_failure(.internal_error, 'tls: ClientHello exceeds one record (${msg.len} > ${max_plaintext_record_len} bytes)')
	}
	c.write_records([new_record(.handshake, msg), change_cipher_spec()])!
}

// handshake_read_timeleft returns ms left before the dial deadline, failing with the
// pinned unexpected_message alert when spent, and shrinks the socket read timeout
// to the remainder when it is the tighter bound.
fn (mut c Conn) handshake_read_timeleft() !i64 {
	remaining_ms := c.deadline_ms - mono_ms()
	if remaining_ms <= 0 {
		return c.classify_failure(.unexpected_message, 'tls: handshake deadline exceeded (unexpected_message)')
	}
	if remaining_ms * i64(time.millisecond) < i64(c.sock_timeout) {
		c.sock.set_read_timeout(remaining_ms * time.millisecond)
	}
	return remaining_ms
}

// handle_encrypted_flight_record decrypts one handshake-phase encrypted record and routes its
// inner content type: handshake bytes feed the core and the client Finished travels
// under the client handshake keys; alerts are terminal peer faults (§5.1/§6).
fn (mut c Conn) handle_encrypted_flight_record(rec Record) ! {
	// Everything under the server's handshake keys has outer type application_data (RFC 8446 §5.1/§5.2).
	if c.hs.state() == .start || c.hs.state() == .wait_sh {
		return c.classify_failure(.unexpected_message, 'tls: unexpected encrypted record before ServerHello (unexpected_message)')
	}
	mut dec := c.rec.s_hsk_protection.decrypt_record(rec) or {
		// Mirror the read() path: a record that will not open is a peer-caused
		// integrity failure, so surface its specific alert, not internal_error.
		return c.classify_failure(decrypt_failure_alert(err.msg()), err.msg())
	}
	// §7.5: scrub this handshake record's decrypted staging copy on every exit
	// (function-scope defer is safe here — handle_encrypted_flight_record is not a loop); the
	// body below consumes dec.plaintext synchronously before the defer fires.
	defer {
		zero_bytes(mut dec.plaintext)
	}
	// Inner-type routing is record-layer policy; the core has no ContentType concept.
	match dec.content_type {
		.handshake {
			// §5.1: handshake content must carry message bytes; an empty inner
			// payload advances no state and would only burn AEAD opens.
			if dec.plaintext.len == 0 {
				return c.classify_failure(.unexpected_message, 'tls: empty handshake record under handshake keys (unexpected_message)')
			}
			msgs := c.hs.receive_handshake_bytes(dec.plaintext)!
			for m in msgs {
				// The client's Finished travels under the client handshake keys (RFC 8446 §7.1).
				enc := c.rec.c_hsk_protection.encrypt_record(.handshake, m.msg())!
				c.write_records([enc])!
			}
		}
		.alert {
			// RFC 8446 §6: the server may surface a fatal alert under the handshake keys.
			peer_alert := handshake.parse_alert(dec.plaintext) or {
				// A malformed encrypted alert body is a decode error, not the stale internal_error.
				return c.classify_failure(.decode_error, 'tls: malformed encrypted alert (decode_error): ${err.msg()}')
			}
			if peer_alert.description == .close_notify {
				c.peer_terminated = true
				return error('tls: peer sent close_notify during handshake (level=${peer_alert.level})')
			}
			c.peer_terminated = true
			// Include the level like the plaintext arm above: during the handshake every
			// alert is terminal, and the level distinguishes a conforming fatal from a
			// legacy warning-shaped peer in the surfaced error (RFC 8446 §6).
			return error('tls: received alert ${peer_alert.description} during handshake (level=${peer_alert.level})')
		}
		else {
			// Only handshake and alert are legal here; anything else violates §5.6.
			return c.classify_failure(.unexpected_message, 'tls: unexpected content type in handshake phase: ${dec.content_type}')
		}
	}
}

// read_raw_record blocks until one complete TLS record (header + payload) is
// buffered, then parses it in place off recv_buf; TCP may split or coalesce records.
fn (mut c Conn) read_raw_record() !Record {
	// sock_buf is refilled in place, so a record arriving in TCP pieces costs no allocation.
	for {
		avail := c.recv_buf.len - c.recv_pos
		if avail >= record_header_len {
			// Peek the length field (offset 3) without consuming it; TCP can split records.
			payload_len := int(u16(c.recv_buf[c.recv_pos + 3]) << 8 | u16(c.recv_buf[c.recv_pos + 4]))
			if payload_len > max_ciphertext_record_len {
				c.fail_alert = .record_overflow
				return error('record: record length ${payload_len} exceeds maximum allowed ${max_ciphertext_record_len}')
			}
			total := record_header_len + payload_len
			if avail >= total {
				content_type_from_u8(c.recv_buf[c.recv_pos]) or {
					c.fail_alert = .unexpected_message
					return err
				}
				// Parse directly over the live recv_buf; read_bytes copies the payload out.
				mut r := wire.new_reader(c.recv_buf[c.recv_pos..c.recv_pos + total])
				c.recv_pos += total
				rec := read_record(mut r)!
				c.try_reclaim_recv_buf()
				return rec
			} else if c.deadline_ms != 0 && avail > max_handshake_recv_buf_len {
				// Handshake-phase only: dribbled bytes must not grow the framing
				// buffer without bound while no record completes.
				c.fail_alert = .unexpected_message
				return error('record: ${avail} unconsumed bytes exceed the handshake framing bound ${max_handshake_recv_buf_len}')
			}
		}
		if c.sock_closed {
			return error('tls: connection closed (no socket available to read further records)')
		}
		if c.deadline_ms != 0 {
			// The deadline must also bite inside an incomplete record: a slow-loris
			// dribble never completes one, so this fill loop is where the stall lives.
			c.handshake_read_timeleft()!
		}
		// Never hold guard across the indefinite blocking socket read: releasing it lets a
		// concurrent close() acquire guard. Register as a parked reader first — under guard,
		// atomically with the sock_closed check — so close() defers the fd close instead of
		// recycling it under this recv(); the close still surfaces via the read's own timeout.
		c.readers++
		c.guard.unlock()
		// Classify socket-level failures: a mundane I/O condition (per-op timeout, EINTR,
		// EAGAIN) returns as a non-sticky transport tag (is_transport_error) so read() can
		// retry; EOF/reset stays terminal because a truncated stream is a §6.1
		// truncation-attack boundary. Every branch returns, so the or-block never falls through.
		n := c.sock.read(mut c.sock_buf) or {
			c.guard.lock()
			c.finish_parked_read()
			// A concurrent close()/abort may have run in the unguarded window: surface its
			// sticky failure before doing any further work.
			if c.fatal_error != '' {
				return error(c.fatal_error)
			}
			if c.deadline_ms != 0 && mono_ms() >= c.deadline_ms {
				return c.classify_failure(.unexpected_message, 'tls: handshake deadline exceeded (unexpected_message)')
			}
			msg := err.msg().to_lower()
			if msg.contains('timed out') || msg.contains('timeout') || msg.contains('eagain') || msg.contains('would block') || msg.contains('interrupted') {
				return error('tls: socket read timed out or interrupted')
			}
			// EOF, reset, broken pipe, aborted: the peer ended the stream without a
			// close_notify. V surfaces a clean FIN as an `EOF` read error, not `n == 0`, so
			// truncation lands here. §6.1 makes it terminal and sticky (a silent clean EOF
			// would hide a truncation attack), so reuse the 'connection closed' wording.
			return error('tls: connection closed while reading record')
		}
		c.guard.lock()
		c.finish_parked_read()
		// The promised post-reacquire terminal-flag re-check (see the Conn lock
		// discipline): if close()/abort ran during the unguarded read, stop here
		// instead of decrypting against protections destroy_all already wiped —
		// which would surface as a spurious protocol error or a plaintext alert.
		if c.fatal_error != '' {
			return error(c.fatal_error)
		}
		if c.closed || c.sock_closed {
			// A concurrent close()/abort turned the transient condition terminal.
			return error('tls: connection closed while reading record')
		}
		if n == 0 {
			return error('tls: connection closed while reading record')
		}
		c.recv_buf << c.sock_buf[..n]
	}
	// Every branch inside the loop returns or keeps looping; this fail-closed tail
	// exists only so no future edit can fall out of the loop unreturned.
	return error('tls: unreachable')
}

// try_reclaim_recv_buf reclaims recv_buf's consumed prefix: a full drain clear()s the
// buffer in place to keep its capacity, and past recv_buf_compact_threshold it shifts
// the live tail down once; a smaller prefix is left untouched until the next read.
fn (mut c Conn) try_reclaim_recv_buf() {
	if c.recv_pos == c.recv_buf.len {
		c.recv_buf.clear()
		c.recv_pos = 0
	} else if c.recv_pos >= recv_buf_compact_threshold {
		c.recv_buf = c.recv_buf[c.recv_pos..].clone()
		c.recv_pos = 0
	}
}

// handle_alert processes an alert received after the handshake. It marks the
// connection closed and returns 0 for a clean close_notify, else an error.
fn (mut c Conn) handle_alert(payload []u8) !int {
	// read() already holds guard, so this arm terminates the connection and destroys
	// every secret and buffer directly (RFC 8446 §1.3.1/§7.5).
	// Any alert is terminal: mark closed before decoding, even a malformed body.
	c.closed = true
	peer_alert := handshake.parse_alert(payload) or {
		// A malformed alert body is fatal: destroy everything, close the socket,
		// then pin a sticky error so later reads re-report it.
		c.fatal_error = 'tls: received malformed alert'
		c.destroy_all() or {
			c.close_tcp_socket()
			return err
		}
		c.close_tcp_socket()
		return error(c.fatal_error)
	}
	// close_notify is the one clean-shutdown signal: return 0 like a normal EOF read.
	// Accept it under both levelings: this stack and older peers (Go crypto/tls)
	// still send the warning-level legacy form, while §6 makes the fatal-level
	// form the TLS 1.3 conforming one — both end the stream cleanly.
	if peer_alert.description == .close_notify {
		// §6.1 demands a symmetric answer: reply with our close_notify while the write
		// keys still exist, before destroy_all wipes them. close_notify_sent is set
		// first, so a failed reply is never retried — same rule as in close().
		c.close_notify_sent = true
		c.send_close_notify()
		// §6: never alert back after the peer terminated; the reply above already
		// ran while the keys existed, and setting the latch keeps dial()'s teardown
		// wrapper from emitting a stray alert if a later path reuses this Conn.
		c.peer_terminated = true
		c.destroy_all() or {
			c.close_tcp_socket()
			return err
		}
		c.close_tcp_socket()
		return 0
	}
	// A fatal (or illegal warning) alert needs no reply — §6 forbids alerting back —
	// but our own keys must die with it (§7.5).
	c.destroy_all() or {
		c.close_tcp_socket()
		return err
	}
	c.close_tcp_socket()
	// Warning level is prohibited in TLS 1.3 except close_notify (§6.2): a peer
	// dressing a non-close_notify alert as a warning is non-conforming, so refuse
	// it as a sticky unexpected_message — never a clean shutdown — so a truncated
	// stream cannot masquerade as a graceful one.
	if peer_alert.level == .warning {
		c.fatal_error = 'tls: received warning-level alert ${peer_alert.description} in TLS 1.3 (unexpected_message)'
		return error(c.fatal_error)
	}
	// Every other alert is fatal (RFC 8446 §6): surface the peer's code, sticky.
	c.fatal_error = 'tls: received alert ${peer_alert.description} (level=${peer_alert.level})'
	return error(c.fatal_error)
}

// handle_post_handshake consumes NewSessionTicket and KeyUpdate messages from a
// decrypted post-handshake plaintext, reassembling fragments (RFC 8446 §5.2).
fn (mut c Conn) handle_post_handshake(data []u8) ! {
	// Check before appending so a hostile fragment cannot make the buffer exceed
	// the per-message cap, even transiently.
	max_buffered := record_header_len + handshake.max_handshake_message_len
	if data.len > max_buffered - c.rec.post_hsk_buf.len {
		return c.abort_connection(.record_overflow, 'tls: post-handshake reassembly exceeds ${handshake.max_handshake_message_len} bytes without completing a message (record_overflow)')
	}
	c.rec.post_hsk_buf << data
	mut messages_seen := 0
	for {
		msg := handshake.next_unframed_message(mut c.rec.post_hsk_buf) or {
			return c.abort_connection(.decode_error, err.msg())
		}
		if msg.len == 0 {
			return // header or body still incomplete; wait for the next record
		}
		// Bound before the message is parsed, rekeyed, or answered, so the flood's
		// work is cut off at the cap rather than paid for one extra message.
		messages_seen++
		if messages_seen > max_post_handshake_messages_per_record {
			return c.abort_connection(.unexpected_message, 'tls: too many post-handshake messages in one record (unexpected_message)')
		}
		mut r := wire.new_reader(msg)
		msg_type, length := handshake.read_handshake_header(mut r) or {
			return c.abort_connection(.decode_error, err.msg())
		}
		body := r.read_bytes(length) or {
			return c.abort_connection(.decode_error, err.msg())
		}
		match msg_type {
			.new_session_ticket {
				ticket := handshake.parse_new_session_ticket(body) or {
					alert := if err.msg().contains('illegal_parameter') {
						handshake.AlertDescription.illegal_parameter
					} else {
						handshake.AlertDescription.decode_error
					}
					return c.abort_connection(alert, err.msg())
				}
				// Keep a bounded sample of the backlog; drop the overflow.
				if c.session_tickets.len < max_stored_session_tickets {
					c.session_tickets << ticket
				}
				// An accepted non-KeyUpdate message breaks the run (§4.5).
				c.rec.clear_key_update_run()
			}
			.key_update {
				// The record facade owns this mechanism: it enforces §4.5's one-in-a-row rule
				// before the parse and before any rekey derivation, then advances the read leg
				// (seq restarts at 0, §5.3). Only answering a request_update needs this side's
				// socket, so the record layer hands that back rather than owning the transport.
				request_update := c.rec.receive_key_update(mut c.hs, body) or {
					return c.abort_connection(key_update_failure_alert(err.msg()), err.msg())
				}
				if request_update {
					c.send_key_update() or {
						return c.abort_connection(.internal_error, err.msg())
					}
				}
			}
			.certificate_request {
				// This client did not offer post_handshake_auth, so this request is forbidden.
				return c.abort_connection(.unexpected_message, 'tls: unexpected post-handshake CertificateRequest (unexpected_message)')
			}
			else {
				// RFC 8446 §4.6: any other message is a fatal unexpected_message error
				return c.abort_connection(.unexpected_message, 'tls: unexpected post-handshake message ${msg_type} (unexpected_message)')
			}
		}
	}
}

// write_records serializes records into one buffer and writes the full batch,
// continuing after short writes. It returns only after all bytes are sent or an
// error occurs, so callers do not need a partial byte count (RFC 8446 §5).
fn (mut c Conn) write_records(recs []Record) ! {
	if c.sock_closed || c.sock == unsafe { nil } {
		return error('tls: socket is closed')
	}
	if recs.len == 0 {
		return
	}
	mut total := 0
	for rec in recs {
		total += record_header_len + rec.payload.len
	}
	mut batched := []u8{cap: total}
	for rec in recs {
		rec.serialize_into(mut batched)
	}
	mut written := 0
	for written < batched.len {
		n := c.sock.write(batched[written..])!
		if n <= 0 {
			return error('tls: socket write made no progress (${written}/${batched.len} bytes)')
		}
		written += n
	}
}

// write_application_data encrypts and sends application data while the caller
// holds guard. Any failure is terminal because sequence numbers or stream bytes
// may already have advanced, so it aborts the connection in place.
@[direct_array_access]
fn (mut c Conn) write_application_data(data []u8) !int {
	// A fatal error is sticky: refuse to write application data on a dead
	// connection, mirroring read()'s guard (§6.1).
	if c.fatal_error != '' {
		return error(c.fatal_error)
	}
	return c.write_application_data_inner(data) or {
		alert := if err.msg().contains('sequence limit') {
			handshake.AlertDescription.record_overflow
		} else {
			handshake.AlertDescription.internal_error
		}
		// Terminal failure: pin the sticky error, send the best-effort fatal alert, close
		// the socket and destroy every secret/buffer in one phase; the caller holds guard.
		return c.abort_connection(alert, 'tls: application write failed: ${err.msg()}')
	}
}

// write_application_data_inner performs record generation and socket writes.
// The caller holds guard and owns terminal error handling.
@[direct_array_access]
fn (mut c Conn) write_application_data_inner(data []u8) !int {
	// Chunk to max_plaintext_record_len (2^14): TLS forbids larger records (RFC 8446 §5.1).
	mut offset := 0
	mut batch := []Record{}
	mut batch_len := 0
	for offset < data.len {
		// §5.5: rekey one slot early so the KeyUpdate notice still fits under the old key.
		if c.rec.needs_rekey() {
			// Flush the pending batch first: it must precede the KeyUpdate notice on the wire.
			c.write_records(batch)!
			batch = []Record{}
			batch_len = 0
			c.send_key_update()!
		}
		end := if data.len - offset > max_plaintext_record_len {
			offset + max_plaintext_record_len
		} else {
			data.len
		}
		chunk := data[offset..end]
		enc := c.rec.c_app_protection.encrypt_record(.application_data, chunk)!
		batch << enc
		batch_len += record_header_len + enc.payload.len
		// Ship at the batch bound or on the last chunk; coalescing never mixes generations.
		if batch_len >= write_batch_threshold || end == data.len {
			c.write_records(batch)!
			batch = []Record{}
			batch_len = 0
		}
		offset = end
	}
	return data.len
}

// send_key_update emits a KeyUpdate(update_not_requested) under the current write keys
// and then swaps in the next write generation (RFC 8446 §4.6.3/§5.5) — the record facade
// builds the notice and completes the rekey on either side of this socket write, so the
// notice is always the last record of the generation it retires. Both the §5.5 write-path
// trigger and the answer to a peer's request_update come through here. The caller must
// already hold guard.
fn (mut c Conn) send_key_update() ! {
	notice := c.rec.build_key_update_notice()!
	c.write_records([notice])!
	c.rec.complete_key_update(mut c.hs)!
}

// key_update_failure_alert maps a receive_key_update error to the fatal alert it must
// surface: each record-layer and core path tags its own error, so the RFC name in the
// message decides — a §4.5 run violation is unexpected_message, a misframed body
// decode_error, a bad request_update value illegal_parameter, and a derivation or
// install fault falls back to internal_error (RFC 8446 §4.6.3/§6). Same approach as
// decrypt_failure_alert.
fn key_update_failure_alert(msg string) handshake.AlertDescription {
	if msg.contains('unexpected_message') {
		return .unexpected_message
	}
	if msg.contains('illegal_parameter') {
		return .illegal_parameter
	}
	if msg.contains('decode_error') {
		return .decode_error
	}
	return .internal_error
}

// abort_connection terminates the connection while the caller holds guard: it pins the
// sticky failure, sends the best-effort fatal alert, closes the socket, destroys every
// secret and buffer, and returns the error to propagate (RFC 8446 §6). It never locks,
// so the read-origin, write-origin and alert aborts all share this one body without a
// recursive acquisition; the handshake-phase callers run under do_handshake's guard.
fn (mut c Conn) abort_connection(alert handshake.AlertDescription, message string) IError {
	c.fail_alert = alert
	c.closed = true
	c.fatal_error = message
	c.send_failure_alert()
	c.close_tcp_socket()
	c.destroy_all() or { return err }
	return error(message)
}

// classify_failure records the alert the adapter must surface for a handshake-phase
// failure and returns the terminating error, so a call site cannot pin half of it.
// Unlike abort_connection, it deliberately does NOT tear down: every error
// propagates to dial()'s single teardown wrapper, which sends the pinned alert
// instead of the stale .internal_error fallback for an unset fail_alert.
fn (mut c Conn) classify_failure(alert handshake.AlertDescription, message string) IError {
	c.fail_alert = alert
	return error(message)
}

// destroy_all is the single terminal teardown, run with guard held: it drops the core's
// live traffic secrets (the whole key schedule unless the handshake reached .connected),
// wipes the record facade's four protection states, and scrubs the read-side staging
// buffers, so no close or abort path can wipe one layer and forget another
// (RFC 8446 §1.3.1/§7.5). It never locks itself, so the read-origin abort, handle_alert,
// close() and the dial() teardown all share one body without a recursive acquisition;
// wipe() is idempotent, so it is safe on protection state that was never installed.
fn (mut c Conn) destroy_all() ! {
	if c.hs.state() == .connected {
		// The core reaches .connected before do_handshake's explicit handshake zeroize
		// runs, so a failure in that tail window must wipe both levels; the handshake
		// zeroize is idempotent, making the normal double call after it is a no-op.
		c.hs.zeroize_traffic_secrets()!
	} else {
		c.hs.fail_and_zeroize_secrets()
	}
	c.rec.wipe_protection_states()
	// Scrub undelivered plaintext and any partial post-handshake message staged for the
	// reader (§7.5 spirit: no decrypted or framed bytes left behind in freed memory).
	c.wipe_app_buf()
	c.rec.wipe_post_hsk_buf()
}

// send_failure_alert emits the fatal alert for a dead handshake (RFC 8446 §6),
// picking the protection phase from the states actually installed.
fn (mut c Conn) send_failure_alert() {
	if c.peer_terminated {
		return
	}
	// One alert per failure (§6): a second caller (abort_connection then dial()'s
	// wrapper) must never re-alert, least of all plaintext onto a closed socket.
	if c.failure_alert_sent {
		return
	}
	c.failure_alert_sent = true
	desc := c.fail_alert or { c.hs.failure_alert() }
	payload := handshake.new_alert(desc).encode()
	// A protection state is initialized exactly when its IV is the full nonce base.
	if c.rec.c_app_protection.iv.len == aead_nonce_len {
		enc := c.rec.c_app_protection.encrypt_record(.alert, payload) or { return }
		c.write_records([enc]) or {}
		return
	}
	if c.rec.c_hsk_protection.iv.len == aead_nonce_len {
		enc := c.rec.c_hsk_protection.encrypt_record(.alert, payload) or { return }
		c.write_records([enc]) or {}
		return
	}
	c.write_records([new_record(.alert, payload)]) or {}
}

// finish_parked_read is called by a reader under guard once it stops parking in
// sock.read(): it drops the parked-reader count and, if a close() deferred the fd
// close behind this very reader, performs that close once no reader remains. Safe
// to call even when nothing is pending.
fn (mut c Conn) finish_parked_read() {
	c.readers--
	if c.sock_close_pending && c.readers <= 0 && !c.sock_closed && c.sock != unsafe { nil } {
		c.sock.close() or {}
		c.sock_closed = true
		c.sock_close_pending = false
	}
}

// close_tcp_socket releases the TCP socket without sending TLS shutdown alerts.
// It is guarded separately because handle_alert marks closed but leaves it open.
// If a reader is parked in sock.read() (`readers > 0`) the descriptor is NOT closed
// here — that would let another thread recycle the fd under the in-flight recv() —
// instead the close is deferred to that reader's finish_parked_read(). A parked read
// is not woken by close() on POSIX regardless (see the Conn lock discipline), so
// deferring the close costs nothing on the wake-up path.
fn (mut c Conn) close_tcp_socket() {
	if c.sock_closed || c.sock == unsafe { nil } {
		return
	}
	if c.readers > 0 {
		c.sock_close_pending = true
		return
	}
	c.sock.close() or {}
	c.sock_closed = true
}

// send_close_notify writes a close_notify alert, encrypted with the application
// keys (RFC 8446 §5.2/§6.1); a plaintext alert would violate §6, so send nothing.
fn (mut c Conn) send_close_notify() {
	if c.sock_closed || c.sock == unsafe { nil } {
		return
	}
	if c.hs.state() != .connected {
		return
	}
	notify := handshake.new_alert(.close_notify)
	enc := c.rec.c_app_protection.encrypt_record(.alert, notify.encode()) or {
		return
	}
	c.write_records([enc]) or {}
}

// wipe_app_buf zeroes then drops the staged-but-undelivered application
// plaintext, so freed buffer memory never retains decrypted bytes (§7.5 spirit).
fn (mut c Conn) wipe_app_buf() {
	zero_bytes(mut c.app_buf)
	c.app_buf.clear()
	c.app_pos = 0
}

// Helpers (utility)

// new_tls_protection_from_keys builds one direction's protection state from its opaque key
// material and drains the key-and-IV slots it needs: the AEAD backend keeps its own key
// and the protection state clones its IV, so both values are scrubbed here before
// returning (§7.5). The takes empty this by-value copy's fields; the caller's handle
// keeps the remaining bytes (the always-empty TLS `hp` slot included) for its own
// wipe() to cover. Which slot the result lands in is the caller's choice, made by name
// off the generation (`take_client_material()` / `take_server_material()`).
// Sequence numbers restart at 0 per generation (§5.3).
fn new_tls_protection_from_keys(suite handshake.CipherSuite, mut keys handshake.KeyMaterial) !TLSProtection {
	mut key := keys.take_key()
	mut iv := keys.take_iv()
	defer {
		zero_bytes(mut key)
		zero_bytes(mut iv)
	}
	aead := handshake.new_aead_for_suite(suite, key)!
	return new_tls_protection(aead, iv)!
}

// is_transport_error reports whether a read_raw_record failure carries the socket
// I/O tag: a transient condition (per-op timeout, EINTR, EAGAIN) the caller may
// retry, not a protocol fault. Un-tagged errors — stream damage or framing
// failures — stay terminal so truncation is still detected (§6.1).
fn is_transport_error(msg string) bool {
	return msg.starts_with('tls: socket read timed out or interrupted')
}

// decrypt_failure_alert maps a decrypt_record error to the fatal alert the record
// layer must surface: rekey exhaustion is record_overflow, a failed tag is
// bad_record_mac, anything else is unexpected_message (RFC 8446 §5.5/§6).
fn decrypt_failure_alert(msg string) handshake.AlertDescription {
	if msg.contains('sequence limit') {
		return .record_overflow
	}
	if msg.contains('AEAD decryption failed') {
		return .bad_record_mac
	}
	return .unexpected_message
}

// zero_bytes wipes a buffer in place before it is dropped or reused, so freed
// memory retains no plaintext (RFC 8446 §7.5 spirit). It is tls's one zero-fill;
// the other modules keep their own private equivalents.
@[direct_array_access]
fn zero_bytes(mut buf []u8) {
	for i in 0 .. buf.len {
		buf[i] = 0
	}
}

// mono_ms is the module's monotonic millisecond clock, read from
// time.sys_mono_now() (CLOCK_MONOTONIC, nanoseconds ÷ 1e6 per millisecond): an NTP
// step or wall-clock edit cannot stretch or truncate the handshake window. The
// origin is system boot, not the epoch, so only differences against a mono_ms()
// baseline are used.
fn mono_ms() i64 {
	return i64(time.sys_mono_now()) / i64(time.millisecond)
}

// host_from_address extracts the host part of a dial address ("host:port" or
// "[ipv6]:port") so a bare address still supplies the SNI and verification name.
fn host_from_address(address string) string {
	// Bracketed form is an IPv6 literal: return the inner address, or the string unchanged.
	if address.starts_with('[') {
		end := address.index(']') or { return address }
		return address[1..end]
	}
	// host:port — split on the LAST colon so the port separator is removed.
	if colon := address.last_index(':') {
		return address[..colon]
	}
	// No colon at all: the entire string is already just a host (e.g. "localhost").
	return address
}
