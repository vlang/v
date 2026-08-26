// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net
import net.quic
import crypto.ecdsa
import crypto.rand
import sync
import time

// h3_server.v: Phase 13e -- a minimal HTTP/3 server driver bridging one UDP
// socket to quic.QuicListener/quic.H3Conn's caller-driven poll()/
// process_timeouts() surface, mirroring h2_server.v's Handler-based
// dispatch and request/response construction (pseudo-header validation,
// content-length cross-check, trailer emission).
//
// Deliberately NOT a thread-per-connection model like h2_server.v's own
// (one H2ServerConn per accepted TCP connection, each running its own
// serve() on its own thread): h2_server.v can do that because TCP already
// gives each connection an isolated socket a thread can block on
// independently. QUIC/HTTP-3 multiplexes MANY logical connections behind
// ONE UDP socket, demuxed by connection ID through a single shared table
// (quic.QuicListener) -- splitting that demux across threads would need
// its own synchronization this module doesn't have, and PROGRESS.md's own
// "Scope decisions in effect" already commits this whole net.quic/net.http
// HTTP/3 stack to a single-threaded, caller-driven event loop for exactly
// this reason. H3Server therefore drives every connection it manages from
// ONE loop (serve()), same as h3_mux_conn.v's own single-driver-thread
// client model, just generalized from one connection to many via
// quic.QuicListener.
//
// Consequence of that single-loop model: unlike h2_server.v (where a write
// failure on one connection's own H2ServerConn.serve() only ever ends that
// ONE TCP connection's loop), a per-connection failure here must never be
// allowed to propagate out of serve() itself -- doing so would take down
// every OTHER connection this server is managing. Every per-connection
// operation below (drive_events, request dispatch, response sends) is
// therefore caught and contained to just that one connection, never
// bubbled up as serve()'s own error return.

// h3_server_max_request_body caps the in-memory request body this server
// will buffer before answering with a 413-equivalent failure. Mirrors
// h2_server.v's identical h2_server_max_request_body reasoning and value.
const h3_server_max_request_body = 8 * 1024 * 1024

// h3_server_stream_key builds the composite key H3Server's own per-
// request-stream buffering table (H3Server.streams) is keyed by --
// composite because ONE H3Server manages MANY connections at once (unlike
// h2_server.v's H2ServerConn, which is one instance per TCP connection and
// so only ever needs a bare stream_id to key by). `stream_id` always
// formats as pure decimal digits, so the LAST `:` in the result is always
// this function's own separator, never one embedded in `conn_id` -- safe
// against collision (both as a map key and against prune_streams_for_conn's
// own `starts_with` prefix check below) only because `conn_id` values
// (quic.QuicConn.connection_id(), a scid) are all the SAME fixed
// quic.local_cid_len length today, so no one conn_id can ever be a proper
// prefix of another's key. Would need re-deriving if this module ever
// supported variable-length or multiple connection IDs per connection.
fn h3_server_stream_key(conn_id string, stream_id u64) string {
	return '${conn_id}:${stream_id}'
}

// H3ServerStream buffers one in-flight request until its FIN arrives --
// the HTTP/3 counterpart of h2_server.v's H2ServerStream. Simpler than
// that type: HTTP/3's own message-framing state machine (already fully
// enforced inside quic.H3Conn/h3_request_stream.v before an event ever
// reaches this file) rules out most of what H2ServerStream has to track
// for itself (HPACK block assembly, END_HEADERS/CONTINUATION bookkeeping,
// self-dependent-PRIORITY detection -- none of those concepts exist in
// HTTP/3's wire format at all).
struct H3ServerStream {
mut:
	headers []quic.QpackFieldLine
	body    []u8
}

// H3ServerParams configures a new H3Server for its whole lifetime --
// mirrors quic.QuicListenerParams (which this wraps) plus the Handler
// every fully-buffered request is dispatched to.
pub struct H3ServerParams {
pub:
	alpn_protocols    []string = ['h3']
	certificate_chain []quic.CertificateEntry
	signing_key       ecdsa.PrivateKey
	// retry_token_key is this server instance's own long-lived secret for
	// quic.QuicListener's Retry-token machinery (see
	// quic.QuicListenerParams.retry_token_key's own doc comment) --
	// auto-generated via crypto.rand if left empty, since (unlike the
	// certificate/key pair) there is no reason a caller must supply this
	// themselves; a caller that DOES want to pin/persist it across
	// restarts (e.g. so a token issued just before a restart is still
	// honored just after) may still set it explicitly.
	retry_token_key      []u8
	always_retry         bool = true
	transport_parameters quic.QuicTransportParameters
	handler              Handler
}

// H3Server drives one HTTP/3 server across a single UDP socket -- see this
// file's own module doc comment for why that's necessarily a single loop
// across every connection, not one thread per connection.
@[heap]
pub struct H3Server {
mut:
	socket    &net.UdpConn
	listener  &quic.QuicListener
	h3_params quic.H3ConnParams
	h3_conns  map[string]&quic.H3Conn
	streams   map[string]&H3ServerStream
	// peer_by_str recovers a real net.Addr from the opaque []u8 peer
	// identifier quic.QuicListener's own outgoing datagrams carry (that
	// module is deliberately transport-agnostic -- see listener.v's own
	// module doc comment -- so IT never touches net.Addr itself). Keyed by
	// addr.str() -- the exact bytes this server itself chose to pass as
	// the `peer []u8` parameter on the way in, so every peer QuicListener
	// ever echoes back on the way out was necessarily recorded here first.
	// Deliberately unbounded for v1 (grows with the number of distinct
	// peer addresses ever seen, never pruned) -- a documented, deferrable
	// follow-up, not a correctness gap: bounding it would need visibility
	// into WHEN quic.QuicListener retires a connection that this server
	// doesn't have (QuicListener exposes no per-connection introspection
	// beyond connection_count()), and growing it costs an attacker the
	// same real packet volume as any other UDP-flood-shaped cost, not a
	// disproportionate amplification.
	peer_by_str map[string]net.Addr
	handler     Handler
	// shutdown_mu/closing let close() (typically called from a DIFFERENT
	// thread than the one running serve()'s own loop -- e.g. a signal
	// handler, or the owning goroutine's caller) tell that loop to stop
	// WITHOUT depending on socket-close-interrupts-a-blocked-read-on-
	// another-thread being reliable: that behavior is a genuine, well-
	// known POSIX/platform gotcha (observed directly on this project's own
	// Mac dev environment: a concurrent close() did not reliably unblock
	// serve()'s in-progress socket.read() promptly). serve()'s own read
	// timeout is always bounded (h3_driver_poll_interval or sooner), so
	// checking this flag once per loop iteration bounds the worst-case
	// shutdown latency to one iteration regardless of whether the socket
	// close itself ever interrupts anything.
	shutdown_mu &sync.Mutex = sync.new_mutex()
	closing     bool
}

// new_h3_server binds a UDP socket at listen_addr and constructs the
// quic.QuicListener/H3ConnParams state a serve() call then drives. Fails
// only on a bind failure or an invalid retry_token_key length (mirrors
// quic.new_quic_listener's own failure surface) -- everything else in
// `params` is validated lazily, per connection attempt, exactly like
// quic.QuicListenerParams' own fields are.
pub fn new_h3_server(listen_addr string, params H3ServerParams) !&H3Server {
	mut retry_key := params.retry_token_key.clone()
	if retry_key.len == 0 {
		retry_key = rand.bytes(quic.retry_token_key_len)!
	}
	listener := quic.new_quic_listener(quic.QuicListenerParams{
		transport_parameters: params.transport_parameters
		alpn_protocols:       params.alpn_protocols
		certificate_chain:    params.certificate_chain
		signing_key:          params.signing_key
		retry_token_key:      retry_key
		always_retry:         params.always_retry
	})!
	socket := net.listen_udp(listen_addr)!
	return &H3Server{
		socket:    socket
		listener:  listener
		h3_params: quic.H3ConnParams{
			settings:                     h3_default_own_settings()
			own_qpack_max_table_capacity: h3_default_own_qpack_max_table_capacity
		}
		handler:   params.handler
	}
}

// local_addr reports the address this server's UDP socket is actually
// bound to -- needed by any caller (real deployment or test) that binds
// with an OS-assigned port (e.g. `new_h3_server(':0', ...)`) and must
// discover which one it got.
pub fn (s &H3Server) local_addr() !net.Addr {
	return s.socket.sock.address()
}

// close signals serve()'s loop to stop (checked once per iteration, so
// worst-case shutdown latency is bounded by one iteration's read timeout
// -- see shutdown_mu/closing's own doc comment for why this is the
// primary shutdown signal rather than a best-effort nicety) and closes
// this server's underlying socket. Safe to call from a different thread
// than the one running serve()'s own loop.
pub fn (mut s H3Server) close() ! {
	s.shutdown_mu.lock()
	s.closing = true
	s.shutdown_mu.unlock()
	s.socket.close()!
}

// serve runs this server's single-threaded accept/demux/dispatch loop
// (see this file's own module doc comment) until close() is called or the
// socket errors. Blocks the calling thread.
pub fn (mut s H3Server) serve() ! {
	mut buf := []u8{len: h3_datagram_buf_size}
	mut next_timeout := ?u64(none)
	for {
		s.shutdown_mu.lock()
		should_stop := s.closing
		s.shutdown_mu.unlock()
		if should_stop {
			return
		}
		mut wait := h3_driver_poll_interval
		if nt := next_timeout {
			now_ms := h3_now_ms()
			mut remaining := i64(0)
			if nt > now_ms {
				remaining = i64(nt - now_ms)
			}
			candidate := remaining * time.millisecond
			if candidate < wait {
				wait = candidate
			}
		}
		if wait <= 0 {
			wait = 1 * time.millisecond
		}
		s.socket.set_read_timeout(wait)

		n, addr := s.socket.read(mut buf) or {
			if err.code() != net.err_timed_out_code {
				return
			}
			0, net.Addr{}
		}
		now := h3_now_ms()
		mut result := quic.QuicListenerPollResult{}
		if n > 0 {
			peer_str := addr.str()
			s.peer_by_str[peer_str] = addr
			result = s.listener.poll(buf[..n].clone(), peer_str.bytes(), now)!
		} else {
			result = s.listener.process_timeouts(now)!
		}
		next_timeout = result.next_timeout
		s.absorb_and_dispatch(result)
		for dg in result.outgoing {
			peer := s.peer_by_str[dg.peer.bytestr()] or { continue }
			s.socket.write_to(peer, dg.bytes) or { continue }
		}
	}
}

// absorb_and_dispatch groups one quic.QuicListenerPollResult's events by
// which connection produced them (poll() -- one incoming datagram -- only
// ever touches ONE connection, but process_timeouts() drives EVERY
// managed connection in a single call, so more than one group is a real
// possibility here), lazily wraps each newly-seen connection in its own
// H3Conn, drives it, and dispatches whatever H3Events come out. Never
// returns an error -- see this file's own module doc comment for why a
// single connection's failure must never propagate out of serve() and
// affect every other connection this server is managing; every fallible
// step here is caught and contained to just the one connection it belongs
// to.
fn (mut s H3Server) absorb_and_dispatch(result quic.QuicListenerPollResult) {
	mut order := []string{}
	mut events_by_key := map[string][]quic.QuicEvent{}
	mut conn_by_key := map[string]&quic.QuicConn{}
	for ev in result.events {
		key := ev.conn.connection_id()
		if key !in events_by_key {
			order << key
			events_by_key[key] = []quic.QuicEvent{}
			conn_by_key[key] = ev.conn
		}
		events_by_key[key] << ev.event
	}
	for key in order {
		events := events_by_key[key] or { continue }
		mut qc := conn_by_key[key] or { continue }
		mut h3c := s.h3_conns[key] or {
			c := quic.new_h3_conn(mut qc, s.h3_params)
			s.h3_conns[key] = c
			c
		}
		h3r := h3c.drive_events(events) or {
			// A connection-scoped H3-level protocol violation on THIS
			// connection alone -- e.g. require_valid_frame_for_role's own
			// doc comment: "a wrong-frame-type-for-this-role violation
			// reflects a fundamental confusion about what the peer thinks
			// this stream is, not a fault isolated to one request/response
			// exchange". Must never propagate out of serve() and affect
			// every OTHER connection this server is managing (this file's
			// own module doc comment) -- but dropping just s.h3_conns[key]
			// here WITHOUT ALSO closing the underlying qc would leave that
			// QuicConn alive and still registered in s.listener: the very
			// next event for it would silently construct a BRAND NEW,
			// blank H3Conn wrapper (losing every bit of prior H3-level
			// state -- in-flight request streams, this connection's own
			// already-opened control/QPACK stream IDs) and re-run open_
			// own_streams_if_ready from scratch, opening a SECOND set of
			// control/QPACK streams on a connection that already has one.
			// qc.close() is the correct fix -- mirrors h3_mux_conn.v's own
			// driver_loop, which answers an identical h3.poll()/process_
			// timeouts() error by calling fail_conn() to tear the whole
			// connection down, never by just discarding its own local H3
			// wrapper state.
			code := if err.code() != 0 {
				u64(err.code())
			} else {
				quic.H3ErrorCode.general_protocol_error.code()
			}
			qc.close(code, err.msg())
			s.h3_conns.delete(key)
			s.prune_streams_for_conn(key)
			continue
		}
		for hev in h3r.events {
			s.handle_h3_event(key, mut h3c, hev)
		}
		if h3c.closed() {
			s.h3_conns.delete(key)
			s.prune_streams_for_conn(key)
		}
	}
}

// prune_streams_for_conn drops every H3ServerStream entry belonging to
// conn_id -- called once a connection is gone (closed, or dropped for a
// protocol violation) so its in-flight request buffers don't linger
// forever. O(n) over every currently-buffered stream across ALL
// connections; acceptable here since that whole table is already bounded
// by the total number of concurrently in-flight requests this server is
// managing (itself bounded by every live connection's own
// initial_max_streams_bidi), mirroring h2_server.v's own tolerance for a
// bounded linear scan (h2_server_max_locally_reset_tracked's eviction).
fn (mut s H3Server) prune_streams_for_conn(conn_id string) {
	prefix := '${conn_id}:'
	mut dead := []string{}
	for key in s.streams.keys() {
		if key.starts_with(prefix) {
			dead << key
		}
	}
	for key in dead {
		s.streams.delete(key)
	}
}

// handle_h3_event actions one H3Event from a server-role H3Conn: buffers
// request_headers/request_data, and runs the completed request through
// this server's Handler once request_ended fires. request_trailers is
// deliberately NOT delivered anywhere (a documented v1 scope limit,
// mirroring several other "the wire-level machinery exists, the
// caller-facing delivery does not yet" notes already accepted throughout
// this module -- request trailers are rare in practice, unlike RESPONSE
// trailers, which send_response below DOES support, matching
// h2_server.v's own asymmetric support for exactly the same reason).
fn (mut s H3Server) handle_h3_event(conn_id string, mut h3c quic.H3Conn, ev quic.H3Event) {
	match ev.kind {
		.request_headers {
			stream_id := ev.stream_id or { return }
			key := h3_server_stream_key(conn_id, stream_id)
			s.streams[key] = &H3ServerStream{
				headers: ev.headers
			}
		}
		.request_data {
			stream_id := ev.stream_id or { return }
			key := h3_server_stream_key(conn_id, stream_id)
			mut st := s.streams[key] or { return }
			if st.body.len + ev.data.len > h3_server_max_request_body {
				s.send_error_response(mut h3c, stream_id, 413)
				s.streams.delete(key)
				return
			}
			st.body << ev.data
		}
		.request_ended {
			stream_id := ev.stream_id or { return }
			key := h3_server_stream_key(conn_id, stream_id)
			st := s.streams[key] or { return }
			s.run_request(mut h3c, stream_id, st)
			s.streams.delete(key)
		}
		.request_error {
			stream_id := ev.stream_id or { return }
			s.streams.delete(h3_server_stream_key(conn_id, stream_id))
		}
		else {
			// settings_received/goaway/connection_error: nothing for this
			// layer to act on beyond absorb_and_dispatch's own closed()
			// check (connection_error) -- and response_*/response_ended
			// never fire on a server-role connection at all (h3_conn.v's
			// own module doc comment).
		}
	}
}

// run_request validates and builds a Request from a fully-buffered
// H3ServerStream, runs it through the Handler, and sends the resulting
// Response back -- mirrors h2_server.v's run_request. Never fails
// outward: a malformed request answers with a best-effort 400 instead of
// resetting the stream, since quic.H3Conn has no per-stream RST_STREAM/
// STOP_SENDING send API yet (h3_conn.v's own fail_request_stream doc
// comment documents this as an existing, separate scope limit).
fn (mut s H3Server) run_request(mut h3c quic.H3Conn, stream_id u64, st &H3ServerStream) {
	req := h3_build_request(st) or {
		s.send_error_response(mut h3c, stream_id, 400)
		return
	}
	resp := s.handler.handle(req)
	s.send_response(mut h3c, stream_id, resp)
}

// h3_build_request validates st's pseudo-headers (RFC 9114 §4.3.1) and
// converts it into a net.http Request -- mirrors h2_server.v's
// h2_validate_request_pseudo + build_request, merged into one function
// since, unlike h2_server.v's H2ServerStream, H3ServerStream never needs
// its headers validated independently of building the request from them.
fn h3_build_request(st &H3ServerStream) !Request {
	h3_validate_request_pseudo(st.headers)!
	mut req := Request{
		version: .v3_0
		header:  new_header()
	}
	mut method := ''
	mut path := ''
	mut authority := ''
	mut content_length := -1
	for f in st.headers {
		match f.name {
			':method' {
				method = f.value
			}
			':path' {
				path = f.value
			}
			':authority' {
				authority = f.value
			}
			':scheme' {
				// Validated already; handlers infer the scheme from Host,
				// matching the HTTP/1.1 and HTTP/2 paths.
			}
			else {
				if f.name.starts_with(':') {
					continue
				}
				if f.name == 'content-length' {
					if !h2_all_digits(f.value) {
						return error('h3 server: malformed content-length "${f.value}"')
					}
					cl := f.value.int()
					// RFC 9110 §8.6: multiple content-length fields with
					// differing values are malformed -- validate every
					// occurrence, not just the last.
					if content_length >= 0 && content_length != cl {
						return error('h3 server: conflicting content-length values ${content_length} and ${cl}')
					}
					content_length = cl
				}
				req.header.add_custom(f.name, f.value) or {}
			}
		}
	}
	// RFC 9114 §4.2 mirrors RFC 9113 §8.1.2.6: a declared content-length
	// MUST equal the sum of the DATA payload lengths.
	if content_length >= 0 && content_length != st.body.len {
		return error('h3 server: content-length ${content_length} != DATA length ${st.body.len}')
	}
	req.method = method_from_str(method)
	if authority != '' && !req.header.contains(.host) {
		req.header.add(.host, authority)
	}
	req.url = path
	req.data = st.body.bytestr()
	req.host = authority
	return req
}

// h3_validate_request_pseudo enforces the RFC 9114 §4.3.1 rules a request
// field section must satisfy -- RFC 9114 §4.1.1 mirrors RFC 9113 §8.1.2.2's
// pseudo-header rules almost verbatim for HTTP/3 (already the precedent
// h3_client.v's own to_h3_request doc comment states for the outgoing
// side: "RFC 9114 §4.1.1 intentionally mirrors RFC 9113 §8.1.2.2 here, so
// there is nothing HTTP/3-specific to re-derive"): only the request
// pseudo-headers, each at most once, all appearing before any regular
// field, with :method, :path and :scheme present. Every regular field is
// validated via h2_request_field_error (h2_server.v), reused directly for
// the identical reason -- RFC 9114 §4.2 mirrors RFC 9113 §8.2.2's
// forbidden-octet/connection-specific-field/TE rules verbatim.
fn h3_validate_request_pseudo(headers []quic.QpackFieldLine) ! {
	mut seen_regular := false
	mut has_method := false
	mut has_path := false
	mut has_scheme := false
	mut has_authority := false
	for f in headers {
		if f.name.starts_with(':') {
			if seen_regular {
				return error('pseudo-header "${f.name}" after a regular field')
			}
			if h2_field_value_has_forbidden_octet(f.value) {
				return error('forbidden NUL/CR/LF octet in pseudo-header "${f.name}"')
			}
			match f.name {
				':method' {
					if has_method {
						return error('duplicate :method pseudo-header')
					}
					if f.value == '' {
						return error('empty :method pseudo-header')
					}
					has_method = true
				}
				':path' {
					if has_path {
						return error('duplicate :path pseudo-header')
					}
					if f.value == '' {
						return error('empty :path pseudo-header')
					}
					has_path = true
				}
				':scheme' {
					if has_scheme {
						return error('duplicate :scheme pseudo-header')
					}
					if f.value == '' {
						return error('empty :scheme pseudo-header')
					}
					has_scheme = true
				}
				':authority' {
					if has_authority {
						return error('duplicate :authority pseudo-header')
					}
					has_authority = true
				}
				else {
					return error('unknown request pseudo-header "${f.name}"')
				}
			}
		} else {
			seen_regular = true
			reason := h2_request_field_error(f.name, f.value)
			if reason != '' {
				return error(reason)
			}
		}
	}
	if !has_method || !has_path || !has_scheme {
		return error('request omits a mandatory pseudo-header (:method/:path/:scheme)')
	}
}

// send_response QPACK-encodes resp and sends it as this stream's response,
// mirroring h2_server.v's own send_response (Trailers-Only shape when
// there's no body, trailers filtered/forbidden-octet-checked the same
// way). Best-effort: a send failure here means this ONE stream's response
// is lost (the peer sees an incomplete/truncated response and can retry),
// never propagated as a connection- or server-wide failure -- see this
// file's own module doc comment.
fn (mut s H3Server) send_response(mut h3c quic.H3Conn, stream_id u64, resp Response) {
	status := if resp.status_code == 0 { 200 } else { resp.status_code }
	mut fields := [
		quic.QpackFieldLine{
			name:  ':status'
			value: status.str()
		},
	]
	for key in resp.header.keys() {
		lkey := key.to_lower()
		if lkey in h2_conn_specific_headers {
			continue
		}
		for val in resp.header.custom_values(key) {
			fields << quic.QpackFieldLine{
				name:  lkey
				value: val
			}
		}
	}
	body := resp.body.bytes()
	has_body := body.len > 0
	trailer_fields := h3_outbound_trailer_fields(resp.trailers)
	has_trailers := trailer_fields.len > 0

	if !has_body && has_trailers {
		fields << trailer_fields
		h3c.send_response_headers(stream_id, fields, true) or {}
		return
	}
	h3c.send_response_headers(stream_id, fields, !has_body) or { return }
	if has_body {
		h3c.send_response_data(stream_id, body, !has_trailers) or { return }
	}
	if has_trailers {
		h3c.send_response_headers(stream_id, trailer_fields, true) or {}
	}
}

// send_error_response answers `stream_id` with a minimal, bodyless
// status-only response -- the best available signal for a locally-detected
// failure (a malformed request, or a request body over
// h3_server_max_request_body), since quic.H3Conn has no per-stream
// RST_STREAM/STOP_SENDING send API yet (see run_request's own doc
// comment). Best-effort, matching send_response.
fn (mut s H3Server) send_error_response(mut h3c quic.H3Conn, stream_id u64, status int) {
	h3c.send_response_headers(stream_id, [
		quic.QpackFieldLine{
			name:  ':status'
			value: status.str()
		},
	], true) or {}
}

// h3_outbound_trailer_fields converts handler-authored trailers into wire
// fields -- the HTTP/3 counterpart of h2_server.v's
// h2_outbound_trailer_fields (not reused directly despite doing the
// identical filtering: that function returns []H2HeaderField, not
// []quic.QpackFieldLine, and its receiver -- an H2ServerConn -- has no h3
// equivalent to construct just to call it; the underlying rule set is
// identical, so both apply the same RFC 9113 §8.2.2/RFC 9114 §4.2
// hop-by-hop filter, the same pseudo-header guard, and the same forbidden-
// octet check).
fn h3_outbound_trailer_fields(trailers Header) []quic.QpackFieldLine {
	mut fields := []quic.QpackFieldLine{}
	for key in trailers.keys() {
		lkey := key.to_lower()
		if lkey.starts_with(':') || lkey in h2_conn_specific_headers {
			continue
		}
		for val in trailers.custom_values(key) {
			if h2_field_value_has_forbidden_octet(val) {
				continue
			}
			fields << quic.QpackFieldLine{
				name:  lkey
				value: val
			}
		}
	}
	return fields
}
