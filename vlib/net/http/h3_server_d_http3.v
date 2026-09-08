// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import net
import net.quic
import crypto.ecdsa
import crypto.rand
import sync

// This file is gated behind `-d http3`, same as h3_client_d_http3.v/
// h3_mux_conn_d_http3.v/h3_udp_dial_d_http3.v -- mirroring the merge of
// #28286 ("net.http: make HTTP/3 opt-in behind `-d http3` to avoid
// requiring OpenSSL") into this branch. Unlike those client-side files
// (which transport.v's own always-compiled dispatch logic references, and
// so need a real transport_h3_notd_http3.v stub to keep type-checking with
// the flag off), nothing else in `module http` calls into H3Server/
// H3ServerParams/new_h3_server -- confirmed via a full-module grep before
// this rename. So no stub file exists for the flag-off case: without
// `-d http3`, `http.new_h3_server` simply does not exist, the same as any
// other undeclared symbol. A caller wanting a softer "compiles either way,
// fails at runtime with a clear message" surface (matching H3MuxConn's own
// stub) would need one written deliberately, including a decision on what
// H3ServerParams' cert/key fields (typed via quic.CertificateEntry/
// ecdsa.PrivateKey) should look like without net.quic available -- left
// as an open, undecided follow-up rather than assumed here.
//
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

// h3_server_max_buffered_request_body_per_connection also bounds the sum of
// unfinished request bodies retained by one peer. Without an aggregate cap,
// each of the default 100 bidirectional streams could independently retain a
// near-limit body.
const h3_server_max_buffered_request_body_per_connection = h3_server_max_request_body

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
	headers  []quic.QpackFieldLine
	body     []u8
	rejected bool
}

struct H3ServerBodyBudget {
mut:
	by_conn map[string]int
}

fn (mut s H3ServerStream) append_body(data []u8) bool {
	if s.rejected {
		return false
	}
	if s.body.len + data.len > h3_server_max_request_body {
		s.body.clear()
		s.rejected = true
		return false
	}
	s.body << data
	return true
}

fn (mut b H3ServerBodyBudget) release(conn_id string, amount int) {
	current := b.by_conn[conn_id]
	remaining := current - amount
	if remaining > 0 {
		b.by_conn[conn_id] = remaining
	} else {
		b.by_conn.delete(conn_id)
	}
}

fn (mut b H3ServerBodyBudget) append(conn_id string, mut stream H3ServerStream, data []u8) bool {
	if stream.rejected {
		return false
	}
	current := b.by_conn[conn_id]
	if data.len > h3_server_max_buffered_request_body_per_connection - current {
		b.release(conn_id, stream.body.len)
		stream.body.clear()
		stream.rejected = true
		return false
	}
	previous_len := stream.body.len
	if !stream.append_body(data) {
		b.release(conn_id, previous_len)
		return false
	}
	b.by_conn[conn_id] = current + data.len
	return true
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
	retry_token_key []u8
	always_retry    bool = true
	// The defaults permit ordinary requests plus HTTP/3's required control
	// and QPACK streams. Callers can still replace the complete parameter set.
	transport_parameters quic.QuicTransportParameters = quic.QuicTransportParameters{
		max_idle_timeout: 30_000
		initial_max_data: 10_000_000
		initial_max_stream_data_bidi_local: 1_000_000
		initial_max_stream_data_bidi_remote: 1_000_000
		initial_max_stream_data_uni: 1_000_000
		initial_max_streams_bidi: 100
		initial_max_streams_uni: 100
	}
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
	// body_budget accounts only bytes retained in streams; entries are released
	// on completion, rejection, reset, or connection close.
	body_budget H3ServerBodyBudget
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
		alpn_protocols: params.alpn_protocols
		certificate_chain: params.certificate_chain
		signing_key: params.signing_key
		retry_token_key: retry_key
		always_retry: params.always_retry
	})!
	socket := net.listen_udp(listen_addr)!
	return &H3Server{
		socket: socket
		listener: listener
		h3_params: quic.H3ConnParams{
			settings: h3_default_own_settings()
			own_qpack_max_table_capacity: h3_default_own_qpack_max_table_capacity
			max_inbound_data_frame_payload: h3_server_max_request_body
		}
		handler: params.handler
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

// free_all_conns releases every currently-tracked H3Conn (each holds a
// native ECDHE private key via its Tls13ServerHandshake, the same
// resource the per-connection close/error paths in serve()'s own loop
// already free). Deliberately called only from serve()'s own thread, at
// its loop-exit point below -- s.h3_conns/s.streams are single-threaded
// state owned by that loop (see this file's own module doc comment);
// close() itself runs on a DIFFERENT thread by design (its own doc
// comment), so mutating these maps there would race serve()'s loop.
fn (mut s H3Server) free_all_conns() {
	for _, mut h3c in s.h3_conns {
		h3c.free()
	}
	s.h3_conns = map[string]&quic.H3Conn{}
	s.streams = map[string]&H3ServerStream{}
	s.body_budget = H3ServerBodyBudget{}
}

// serve runs this server's single-threaded accept/demux/dispatch loop
// (see this file's own module doc comment) until close() is called or the
// socket errors. Blocks the calling thread.
pub fn (mut s H3Server) serve() ! {
	defer {
		s.free_all_conns()
	}
	mut buf := []u8{len: h3_datagram_buf_size}
	mut next_timeout := ?u64(none)
	for {
		s.shutdown_mu.lock()
		should_stop := s.closing
		s.shutdown_mu.unlock()
		if should_stop {
			return
		}
		wait := h3_driver_next_wait(next_timeout, h3_now_ns(), h3_driver_poll_interval)
		s.socket.set_read_timeout(wait)

		n, addr := s.socket.read(mut buf) or {
			if err.code() != net.err_timed_out_code {
				s.shutdown_mu.lock()
				is_closing := s.closing
				s.shutdown_mu.unlock()
				if h3_server_should_propagate_read_error(err.code(), is_closing) {
					return err
				}
				return
			}
			0, net.Addr{}
		}
		now := h3_now_ns()
		mut result := quic.QuicListenerPollResult{}
		if n > 0 {
			peer_str := addr.str()
			s.peer_by_str[peer_str] = addr
			result = s.listener.poll(buf[..n].clone(), peer_str.bytes(), now)!
		} else {
			result = s.listener.process_timeouts(now)!
		}
		s.absorb_and_dispatch(result)
		for dg in result.outgoing {
			peer := s.peer_by_str[dg.peer.bytestr()] or { continue }
			s.socket.write_to(peer, dg.bytes) or { continue }
		}

		// Drive timers after every read, not only after a socket timeout. This
		// also flushes writes queued by absorb_and_dispatch above in the same
		// loop iteration, even while unrelated datagrams keep the socket busy.
		followup := s.listener.process_timeouts(now)!
		next_timeout = followup.next_timeout
		s.absorb_and_dispatch(followup)
		for dg in followup.outgoing {
			peer := s.peer_by_str[dg.peer.bytestr()] or { continue }
			s.socket.write_to(peer, dg.bytes) or { continue }
		}
	}
}

// h3_server_should_propagate_read_error distinguishes an expected timeout or
// shutdown wakeup from an unexpected UDP failure that serve must return.
fn h3_server_should_propagate_read_error(error_code int, is_closing bool) bool {
	return error_code != net.err_timed_out_code && !is_closing
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
	// touched_conns also seeds the dispatch set, with an EMPTY events
	// slice for any connection not already present above -- a connection
	// this poll()/process_timeouts() call actually drove but which
	// produced zero new QuicEvents (see QuicListenerPollResult.
	// touched_conns' own doc comment) still needs h3c.drive_events called
	// so H3Conn.drain_known_peer_streams can read whatever new bytes
	// landed in an already-open stream's receive buffer. Without this, a
	// request whose HEADERS and body arrive in separate UDP datagrams
	// stalls forever: the second datagram's connection never appears in
	// `result.events` at all (its stream was already open, so no new
	// peer_stream_opened fires), so drive_events was never even called
	// for it -- not with an empty slice, not at all. (Codex review, PR
	// #28164 pullrequestreview-5044139767.)
	for c in result.touched_conns {
		key := c.connection_id()
		if key !in events_by_key {
			order << key
			events_by_key[key] = []quic.QuicEvent{}
			conn_by_key[key] = c
		}
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
			h3c.free()
			s.h3_conns.delete(key)
			s.prune_streams_for_conn(key)
			continue
		}
		for hev in h3r.events {
			s.handle_h3_event(key, mut h3c, hev)
		}
		if h3c.closed() {
			h3c.free()
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
	s.body_budget.by_conn.delete(conn_id)
}

// handle_h3_event actions one H3Event from a server-role H3Conn: buffers
// request_headers/request_data, validates request_trailers, and runs the
// completed request through this server's Handler once request_ended fires.
// Valid request trailers are deliberately NOT delivered anywhere (a v1 limit,
// mirroring several other "the wire-level machinery exists, the
// caller-facing delivery does not yet" notes already accepted throughout
// this module -- request trailers are rare in practice, unlike RESPONSE
// trailers, which send_response below DOES support, matching
// h2_server.v's own asymmetric support for exactly the same reason).
fn (mut s H3Server) handle_h3_event(conn_id string, mut h3c quic.H3Conn, ev quic.H3Event) {
	match ev.kind {
		.request_headers {
			// A compliant peer's HEADERS can be QPACK-blocked (an in-flight
			// dynamic-table reference not yet resolved) -- H3Conn delays
			// this event until it resolves, but DATA frames on the SAME
			// stream carry no such dependency and can arrive first. If
			// request_data already lazily created this stream's entry
			// (below) to avoid dropping that early body, preserve it and
			// only attach headers -- overwriting the whole struct here
			// would silently discard the body already buffered.
			stream_id := ev.stream_id or { return }
			key := h3_server_stream_key(conn_id, stream_id)
			if st := s.streams[key] {
				if st.rejected {
					return
				}
			}
			h3_validate_request_pseudo(ev.headers) or {
				mut st := s.streams[key] or {
					new_st := &H3ServerStream{}
					s.streams[key] = new_st
					new_st
				}
				s.body_budget.release(conn_id, st.body.len)
				st.body.clear()
				st.rejected = true
				s.send_error_response(mut h3c, stream_id, 400)
				return
			}
			if mut st := s.streams[key] {
				if st.rejected {
					return
				}
				st.headers = ev.headers
			} else {
				s.streams[key] = &H3ServerStream{
					headers: ev.headers
				}
			}
		}
		.request_data {
			// Mirrors request_headers' own lazy-creation above: a DATA
			// frame can arrive before its stream's HEADERS section
			// resolves (QPACK-blocked, see request_headers' own comment),
			// so no entry may exist yet. Create one instead of dropping
			// the body -- request_headers fills in .headers once it
			// finally arrives, preserving whatever body already
			// accumulated here.
			stream_id := ev.stream_id or { return }
			key := h3_server_stream_key(conn_id, stream_id)
			mut st := s.streams[key] or {
				new_st := &H3ServerStream{}
				s.streams[key] = new_st
				new_st
			}
			was_rejected := st.rejected
			if !s.body_budget.append(conn_id, mut st, ev.data) {
				if was_rejected {
					return
				}
				s.send_error_response(mut h3c, stream_id, 413)
				return
			}
		}
		.request_trailers {
			stream_id := ev.stream_id or { return }
			key := h3_server_stream_key(conn_id, stream_id)
			mut st := s.streams[key] or {
				new_st := &H3ServerStream{}
				s.streams[key] = new_st
				new_st
			}
			if st.rejected {
				return
			}
			h3_validate_request_trailers(ev.headers) or {
				s.body_budget.release(conn_id, st.body.len)
				st.body.clear()
				st.rejected = true
				s.send_error_response(mut h3c, stream_id, 400)
				return
			}
		}
		.request_ended {
			stream_id := ev.stream_id or { return }
			key := h3_server_stream_key(conn_id, stream_id)
			st := s.streams[key] or { return }
			if st.rejected {
				s.streams.delete(key)
				return
			}
			s.run_request(mut h3c, stream_id, st)
			s.body_budget.release(conn_id, st.body.len)
			s.streams.delete(key)
		}
		.request_error {
			stream_id := ev.stream_id or { return }
			if code := ev.error_code {
				if code == quic.H3ErrorCode.excessive_load.code() {
					s.send_error_response(mut h3c, stream_id, 413)
				}
			}
			key := h3_server_stream_key(conn_id, stream_id)
			if st := s.streams[key] {
				s.body_budget.release(conn_id, st.body.len)
			}
			s.streams.delete(key)
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

fn h3_validate_request_trailers(headers []quic.QpackFieldLine) ! {
	for f in headers {
		if f.name.starts_with(':') {
			return error('pseudo-header "${f.name}" is forbidden in request trailers')
		}
		reason := h2_request_field_error(f.name, f.value)
		if reason != '' {
			return error(reason)
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
	s.send_response(mut h3c, stream_id, req.method, resp)
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
		header: new_header()
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
	mut request_host := authority
	if request_host == '' {
		request_host = req.header.get(.host) or {
			return error('h3 server: request omits both :authority and host')
		}
	} else {
		// :authority is the authoritative request-target value. Keep the
		// Header view in sync so handlers cannot route on conflicting hosts.
		req.header.set_custom('host', authority)!
	}
	req.url = path
	req.data = st.body.bytestr()
	req.host = request_host
	return req
}

// h3_validate_request_pseudo enforces the RFC 9114 §4.3.1 rules a request
// field section must satisfy -- RFC 9114 §4.1.1 mirrors RFC 9113 §8.1.2.2's
// pseudo-header rules almost verbatim for HTTP/3 (already the precedent
// h3_client.v's own to_h3_request doc comment states for the outgoing
// side: "RFC 9114 §4.1.1 intentionally mirrors RFC 9113 §8.1.2.2 here, so
// there is nothing HTTP/3-specific to re-derive"): only the request
// pseudo-headers, each at most once, all appearing before any regular
// field. Every regular field is validated via h2_request_field_error
// (h2_server.v), reused directly for the identical reason -- RFC 9114 §4.2
// mirrors RFC 9113 §8.2.2's forbidden-octet/connection-specific-field/TE
// rules verbatim.
//
// This Handler API only represents the Method enum and dispatches after the
// request stream ends. Reject extension methods that the enum cannot preserve,
// and reject CONNECT because a conforming tunnel request waits for a response
// without ending its request stream. Extended CONNECT (`:protocol`) remains
// unsupported as well.
fn h3_validate_request_pseudo(headers []quic.QpackFieldLine) ! {
	mut seen_regular := false
	mut has_method := false
	mut has_path := false
	mut has_scheme := false
	mut seen_authority := false
	mut has_authority := false
	mut has_host := false
	mut method := ''
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
					method = f.value
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
					if seen_authority {
						return error('duplicate :authority pseudo-header')
					}
					seen_authority = true
					has_authority = f.value != ''
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
			if f.name == 'host' && f.value != '' {
				has_host = true
			}
		}
	}
	if !has_method {
		return error('request omits a mandatory pseudo-header (:method/:path/:scheme)')
	}
	parsed_method := method_from_str(method)
	if parsed_method.str() != method {
		return error('request uses unsupported method "${method}"')
	}
	if parsed_method == .connect {
		return error('CONNECT is unsupported by this HTTP/3 server')
	}
	if !has_path || !has_scheme {
		return error('request omits a mandatory pseudo-header (:method/:path/:scheme)')
	}
	if !has_authority && !has_host {
		return error('request omits both :authority and host')
	}
}

// send_response QPACK-encodes resp and sends it as this stream's response,
// mirroring h2_server.v's own send_response (Trailers-Only shape when
// there's no body, trailers filtered/forbidden-octet-checked the same
// way). Best-effort: a send failure here means this ONE stream's response
// is lost (the peer sees an incomplete/truncated response and can retry),
// never propagated as a connection- or server-wide failure -- see this
// file's own module doc comment.
fn (mut s H3Server) send_response(mut h3c quic.H3Conn, stream_id u64, method Method, resp Response) {
	status := h3_final_response_status(resp.status_code) or {
		s.send_error_response(mut h3c, stream_id, 500)
		return
	}
	mut fields := [
		quic.QpackFieldLine{
			name: ':status'
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
				name: lkey
				value: val
			}
		}
	}
	body := resp.body.bytes()
	has_body := body.len > 0 && h3_response_allows_body(method, status)
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

// h3_response_allows_body applies the response cases that never carry
// content. Content-Length remains metadata and is left in the header block.
fn h3_response_allows_body(method Method, status int) bool {
	return method != .head && status != 204 && status != 205 && status != 304
}

// h3_final_response_status normalizes the default status and rejects
// informational responses, which this single-response Handler API cannot
// follow with the mandatory final response.
fn h3_final_response_status(status_code int) !int {
	status := if status_code == 0 { 200 } else { status_code }
	if status < 100 || status > 599 {
		return error('invalid response status ${status}')
	}
	if status >= 100 && status < 200 {
		return error('informational status ${status} cannot be a final response')
	}
	return status
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
			name: ':status'
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
				name: lkey
				value: val
			}
		}
	}
	return fields
}
