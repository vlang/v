// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// This file implements a minimal server-side HTTP/2 connection driver on top
// of the existing framing (h2_frame.v) and HPACK (h2_hpack.v) layers. It is
// intentionally serial: SETTINGS_MAX_CONCURRENT_STREAMS is advertised as 1,
// so the peer sends one request at a time. Concurrent stream handling and
// background writers are a planned follow-up.

// h2_server_max_concurrent_streams is the value advertised in our SETTINGS;
// the peer may not open more streams than this at once.
const h2_server_max_concurrent_streams = u32(1)

// h2_server_default_window is the default initial flow-control window
// (RFC 7540 Section 6.9.2).
const h2_server_default_window = u32(65535)

// h2_server_max_request_body caps the in-memory request body the server will
// accept before responding with a stream error. Bodies larger than this are
// rejected with RST_STREAM(REFUSED_STREAM).
const h2_server_max_request_body = 8 * 1024 * 1024

// h2_server_max_locally_reset_tracked bounds the locally_reset drain-tracking
// set (RFC 9113 §6.4). A peer that keeps the connection open while sending an
// unbounded stream of malformed/refused requests must not be able to grow this
// set without limit; once full, the oldest entry is evicted (still-open drain
// windows race against network RTT, so the newest entries matter most).
const h2_server_max_locally_reset_tracked = 128

// The connection-specific header fields RFC 9113 §8.2.2 forbids in HTTP/2 (a
// request carrying one is malformed; the server must never echo one in a
// response) are the module-level `h2_conn_specific_headers` const defined in
// h2_mux_conn.v — shared with the client path rather than redefined here.

// h2_all_digits reports whether s is a non-empty run of ASCII digits. A value
// that parses with `.int()` is NOT proof of a valid number — `'12x'.int()` is 12
// — so wire-derived numerics (content-length) must be charset-checked first.
fn h2_all_digits(s string) bool {
	if s.len == 0 {
		return false
	}
	for ch in s {
		if ch < `0` || ch > `9` {
			return false
		}
	}
	return true
}

// h2_field_value_has_forbidden_octet reports whether a field value contains an
// octet RFC 9113 §8.2.1 forbids: NUL (0x00), LF (0x0a), or CR (0x0d). Such a
// value makes the request malformed and, if the request were forwarded to an
// HTTP/1.x peer, would be a header-injection / request-smuggling vector.
fn h2_field_value_has_forbidden_octet(value string) bool {
	for ch in value {
		if ch == 0 || ch == 0x0a || ch == 0x0d {
			return true
		}
	}
	return false
}

// h2_request_field_error returns a non-empty reason when a regular (non-pseudo)
// request header field is malformed per RFC 9113 §8.2: names must be lowercase
// and non-empty, values must not contain NUL/CR/LF, connection-specific fields
// are forbidden, and TE may only carry the value "trailers". An empty return
// means the field is valid.
fn h2_request_field_error(name string, value string) string {
	if name.len == 0 {
		return 'empty header field name'
	}
	if name != name.to_lower() {
		return 'uppercase header field name "${name}"'
	}
	if h2_field_value_has_forbidden_octet(value) {
		return 'forbidden NUL/CR/LF octet in value of "${name}"'
	}
	if name in h2_conn_specific_headers {
		return 'connection-specific header field "${name}"'
	}
	if name == 'te' && value.trim_space().to_lower() != 'trailers' {
		return 'TE header field with a value other than "trailers"'
	}
	return ''
}

// h2_validate_request_pseudo enforces the RFC 9113 §8.1.2/§8.3 rules a request
// header list must satisfy: only the request pseudo-headers, each at most once,
// all appearing before any regular field, with :method, :path and :scheme
// present; every regular field must pass h2_request_field_error. A non-ok return
// means the request is malformed (a stream error at the call site).
fn h2_validate_request_pseudo(headers []H2HeaderField) ! {
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
					// RFC 9113 §8.3.1: :path must not be empty for http/https.
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

// H2ServerStream holds the state of one in-flight server-side stream.
struct H2ServerStream {
mut:
	id            u32
	hpack_block   []u8 // assembled HEADERS + CONTINUATION fragments
	headers       []H2HeaderField
	body          []u8
	end_headers   bool
	end_stream    bool
	headers_done  bool // set once we've decoded the HPACK block
	send_window   i64
	trailer_block []u8 // assembled trailer HEADERS + CONTINUATION fragments
	in_trailers   bool // set once a trailer section (a 2nd HEADERS block) begins
	refused       bool // §5.1.2: over the concurrency limit — decode then RST(REFUSED_STREAM)
	over_end      bool // §5.1: HEADERS after END_STREAM — decoded for HPACK sync, then RST(STREAM_CLOSED)
	self_dep      bool // RFC 7540 §5.3.1: HEADERS priority depends on itself — decode then RST(PROTOCOL_ERROR)
}

// H2ServerConn drives one server-side HTTP/2 connection over a transport.
struct H2ServerConn {
mut:
	transport           H2Transport
	encoder             H2HpackEncoder
	decoder             H2HpackDecoder
	peer                H2PeerSettings
	rbuf                []u8
	send_window         i64 = i64(h2_server_default_window)
	streams             map[u32]&H2ServerStream
	locally_reset       map[u32]bool // stream ids for which we sent RST_STREAM; drain in-flight frames per RFC 9113 §6.4
	locally_reset_order []u32        // insertion order of locally_reset keys, oldest first, for bounded eviction
	discard_block       []u8         // accumulates a HEADERS/CONTINUATION block for a locally-reset stream, decoded then discarded
	last_stream_id      u32
	// last_processed_stream_id is the highest stream id the server has actually
	// acted on (RFC 9113 §6.8), used for GOAWAY. Bumped in on_headers at stream
	// creation — the same moment refused is decided — so it correctly includes a
	// stream that's still mid-CONTINUATION (HPACK block not yet fully assembled)
	// if some unrelated error forces a GOAWAY before finalize_headers ever runs.
	// It excludes streams refused purely for exceeding the concurrency limit:
	// §5.1.2 defines REFUSED_STREAM as refusal "prior to any processing", and that
	// RST_STREAM already tells the client the stream is safe to retry elsewhere —
	// counting it here would only inflate last_stream_id past streams whose
	// outcome is genuinely uncertain.
	last_processed_stream_id u32
	awaiting_cont            u32 // non-zero when mid-CONTINUATION on this stream
	closing                  bool
	idle_conns               &TlsIdleConnTracker = unsafe { nil }
	idle_handle              int
}

// mark_locally_reset records that id has been RST_STREAM'd by the server so any
// in-flight frame for it is drained rather than re-RST (RFC 9113 §6.4). Bounded
// per h2_server_max_locally_reset_tracked; never evicts the id whose discard
// block is still being assembled (c.awaiting_cont).
fn (mut c H2ServerConn) mark_locally_reset(id u32) {
	if id in c.locally_reset {
		return
	}
	c.locally_reset[id] = true
	c.locally_reset_order << id
	for c.locally_reset_order.len > h2_server_max_locally_reset_tracked {
		mut evict_idx := 0
		if c.locally_reset_order[evict_idx] == c.awaiting_cont && c.locally_reset_order.len > 1 {
			evict_idx = 1
		}
		evicted := c.locally_reset_order[evict_idx]
		c.locally_reset_order.delete(evict_idx)
		c.locally_reset.delete(evicted)
	}
}

// H2StreamState is the server's view of a client-initiated stream for the
// RFC 9113 §5.1 frame-acceptance rules. An open or half-closed stream is kept
// in c.streams until its response is sent, so:
//   - active: present in c.streams (open / half-closed local)
//   - idle:   never opened — an odd id above any we have accepted, OR any even
//             (server-initiated) id, since this server never opens push streams
//   - closed: a client (odd) id at or below the highest we have accepted that is
//             no longer in the map (already finished or reset), OR any id we
//             have ourselves RST_STREAM'd (c.locally_reset) regardless of
//             last_stream_id — a self-dependent PRIORITY (RFC 7540 §5.3.1) is
//             legal on a stream the client never opened via HEADERS, so an id
//             can be locally-reset without ever having advanced last_stream_id
enum H2StreamState {
	active
	idle
	closed
}

// classify_stream maps a stream id to its §5.1 state from the server's side.
// `last_stream_id` tracks only client-initiated (odd) ids, so "closed" applies
// ONLY to an odd id <= last_stream_id; an even id was never opened by this
// server (it initiates no streams) and is therefore idle, not closed — a frame
// on it is a connection PROTOCOL_ERROR, not STREAM_CLOSED. id 0 (the connection
// control stream) is never a request stream; callers that can receive it
// (WINDOW_UPDATE) handle id 0 before calling this.
fn (c &H2ServerConn) classify_stream(stream_id u32) H2StreamState {
	if stream_id in c.streams {
		return .active
	}
	// An id we have ourselves RST_STREAM'd is closed no matter how it got reset:
	// checking this before the last_stream_id/parity test covers ids that were
	// never opened via HEADERS at all (e.g. a self-dependent PRIORITY frame on
	// an id the client never used) — without this, every caller of
	// classify_stream (on_data, handle_control_frame's WINDOW_UPDATE arm,
	// dispatch_frame's RST_STREAM arm) would misclassify a later in-flight frame
	// for that id as idle and force a connection error instead of draining it
	// per §6.4.
	if stream_id in c.locally_reset {
		return .closed
	}
	if stream_id & 1 == 1 && stream_id <= c.last_stream_id {
		return .closed
	}
	return .idle
}

// serve_h2_conn drives a single HTTP/2 server-side connection until the
// transport closes or a protocol error forces a GOAWAY. `handler` is invoked
// once per fully-received request stream.
fn serve_h2_conn(mut transport H2Transport, mut handler Handler) ! {
	serve_h2_conn_with_idle_tracker(mut transport, mut handler, unsafe { nil }, 0)!
}

fn serve_h2_conn_with_idle_tracker(mut transport H2Transport, mut handler Handler, idle_conns &TlsIdleConnTracker, idle_handle int) ! {
	mut c := &H2ServerConn{
		transport:   transport
		idle_conns:  idle_conns
		idle_handle: idle_handle
	}
	c.serve(mut handler) or {
		// Best-effort GOAWAY before bailing. Skip if one was already sent by
		// the error path (e.g. apply_settings sends FLOW_CONTROL_ERROR).
		if !c.closing {
			c.send_goaway(.protocol_error, err.msg()) or {}
		}
		return err
	}
}

fn (mut c H2ServerConn) serve(mut handler Handler) ! {
	// Register the connection with the idle tracker once for its whole
	// lifetime, instead of around every frame read. The reader thread spends
	// nearly all of its time blocked in a frame read, so per-frame mark/unmark
	// only added shared-lock contention (and an O(n) list scan) on the hot
	// path. On shutdown, close_idle still interrupts the reader by shutting the
	// fd down; an h2 request in flight when the server stops is interrupted —
	// including response writes, which may be truncated mid-DATA-frame. This
	// is acceptable at shutdown and is not relied on by any caller (the
	// graceful "wait for active request" guarantee is HTTP/1.1-only).
	tracked := c.should_track_idle_read()
	if tracked && !c.idle_conns.mark_idle(c.idle_handle) {
		// The server is already shutting down; do not start serving.
		return
	}
	defer {
		if tracked {
			c.idle_conns.unmark_idle(c.idle_handle)
		}
	}
	c.read_client_preface()!
	c.send_initial_settings()!
	for !c.closing {
		frame := c.read_frame() or {
			// Treat a clean transport close as end of session.
			return
		}
		c.dispatch_frame(frame, mut handler)!
	}
}

fn (mut c H2ServerConn) should_track_idle_read() bool {
	return c.idle_handle > 0 && c.idle_conns != unsafe { nil }
}

fn (mut c H2ServerConn) read_client_preface() ! {
	c.fill_at_least(h2_client_preface.len)!
	got := c.rbuf[..h2_client_preface.len].bytestr()
	if got != h2_client_preface {
		return error('h2 server: invalid connection preface')
	}
	c.rbuf = c.rbuf[h2_client_preface.len..].clone()
}

fn (mut c H2ServerConn) send_initial_settings() ! {
	c.send_frame(H2SettingsFrame{
		settings: [
			H2Setting{h2_settings_enable_push, 0},
			H2Setting{h2_settings_max_concurrent_streams, h2_server_max_concurrent_streams},
			H2Setting{h2_settings_initial_window_size, h2_server_default_window},
			H2Setting{h2_settings_max_frame_size, h2_default_max_frame_size},
		]
	})!
}

fn (mut c H2ServerConn) dispatch_frame(frame H2Frame, mut handler Handler) ! {
	// RFC 7540 §6.10: once a HEADERS or PUSH_PROMISE without END_HEADERS is
	// seen, the next frame must be a CONTINUATION on the same stream.
	if c.awaiting_cont != 0 {
		if frame is H2ContinuationFrame {
			if frame.stream_id != c.awaiting_cont {
				return error('h2 server: CONTINUATION on the wrong stream')
			}
		} else {
			return error('h2 server: expected CONTINUATION after HEADERS without END_HEADERS')
		}
	} else if frame is H2ContinuationFrame {
		// A CONTINUATION is only legal immediately after a HEADERS/PUSH_PROMISE/
		// CONTINUATION block that ended without END_HEADERS (RFC 9113 §6.10). With
		// no block in progress, this is an orphan CONTINUATION — a connection error
		// even if frame.stream_id was previously locally_reset (the discard path in
		// on_continuation relies on this gate to guarantee awaiting_cont ==
		// frame.stream_id before it ever runs).
		return error('h2 server: unexpected CONTINUATION frame')
	}
	match frame {
		H2SettingsFrame, H2PingFrame, H2WindowUpdateFrame {
			c.handle_control_frame(frame)!
		}
		H2GoawayFrame {
			c.closing = true
		}
		H2RstStreamFrame {
			// RFC 9113 §5.1/§6.4: RST_STREAM on the connection stream (id 0) or an
			// idle stream is a connection error PROTOCOL_ERROR. On an open stream it
			// cancels it; on an already-closed stream it is ignored.
			if frame.stream_id == 0 || c.classify_stream(frame.stream_id) == .idle {
				return error('h2 server: RST_STREAM on idle stream ${frame.stream_id}')
			}
			c.streams.delete(frame.stream_id)
		}
		H2PriorityFrame {
			// Priority is advisory and otherwise ignored (deprecated in RFC 9113
			// §5.3), but RFC 7540 §5.3.1: a stream cannot depend on itself — a
			// self-dependency is a STREAM error PROTOCOL_ERROR. Skip the RST for
			// any ALREADY-CLOSED stream (locally reset OR completed normally via
			// run_request — classify_stream treats both as closed): PRIORITY is
			// legal on a closed stream, and RST_STREAM is not, so re-RST-ing one
			// would itself send a frame on a closed stream (§5.1).
			if frame.stream_dep == frame.stream_id && c.classify_stream(frame.stream_id) != .closed {
				c.send_rst_stream(frame.stream_id, .protocol_error)!
				c.mark_locally_reset(frame.stream_id)
				c.streams.delete(frame.stream_id)
			}
		}
		H2HeadersFrame {
			c.on_headers(frame, mut handler)!
		}
		H2ContinuationFrame {
			c.on_continuation(frame, mut handler)!
		}
		H2DataFrame {
			c.on_data(frame, mut handler)!
		}
		H2PushPromiseFrame {
			// Clients should not push. Treat as a protocol error.
			return error('h2 server: PUSH_PROMISE from client')
		}
		H2UnknownFrame {
			// RFC 7540 §4.1: ignore unknown frame types.
		}
	}
}

// handle_control_frame services SETTINGS, PING, and WINDOW_UPDATE frames that
// may arrive at any point in the session, including while a response write is
// blocked in send_body waiting for flow-control credit. Both dispatch_frame and
// pump_for_window delegate to this function so the logic — and any validation
// errors — exist in exactly one place.
fn (mut c H2ServerConn) handle_control_frame(frame H2Frame) ! {
	match frame {
		H2SettingsFrame {
			if !frame.ack {
				c.apply_settings(frame.settings)!
				c.send_frame(H2SettingsFrame{
					ack: true
				})!
			}
		}
		H2PingFrame {
			if !frame.ack {
				c.send_frame(H2PingFrame{
					ack:  true
					data: frame.data
				})!
			}
		}
		H2WindowUpdateFrame {
			inc := frame.window_size_increment
			if frame.stream_id == 0 {
				// RFC 9113 §6.9: a zero increment on the connection is a connection
				// error PROTOCOL_ERROR. §6.9.1: growing the connection window past
				// 2^31-1 is a connection error FLOW_CONTROL_ERROR — send the correct
				// code before unwinding (serve()'s catch defaults to PROTOCOL_ERROR).
				if inc == 0 {
					return error('h2 server: connection WINDOW_UPDATE with a zero increment (RFC 9113 §6.9 PROTOCOL_ERROR)')
				}
				new_window := c.send_window + i64(inc)
				if new_window > i64(0x7fff_ffff) {
					c.send_goaway(.flow_control_error,
						'connection flow-control window exceeds 2^31-1') or {}
					return error('h2 server: connection flow-control window exceeded 2^31-1 (RFC 9113 §6.9.1 FLOW_CONTROL_ERROR)')
				}
				c.send_window = new_window
			} else if mut s := c.streams[frame.stream_id] {
				// Stream-scoped versions of the same rules are STREAM errors
				// (RFC 9113 §6.9/§6.9.1): reset the offending stream, keep the
				// connection alive.
				if inc == 0 {
					c.send_rst_stream(s.id, .protocol_error)!
					c.mark_locally_reset(s.id)
					c.streams.delete(s.id)
					return
				}
				new_window := s.send_window + i64(inc)
				if new_window > i64(0x7fff_ffff) {
					c.send_rst_stream(s.id, .flow_control_error)!
					c.mark_locally_reset(s.id)
					c.streams.delete(s.id)
					return
				}
				s.send_window = new_window
			} else if c.classify_stream(frame.stream_id) == .idle {
				// RFC 9113 §5.1: a WINDOW_UPDATE on an idle stream is a connection
				// error PROTOCOL_ERROR. On a closed stream it is ignored.
				return error('h2 server: WINDOW_UPDATE on idle stream ${frame.stream_id}')
			}
		}
		else {}
	}
}

fn (mut c H2ServerConn) apply_settings(settings []H2Setting) ! {
	for s in settings {
		match s.id {
			h2_settings_header_table_size {
				// Constrains our *encoder* (the server's response-header encoder
				// whose output the client decodes with its advertised table size).
				c.peer.header_table_size = s.value
				c.encoder.dyn_table.set_max_size(int(s.value))
				c.encoder.pending_max_table_size = int(s.value)
			}
			h2_settings_enable_push {
				// RFC 9113 §6.5.2: any value other than 0 or 1 is a connection
				// error PROTOCOL_ERROR.
				if s.value > 1 {
					return error('h2 server: SETTINGS_ENABLE_PUSH ${s.value} is not 0 or 1 (RFC 9113 §6.5.2 PROTOCOL_ERROR)')
				}
				c.peer.enable_push = s.value != 0
			}
			h2_settings_max_concurrent_streams {
				c.peer.max_concurrent_streams = s.value
			}
			h2_settings_initial_window_size {
				// RFC 7540 §6.5.3: values above 2^31-1 are a FLOW_CONTROL_ERROR,
				// not a PROTOCOL_ERROR; send the correct code before unwinding.
				if s.value > u32(0x7fff_ffff) {
					c.send_goaway(.flow_control_error,
						'SETTINGS_INITIAL_WINDOW_SIZE ${s.value} exceeds 2^31-1') or {}
					return error('h2 server: SETTINGS_INITIAL_WINDOW_SIZE ${s.value} exceeds 2^31-1 (FLOW_CONTROL_ERROR)')
				}
				// RFC 7540 §6.9.2: validate ALL stream windows before mutating any
				// so that a delta overflow on stream N does not leave streams 1..N-1
				// partially updated. Send FLOW_CONTROL_ERROR (not PROTOCOL_ERROR).
				delta := i64(s.value) - i64(c.peer.initial_window_size)
				for _, st in c.streams {
					if st.send_window + delta > i64(0x7fff_ffff) {
						c.send_goaway(.flow_control_error,
							'SETTINGS_INITIAL_WINDOW_SIZE delta overflows stream ${st.id} send window') or {}
						return error('h2 server: SETTINGS_INITIAL_WINDOW_SIZE delta overflows stream ${st.id} send window (RFC 7540 §6.9.2 FLOW_CONTROL_ERROR)')
					}
				}
				c.peer.initial_window_size = s.value
				for _, mut st in c.streams {
					st.send_window += delta
				}
			}
			h2_settings_max_frame_size {
				// RFC 7540 §6.5.2: valid range is 2^14 (16384)..2^24-1 inclusive.
				if s.value < h2_default_max_frame_size || s.value > h2_max_max_frame_size {
					return error('h2 server: SETTINGS_MAX_FRAME_SIZE ${s.value} out of range [16384, 16777215]')
				}
				c.peer.max_frame_size = s.value
			}
			h2_settings_max_header_list_size {
				c.peer.max_header_list_size = s.value
			}
			else {}
		}
	}
}

fn (mut c H2ServerConn) on_headers(frame H2HeadersFrame, mut handler Handler) ! {
	// A HEADERS block for a stream that is already open is a trailer section
	// (RFC 9113 §8.1), not a new request.
	if mut existing := c.streams[frame.stream_id] {
		// A fresh top-level HEADERS frame always starts a brand-new block (RFC
		// 9113 §8.1); any bytes left in trailer_block are from a PRIOR, already-
		// decoded trailer section and must not be redecoded — decode() mutates
		// the dynamic table, so replaying old bytes desyncs it (RFC 7541 §2.2).
		existing.trailer_block = []
		if existing.end_stream {
			// Half-closed (remote): the stream already delivered END_STREAM.
			// The further HEADERS block is invalid (RFC 9113 §5.1, STREAM_CLOSED),
			// but we must still route it through on_trailers → finalize_trailers so
			// the HPACK block is decoded: the decoder is stateful and connection-wide
			// (RFC 7541 §2.2) — skipping a block desyncs all future requests.
			// finalize_trailers RSTs on over_end after the decode.
			// If the block is fragmented (END_HEADERS=false), on_trailers sets
			// awaiting_cont; the client must then complete the CONTINUATION sequence
			// before the RST fires. Per RFC 9113 §6.10, any non-CONTINUATION frame
			// during that wait is a connection PROTOCOL_ERROR.
			existing.over_end = true
		}
		c.on_trailers(mut existing, frame, mut handler)!
		return
	}
	// A HEADERS frame for a stream we have already RST'd ourselves is a stray
	// frame that was in flight before the client received our RST_STREAM (most
	// likely a trailer block). The decoder is stateful and connection-wide (RFC
	// 7541 §2.2), so the block must still be decoded to stay in sync — it is
	// then discarded rather than run as a request (we already reset the stream).
	if frame.stream_id in c.locally_reset {
		c.discard_block = frame.fragment.clone()
		if !frame.end_headers {
			c.awaiting_cont = frame.stream_id
			return
		}
		c.decoder.decode(c.discard_block) or {
			c.send_goaway(.compression_error, 'HPACK decode error') or {}
			return error('h2 server: HPACK decode error (COMPRESSION_ERROR)')
		}
		c.discard_block = []
		return
	}
	// Stream ids from the client must be odd and strictly increasing.
	if frame.stream_id & 1 == 0 || frame.stream_id <= c.last_stream_id {
		return error('h2 server: invalid client stream id ${frame.stream_id}')
	}
	c.last_stream_id = frame.stream_id
	mut s := &H2ServerStream{
		id:          frame.stream_id
		hpack_block: frame.fragment.clone()
		end_headers: frame.end_headers
		end_stream:  frame.end_stream
		send_window: i64(c.peer.initial_window_size)
		self_dep:    frame.has_priority && frame.stream_dep == frame.stream_id
	}
	// RFC 9113 §5.1.2: a stream that would exceed the concurrency limit we
	// advertised is refused. We still assemble and HPACK-decode its header block
	// (the decoder is stateful — skipping it would desync the dynamic table) but
	// answer with RST_STREAM(REFUSED_STREAM) instead of serving it; see
	// finalize_headers. The just-created stream is not yet counted in c.streams.
	if u32(c.streams.len) >= h2_server_max_concurrent_streams {
		s.refused = true
	} else {
		c.last_processed_stream_id = frame.stream_id
	}
	c.streams[frame.stream_id] = s
	if !frame.end_headers {
		c.awaiting_cont = frame.stream_id
		return
	}
	c.finalize_headers(mut s, mut handler)!
}

fn (mut c H2ServerConn) on_continuation(frame H2ContinuationFrame, mut handler Handler) ! {
	mut s := c.streams[frame.stream_id] or {
		// Continuing a discard block for a stream we already RST'd (see on_headers).
		// Only valid while a discard block on THIS id is actually mid-assembly
		// (RFC 9113 §6.10): a standalone CONTINUATION with no preceding HEADERS
		// lacking END_HEADERS is a connection error even if the id was once reset.
		if frame.stream_id in c.locally_reset {
			if c.discard_block.len + frame.fragment.len > h2_max_recv_header_block {
				return error('h2 server: discarded header block exceeds ${h2_max_recv_header_block} bytes')
			}
			c.discard_block << frame.fragment
			if frame.end_headers {
				c.awaiting_cont = 0
				c.decoder.decode(c.discard_block) or {
					c.send_goaway(.compression_error, 'HPACK decode error') or {}
					return error('h2 server: HPACK decode error (COMPRESSION_ERROR)')
				}
				c.discard_block = []
			}
			return
		}
		return error('h2 server: CONTINUATION for unknown stream ${frame.stream_id}')
	}
	if s.in_trailers {
		if s.trailer_block.len + frame.fragment.len > h2_max_recv_header_block {
			return error('h2 server: trailer block exceeds ${h2_max_recv_header_block} bytes')
		}
		s.trailer_block << frame.fragment
		if frame.end_headers {
			c.awaiting_cont = 0
			c.finalize_trailers(mut s, mut handler)!
		}
		return
	}
	if s.hpack_block.len + frame.fragment.len > h2_max_recv_header_block {
		return error('h2 server: request header block exceeds ${h2_max_recv_header_block} bytes')
	}
	s.hpack_block << frame.fragment
	if frame.end_headers {
		s.end_headers = true
		c.awaiting_cont = 0
		c.finalize_headers(mut s, mut handler)!
	}
}

fn (mut c H2ServerConn) finalize_headers(mut s H2ServerStream, mut handler Handler) ! {
	// An HPACK decoding failure is a CONNECTION error (RFC 9113 §4.3): the decoder
	// mutates the dynamic table as it runs, so a failure leaves our table out of
	// sync with the peer and no later header block on this connection can be decoded
	// reliably. Close the connection with COMPRESSION_ERROR rather than RST-ing one
	// stream. (send_goaway sets c.closing, so serve()'s catch won't re-GOAWAY.)
	s.headers = c.decoder.decode(s.hpack_block) or {
		c.send_goaway(.compression_error, 'HPACK decode error') or {}
		return error('h2 server: HPACK decode error (COMPRESSION_ERROR)')
	}
	s.headers_done = true
	// RFC 7540 §5.3.1: a stream cannot depend on itself; a HEADERS priority
	// self-dependency is a STREAM error PROTOCOL_ERROR. Checked BEFORE the
	// concurrency-limit refusal below: REFUSED_STREAM tells the client the
	// request is safe to retry unchanged, but a self-dependent request is
	// malformed regardless of the concurrency limit — retrying it would only
	// repeat the same error, so a stream that is both over-limit and
	// self-dependent must be reported as PROTOCOL_ERROR, not REFUSED_STREAM.
	if s.self_dep {
		c.send_rst_stream(s.id, .protocol_error)!
		c.mark_locally_reset(s.id)
		c.streams.delete(s.id)
		return
	}
	// §5.1.2: an over-limit stream was decoded only to keep HPACK in sync; refuse
	// it now without validating or running the request.
	if s.refused {
		c.send_rst_stream(s.id, .refused_stream)!
		c.mark_locally_reset(s.id)
		c.streams.delete(s.id)
		return
	}
	// A well-decoded but malformed request is a STREAM error (RFC 9113 §8.1.1) —
	// reset just this stream and keep the connection alive.
	h2_validate_request_pseudo(s.headers) or {
		c.send_rst_stream(s.id, .protocol_error)!
		c.mark_locally_reset(s.id)
		c.streams.delete(s.id)
		return
	}
	if s.end_stream {
		c.run_request(mut s, mut handler)!
	}
}

// on_trailers handles a second HEADERS block on an already-open stream: an
// HTTP/2 trailer section (RFC 9113 §8.1). The block is always assembled and
// decoded; whether it is well-formed — it must carry END_STREAM and no
// pseudo-headers — is decided in finalize_trailers. A well-decoded but malformed
// trailer section is reset as a stream error; an HPACK decode failure is a
// connection error (§4.3) since it desyncs the decoder.
fn (mut c H2ServerConn) on_trailers(mut s H2ServerStream, frame H2HeadersFrame, mut handler Handler) ! {
	s.in_trailers = true
	s.end_stream = frame.end_stream
	// RFC 7540 §5.3.1 applies to ANY HEADERS carrying priority, trailers included.
	if frame.has_priority && frame.stream_dep == frame.stream_id {
		s.self_dep = true
	}
	s.trailer_block << frame.fragment
	if !frame.end_headers {
		c.awaiting_cont = frame.stream_id
		return
	}
	c.finalize_trailers(mut s, mut handler)!
}

fn (mut c H2ServerConn) finalize_trailers(mut s H2ServerStream, mut handler Handler) ! {
	// An HPACK decoding failure is a CONNECTION error (RFC 9113 §4.3): the decoder's
	// dynamic table is now out of sync with the peer. Close the connection rather
	// than RST one stream.
	trailers := c.decoder.decode(s.trailer_block) or {
		c.send_goaway(.compression_error, 'HPACK decode error in trailers') or {}
		return error('h2 server: HPACK decode error in trailers (COMPRESSION_ERROR)')
	}
	// A HEADERS block received after END_STREAM (RFC 9113 §5.1): decoded above for
	// HPACK sync; now RST the stream with STREAM_CLOSED and stop.
	if s.over_end {
		c.send_rst_stream(s.id, .stream_closed)!
		c.mark_locally_reset(s.id)
		c.streams.delete(s.id)
		return
	}
	// A well-decoded but malformed trailer section is a STREAM error (§8.1.1).
	mut reason := if s.self_dep {
		// RFC 7540 §5.3.1: a stream cannot depend on itself.
		'trailing HEADERS priority depends on itself'
	} else if !s.end_stream {
		// A trailer section MUST terminate the stream (RFC 9113 §8.1).
		'trailing HEADERS without END_STREAM'
	} else {
		''
	}
	if reason == '' {
		for f in trailers {
			// Trailers carry no pseudo-headers and follow the §8.2 field rules.
			reason = if f.name.starts_with(':') {
				'pseudo-header "${f.name}" in trailers'
			} else {
				h2_request_field_error(f.name, f.value)
			}
			if reason != '' {
				break
			}
		}
	}
	if reason != '' {
		c.send_rst_stream(s.id, .protocol_error)!
		c.mark_locally_reset(s.id)
		c.streams.delete(s.id)
		return
	}
	c.run_request(mut s, mut handler)!
}

fn (mut c H2ServerConn) on_data(frame H2DataFrame, mut handler Handler) ! {
	mut s := c.streams[frame.stream_id] or {
		// RFC 9113 §5.1/§6.1: DATA is only valid on an open or half-closed (local)
		// stream. On an idle stream (never opened) it is a connection error
		// PROTOCOL_ERROR; on a closed stream (already finished or reset) it is a
		// STREAM_CLOSED stream error.
		if c.classify_stream(frame.stream_id) == .idle {
			return error('h2 server: DATA on idle stream ${frame.stream_id}')
		}
		// Credit the connection window (the peer spent it within its window).
		if frame.flow_size > 0 {
			c.send_window_update(0, u32(frame.flow_size)) or {}
		}
		// If we already sent RST_STREAM for this stream, the DATA was in-flight
		// before the client received the RST. Drain it silently (RFC 9113 §6.4).
		if frame.stream_id !in c.locally_reset {
			c.send_rst_stream(frame.stream_id, .stream_closed)!
			c.mark_locally_reset(frame.stream_id)
		}
		return
	}
	if !s.headers_done {
		return error('h2 server: DATA before END_HEADERS')
	}
	if s.end_stream {
		// Half-closed (remote): the stream already delivered END_STREAM (its
		// response may still be in flight while we are blocked on flow control).
		// Further DATA is a STREAM_CLOSED stream error (RFC 9113 §5.1). Credit the
		// connection window first, then reset just this stream.
		if frame.flow_size > 0 {
			c.send_window_update(0, u32(frame.flow_size)) or {}
		}
		c.send_rst_stream(s.id, .stream_closed)!
		c.mark_locally_reset(s.id)
		c.streams.delete(s.id)
		return
	}
	if frame.data.len > 0 {
		if s.body.len + frame.data.len > h2_server_max_request_body {
			// Credit the connection window before resetting so the peer is
			// not penalised for bytes it legitimately sent within its window.
			c.send_window_update(0, u32(frame.flow_size)) or {}
			c.send_rst_stream(s.id, .refused_stream)!
			c.mark_locally_reset(s.id)
			c.streams.delete(s.id)
			return
		}
		s.body << frame.data
	}
	// Replenish flow control using flow_size (full wire payload including padding),
	// per RFC 7540 §6.9.1. Credit unconditionally when flow_size>0: a padding-only
	// DATA frame (data.len==0, flow_size>0) still consumes the peer's send window
	// and must be credited back. (Formerly inside the data.len>0 block — padding-only
	// frames leaked window silently.)
	if frame.flow_size > 0 {
		c.send_window_update(0, u32(frame.flow_size))!
		c.send_window_update(s.id, u32(frame.flow_size))!
	}
	if frame.end_stream {
		s.end_stream = true
		c.run_request(mut s, mut handler)!
	}
}

fn (mut c H2ServerConn) run_request(mut s H2ServerStream, mut handler Handler) ! {
	req := c.build_request(s) or {
		c.send_rst_stream(s.id, .protocol_error)!
		c.mark_locally_reset(s.id)
		c.streams.delete(s.id)
		return
	}
	resp := handler.handle(req)
	c.send_response(s.id, resp, mut handler)!
	c.streams.delete(s.id)
}

fn (mut c H2ServerConn) build_request(s &H2ServerStream) !Request {
	mut req := Request{
		version: .v2_0
		header:  new_header()
	}
	// Pseudo-headers were already validated in h2_validate_request_pseudo
	// (presence, no duplicates, ordering, allowed set); here we only extract.
	mut method := ''
	mut path := ''
	mut authority := ''
	mut content_length := -1
	for f in s.headers {
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
				// Validated already; handlers infer the scheme from Host, matching
				// the HTTP/1.1 path.
			}
			else {
				if f.name.starts_with(':') {
					continue
				}
				if f.name == 'content-length' {
					if !h2_all_digits(f.value) {
						return error('h2 server: malformed content-length "${f.value}"')
					}
					cl := f.value.int()
					// Multiple content-length fields with differing values are a
					// malformed request (RFC 9110 §8.6); validate every occurrence,
					// not just the last.
					if content_length >= 0 && content_length != cl {
						return error('h2 server: conflicting content-length values ${content_length} and ${cl}')
					}
					content_length = cl
				}
				req.header.add_custom(f.name, f.value) or {}
			}
		}
	}
	// RFC 9113 §8.1.2.6: a declared content-length MUST equal the sum of the DATA
	// payload lengths; a mismatch is a malformed request (stream error).
	if content_length >= 0 && content_length != s.body.len {
		return error('h2 server: content-length ${content_length} != DATA length ${s.body.len}')
	}
	req.method = method_from_str(method)
	if authority != '' && !req.header.contains(.host) {
		req.header.add(.host, authority)
	}
	// Match the HTTP/1.1 path: req.url is the request-target (the :path
	// pseudo-header), so handlers see the same shape on both transports.
	req.url = path
	req.data = s.body.bytestr()
	req.host = authority
	return req
}

fn (mut c H2ServerConn) send_response(stream_id u32, resp Response, mut handler Handler) ! {
	status := if resp.status_code == 0 { 200 } else { resp.status_code }
	mut fields := [H2HeaderField{':status', status.str()}]
	for key in resp.header.keys() {
		lkey := key.to_lower()
		// Drop hop-by-hop headers; HTTP/2 forbids them (RFC 9113 §8.2.2).
		if lkey in h2_conn_specific_headers {
			continue
		}
		for val in resp.header.custom_values(key) {
			fields << H2HeaderField{lkey, val}
		}
	}
	body := resp.body.bytes()
	has_body := body.len > 0
	block := c.encoder.encode(fields)
	c.send_header_block(stream_id, block, !has_body)!
	if has_body {
		c.send_body(stream_id, body, mut handler)!
	}
}

fn (mut c H2ServerConn) send_header_block(stream_id u32, block []u8, end_stream bool) ! {
	max := int(c.peer.max_frame_size)
	if block.len <= max {
		c.send_frame(H2HeadersFrame{
			stream_id:   stream_id
			fragment:    block
			end_headers: true
			end_stream:  end_stream
		})!
		return
	}
	c.send_frame(H2HeadersFrame{
		stream_id:   stream_id
		fragment:    block[..max]
		end_headers: false
		end_stream:  end_stream
	})!
	mut off := max
	for off < block.len {
		mut next := off + max
		if next > block.len {
			next = block.len
		}
		c.send_frame(H2ContinuationFrame{
			stream_id:   stream_id
			fragment:    block[off..next]
			end_headers: next == block.len
		})!
		off = next
	}
}

fn (mut c H2ServerConn) send_body(stream_id u32, body []u8, mut handler Handler) ! {
	max := int(c.peer.max_frame_size)
	mut off := 0
	for off < body.len {
		// Respect both the connection and per-stream send windows
		// (RFC 7540 Section 6.9). When either is exhausted, read frames until
		// the peer grows a window with WINDOW_UPDATE.
		for c.send_window <= 0 || c.stream_send_window(stream_id) <= 0 {
			c.pump_for_window(mut handler)!
			// pump_for_window dispatches inbound frames; if the peer reset this
			// stream (RST_STREAM, or an illegal frame on the now half-closed
			// stream), it is gone from the map. Stop writing its response, but do
			// NOT error — a single stream's reset is a stream-scoped event and must
			// not tear down the whole connection.
			if stream_id !in c.streams {
				return
			}
		}
		avail := if c.send_window < c.stream_send_window(stream_id) {
			c.send_window
		} else {
			c.stream_send_window(stream_id)
		}
		mut chunk := body.len - off
		if chunk > max {
			chunk = max
		}
		if i64(chunk) > avail {
			chunk = int(avail)
		}
		next := off + chunk
		c.send_frame(H2DataFrame{
			stream_id:  stream_id
			data:       body[off..next]
			end_stream: next == body.len
		})!
		c.send_window -= i64(chunk)
		if mut s := c.streams[stream_id] {
			s.send_window -= i64(chunk)
		}
		off = next
	}
}

// stream_send_window returns the current per-stream send window, or 0 if the
// stream is gone.
fn (c &H2ServerConn) stream_send_window(stream_id u32) i64 {
	if s := c.streams[stream_id] {
		return s.send_window
	}
	return 0
}

// pump_for_window reads one frame while a response is blocked on flow control and
// routes it through the SAME dispatch path as the main loop. Delegating to
// dispatch_frame (rather than re-implementing a subset) means every rule applies
// here too: a HEADERS frame arriving before the unblocking WINDOW_UPDATE is
// HPACK-decoded and — being over the concurrency limit — answered with
// RST_STREAM(REFUSED_STREAM), instead of being silently dropped (which desynced
// the HPACK decoder and skipped the required reset). dispatch_frame does not
// re-enter run_request from here: a new stream is refused (never served), and the
// active stream is half-closed (remote) so its further DATA/HEADERS are
// STREAM_CLOSED — the caller (send_body) detects the resulting reset and stops.
fn (mut c H2ServerConn) pump_for_window(mut handler Handler) ! {
	frame := c.read_frame()!
	c.dispatch_frame(frame, mut handler)!
}

fn (mut c H2ServerConn) send_window_update(stream_id u32, inc u32) ! {
	if inc == 0 {
		return
	}
	c.send_frame(H2WindowUpdateFrame{
		stream_id:             stream_id
		window_size_increment: inc
	})!
}

fn (mut c H2ServerConn) send_rst_stream(stream_id u32, code H2ErrorCode) ! {
	c.send_frame(H2RstStreamFrame{
		stream_id:  stream_id
		error_code: u32(code)
	})!
}

fn (mut c H2ServerConn) send_goaway(code H2ErrorCode, msg string) ! {
	c.send_frame(H2GoawayFrame{
		last_stream_id: c.last_processed_stream_id
		error_code:     u32(code)
		debug_data:     msg.bytes()
	})!
	c.closing = true
}

fn (mut c H2ServerConn) read_frame() !H2Frame {
	c.fill_at_least(h2_frame_header_len)!
	header := h2_parse_frame_header(c.rbuf)!
	// Check against our own advertised receive limit (sent in send_initial_settings).
	// H2ServerConn always advertises h2_default_max_frame_size and never renegotiates
	// it, so the constant is correct here. c.peer.max_frame_size is the client's
	// receive limit (our outbound cap) and must not be used for inbound checking.
	if header.length > h2_default_max_frame_size {
		return error('h2 server: frame larger than SETTINGS_MAX_FRAME_SIZE (${header.length})')
	}
	total := h2_frame_header_len + int(header.length)
	c.fill_at_least(total)!
	frame := h2_parse_frame(header, c.rbuf[h2_frame_header_len..total])!
	c.rbuf = c.rbuf[total..].clone()
	return frame
}

fn (mut c H2ServerConn) fill_at_least(n int) ! {
	for c.rbuf.len < n {
		mut tmp := []u8{len: h2_conn_read_chunk}
		got := c.transport.read(mut tmp)!
		if got <= 0 {
			return error('h2 server: connection closed by peer')
		}
		c.rbuf << tmp[..got]
	}
}

fn (mut c H2ServerConn) send_frame(f H2Frame) ! {
	c.write_all(f.encode())!
}

fn (mut c H2ServerConn) write_all(data []u8) ! {
	mut sent := 0
	for sent < data.len {
		n := c.transport.write(data[sent..])!
		if n <= 0 {
			return error('h2 server: transport write returned ${n}')
		}
		sent += n
	}
}
