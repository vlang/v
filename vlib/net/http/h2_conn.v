// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import time

// --- HTTP/2 (RFC 7540 / 7541) file map -----------------------------------------
// The h2 implementation is split by layer; h2_conn.v (this file) is the lead.
//   h2_frame.v               binary framing layer (RFC 7540 §4, §6): parse/encode
//   h2_hpack.v               HPACK header compression (RFC 7541)
//   h2_hpack_static.v        HPACK static table data (RFC 7541 App. A)
//   h2_hpack_huffman*.v      HPACK Huffman code + table (generated)
//   h2_error.v               H2ErrorCode for RST_STREAM / GOAWAY
//   h2_conn.v                synchronous single-stream client connection (this file)
//   h2_mux_conn.v            multiplexed client: one connection, many concurrent streams
//   h2_server.v              server-side connection driver
//   h2_client.v              glue: net.http Request/Response <-> the h2 layer
//   vschannel_h2_windows.c.v HTTP/2 over the Windows SChannel TLS transport
// Each *.v has a sibling *_test.v with hermetic in-memory-transport tests.

// This file implements a minimal HTTP/2 client connection (RFC 7540) on top of
// the framing and HPACK layers. It is intentionally synchronous and handles a
// single request at a time: it sends one request, then reads frames until that
// stream completes, servicing connection-level frames (SETTINGS, PING,
// WINDOW_UPDATE, GOAWAY) inline. Concurrent multiplexing over one connection is
// provided separately by h2_mux_conn.v; this remains the smallest synchronous client.

// h2_client_preface is the fixed sequence a client sends to start an HTTP/2
// connection, immediately followed by a SETTINGS frame (RFC 7540 Section 3.5).
pub const h2_client_preface = 'PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n'

// h2_default_initial_window is the initial flow-control window for both the
// connection and new streams (RFC 7540 Section 6.9.2).
pub const h2_default_initial_window = u32(65535)

// h2_conn_read_chunk is the size of each transport read.
const h2_conn_read_chunk = 16 * 1024

// H2Transport is the byte transport an H2Conn runs over: typically an
// ALPN-negotiated `h2` TLS connection, but any reader/writer works (which makes
// the connection testable without a socket). Its method signatures match
// net.ssl.SSLConn, so an SSLConn satisfies it directly.
pub interface H2Transport {
mut:
	read(mut buf []u8) !int
	write(buf []u8) !int
}

// H2PeerSettings holds the peer's SETTINGS, with HTTP/2 defaults.
struct H2PeerSettings {
mut:
	header_table_size      u32  = 4096
	enable_push            bool = true
	max_concurrent_streams u32  = max_u32
	initial_window_size    u32  = 65535
	max_frame_size         u32  = 16384
	max_header_list_size   u32  = max_u32
}

// H2DataFn is called for each DATA frame received on the response stream, with
// the chunk's bytes, the cumulative body bytes received (including this chunk),
// the body length from Content-Length if known (else 0), and the response
// status code.
pub type H2DataFn = fn (chunk []u8, body_so_far u64, body_expected u64, status int) !

// H2ClientRequest describes a single HTTP/2 request. Header names in `headers`
// must be lowercase (RFC 7540 Section 8.1.2); the pseudo-headers are filled in
// from the other fields.
pub struct H2ClientRequest {
pub:
	method    string = 'GET'
	scheme    string = 'https'
	authority string
	path      string = '/'
	headers   []H2HeaderField
	body      []u8
	// Optional response chunk callback, called after each DATA frame's payload
	// is received. The arguments are the chunk bytes (not yet copied into the
	// response body), the cumulative body bytes received so far (including this
	// chunk), the body length from Content-Length (0 when not present), and the
	// response status code.
	on_data H2DataFn = unsafe { nil }
	// stop_copying_limit, when >= 0, caps the cumulative body bytes copied into
	// the response body; further DATA chunks are dropped but the callback keeps
	// firing and the stream is drained to completion.
	stop_copying_limit i64 = -1
	// stop_receiving_limit, when >= 0, causes the response read loop to break
	// once that many body bytes have been received. The callback fires for the
	// final chunk; no further callbacks fire after that.
	stop_receiving_limit i64 = -1
	// read_timeout, when > 0, bounds how long H2MuxConn.do() may wait for this
	// request's response before failing it. Ignored by the one-shot H2Conn:
	// its blocking transport read is already bounded by the underlying
	// connection's own read_timeout, and any timeout there fails the whole
	// (single-stream) connection directly. H2MuxConn's background reader, by
	// contrast, treats every transport read timeout as a benign wake-up (it
	// must survive one to keep servicing other streams), so nothing bounds an
	// individual request's wait unless this field does.
	read_timeout time.Duration
}

// H2ClientResponse is the result of an HTTP/2 request.
pub struct H2ClientResponse {
pub mut:
	status  int
	headers []H2HeaderField
	body    []u8
}

// H2Conn is a client-side HTTP/2 connection.
pub struct H2Conn {
mut:
	transport          H2Transport
	encoder            H2HpackEncoder
	decoder            H2HpackDecoder
	peer               H2PeerSettings
	rbuf               []u8      // buffered bytes read from the transport, not yet consumed
	pending            []H2Frame // stream frames read early (while sending), to replay
	next_stream_id     u32 = 1 // clients use odd stream ids
	cur_stream_id      u32 // the stream currently being driven by do()
	send_window        i64 = 65535 // connection-level send flow-control window
	stream_send_window i64 // send window for cur_stream_id
	handshaked         bool
	goaway             bool
	// aborted is set when this connection terminated a stream early
	// (RST_STREAM sent without draining the remaining DATA). Subsequent
	// requests on the same connection must fail rather than risk being starved
	// by leftover DATA frames the peer had already sent for the cancelled
	// stream.
	aborted bool
}

// new_h2_conn creates a client connection over `transport`. The HTTP/2
// connection preface is sent lazily on the first request.
pub fn new_h2_conn(transport H2Transport) &H2Conn {
	return &H2Conn{
		transport: transport
	}
}

// do sends `req` and returns the response, after the request's stream closes.
pub fn (mut c H2Conn) do(req H2ClientRequest) !H2ClientResponse {
	c.handshake()!
	if c.goaway {
		return error('h2: connection is shutting down (GOAWAY)')
	}
	if c.aborted {
		return error('h2: connection is no longer usable after an early stream termination')
	}
	stream_id := c.next_stream_id
	c.next_stream_id += 2
	c.cur_stream_id = stream_id
	c.stream_send_window = i64(c.peer.initial_window_size)

	mut fields := [
		H2HeaderField{':method', req.method},
		H2HeaderField{':scheme', req.scheme},
		H2HeaderField{':authority', req.authority},
		H2HeaderField{':path', req.path},
	]
	for h in req.headers {
		fields << h
	}
	// RFC 9113 §6.5.2: honor the peer's advisory SETTINGS_MAX_HEADER_LIST_SIZE —
	// refuse an over-limit request locally instead of having the server reject it
	// after the round trip.
	if c.peer.max_header_list_size != max_u32 {
		size := h2_header_list_size(fields)
		if size > u64(c.peer.max_header_list_size) {
			return error('h2: request header list (${size} bytes) exceeds peer SETTINGS_MAX_HEADER_LIST_SIZE (${c.peer.max_header_list_size})')
		}
	}
	block := c.encoder.encode(fields)
	has_body := req.body.len > 0
	c.send_header_block(stream_id, block, !has_body)!
	if has_body {
		c.send_body(stream_id, req.body)!
	}
	return c.read_response(stream_id, req)!
}

fn (mut c H2Conn) handshake() ! {
	if c.handshaked {
		return
	}
	c.write_all(h2_client_preface.bytes())!
	// Advertise our SETTINGS: refuse server push, and use the protocol defaults
	// otherwise. (Our decoder uses the default 4096-byte HPACK table.)
	c.send_frame(H2SettingsFrame{
		settings: [
			H2Setting{h2_settings_enable_push, 0},
			H2Setting{h2_settings_initial_window_size, h2_default_initial_window},
			H2Setting{h2_settings_max_frame_size, h2_default_max_frame_size},
		]
	})!
	c.handshaked = true
}

// read_response reads frames until `stream_id` is closed, returning its
// response and servicing connection-level frames along the way. The streaming
// options on `req` (on_data callback and the two stop limits) are honored
// while reading DATA frames, matching the HTTP/1.1 streaming semantics.
fn (mut c H2Conn) read_response(stream_id u32, req H2ClientRequest) !H2ClientResponse {
	mut resp := H2ClientResponse{}
	mut got_headers := false
	mut body_so_far := u64(0)
	mut body_expected := u64(0)
	mut has_content_length := false
	for {
		frame := c.next_frame()!
		if c.handle_conn_frame(frame)! {
			continue
		}
		match frame {
			H2HeadersFrame {
				if frame.stream_id != stream_id {
					continue
				}
				fragment := c.collect_header_block(frame.fragment, frame.end_headers, stream_id)!
				// Decode once — HPACK table state is advanced here.
				decoded := c.decoder.decode(fragment)!
				if got_headers {
					// A second HEADERS block carries trailers, which MUST end the
					// stream (RFC 9113 §8.1). Without END_STREAM read_response would
					// loop forever waiting for a stream end that never comes; the mux
					// path resets such a stream, so the synchronous client fails too.
					if !frame.end_stream {
						return error('h2: trailers HEADERS frame must carry END_STREAM')
					}
					for f in decoded {
						// RFC 9113 §8.1: trailers MUST NOT contain pseudo-header fields;
						// the §8.2 field-name rules apply. A malformed field makes the
						// response malformed — fail the request (mirrors the mux reset).
						if f.name.starts_with(':') {
							return error('h2: malformed trailers: pseudo-header ${f.name}')
						}
						reason := h2_response_field_error(f.name)
						if reason != '' {
							return error('h2: malformed trailers: ${reason}')
						}
						resp.headers << f
					}
					break
				}
				// First (interim or final) response HEADERS. Find :status. A
				// response MUST carry a valid :status (RFC 9113 §8.3.1), and 101 is
				// forbidden in HTTP/2 (§8.1.1).
				mut status := 0
				mut status_seen := false
				for f in decoded {
					if f.name == ':status' {
						if f.value.len == 3 && all_digits(f.value) {
							status = f.value.int()
						} else {
							return error('h2: malformed :status value: ${f.value}')
						}
						status_seen = true
						break
					}
				}
				if !status_seen || status < 100 || status > 599 || status == 101 {
					// Missing / out-of-range / 101 status: fail rather than latch a
					// bogus status or (for 101 or a sub-200 interim) loop forever
					// waiting for a "final" HEADERS. Mirrors the mux path, which
					// resets the stream with PROTOCOL_ERROR.
					return error('h2: response with a missing or invalid :status: ${status}')
				}
				if status >= 100 && status < 200 {
					// 1xx informational: discard and continue waiting for the
					// final HEADERS block. Do not set got_headers here.
					// A 1xx is not a final response and must not end the stream
					// (RFC 9113 §8.1); END_STREAM here is malformed. Fail rather
					// than loop forever waiting for a final response the stream can
					// no longer send. (The mux path rejects this as a stream-level
					// PROTOCOL_ERROR; the synchronous client has no other stream to
					// keep alive, so a connection-level error is appropriate here.)
					if frame.end_stream {
						return error('h2: server set END_STREAM on a 1xx informational response')
					}
					continue
				}
				// Final response (status >= 200): populate, skipping pseudo-headers.
				resp.status = status
				mut seen_regular := false
				mut seen_status := false
				for f in decoded {
					if f.name.starts_with(':') {
						// RFC 9113 §8.3: in a response only :status is valid; pseudo-
						// headers MUST precede regular fields and MUST NOT be duplicated.
						// An undefined pseudo, :status after a regular field, or a
						// second :status is malformed.
						if f.name != ':status' || seen_regular || seen_status {
							return error('h2: malformed response: invalid pseudo-header ${f.name}')
						}
						seen_status = true
						continue
					}
					seen_regular = true
					// RFC 9113 §8.2: reject uppercase/empty names and connection-specific
					// fields rather than delivering a malformed response to the caller.
					reason := h2_response_field_error(f.name)
					if reason != '' {
						return error('h2: malformed response: ${reason}')
					}
					resp.headers << f
					if f.name == 'content-length' {
						if all_digits(f.value) {
							body_expected = f.value.u64()
							has_content_length = true
						} else {
							return error('h2: malformed Content-Length: ${f.value}')
						}
					}
				}
				got_headers = true
				if frame.end_stream {
					break
				}
			}
			H2DataFrame {
				if frame.stream_id != stream_id {
					continue
				}
				if !got_headers {
					// DATA before the response HEADERS is a protocol error
					// (RFC 9113 §8.1); the mux path rejects it. Fail rather than
					// deliver body bytes for a response that has no status yet.
					return error('h2: DATA frame before response HEADERS')
				}
				if frame.data.len > 0 {
					body_so_far += u64(frame.data.len)
					// Append the chunk to the response body unless the copy
					// limit has been reached; the callback still fires.
					if req.stop_copying_limit < 0
						|| i64(body_so_far) - i64(frame.data.len) < req.stop_copying_limit {
						if req.stop_copying_limit >= 0 && i64(body_so_far) > req.stop_copying_limit {
							remaining := req.stop_copying_limit - (i64(body_so_far) - i64(frame.data.len))
							if remaining > 0 {
								resp.body << frame.data[..int(remaining)]
							}
						} else {
							resp.body << frame.data
						}
					}
					if req.on_data != unsafe { nil } {
						req.on_data(frame.data, body_so_far, body_expected, resp.status)!
					}
				}
				// Replenish flow control using flow_size (full wire payload including
				// padding), per RFC 7540 §6.9.1. Credit unconditionally when flow_size>0:
				// a padding-only DATA frame (data.len==0, flow_size>0) still consumes the
				// peer's send window and must be credited back. (Formerly inside the
				// data.len>0 block — padding-only frames leaked window silently.)
				if frame.flow_size > 0 {
					c.send_window_update(0, u32(frame.flow_size))!
					c.send_window_update(stream_id, u32(frame.flow_size))!
				}
				if frame.end_stream {
					break
				}
				if req.stop_receiving_limit >= 0 && i64(body_so_far) >= req.stop_receiving_limit {
					// Cancel the stream (RFC 7540 Section 8.1.4 / 5.4.2) so the
					// peer stops sending more DATA, and mark the connection
					// unusable: in-flight DATA frames that the peer has already
					// sent for this stream would otherwise consume the
					// connection-level receive window and block subsequent
					// requests on the same H2Conn.
					c.send_frame(H2RstStreamFrame{
						stream_id:  stream_id
						error_code: u32(H2ErrorCode.cancel)
					})!
					c.aborted = true
					break
				}
			}
			H2RstStreamFrame {
				if frame.stream_id == stream_id {
					return error('h2: stream reset by peer (${h2_error_code_name(frame.error_code)})')
				}
			}
			H2PushPromiseFrame {
				// We sent SETTINGS_ENABLE_PUSH=0 in the preface; receiving
				// PUSH_PROMISE is a connection error (RFC 7540 §8.2 PROTOCOL_ERROR).
				// Ignoring it without decoding the embedded HPACK block would also
				// desync the dynamic table.
				return error('h2: unexpected PUSH_PROMISE (server push was disabled)')
			}
			else {
				// PRIORITY / stray CONTINUATION / unknown: ignore.
			}
		}
	}
	if !got_headers {
		return error('h2: stream closed without a response')
	}
	// Verify body completeness when Content-Length was advertised (RFC 7230 §3.3.2).
	// Skip HEAD responses and 204/304 which carry no body by definition.
	// Skip early-cancelled streams where we sent RST_STREAM ourselves.
	body_allowed := req.method != 'HEAD' && resp.status != 204 && resp.status != 304
	if has_content_length && body_allowed && !c.aborted && body_so_far != body_expected {
		return error('h2: response body ${body_so_far} bytes does not match Content-Length ${body_expected}')
	}
	return resp
}

// collect_header_block returns the full HPACK block for a HEADERS frame,
// reading and concatenating CONTINUATION frames until END_HEADERS.
fn (mut c H2Conn) collect_header_block(first []u8, end_headers bool, stream_id u32) ![]u8 {
	if end_headers {
		return first
	}
	mut fragment := first.clone()
	for {
		frame := c.read_frame()!
		if frame is H2ContinuationFrame {
			if frame.stream_id != stream_id {
				return error('h2: CONTINUATION on the wrong stream')
			}
			fragment << frame.fragment
			if fragment.len > h2_max_recv_header_block {
				return error('h2: response header block exceeds ${h2_max_recv_header_block} bytes')
			}
			if frame.end_headers {
				break
			}
		} else {
			return error('h2: expected a CONTINUATION frame')
		}
	}
	return fragment
}

// handle_conn_frame services a connection-level frame, returning true if the
// frame was fully handled here (so the caller should skip it).
fn (mut c H2Conn) handle_conn_frame(frame H2Frame) !bool {
	match frame {
		H2SettingsFrame {
			if !frame.ack {
				c.apply_settings(frame.settings)!
				c.send_frame(H2SettingsFrame{
					ack: true
				})!
			}
			return true
		}
		H2PingFrame {
			if !frame.ack {
				c.send_frame(H2PingFrame{
					ack:  true
					data: frame.data
				})!
			}
			return true
		}
		H2WindowUpdateFrame {
			inc := frame.window_size_increment
			if frame.stream_id == 0 {
				if inc == 0 {
					// RFC 7540 §6.9: connection-level zero increment is a connection
					// error (PROTOCOL_ERROR).
					return error('h2: connection WINDOW_UPDATE with zero increment (RFC 7540 §6.9 PROTOCOL_ERROR)')
				}
				new_window := c.send_window + i64(inc)
				if new_window > i64(0x7fff_ffff) {
					// RFC 7540 §6.9.1: exceeding 2^31-1 is a FLOW_CONTROL_ERROR.
					return error('h2: connection flow-control window exceeded 2^31-1 (RFC 7540 §6.9.1)')
				}
				c.send_window = new_window
			} else if frame.stream_id == c.cur_stream_id {
				if inc == 0 {
					// RFC 7540 §6.9: stream-level zero increment is a stream error
					// (RST_STREAM). H2Conn has no multi-stream tracking so we treat
					// it as connection-fatal to avoid accepting a broken stream state.
					return error('h2: stream WINDOW_UPDATE with zero increment (RFC 7540 §6.9 PROTOCOL_ERROR)')
				}
				new_window := c.stream_send_window + i64(inc)
				if new_window > i64(0x7fff_ffff) {
					return error('h2: stream flow-control window exceeded 2^31-1 (RFC 7540 §6.9.1)')
				}
				c.stream_send_window = new_window
			}
			// Zero-increment on an untracked stream: silently ignore — H2Conn
			// cannot send RST_STREAM for a stream it does not own.
			return true
		}
		H2GoawayFrame {
			c.goaway = true
			return error('h2: GOAWAY received (${h2_error_code_name(frame.error_code)})')
		}
		else {
			return false
		}
	}
}

fn (mut c H2Conn) apply_settings(settings []H2Setting) ! {
	for s in settings {
		match s.id {
			h2_settings_header_table_size {
				c.peer.header_table_size = s.value
				// RFC 7541 §6.3: SETTINGS_HEADER_TABLE_SIZE from the server
				// constrains our ENCODER's dynamic table (outgoing request headers).
				// Evict entries exceeding the new limit immediately so we never
				// reference indices the peer has already evicted; the pending flag
				// causes encode() to emit the required Dynamic Table Size Update
				// prefix at the start of the next header block.
				c.encoder.dyn_table.set_max_size(int(s.value))
				c.encoder.pending_max_table_size = int(s.value)
			}
			h2_settings_enable_push {
				c.peer.enable_push = s.value != 0
			}
			h2_settings_max_concurrent_streams {
				c.peer.max_concurrent_streams = s.value
			}
			h2_settings_initial_window_size {
				if s.value > u32(0x7fff_ffff) {
					// RFC 7540 §6.5.3: values above 2^31-1 are a FLOW_CONTROL_ERROR.
					return error('h2: peer SETTINGS_INITIAL_WINDOW_SIZE ${s.value} exceeds 2^31-1')
				}
				// RFC 7540 §6.9.2: a change to the initial window size
				// retroactively adjusts the active stream's send window by the delta.
				// Validate before mutating so a rejected SETTINGS leaves state consistent.
				delta := i64(s.value) - i64(c.peer.initial_window_size)
				new_window := c.stream_send_window + delta
				if new_window > i64(0x7fff_ffff) {
					// RFC 7540 §6.9.2: if applying the delta pushes the stream
					// window above 2^31-1 it is a FLOW_CONTROL_ERROR.
					return error('h2: SETTINGS_INITIAL_WINDOW_SIZE delta overflows stream send window')
				}
				c.peer.initial_window_size = s.value
				c.stream_send_window = new_window
			}
			h2_settings_max_frame_size {
				if s.value < h2_default_max_frame_size || s.value > u32(0x00ff_ffff) {
					// RFC 7540 §6.5.2: valid range is 2^14..2^24-1. Values outside
					// this range are a connection SETTINGS_ERROR; a zero value would
					// also make our HEADERS/DATA chunk step zero and hang senders.
					return error('h2: peer SETTINGS_MAX_FRAME_SIZE ${s.value} out of range [16384, 16777215]')
				}
				c.peer.max_frame_size = s.value
			}
			h2_settings_max_header_list_size {
				c.peer.max_header_list_size = s.value
			}
			else {} // unknown settings are ignored (RFC 7540 §6.5.2)
		}
	}
}

// send_header_block sends an HPACK header block as a HEADERS frame, splitting
// it across CONTINUATION frames when it exceeds the peer's max frame size
// (RFC 7540 Section 4.3). END_STREAM, when set, goes on the HEADERS frame.
fn (mut c H2Conn) send_header_block(stream_id u32, block []u8, end_stream bool) ! {
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

// send_body writes the request body as DATA frames, chunked to the peer's max
// frame size and bounded by both the connection and the per-stream send
// flow-control windows (RFC 7540 Section 6.9).
fn (mut c H2Conn) send_body(stream_id u32, body []u8) ! {
	max := int(c.peer.max_frame_size)
	mut off := 0
	for off < body.len {
		for c.send_window <= 0 || c.stream_send_window <= 0 {
			// Wait for the peer to grow a window. Connection-level frames are
			// handled; any stream frames are stashed for read_response.
			frame := c.next_frame()!
			if !c.handle_conn_frame(frame)! {
				c.pending << frame
			}
		}
		avail := if c.send_window < c.stream_send_window {
			c.send_window
		} else {
			c.stream_send_window
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
		c.stream_send_window -= i64(chunk)
		off = next
	}
}

fn (mut c H2Conn) send_window_update(stream_id u32, inc u32) ! {
	if inc == 0 {
		return
	}
	c.send_frame(H2WindowUpdateFrame{
		stream_id:             stream_id
		window_size_increment: inc
	})!
}

// next_frame returns the next frame, replaying any stashed stream frames first.
fn (mut c H2Conn) next_frame() !H2Frame {
	if c.pending.len > 0 {
		f := c.pending[0]
		c.pending.delete(0)
		return f
	}
	return c.read_frame()!
}

// read_frame reads and decodes one frame from the transport, enforcing the
// receive limit we advertised to the peer in our own SETTINGS. This is
// h2_default_max_frame_size, which H2Conn always sends and never renegotiates.
// (c.peer.max_frame_size is the peer's receive limit — our outbound cap —
// and must not be used here.)
fn (mut c H2Conn) read_frame() !H2Frame {
	c.fill_at_least(h2_frame_header_len)!
	header := h2_parse_frame_header(c.rbuf)!
	if header.length > h2_default_max_frame_size {
		return error('h2: frame larger than SETTINGS_MAX_FRAME_SIZE (${header.length})')
	}
	total := h2_frame_header_len + int(header.length)
	c.fill_at_least(total)!
	frame := h2_parse_frame(header, c.rbuf[h2_frame_header_len..total])!
	c.rbuf = c.rbuf[total..].clone()
	return frame
}

// fill_at_least reads from the transport until rbuf holds at least n bytes.
fn (mut c H2Conn) fill_at_least(n int) ! {
	for c.rbuf.len < n {
		mut tmp := []u8{len: h2_conn_read_chunk}
		got := c.transport.read(mut tmp)!
		if got <= 0 {
			return error('h2: connection closed by peer')
		}
		c.rbuf << tmp[..got]
	}
}

fn (mut c H2Conn) send_frame(f H2Frame) ! {
	c.write_all(f.encode())!
}

fn (mut c H2Conn) write_all(data []u8) ! {
	mut sent := 0
	for sent < data.len {
		n := c.transport.write(data[sent..])!
		if n <= 0 {
			return error('h2: transport write returned ${n}')
		}
		sent += n
	}
}

// h2_error_code_name renders an HTTP/2 error code, falling back to hex for
// codes outside the defined range.
fn h2_error_code_name(code u32) string {
	if code <= u32(H2ErrorCode.http_1_1_required) {
		return unsafe { H2ErrorCode(code) }.str()
	}
	return 'unknown(0x${code.hex()})'
}
