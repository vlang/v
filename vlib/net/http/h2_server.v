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

// H2ServerStream holds the state of one in-flight server-side stream.
struct H2ServerStream {
mut:
	id           u32
	hpack_block  []u8 // assembled HEADERS + CONTINUATION fragments
	headers      []H2HeaderField
	body         []u8
	end_headers  bool
	end_stream   bool
	headers_done bool // set once we've decoded the HPACK block
	send_window  i64
}

// H2ServerConn drives one server-side HTTP/2 connection over a transport.
struct H2ServerConn {
mut:
	transport      H2Transport
	encoder        H2HpackEncoder
	decoder        H2HpackDecoder
	peer           H2PeerSettings
	rbuf           []u8
	send_window    i64 = i64(h2_server_default_window)
	streams        map[u32]&H2ServerStream
	last_stream_id u32
	awaiting_cont  u32 // non-zero when mid-CONTINUATION on this stream
	closing        bool
	idle_conns     &TlsIdleConnTracker = unsafe { nil }
	idle_handle    int
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
		// Best-effort GOAWAY before bailing.
		c.send_goaway(.protocol_error, err.msg()) or {}
		return err
	}
}

fn (mut c H2ServerConn) serve(mut handler Handler) ! {
	c.read_client_preface_idle()!
	c.send_initial_settings()!
	for !c.closing {
		frame := c.read_idle_frame() or {
			// Treat a clean transport close as end of session.
			return
		}
		c.dispatch_frame(frame, mut handler)!
	}
}

fn (mut c H2ServerConn) should_track_idle_read() bool {
	return c.idle_handle > 0 && c.idle_conns != unsafe { nil }
}

fn (mut c H2ServerConn) read_client_preface_idle() ! {
	if !c.should_track_idle_read() {
		c.read_client_preface()!
		return
	}
	if !c.idle_conns.mark_idle(c.idle_handle) {
		return error('h2 server: connection is shutting down')
	}
	defer {
		c.idle_conns.unmark_idle(c.idle_handle)
	}
	c.read_client_preface()!
}

fn (mut c H2ServerConn) read_idle_frame() !H2Frame {
	if !c.should_track_idle_read() {
		return c.read_frame()!
	}
	if !c.idle_conns.mark_idle(c.idle_handle) {
		return error('h2 server: connection is shutting down')
	}
	defer {
		c.idle_conns.unmark_idle(c.idle_handle)
	}
	return c.read_frame()!
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
	}
	match frame {
		H2SettingsFrame {
			if !frame.ack {
				c.apply_settings(frame.settings)
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
			if frame.stream_id == 0 {
				c.send_window += i64(frame.window_size_increment)
			} else if mut s := c.streams[frame.stream_id] {
				s.send_window += i64(frame.window_size_increment)
			}
		}
		H2GoawayFrame {
			c.closing = true
		}
		H2RstStreamFrame {
			c.streams.delete(frame.stream_id)
		}
		H2PriorityFrame {
			// Priority is advisory; ignore.
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

fn (mut c H2ServerConn) apply_settings(settings []H2Setting) {
	for s in settings {
		match s.id {
			h2_settings_header_table_size {
				c.peer.header_table_size = s.value
			}
			h2_settings_enable_push {
				c.peer.enable_push = s.value != 0
			}
			h2_settings_max_concurrent_streams {
				c.peer.max_concurrent_streams = s.value
			}
			h2_settings_initial_window_size {
				// RFC 7540 Section 6.9.2: a change to the initial window size
				// adjusts the send window of every active stream by the delta.
				delta := i64(s.value) - i64(c.peer.initial_window_size)
				c.peer.initial_window_size = s.value
				for _, mut st in c.streams {
					st.send_window += delta
				}
			}
			h2_settings_max_frame_size {
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
		return error('h2 server: CONTINUATION for unknown stream ${frame.stream_id}')
	}
	s.hpack_block << frame.fragment
	if frame.end_headers {
		s.end_headers = true
		c.awaiting_cont = 0
		c.finalize_headers(mut s, mut handler)!
	}
}

fn (mut c H2ServerConn) finalize_headers(mut s H2ServerStream, mut handler Handler) ! {
	s.headers = c.decoder.decode(s.hpack_block) or {
		c.send_rst_stream(s.id, .compression_error)!
		c.streams.delete(s.id)
		return
	}
	s.headers_done = true
	if s.end_stream {
		c.run_request(mut s, mut handler)!
	}
}

fn (mut c H2ServerConn) on_data(frame H2DataFrame, mut handler Handler) ! {
	mut s := c.streams[frame.stream_id] or {
		// DATA for an unknown stream (likely already RST'd); just drop and
		// keep flow control consistent.
		if frame.data.len > 0 {
			c.send_window_update(0, u32(frame.data.len))!
		}
		return
	}
	if !s.headers_done {
		return error('h2 server: DATA before END_HEADERS')
	}
	if frame.data.len > 0 {
		if s.body.len + frame.data.len > h2_server_max_request_body {
			c.send_rst_stream(s.id, .refused_stream)!
			c.streams.delete(s.id)
			return
		}
		s.body << frame.data
		// Replenish the connection window; per-stream we replenish on
		// completion since we hold the body in memory.
		c.send_window_update(0, u32(frame.data.len))!
		c.send_window_update(s.id, u32(frame.data.len))!
	}
	if frame.end_stream {
		s.end_stream = true
		c.run_request(mut s, mut handler)!
	}
}

fn (mut c H2ServerConn) run_request(mut s H2ServerStream, mut handler Handler) ! {
	req := c.build_request(s) or {
		c.send_rst_stream(s.id, .protocol_error)!
		c.streams.delete(s.id)
		return
	}
	resp := handler.handle(req)
	c.send_response(s.id, resp)!
	c.streams.delete(s.id)
}

fn (mut c H2ServerConn) build_request(s &H2ServerStream) !Request {
	mut req := Request{
		version: .v2_0
		header:  new_header()
	}
	mut method := ''
	mut path := ''
	mut authority := ''
	mut scheme := 'https'
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
				scheme = f.value
			}
			else {
				if f.name.starts_with(':') {
					return error('h2 server: unknown pseudo-header ${f.name}')
				}
				req.header.add_custom(f.name, f.value) or {}
			}
		}
	}
	if method == '' || path == '' {
		return error('h2 server: missing :method or :path')
	}
	req.method = method_from_str(method)
	if authority != '' && !req.header.contains(.host) {
		req.header.add(.host, authority)
	}
	// Match the HTTP/1.1 path: req.url is the request-target (the :path
	// pseudo-header), so handlers see the same shape on both transports.
	_ = scheme // :scheme is parsed and discarded; handlers infer it from Host
	req.url = path
	req.data = s.body.bytestr()
	req.host = authority
	return req
}

fn (mut c H2ServerConn) send_response(stream_id u32, resp Response) ! {
	status := if resp.status_code == 0 { 200 } else { resp.status_code }
	mut fields := [H2HeaderField{':status', status.str()}]
	for key in resp.header.keys() {
		lkey := key.to_lower()
		// Drop hop-by-hop headers; HTTP/2 forbids them (RFC 7540 §8.1.2.2).
		if lkey in ['connection', 'keep-alive', 'transfer-encoding', 'upgrade', 'proxy-connection'] {
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
		c.send_body(stream_id, body)!
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

fn (mut c H2ServerConn) send_body(stream_id u32, body []u8) ! {
	max := int(c.peer.max_frame_size)
	mut off := 0
	for off < body.len {
		// Respect both the connection and per-stream send windows
		// (RFC 7540 Section 6.9). When either is exhausted, read frames until
		// the peer grows a window with WINDOW_UPDATE.
		for c.send_window <= 0 || c.stream_send_window(stream_id) <= 0 {
			c.pump_for_window(stream_id)!
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

// pump_for_window reads one frame while a response is blocked on flow control,
// servicing connection-level frames (SETTINGS / PING / WINDOW_UPDATE) and a
// RST_STREAM for the stream being written.
fn (mut c H2ServerConn) pump_for_window(stream_id u32) ! {
	frame := c.read_frame()!
	match frame {
		H2SettingsFrame {
			if !frame.ack {
				c.apply_settings(frame.settings)
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
			if frame.stream_id == 0 {
				c.send_window += i64(frame.window_size_increment)
			} else if mut s := c.streams[frame.stream_id] {
				s.send_window += i64(frame.window_size_increment)
			}
		}
		H2RstStreamFrame {
			if frame.stream_id == stream_id {
				return error('h2 server: stream reset by peer while writing response')
			}
		}
		else {
			// With SETTINGS_MAX_CONCURRENT_STREAMS=1 no other stream frames are
			// expected mid-response; ignore anything else defensively.
		}
	}
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
		last_stream_id: c.last_stream_id
		error_code:     u32(code)
		debug_data:     msg.bytes()
	})!
}

fn (mut c H2ServerConn) read_frame() !H2Frame {
	c.fill_at_least(h2_frame_header_len)!
	header := h2_parse_frame_header(c.rbuf)!
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
