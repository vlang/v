// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// This file implements a minimal HTTP/2 client connection (RFC 7540) on top of
// the framing and HPACK layers. It is intentionally synchronous and handles a
// single request at a time: it sends one request, then reads frames until that
// stream completes, servicing connection-level frames (SETTINGS, PING,
// WINDOW_UPDATE, GOAWAY) inline. Stream multiplexing and a background reader
// are a follow-up; this is the smallest useful, testable client.

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
	transport      H2Transport
	encoder        H2HpackEncoder
	decoder        H2HpackDecoder
	peer           H2PeerSettings
	rbuf           []u8      // buffered bytes read from the transport, not yet consumed
	pending        []H2Frame // stream frames read early (while sending), to replay
	next_stream_id u32 = 1 // clients use odd stream ids
	send_window    i64 = 65535
	handshaked     bool
	goaway         bool
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
	stream_id := c.next_stream_id
	c.next_stream_id += 2

	mut fields := [
		H2HeaderField{':method', req.method},
		H2HeaderField{':scheme', req.scheme},
		H2HeaderField{':authority', req.authority},
		H2HeaderField{':path', req.path},
	]
	for h in req.headers {
		fields << h
	}
	block := c.encoder.encode(fields)
	has_body := req.body.len > 0
	c.send_frame(H2HeadersFrame{
		stream_id:   stream_id
		fragment:    block
		end_headers: true
		end_stream:  !has_body
	})!
	if has_body {
		c.send_body(stream_id, req.body)!
	}
	return c.read_response(stream_id)!
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
// response and servicing connection-level frames along the way.
fn (mut c H2Conn) read_response(stream_id u32) !H2ClientResponse {
	mut resp := H2ClientResponse{}
	mut got_headers := false
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
				for f in c.decoder.decode(fragment)! {
					if f.name == ':status' {
						resp.status = f.value.int()
					} else if !f.name.starts_with(':') {
						resp.headers << f
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
				resp.body << frame.data
				if frame.data.len > 0 {
					// Replenish flow control so the peer keeps sending.
					c.send_window_update(0, u32(frame.data.len))!
					c.send_window_update(stream_id, u32(frame.data.len))!
				}
				if frame.end_stream {
					break
				}
			}
			H2RstStreamFrame {
				if frame.stream_id == stream_id {
					return error('h2: stream reset by peer (${h2_error_code_name(frame.error_code)})')
				}
			}
			else {
				// PRIORITY / PUSH_PROMISE / stray CONTINUATION / unknown: ignore.
			}
		}
	}
	if !got_headers {
		return error('h2: stream closed without a response')
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
				c.apply_settings(frame.settings)
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
			// Only the connection-level send window is tracked in this
			// single-stream client; per-stream grants are not yet needed.
			if frame.stream_id == 0 {
				c.send_window += i64(frame.window_size_increment)
			}
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

fn (mut c H2Conn) apply_settings(settings []H2Setting) {
	for s in settings {
		match s.id {
			h2_settings_header_table_size { c.peer.header_table_size = s.value }
			h2_settings_enable_push { c.peer.enable_push = s.value != 0 }
			h2_settings_max_concurrent_streams { c.peer.max_concurrent_streams = s.value }
			h2_settings_initial_window_size { c.peer.initial_window_size = s.value }
			h2_settings_max_frame_size { c.peer.max_frame_size = s.value }
			h2_settings_max_header_list_size { c.peer.max_header_list_size = s.value }
			else {} // unknown settings are ignored (RFC 7540 Section 6.5.2)
		}
	}
}

// send_body writes the request body as DATA frames, chunked to the peer's
// max frame size and bounded by the connection-level flow-control window.
fn (mut c H2Conn) send_body(stream_id u32, body []u8) ! {
	max := int(c.peer.max_frame_size)
	mut off := 0
	for off < body.len {
		for c.send_window <= 0 {
			// Wait for the peer to grow the window. Connection-level frames are
			// handled; any stream frames are stashed for read_response.
			frame := c.next_frame()!
			if !c.handle_conn_frame(frame)! {
				c.pending << frame
			}
		}
		mut chunk := body.len - off
		if chunk > max {
			chunk = max
		}
		if i64(chunk) > c.send_window {
			chunk = int(c.send_window)
		}
		next := off + chunk
		c.send_frame(H2DataFrame{
			stream_id:  stream_id
			data:       body[off..next]
			end_stream: next == body.len
		})!
		c.send_window -= i64(chunk)
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

// read_frame reads and decodes one frame from the transport, enforcing our
// advertised max frame size.
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
