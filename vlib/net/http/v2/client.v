// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

import net
import net.ssl

// Method represents HTTP request methods
pub enum Method {
	get
	post
	put
	patch
	delete
	head
	options
}

// str returns the string representation of the HTTP method
pub fn (m Method) str() string {
	return match m {
		.get { 'GET' }
		.post { 'POST' }
		.put { 'PUT' }
		.patch { 'PATCH' }
		.delete { 'DELETE' }
		.head { 'HEAD' }
		.options { 'OPTIONS' }
	}
}

// Request represents a simplified HTTP/2 request
pub struct Request {
pub:
	method  Method
	url     string
	host    string
	data    string
	headers map[string]string
}

// Response represents a simplified HTTP/2 response
pub struct Response {
pub:
	status_code int
	headers     map[string]string
	body        string
}

// Connection represents an HTTP/2 connection with full duplex streaming over TLS
pub struct Connection {
mut:
	ssl_conn           &ssl.SSLConn = unsafe { nil }
	encoder            Encoder
	decoder            Decoder
	streams            map[u32]&Stream
	next_stream_id     u32 = 1
	settings           Settings
	remote_settings    Settings
	window_size        i64 = 65535
	remote_window_size i64 = 65535
	last_stream_id     u32
	closed             bool
}

// Settings holds HTTP/2 connection settings per RFC 7540 Section 6.5
pub struct Settings {
pub mut:
	header_table_size      u32  = 4096
	enable_push            bool = true
	max_concurrent_streams u32  = 100
	initial_window_size    u32  = 65535
	max_frame_size         u32  = 16384
	max_header_list_size   u32 // 0 = unlimited
}

// Stream represents an HTTP/2 stream with flow control
pub struct Stream {
pub mut:
	id          u32
	state       StreamState
	window_size i64 = 65535
	headers     []HeaderField
	data        []u8
	end_stream  bool
	end_headers bool
}

// StreamState represents HTTP/2 stream states per RFC 7540 Section 5.1
pub enum StreamState {
	idle
	reserved_local
	reserved_remote
	open
	half_closed_local
	half_closed_remote
	closed
}

// Client represents an HTTP/2 client with connection pooling
pub struct Client {
mut:
	conn        Connection
	buffer_pool BufferPool
}

// new_client creates a new HTTP/2 client with TLS + ALPN 'h2' negotiation,
// connection preface, and settings exchange.
// The address should be in the form 'hostname:port' (e.g. 'example.com:443').
pub fn new_client(address string) !Client {
	host, port := net.split_address(address)!

	// Create TLS connection with ALPN 'h2' for HTTP/2 negotiation (RFC 7540 Section 3.3)
	mut ssl_conn := ssl.new_ssl_conn(
		alpn_protocols: ['h2']
	)!
	ssl_conn.dial(host, port)!

	// Send HTTP/2 connection preface over TLS (PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n)
	ssl_conn.write_string(preface)!

	// Initialize connection with HPACK encoder/decoder
	mut conn := Connection{
		ssl_conn: ssl_conn
		encoder:  new_encoder()
		decoder:  new_decoder()
		settings: Settings{
			enable_push: false // clients must not enable push
		}
	}

	// Exchange SETTINGS frames
	conn.write_settings()!
	conn.read_settings()!

	// Create buffer pool for efficient frame encoding
	pool := new_buffer_pool(16384, 10)

	return Client{
		conn:        conn
		buffer_pool: pool
	}
}

// write_settings sends a SETTINGS frame to configure connection parameters
pub fn (mut c Connection) write_settings() ! {
	// Pre-allocate payload with exact size (5 settings * 6 bytes each = 30 bytes)
	mut payload := []u8{cap: 30}

	// Helper function to encode a setting
	encode_setting := fn (mut payload []u8, id SettingId, value u32) {
		payload << u8(u16(id) >> 8)
		payload << u8(u16(id))
		payload << u8(value >> 24)
		payload << u8(value >> 16)
		payload << u8(value >> 8)
		payload << u8(value)
	}

	// Encode each setting
	encode_setting(mut payload, .header_table_size, c.settings.header_table_size)
	encode_setting(mut payload, .enable_push, if c.settings.enable_push { u32(1) } else { u32(0) })
	encode_setting(mut payload, .max_concurrent_streams, c.settings.max_concurrent_streams)
	encode_setting(mut payload, .initial_window_size, c.settings.initial_window_size)
	encode_setting(mut payload, .max_frame_size, c.settings.max_frame_size)

	frame := Frame{
		header:  FrameHeader{
			length:     u32(payload.len)
			frame_type: .settings
			flags:      0
			stream_id:  0
		}
		payload: payload
	}

	c.write_frame(frame)!
}

// read_settings reads and processes a SETTINGS frame from the server,
// skipping over non-SETTINGS frames (e.g. WINDOW_UPDATE) that may precede it.
pub fn (mut c Connection) read_settings() ! {
	// Limit the number of frames to read before receiving SETTINGS
	// to prevent infinite loop if the server never sends one.
	max_frames := 10
	for frame_count := 0; frame_count < max_frames; frame_count++ {
		frame := c.read_frame()!

		match frame.header.frame_type {
			.settings {
				// ACK frame has no payload
				if frame.header.has_flag(.ack) {
					return
				}

				// Parse settings (each setting is 6 bytes: 2-byte ID + 4-byte value)
				mut idx := 0
				for idx < frame.payload.len {
					if idx + 6 > frame.payload.len {
						return error('invalid SETTINGS frame: incomplete setting at byte ${idx}')
					}

					id := (u16(frame.payload[idx]) << 8) | u16(frame.payload[idx + 1])
					value := (u32(frame.payload[idx + 2]) << 24) | (u32(frame.payload[idx + 3]) << 16) | (u32(frame.payload[
						idx + 4]) << 8) | u32(frame.payload[idx + 5])

					setting_id := setting_id_from_u16(id) or {
						// Per RFC 7540 Section 6.5.2, unknown settings must be ignored
						idx += 6
						continue
					}

					match setting_id {
						.header_table_size { c.remote_settings.header_table_size = value }
						.enable_push { c.remote_settings.enable_push = value != 0 }
						.max_concurrent_streams { c.remote_settings.max_concurrent_streams = value }
						.initial_window_size { c.remote_settings.initial_window_size = value }
						.max_frame_size { c.remote_settings.max_frame_size = value }
						.max_header_list_size { c.remote_settings.max_header_list_size = value }
					}

					idx += 6
				}

				// Send SETTINGS ACK
				ack_frame := Frame{
					header:  FrameHeader{
						length:     0
						frame_type: .settings
						flags:      u8(FrameFlags.ack)
						stream_id:  0
					}
					payload: []u8{}
				}

				c.write_frame(ack_frame)!
				return
			}
			.window_update {
				// WINDOW_UPDATE on stream 0 adjusts the connection window
				if frame.header.stream_id == 0 && frame.payload.len >= 4 {
					increment := (u32(frame.payload[0]) << 24) | (u32(frame.payload[1]) << 16) | (u32(frame.payload[2]) << 8) | u32(frame.payload[3])
					c.remote_window_size += i64(increment & 0x7fffffff)
				}
				continue
			}
			.goaway {
				mut error_code := u32(0)
				if frame.payload.len >= 8 {
					error_code = (u32(frame.payload[4]) << 24) | (u32(frame.payload[5]) << 16) | (u32(frame.payload[6]) << 8) | u32(frame.payload[7])
				}
				debug_data := if frame.payload.len > 8 {
					frame.payload[8..].bytestr()
				} else {
					''
				}
				return error('server sent GOAWAY (error code: ${error_code}, debug: ${debug_data})')
			}
			else {
				// Skip unexpected frames during setup
				continue
			}
		}
	}
	return error('did not receive SETTINGS frame within ${max_frames} frames')
}

// write_frame writes an HTTP/2 frame to the TLS connection
pub fn (mut c Connection) write_frame(frame Frame) ! {
	data := frame.encode()
	$if trace_http2 ? {
		eprintln('[HTTP/2] write frame: type=${frame.header.frame_type} len=${frame.header.length} flags=0x${frame.header.flags:02x} stream=${frame.header.stream_id} raw_len=${data.len}')
	}
	c.ssl_conn.write(data)!
}

// read_frame reads an HTTP/2 frame from the TLS connection
pub fn (mut c Connection) read_frame() !Frame {
	mut header_buf := []u8{len: frame_header_size}
	read_exact(mut c.ssl_conn, mut header_buf)!

	header := parse_frame_header(header_buf)!

	mut payload := []u8{len: int(header.length)}
	if header.length > 0 {
		read_exact(mut c.ssl_conn, mut payload)!
	}

	$if trace_http2 ? {
		eprintln('[HTTP/2] read frame: type=${header.frame_type} len=${header.length} flags=0x${header.flags:02x} stream=${header.stream_id}')
	}

	return Frame{
		header:  header
		payload: payload
	}
}

// read_exact reads exactly buf.len bytes from the SSL connection.
// SSL read may return fewer bytes than requested, so this loops until full.
fn read_exact(mut conn ssl.SSLConn, mut buf []u8) ! {
	mut total := 0
	for total < buf.len {
		n := conn.read(mut buf[total..]) or {
			if total == 0 {
				return err
			}
			return error('unexpected EOF after ${total} of ${buf.len} bytes')
		}
		if n == 0 {
			return error('unexpected EOF after ${total} of ${buf.len} bytes')
		}
		total += n
	}
}

// request sends an HTTP/2 request and returns the response from the server
pub fn (mut c Client) request(req Request) !Response {
	// Allocate new stream ID (client uses odd stream IDs)
	stream_id := c.conn.next_stream_id
	c.conn.next_stream_id += 2

	// Create and register stream
	mut stream := &Stream{
		id:    stream_id
		state: .idle
	}
	c.conn.streams[stream_id] = stream

	// Build pseudo-headers and custom headers
	mut headers := [
		HeaderField{':method', req.method.str()},
		HeaderField{':scheme', 'https'},
		HeaderField{':path', req.url},
		HeaderField{':authority', req.host},
	]
	for key, value in req.headers {
		headers << HeaderField{key.to_lower(), value}
	}

	// Encode headers using HPACK
	encoded_headers := c.conn.encoder.encode(headers)

	$if trace_http2 ? {
		eprintln('[HTTP/2] HPACK encoded ${headers.len} headers -> ${encoded_headers.len} bytes: ${encoded_headers.hex()}')
		for h in headers {
			eprintln('[HTTP/2]   ${h.name}: ${h.value}')
		}
	}

	// Send HEADERS frame
	mut flags := u8(FrameFlags.end_headers)
	if req.data.len == 0 {
		flags |= u8(FrameFlags.end_stream)
	}

	headers_frame := Frame{
		header:  FrameHeader{
			length:     u32(encoded_headers.len)
			frame_type: .headers
			flags:      flags
			stream_id:  stream_id
		}
		payload: encoded_headers
	}

	c.conn.write_frame(headers_frame)!
	stream.state = if req.data.len == 0 { .half_closed_local } else { .open }

	// Send DATA frame if there's a body
	if req.data.len > 0 {
		data_frame := Frame{
			header:  FrameHeader{
				length:     u32(req.data.len)
				frame_type: .data
				flags:      u8(FrameFlags.end_stream)
				stream_id:  stream_id
			}
			payload: req.data.bytes()
		}
		c.conn.write_frame(data_frame)!
		stream.state = .half_closed_local
	}

	return c.read_response(stream_id)!
}

// read_response reads and assembles the response for a specific stream
fn (mut c Client) read_response(stream_id u32) !Response {
	mut stream := c.conn.streams[stream_id] or { return error('stream ${stream_id} not found') }

	for !stream.end_stream || !stream.end_headers {
		frame := c.conn.read_frame()!

		// Handle frames for this stream or connection-level frames
		match frame.header.frame_type {
			.headers {
				c.handle_headers_frame(frame, mut stream, stream_id)!
			}
			.data {
				c.handle_data_frame(frame, mut stream, stream_id)!
			}
			.settings {
				// SETTINGS ACK or unsolicited SETTINGS during response
				if !frame.header.has_flag(.ack) {
					// Send ACK for unsolicited SETTINGS
					ack_frame := Frame{
						header:  FrameHeader{
							length:     0
							frame_type: .settings
							flags:      u8(FrameFlags.ack)
							stream_id:  0
						}
						payload: []u8{}
					}
					c.conn.write_frame(ack_frame)!
				}
			}
			.ping {
				c.handle_ping_frame(frame)!
			}
			.goaway {
				return error('connection closed by server (GOAWAY)')
			}
			.rst_stream {
				c.handle_rst_stream_frame(frame, stream_id)!
			}
			.window_update {} // Acknowledge window updates without action for now
			.push_promise {
				// RFC 7540 §8.2: A client that has set SETTINGS_ENABLE_PUSH=0
				// MUST treat receipt of a PUSH_PROMISE frame as a connection error.
				return error('received PUSH_PROMISE but push is disabled (RFC 7540 §8.2)')
			}
			else {} // Ignore unknown frame types per RFC 7540
		}
	}

	return c.build_response(stream)
}

// handle_headers_frame processes HEADERS frame for a stream
fn (mut c Client) handle_headers_frame(frame Frame, mut stream Stream, stream_id u32) ! {
	if frame.header.stream_id != stream_id {
		return
	}

	headers := c.conn.decoder.decode(frame.payload)!
	stream.headers << headers

	if frame.header.has_flag(.end_headers) {
		stream.end_headers = true
	}
	if frame.header.has_flag(.end_stream) {
		stream.end_stream = true
		stream.state = .half_closed_remote
	}
}

// handle_data_frame processes DATA frame for a stream
fn (c Client) handle_data_frame(frame Frame, mut stream Stream, stream_id u32) ! {
	if frame.header.stream_id != stream_id {
		return
	}

	stream.data << frame.payload

	if frame.header.has_flag(.end_stream) {
		stream.end_stream = true
		stream.state = .closed
	}
}

// handle_ping_frame responds to PING frame with ACK
fn (mut c Client) handle_ping_frame(frame Frame) ! {
	pong := Frame{
		header:  FrameHeader{
			length:     u32(frame.payload.len)
			frame_type: .ping
			flags:      u8(FrameFlags.ack)
			stream_id:  0
		}
		payload: frame.payload
	}
	c.conn.write_frame(pong)!
}

// handle_rst_stream_frame handles RST_STREAM frame
fn (c Client) handle_rst_stream_frame(frame Frame, stream_id u32) ! {
	if frame.header.stream_id == stream_id {
		return error('stream reset by server (RST_STREAM)')
	}
}

// build_response constructs Response from stream data
fn (c Client) build_response(stream &Stream) Response {
	mut status_code := 200
	mut response_headers := map[string]string{}

	for header in stream.headers {
		if header.name == ':status' {
			status_code = header.value.int()
		} else if !header.name.starts_with(':') {
			response_headers[header.name] = header.value
		}
	}

	return Response{
		body:        stream.data.bytestr()
		status_code: status_code
		headers:     response_headers
	}
}

// close closes the HTTP/2 connection gracefully with GOAWAY frame
pub fn (mut c Client) close() {
	if c.conn.closed {
		return
	}

	// Send GOAWAY frame (last stream ID = 0, error code = 0)
	goaway := Frame{
		header:  FrameHeader{
			length:     8
			frame_type: .goaway
			flags:      0
			stream_id:  0
		}
		payload: [u8(0), 0, 0, 0, 0, 0, 0, 0]
	}

	c.conn.write_frame(goaway) or {}
	c.conn.ssl_conn.shutdown() or {}
	c.conn.closed = true
}
