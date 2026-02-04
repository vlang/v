// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

import net

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

// SimpleRequest represents a simplified HTTP/2 request
pub struct SimpleRequest {
pub:
	method  Method
	url     string
	host    string
	data    string
	headers map[string]string
}

// SimpleResponse represents a simplified HTTP/2 response
pub struct SimpleResponse {
pub:
	status_code int
	headers     map[string]string
	body        string
}

// Connection represents an HTTP/2 connection with full duplex streaming
pub struct Connection {
mut:
	conn               net.TcpConn
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

// new_client creates a new HTTP/2 client with connection preface and settings exchange
pub fn new_client(address string) !Client {
	mut tcp_conn := net.dial_tcp(address)!

	// Send HTTP/2 connection preface (PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n)
	tcp_conn.write_string(preface)!

	// Initialize connection with HPACK encoder/decoder
	mut conn := Connection{
		conn:    tcp_conn
		encoder: new_encoder()
		decoder: new_decoder()
	}

	// Exchange SETTINGS frames
	conn.send_settings()!
	conn.read_settings()!

	// Create buffer pool for efficient frame encoding
	pool := new_buffer_pool(16384, 10) // 10 buffers of 16KB each

	return Client{
		conn:        conn
		buffer_pool: pool
	}
}

// send_settings sends a SETTINGS frame to configure connection parameters
pub fn (mut c Connection) send_settings() ! {
	// Pre-allocate payload with exact size (5 settings * 6 bytes each = 30 bytes)
	mut payload := []u8{cap: 30}

	// Helper function to encode a setting
	encode_setting := fn (mut payload []u8, id SettingsId, value u32) {
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

// read_settings reads and processes a SETTINGS frame from the server
pub fn (mut c Connection) read_settings() ! {
	frame := c.read_frame()!

	if frame.header.frame_type != .settings {
		return error('expected SETTINGS frame, got ${frame.header.frame_type}')
	}

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

		match unsafe { SettingsId(id) } {
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
}

// write_frame writes an HTTP/2 frame to the TCP connection
pub fn (mut c Connection) write_frame(frame Frame) ! {
	data := frame.encode()
	c.conn.write(data)!
}

// read_frame reads an HTTP/2 frame from the TCP connection
pub fn (mut c Connection) read_frame() !Frame {
	mut header_buf := []u8{len: frame_header_size}
	c.conn.read(mut header_buf)!

	header := parse_frame_header(header_buf)!

	mut payload := []u8{len: int(header.length)}
	if header.length > 0 {
		c.conn.read(mut payload)!
	}

	return Frame{
		header:  header
		payload: payload
	}
}

// request sends an HTTP/2 request and returns the response from the server
pub fn (mut c Client) request(req SimpleRequest) !SimpleResponse {
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
fn (mut c Client) read_response(stream_id u32) !SimpleResponse {
	mut stream := c.conn.streams[stream_id] or { return error('stream ${stream_id} not found') }

	for !stream.end_stream || !stream.end_headers {
		frame := c.conn.read_frame()!

		// Handle frames for this stream or connection-level frames
		match frame.header.frame_type {
			.headers { c.handle_headers_frame(frame, mut stream, stream_id)! }
			.data { c.handle_data_frame(frame, mut stream, stream_id)! }
			.settings { c.conn.read_settings()! }
			.ping { c.handle_ping_frame(frame)! }
			.goaway { return error('connection closed by server (GOAWAY)') }
			.rst_stream { c.handle_rst_stream_frame(frame, stream_id)! }
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

// build_response constructs SimpleResponse from stream data
fn (c Client) build_response(stream &Stream) SimpleResponse {
	mut status_code := 200
	mut response_headers := map[string]string{}

	for header in stream.headers {
		if header.name == ':status' {
			status_code = header.value.int()
		} else if !header.name.starts_with(':') {
			response_headers[header.name] = header.value
		}
	}

	return SimpleResponse{
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
	c.conn.conn.close() or {}
	c.conn.closed = true
}
