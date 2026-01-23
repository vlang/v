// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

import net

// HTTP Method
pub enum Method {
	get
	post
	put
	patch
	delete
	head
	options
}

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

// SimpleRequest represents a simplified HTTP request for v2
pub struct SimpleRequest {
pub:
	method  Method
	url     string
	host    string
	data    string
	headers map[string]string
}

// SimpleResponse represents a simplified HTTP response
pub struct SimpleResponse {
pub:
	status_code int
	headers     map[string]string
	body        string
}

// Connection represents an HTTP/2 connection
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

// Settings holds HTTP/2 connection settings
pub struct Settings {
pub mut:
	header_table_size      u32  = 4096
	enable_push            bool = true
	max_concurrent_streams u32  = 100
	initial_window_size    u32  = 65535
	max_frame_size         u32  = 16384
	max_header_list_size   u32 // unlimited
}

// Stream represents an HTTP/2 stream
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

// StreamState represents the state of an HTTP/2 stream
pub enum StreamState {
	idle
	reserved_local
	reserved_remote
	open
	half_closed_local
	half_closed_remote
	closed
}

// Client represents an HTTP/2 client
pub struct Client {
mut:
	conn Connection
}

// new_client creates a new HTTP/2 client
pub fn new_client(address string) !Client {
	mut tcp_conn := net.dial_tcp(address)!

	// Send HTTP/2 connection preface
	tcp_conn.write_string(preface)!

	// Create connection
	mut conn := Connection{
		conn:    tcp_conn
		encoder: new_encoder()
		decoder: new_decoder()
	}

	// Send initial SETTINGS frame
	conn.send_settings()!

	// Wait for server SETTINGS
	conn.read_settings()!

	return Client{
		conn: conn
	}
}

// send_settings sends a SETTINGS frame
pub fn (mut c Connection) send_settings() ! {
	mut settings := map[u16]u32{}
	settings[u16(SettingsId.header_table_size)] = c.settings.header_table_size
	settings[u16(SettingsId.enable_push)] = if c.settings.enable_push { u32(1) } else { u32(0) }
	settings[u16(SettingsId.max_concurrent_streams)] = c.settings.max_concurrent_streams
	settings[u16(SettingsId.initial_window_size)] = c.settings.initial_window_size
	settings[u16(SettingsId.max_frame_size)] = c.settings.max_frame_size

	mut payload := []u8{}
	for id, value in settings {
		payload << u8(id >> 8)
		payload << u8(id)
		payload << u8(value >> 24)
		payload << u8(value >> 16)
		payload << u8(value >> 8)
		payload << u8(value)
	}

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

// read_settings reads and processes a SETTINGS frame
pub fn (mut c Connection) read_settings() ! {
	frame := c.read_frame()!

	if frame.header.frame_type != .settings {
		return error('expected SETTINGS frame, got ${frame.header.frame_type}')
	}

	if frame.header.has_flag(.ack) {
		return
	}

	// Parse settings
	mut idx := 0
	for idx < frame.payload.len {
		if idx + 6 > frame.payload.len {
			return error('invalid SETTINGS frame')
		}

		id := (u16(frame.payload[idx]) << 8) | u16(frame.payload[idx + 1])
		value := (u32(frame.payload[idx + 2]) << 24) | (u32(frame.payload[idx + 3]) << 16) | (u32(frame.payload[
			idx + 4]) << 8) | u32(frame.payload[idx + 5])

		match unsafe { SettingsId(id) } {
			.header_table_size {
				c.remote_settings.header_table_size = value
			}
			.enable_push {
				c.remote_settings.enable_push = value != 0
			}
			.max_concurrent_streams {
				c.remote_settings.max_concurrent_streams = value
			}
			.initial_window_size {
				c.remote_settings.initial_window_size = value
			}
			.max_frame_size {
				c.remote_settings.max_frame_size = value
			}
			.max_header_list_size {
				c.remote_settings.max_header_list_size = value
			}
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

// write_frame writes a frame to the connection
pub fn (mut c Connection) write_frame(frame Frame) ! {
	data := frame.encode()
	c.conn.write(data)!
}

// read_frame reads a frame from the connection
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

// request sends an HTTP/2 request and returns the response
pub fn (mut c Client) request(req SimpleRequest) !SimpleResponse {
	// Allocate new stream ID
	stream_id := c.conn.next_stream_id
	c.conn.next_stream_id += 2 // Client uses odd stream IDs

	// Create stream
	mut stream := &Stream{
		id:    stream_id
		state: .idle
	}
	c.conn.streams[stream_id] = stream

	// Build headers
	mut headers := []HeaderField{}
	headers << HeaderField{':method', req.method.str()}
	headers << HeaderField{':scheme', 'https'}
	headers << HeaderField{':path', req.url}
	headers << HeaderField{':authority', req.host}

	// Add custom headers
	for key, value in req.headers {
		headers << HeaderField{key.to_lower(), value}
	}

	// Encode headers
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

	// Read response
	return c.read_response(stream_id)!
}

// read_response reads the response for a stream
fn (mut c Client) read_response(stream_id u32) !SimpleResponse {
	mut stream := c.conn.streams[stream_id] or { return error('stream ${stream_id} not found') }

	for !stream.end_stream || !stream.end_headers {
		frame := c.conn.read_frame()!

		match frame.header.frame_type {
			.headers {
				if frame.header.stream_id != stream_id {
					continue
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
			.data {
				if frame.header.stream_id != stream_id {
					continue
				}

				stream.data << frame.payload

				if frame.header.has_flag(.end_stream) {
					stream.end_stream = true
					stream.state = .closed
				}
			}
			.settings {
				c.conn.read_settings()!
			}
			.ping {
				// Send PING ACK
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
			.goaway {
				return error('connection closed by server')
			}
			.rst_stream {
				if frame.header.stream_id == stream_id {
					return error('stream reset by server')
				}
			}
			else {
				// Ignore unknown frame types
			}
		}
	}

	// Build response
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

// close closes the HTTP/2 connection
pub fn (mut c Client) close() {
	if !c.conn.closed {
		// Send GOAWAY frame
		goaway := Frame{
			header:  FrameHeader{
				length:     8
				frame_type: .goaway
				flags:      0
				stream_id:  0
			}
			payload: [u8(0), 0, 0, 0, 0, 0, 0, 0] // Last stream ID = 0, No error
		}

		c.conn.write_frame(goaway) or {}
		c.conn.conn.close() or {}
		c.conn.closed = true
	}
}
