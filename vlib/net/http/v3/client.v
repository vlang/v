// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v3

import net.quic

// HTTP/3 implementation (RFC 9114)
// HTTP/3 uses QUIC as the transport protocol

// FrameType represents HTTP/3 frame types
pub enum FrameType as u64 {
	data         = 0x0
	headers      = 0x1
	cancel_push  = 0x3
	settings     = 0x4
	push_promise = 0x5
	goaway       = 0x7
	max_push_id  = 0xd
}

// Frame represents an HTTP/3 frame
pub struct Frame {
pub mut:
	frame_type FrameType
	length     u64
	payload    []u8
}

// Settings holds HTTP/3 settings
pub struct Settings {
pub mut:
	max_field_section_size   u64
	qpack_max_table_capacity u64 = 4096
	qpack_blocked_streams    u64 = 100
}

// Method represents HTTP request methods.
// Note: this enum mirrors net.http.Method and exists independently to avoid
// circular imports between net.http and net.http.v3.
// TODO: Unify with net.http.Method once cross-module import constraints allow.
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

// Request represents a simplified HTTP request for v3
pub struct Request {
pub:
	method  Method
	url     string
	host    string
	data    string
	headers map[string]string
}

// Response represents a simplified HTTP response
pub struct Response {
pub:
	status_code int
	headers     map[string]string
	body        string
}

// Client represents an HTTP/3 client
pub struct Client {
mut:
	address           string
	quic_conn         quic.Connection
	settings          Settings
	next_stream_id    u64
	qpack_encoder     Encoder
	qpack_decoder     Decoder
	session_cache     quic.SessionCache
	zero_rtt_enabled  bool = true
	migration_enabled bool = true
}

// new_client creates a new HTTP/3 client and establishes a QUIC connection
pub fn new_client(address string) !Client {
	// Try to create QUIC connection
	quic_conn := quic.new_connection(
		remote_addr: address
		alpn:        ['h3']
		enable_0rtt: true
	) or {
		return error('HTTP/3 connection failed: ${err}

HTTP/3 requires QUIC protocol support, which needs:
1. QUIC library (ngtcp2, quiche, or msquic)
2. TLS 1.3 support
3. UDP socket handling

Current status: QUIC implementation is not complete.
The request will automatically fall back to HTTP/2 or HTTP/1.1.

To enable HTTP/3:
- Install QUIC library: brew install ngtcp2 (macOS)
- Implement QUIC C bindings in vlib/net/quic/
- Complete QUIC handshake and packet handling')
	}

	return Client{
		address:           address
		quic_conn:         quic_conn
		qpack_encoder:     new_qpack_encoder(4096, 100)
		qpack_decoder:     new_qpack_decoder(4096, 100)
		session_cache:     quic.new_session_cache()
		zero_rtt_enabled:  true
		migration_enabled: true
	}
}

// request sends an HTTP/3 request and returns the response from the server
pub fn (mut c Client) request(req Request) !Response {
	// Allocate stream ID (client-initiated bidirectional: 0, 4, 8, 12, ...)
	stream_id := c.next_stream_id
	c.next_stream_id += 4 // RFC 9114: client-initiated bidirectional streams increment by 4

	// Build headers with capacity
	mut headers := []HeaderField{cap: 4 + req.headers.len}
	headers << HeaderField{
		name:  ':method'
		value: req.method.str()
	}
	headers << HeaderField{
		name:  ':scheme'
		value: 'https'
	}
	headers << HeaderField{
		name:  ':path'
		value: req.url
	}
	headers << HeaderField{
		name:  ':authority'
		value: req.host
	}

	// Add custom headers
	for key, value in req.headers {
		headers << HeaderField{
			name:  key.to_lower()
			value: value
		}
	}

	// Encode headers using QPACK
	encoded_headers := c.qpack_encoder.encode(headers)

	// Send HEADERS frame
	headers_frame := Frame{
		frame_type: .headers
		length:     u64(encoded_headers.len)
		payload:    encoded_headers
	}

	c.send_frame(stream_id, headers_frame)!

	// Send DATA frame if there's a body
	if req.data.len > 0 {
		data_frame := Frame{
			frame_type: .data
			length:     u64(req.data.len)
			payload:    req.data.bytes()
		}

		c.send_frame(stream_id, data_frame)!
	}

	// Read response
	return c.read_response(stream_id)!
}

// send_frame sends an HTTP/3 frame on a stream
fn (mut c Client) send_frame(stream_id u64, frame Frame) ! {
	// Encode frame
	mut data := []u8{}

	// Variable-length integer encoding for type and length
	data << encode_varint(u64(frame.frame_type))!
	data << encode_varint(frame.length)!
	data << frame.payload

	// Send on QUIC stream
	c.quic_conn.send(stream_id, data)!
}

// read_response reads the HTTP/3 response
fn (mut c Client) read_response(stream_id u64) !Response {
	// Read frames from QUIC stream
	data := c.quic_conn.recv(stream_id)!

	// Parse frames
	mut idx := 0
	mut headers := []HeaderField{}
	mut body := []u8{}

	for idx < data.len {
		// Decode frame type
		frame_type_val, bytes_read := decode_varint(data[idx..])!
		idx += bytes_read

		// Decode frame length
		frame_length, bytes_read2 := decode_varint(data[idx..])!
		idx += bytes_read2

		// Read payload
		if idx + int(frame_length) > data.len {
			return error('incomplete frame')
		}

		payload := data[idx..idx + int(frame_length)]
		idx += int(frame_length)

		frame_type := frame_type_from_u64(frame_type_val) or {
			// Ignore unknown frame types per RFC 9114
			continue
		}

		match frame_type {
			.headers {
				// Use proper QPACK Decoder for consistency with encoder (Issue #17)
				headers = c.qpack_decoder.decode(payload)!
			}
			.data {
				body << payload
			}
			.goaway {
				// Server initiated graceful shutdown; stop processing
				break
			}
			else {
				// Ignore unknown frames per RFC 9114 §9
			}
		}
	}

	// Build response
	mut status_code := 200
	mut response_headers := map[string]string{}

	for header in headers {
		if header.name == ':status' {
			status_code = header.value.int()
		} else if !header.name.starts_with(':') {
			response_headers[header.name] = header.value
		}
	}

	return Response{
		body:        body.bytestr()
		status_code: status_code
		headers:     response_headers
	}
}

// close closes the HTTP/3 client and terminates the QUIC connection
pub fn (mut c Client) close() {
	// Send GOAWAY to signal graceful shutdown (best-effort; ignore errors)
	c.send_goaway(c.next_stream_id) or {}
	c.quic_conn.close()
}

// send_settings sends an HTTP/3 SETTINGS frame on the control stream (RFC 9114 §6.2.1).
// An empty SETTINGS frame is valid and means all settings use their default values.
// This must be called once after connection setup.
pub fn (mut c Client) send_settings() ! {
	// Control stream uses stream ID 2 (client-initiated unidirectional, per RFC 9114 §6.2)
	control_stream_id := u64(2)

	// Stream type byte for control stream (0x00)
	mut data := []u8{}
	data << encode_varint(u64(0x00))! // control stream type

	// SETTINGS frame: type=0x04, length=0 (empty body — all defaults)
	data << encode_varint(u64(FrameType.settings))!
	data << encode_varint(u64(0))! // length = 0

	c.quic_conn.send(control_stream_id, data)!
}

// send_goaway sends a GOAWAY frame on the control stream (RFC 9114 §7.2.6).
// stream_id is the highest request stream ID the server will process.
// Clients send GOAWAY to initiate graceful shutdown of the connection.
pub fn (mut c Client) send_goaway(stream_id u64) ! {
	control_stream_id := u64(2)
	payload := encode_varint(stream_id)!

	mut data := []u8{}
	data << encode_varint(u64(FrameType.goaway))!
	data << encode_varint(u64(payload.len))!
	data << payload

	c.quic_conn.send(control_stream_id, data)!
}

// HeaderField represents a name-value pair
pub struct HeaderField {
pub:
	name  string
	value string
}

// encode_headers encodes headers using QPACK
fn encode_headers(headers []HeaderField) []u8 {
	// Simplified QPACK encoding
	// In a real implementation, this would use dynamic/static tables
	mut result := []u8{}

	for header in headers {
		// Literal with name and value
		result << 0x20 // Literal without indexing
		result << encode_string(header.name) or { panic('encode_headers: ${err}') }
		result << encode_string(header.value) or { panic('encode_headers: ${err}') }
	}

	return result
}

// decode_headers decodes QPACK-encoded headers
fn decode_headers(data []u8) ![]HeaderField {
	mut headers := []HeaderField{}
	mut idx := 0

	for idx < data.len {
		// Read instruction byte
		if idx >= data.len {
			break
		}

		instruction := data[idx]
		idx++

		if (instruction & 0x20) != 0 {
			// Literal without indexing
			name, bytes_read := decode_string(data[idx..])!
			idx += bytes_read

			value, bytes_read2 := decode_string(data[idx..])!
			idx += bytes_read2

			headers << HeaderField{name, value}
		}
	}

	return headers
}

// frame_type_from_u64 validates and converts a u64 to a FrameType enum value.
// Returns an error for unrecognized frame type values.
pub fn frame_type_from_u64(val u64) !FrameType {
	return match val {
		0x0 { .data }
		0x1 { .headers }
		0x3 { .cancel_push }
		0x4 { .settings }
		0x5 { .push_promise }
		0x7 { .goaway }
		0xd { .max_push_id }
		else { error('unknown HTTP/3 frame type: 0x${val:x}') }
	}
}
