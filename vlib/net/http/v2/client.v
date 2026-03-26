// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

import net
import net.ssl
import time

// Client represents an HTTP/2 client
// TODO: buffer pooling can be added later for performance (see optimization.v)
pub struct Client {
mut:
	conn   Connection
	config ClientConfig
}

// new_client creates a new HTTP/2 client with TLS + ALPN 'h2' negotiation,
// connection preface, and settings exchange.
// The address should be in the form 'hostname:port' (e.g. 'example.com:443').
pub fn new_client(address string) !Client {
	return new_client_with_config(address, ClientConfig{})
}

// new_client_with_config creates a new HTTP/2 client with custom configuration.
// The address should be in the form 'hostname:port' (e.g. 'example.com:443').
pub fn new_client_with_config(address string, config ClientConfig) !Client {
	host, port := net.split_address(address)!

	// Create TLS connection with ALPN 'h2' for HTTP/2 negotiation (RFC 7540 Section 3.3).
	// NOTE: V's net.ssl API does not expose the ALPN-selected protocol after handshake,
	// so we cannot verify the server actually chose 'h2'. If the server doesn't support
	// h2, the connection preface or SETTINGS exchange will fail instead.
	// See request_version.v for the full ALPN limitation documentation.
	mut ssl_conn := ssl.new_ssl_conn(
		alpn_protocols: ['h2']
	)!
	ssl_conn.dial(host, port)!

	// Apply read timeout from config if provided, overriding the default 30s.
	// The SSLConn.duration field controls the deadline for each socket read.
	if config.response_timeout > 0 {
		ssl_conn.duration = config.response_timeout
	}

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

	return Client{
		conn:   conn
		config: config
	}
}

// request sends an HTTP/2 request and returns the response from the server
pub fn (mut c Client) request(req Request) !Response {
	// Allocate new stream ID (client uses odd stream IDs)
	stream_id := c.conn.next_stream_id
	c.conn.next_stream_id += 2

	// Track the highest stream ID for GOAWAY
	c.conn.last_stream_id = stream_id

	// Create and register stream
	mut stream := &Stream{
		id:    stream_id
		state: .idle
	}
	c.conn.streams[stream_id] = stream

	c.send_request_headers(req, stream_id, mut stream)!

	if req.data.len > 0 {
		c.send_data_frames(req.data, stream_id, mut stream)!
	}

	return c.read_response(stream_id)!
}

// send_request_headers encodes and sends the HEADERS frame for a request.
fn (mut c Client) send_request_headers(req Request, stream_id u32, mut stream Stream) ! {
	mut headers := [
		HeaderField{':method', req.method.str()},
		HeaderField{':scheme', 'https'},
		HeaderField{':path', req.url},
		HeaderField{':authority', req.host},
	]
	for key, value in req.headers {
		headers << HeaderField{key.to_lower(), value}
	}

	encoded_headers := c.conn.encoder.encode(headers)

	$if trace_http2 ? {
		eprintln('[HTTP/2] HPACK encoded ${headers.len} headers -> ${encoded_headers.len} bytes: ${encoded_headers.hex()}')
		for h in headers {
			eprintln('[HTTP/2]   ${h.name}: ${h.value}')
		}
	}

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
}

// send_data_frames sends DATA frames for the request body, respecting flow control (RFC 7540 §6.9).
fn (mut c Client) send_data_frames(data string, stream_id u32, mut stream Stream) ! {
	data_bytes := data.bytes()
	effective_window := if c.conn.remote_window_size < stream.window_size {
		c.conn.remote_window_size
	} else {
		stream.window_size
	}
	if effective_window <= 0 {
		return error('flow control window exhausted (connection=${c.conn.remote_window_size}, stream=${stream.window_size})')
	}
	chunks := split_data_for_window(data_bytes, effective_window, c.conn.remote_settings.max_frame_size)
	if chunks.len == 0 {
		return error('flow control window too small to send data')
	}
	for i, chunk in chunks {
		is_last := i == chunks.len - 1
		mut data_flags := u8(0)
		if is_last {
			data_flags |= u8(FrameFlags.end_stream)
		}
		data_frame := Frame{
			header:  FrameHeader{
				length:     u32(chunk.len)
				frame_type: .data
				flags:      data_flags
				stream_id:  stream_id
			}
			payload: chunk
		}
		c.conn.write_frame(data_frame)!
		c.conn.remote_window_size -= i64(chunk.len)
		stream.window_size -= i64(chunk.len)
	}
	stream.state = .half_closed_local
}

// response_timeout_duration returns the effective response timeout duration.
// Defaults to 30 seconds when not configured.
fn (c Client) response_timeout_duration() time.Duration {
	if c.config.response_timeout == 0 {
		return 30 * time.second
	}
	return c.config.response_timeout
}

// read_response reads and assembles the response for a specific stream.
// Returns an error if the response is not received within the configured timeout.
fn (mut c Client) read_response(stream_id u32) !Response {
	mut stream := c.conn.streams[stream_id] or { return error('stream ${stream_id} not found') }

	deadline := time.now().add(c.response_timeout_duration())

	for !stream.end_stream || !stream.end_headers {
		// Check timeout on each iteration
		if time.now() > deadline {
			return error('read_response timeout after ${c.response_timeout_duration()}')
		}

		frame := c.conn.read_frame()!
		c.handle_response_frame(frame, mut stream, stream_id)!
	}

	resp := c.build_response(stream)
	// Remove completed stream from map to prevent unbounded growth (Issue #10)
	c.conn.streams.delete(stream_id)
	return resp
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

// close closes the HTTP/2 connection gracefully.
// Sends a GOAWAY frame and shuts down the TLS connection (RFC 7540 §6.8).
pub fn (mut c Client) close() {
	if c.conn.closed {
		return
	}

	// Send GOAWAY and close the TLS connection
	last_id := c.conn.last_stream_id
	goaway := Frame{
		header:  FrameHeader{
			length:     8
			frame_type: .goaway
			flags:      0
			stream_id:  0
		}
		payload: [
			u8((last_id >> 24) & 0x7f),
			u8(last_id >> 16),
			u8(last_id >> 8),
			u8(last_id),
			// error code = 0 (NO_ERROR)
			u8(0),
			u8(0),
			u8(0),
			u8(0),
		]
	}

	c.conn.write_frame(goaway) or {}
	c.conn.ssl_conn.shutdown() or {}
	c.conn.closed = true
}
