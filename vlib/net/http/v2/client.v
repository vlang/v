module v2

// HTTP/2 client with TLS, HPACK, and flow control.
import net
import net.ssl
import time

// Client represents an HTTP/2 client.
pub struct Client {
mut:
	conn   Connection
	config ClientConfig
}

// new_client creates a new HTTP/2 client with TLS + ALPN 'h2' negotiation.
pub fn new_client(address string) !Client {
	return new_client_with_config(address, ClientConfig{})
}

// new_client_with_config creates a new HTTP/2 client with custom configuration.
pub fn new_client_with_config(address string, config ClientConfig) !Client {
	host, port := net.split_address(address)!

	// V's net.ssl API does not expose the ALPN-selected protocol after handshake,
	// so we cannot verify the server actually chose 'h2'. If the server doesn't
	// support h2, the connection preface or SETTINGS exchange will fail instead.
	mut ssl_conn := ssl.new_ssl_conn(
		alpn_protocols: ['h2']
	)!
	ssl_conn.dial(host, port)!

	if config.response_timeout > 0 {
		ssl_conn.duration = config.response_timeout
	}

	ssl_conn.write_string(preface)!

	mut conn := Connection{
		ssl_conn: ssl_conn
		encoder:  new_encoder()
		decoder:  new_decoder()
		settings: Settings{
			enable_push: false
		}
	}

	conn.write_settings()!
	conn.read_settings()!

	return Client{
		conn:   conn
		config: config
	}
}

// request sends an HTTP/2 request and returns the response.
pub fn (mut c Client) request(req Request) !Response {
	enforce_max_concurrent_streams(&c.conn)!

	stream_id := c.conn.next_stream_id
	c.conn.next_stream_id += 2
	c.conn.last_stream_id = stream_id

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

fn (mut c Client) send_request_headers(req Request, stream_id u32, mut stream Stream) ! {
	mut headers := [
		HeaderField{':method', req.method.str()},
		HeaderField{':scheme', 'https'},
		HeaderField{':path', req.url},
		HeaderField{':authority', req.host},
	]
	filtered := filter_connection_specific_headers(req.headers)
	for key, value in filtered {
		headers << HeaderField{key.to_lower(), value}
	}

	split_headers := split_cookie_headers(headers)
	encoded_headers := c.conn.encoder.encode(split_headers)

	$if trace_http2 ? {
		eprintln('[HTTP/2] HPACK encoded ${split_headers.len} headers -> ${encoded_headers.len} bytes: ${encoded_headers.hex()}')
		for h in split_headers {
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
	end_stream := req.data.len == 0
	stream.state = stream.state.next_on_send(.headers, end_stream)
}

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
	stream.state = stream.state.next_on_send(.data, true)
}

fn (c Client) response_timeout_duration() time.Duration {
	if c.config.response_timeout == 0 {
		return 30 * time.second
	}
	return c.config.response_timeout
}

fn (mut c Client) read_response(stream_id u32) !Response {
	mut stream := c.conn.streams[stream_id] or { return error('stream ${stream_id} not found') }

	deadline := time.now().add(c.response_timeout_duration())

	for !stream.end_stream || !stream.end_headers {
		if time.now() > deadline {
			return error('read_response timeout after ${c.response_timeout_duration()}')
		}

		frame := c.conn.read_frame()!
		c.handle_response_frame(frame, mut stream, stream_id)!
	}

	resp := c.build_response(stream)
	c.conn.streams.delete(stream_id)
	return resp
}

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

// close closes the HTTP/2 connection gracefully by sending GOAWAY (RFC 7540 §6.8).
pub fn (mut c Client) close() {
	if c.conn.closed {
		return
	}

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
