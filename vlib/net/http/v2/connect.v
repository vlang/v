module v2

// HTTP/2 CONNECT method tunneling (RFC 7540 §8.3).
// CONNECT requests use only :method and :authority pseudo-headers.
// Data frames on the tunnel stream form a bidirectional byte tunnel.

// ConnectRequest represents an HTTP/2 CONNECT tunnel request.
// Only :method and :authority pseudo-headers are sent per RFC 7540 §8.3.
pub struct ConnectRequest {
pub:
	authority string            // host:port of the target
	headers   map[string]string // additional headers
}

// ConnectTunnel represents a bidirectional tunnel over an HTTP/2 stream.
pub struct ConnectTunnel {
mut:
	conn      &Connection = unsafe { nil }
	stream_id u32
	open      bool
}

// build_connect_headers builds the pseudo-headers for a CONNECT request.
// Per RFC 7540 §8.3, only :method=CONNECT and :authority are included.
// No :scheme or :path pseudo-headers are present.
pub fn build_connect_headers(req ConnectRequest) []HeaderField {
	mut headers := []HeaderField{cap: 2 + req.headers.len}
	headers << HeaderField{
		name:  ':method'
		value: 'CONNECT'
	}
	headers << HeaderField{
		name:  ':authority'
		value: req.authority
	}
	filtered := filter_connection_specific_headers(req.headers)
	for key, value in filtered {
		headers << HeaderField{
			name:  key.to_lower()
			value: value
		}
	}
	return headers
}

// connect sends a CONNECT request and returns a bidirectional tunnel.
// The tunnel allows sending and receiving raw DATA frames on the stream.
pub fn (mut c Client) connect(req ConnectRequest) !ConnectTunnel {
	enforce_max_concurrent_streams(&c.conn)!
	stream_id := c.conn.next_stream_id
	c.conn.next_stream_id += 2
	c.conn.last_stream_id = stream_id

	mut stream := &Stream{
		id:    stream_id
		state: .idle
	}
	c.conn.streams[stream_id] = stream

	headers := build_connect_headers(req)
	encoded := c.conn.encoder.encode(headers)

	frame := Frame{
		header:  FrameHeader{
			length:     u32(encoded.len)
			frame_type: .headers
			flags:      u8(FrameFlags.end_headers)
			stream_id:  stream_id
		}
		payload: encoded
	}
	c.conn.write_frame(frame)!
	stream.state = stream.state.next_on_send(.headers, false)

	resp_frame := c.conn.read_frame()!
	c.handle_response_frame(resp_frame, mut stream, stream_id)!

	status := c.extract_connect_status(stream)
	if status < 200 || status >= 300 {
		return error('CONNECT rejected with status ${status}')
	}

	return ConnectTunnel{
		conn:      &c.conn
		stream_id: stream_id
		open:      true
	}
}

// extract_connect_status reads the :status pseudo-header from stream headers.
fn (c &Client) extract_connect_status(stream &Stream) int {
	for header in stream.headers {
		if header.name == ':status' {
			return header.value.int()
		}
	}
	return 0
}

// send sends raw data through the CONNECT tunnel as a DATA frame.
pub fn (mut t ConnectTunnel) send(data []u8) ! {
	if !t.open {
		return error('tunnel is closed')
	}
	frame := Frame{
		header:  FrameHeader{
			length:     u32(data.len)
			frame_type: .data
			flags:      0
			stream_id:  t.stream_id
		}
		payload: data
	}
	t.conn.write_frame(frame)!
}

// recv reads raw data from the CONNECT tunnel as a DATA frame.
pub fn (mut t ConnectTunnel) recv() ![]u8 {
	if !t.open {
		return error('tunnel is closed')
	}
	frame := t.conn.read_frame()!
	if frame.header.frame_type == .data && frame.header.stream_id == t.stream_id {
		return frame.payload
	}
	return error('unexpected frame type on tunnel stream')
}

// close closes the CONNECT tunnel by sending END_STREAM.
pub fn (mut t ConnectTunnel) close() ! {
	if !t.open {
		return
	}
	end_frame := Frame{
		header:  FrameHeader{
			length:     0
			frame_type: .data
			flags:      u8(FrameFlags.end_stream)
			stream_id:  t.stream_id
		}
		payload: []u8{}
	}
	t.conn.write_frame(end_frame)!
	t.open = false
}
