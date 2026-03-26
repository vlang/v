module v3

// HTTP/3 CONNECT method tunneling (RFC 9114 §4.4).
// CONNECT requests use only :method and :authority pseudo-headers.
// Data on the QUIC stream forms a bidirectional byte tunnel.
import net.quic

// ConnectRequest represents an HTTP/3 CONNECT tunnel request.
// Only :method and :authority pseudo-headers are sent per RFC 9114 §4.4.
pub struct ConnectRequest {
pub:
	authority string            // host:port of the target
	headers   map[string]string // additional headers
}

// ConnectTunnel represents a bidirectional tunnel over an HTTP/3 QUIC stream.
pub struct ConnectTunnel {
mut:
	quic_conn &quic.Connection = unsafe { nil }
	stream_id u64
	open      bool
}

// build_connect_headers builds the pseudo-headers for a CONNECT request.
// Per RFC 9114 §4.4, only :method=CONNECT and :authority are included.
// No :scheme or :path pseudo-headers are present.
pub fn build_connect_headers(req ConnectRequest) []HeaderField {
	mut headers := []HeaderField{cap: 2 + req.headers.len}
	headers << HeaderField{':method', 'CONNECT'}
	headers << HeaderField{':authority', req.authority}
	for key, value in req.headers {
		lower := key.to_lower()
		if lower in h3_forbidden_headers {
			continue
		}
		headers << HeaderField{lower, value}
	}
	return headers
}

// connect sends a CONNECT request and returns a bidirectional tunnel.
// The tunnel allows sending and receiving raw data on the QUIC stream.
pub fn (mut c Client) connect(req ConnectRequest) !ConnectTunnel {
	stream_id := c.next_stream_id
	c.next_stream_id += 4

	if c.last_peer_goaway_stream_id > 0 && stream_id > c.last_peer_goaway_stream_id {
		return error('connection going away, no new streams')
	}

	headers := build_connect_headers(req)
	encoded := c.qpack_encoder.encode(headers)
	c.flush_encoder_instructions()

	headers_frame := Frame{
		frame_type: .headers
		length:     u64(encoded.len)
		payload:    encoded
	}
	c.send_frame(stream_id, headers_frame)!

	resp := c.read_response(stream_id)!
	if resp.status_code < 200 || resp.status_code >= 300 {
		return error('CONNECT rejected with status ${resp.status_code}')
	}

	return ConnectTunnel{
		quic_conn: &c.quic_conn
		stream_id: stream_id
		open:      true
	}
}

// send sends raw data through the CONNECT tunnel on the QUIC stream.
pub fn (mut t ConnectTunnel) send(data []u8) ! {
	if !t.open {
		return error('tunnel is closed')
	}
	mut frame_data := []u8{}
	frame_data << encode_varint(u64(FrameType.data))!
	frame_data << encode_varint(u64(data.len))!
	frame_data << data
	t.quic_conn.send(t.stream_id, frame_data)!
}

// recv reads raw data from the CONNECT tunnel on the QUIC stream.
pub fn (mut t ConnectTunnel) recv() ![]u8 {
	if !t.open {
		return error('tunnel is closed')
	}
	raw := t.quic_conn.recv(t.stream_id)!
	if raw.len == 0 {
		return error('empty response on tunnel stream')
	}
	frame_type_val, bytes_read := decode_varint(raw)!
	frame_length, bytes_read2 := decode_varint(raw[bytes_read..])!
	start := bytes_read + bytes_read2
	if start + int(frame_length) > raw.len {
		return error('incomplete frame on tunnel stream')
	}
	ft := frame_type_from_u64(frame_type_val) or {
		return error('unknown frame type on tunnel stream')
	}
	if ft != .data {
		return error('unexpected frame type on tunnel stream')
	}
	return raw[start..start + int(frame_length)]
}

// close closes the CONNECT tunnel by resetting the QUIC stream.
pub fn (mut t ConnectTunnel) close() ! {
	if !t.open {
		return
	}
	t.quic_conn.reset_stream(t.stream_id, u64(H3ErrorCode.h3_no_error))!
	t.open = false
}
