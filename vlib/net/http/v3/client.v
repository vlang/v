module v3

// HTTP/3 client, types, and framing (RFC 9114).
import net.quic

// max_data_frame_size is the maximum payload size for a single DATA frame (16KB),
// matching the HTTP/2 default. Bodies larger than this are split into multiple frames
// to respect QUIC stream flow control limits (RFC 9114 §4.1).
pub const max_data_frame_size = 16384

// FrameType represents HTTP/3 frame types.
pub enum FrameType as u64 {
	data         = 0x0
	headers      = 0x1
	cancel_push  = 0x3
	settings     = 0x4
	push_promise = 0x5
	goaway       = 0x7
	max_push_id  = 0xd
}

// Frame represents an HTTP/3 frame.
pub struct Frame {
pub mut:
	frame_type FrameType
	length     u64
	payload    []u8
}

// Settings holds HTTP/3 settings.
pub struct Settings {
pub mut:
	max_field_section_size   u64
	qpack_max_table_capacity u64 = 4096
	qpack_blocked_streams    u64 = 100
}

// Method represents HTTP methods for HTTP/3 requests.
// This enum is duplicated from net.http.Method because V does not allow
// circular imports — net.http imports net.http.v3, so net.http.v3 cannot
// import net.http. A future solution could use a shared types module
// (e.g., net.http.common) imported by both.
pub enum Method {
	get
	post
	put
	patch
	delete
	head
	options
}

// str returns the HTTP method as a string.
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

// Request represents an HTTP/3 request.
pub struct Request {
pub:
	method  Method
	url     string
	host    string
	data    string
	headers map[string]string
}

// Response represents an HTTP/3 response.
pub struct Response {
pub:
	status_code int
	headers     map[string]string
	body        string
}

// Client represents an HTTP/3 client.
pub struct Client {
mut:
	address                    string
	quic_conn                  quic.Connection
	settings                   Settings
	next_stream_id             u64
	qpack_encoder              Encoder
	qpack_decoder              Decoder
	session_cache              quic.SessionCache
	zero_rtt_enabled           bool = true
	migration_enabled          bool = true
	uni                        UniStreamManager
	last_peer_goaway_stream_id u64
}

// new_client creates an HTTP/3 client and establishes a QUIC connection.
pub fn new_client(address string) !Client {
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

	mut c := Client{
		address:           address
		quic_conn:         quic_conn
		qpack_encoder:     new_qpack_encoder(4096, 100)
		qpack_decoder:     new_qpack_decoder(4096, 100)
		session_cache:     quic.new_session_cache()
		zero_rtt_enabled:  true
		migration_enabled: true
	}

	c.uni.open_streams(mut c.quic_conn) or {
		$if debug {
			eprintln('warning: failed to open unidirectional streams: ${err}')
		}
	}

	// Send initial SETTINGS on the control stream
	c.send_settings() or {
		$if debug {
			eprintln('warning: failed to send initial SETTINGS: ${err}')
		}
	}

	return c
}

// request sends an HTTP/3 request and returns the response.
pub fn (mut c Client) request(req Request) !Response {
	stream_id := c.next_stream_id
	c.next_stream_id += 4

	if c.last_peer_goaway_stream_id > 0 && stream_id > c.last_peer_goaway_stream_id {
		return error('connection going away, no new streams')
	}

	encoded_headers := c.encode_request_headers(req)
	c.flush_encoder_instructions()

	headers_frame := Frame{
		frame_type: .headers
		length:     u64(encoded_headers.len)
		payload:    encoded_headers
	}

	c.send_frame(stream_id, headers_frame)!

	data_frames := create_data_frames(req.data)
	for frame in data_frames {
		c.send_frame(stream_id, frame)!
	}

	return c.read_response(stream_id)!
}

// encode_request_headers builds and QPACK-encodes headers for the given request.
fn (mut c Client) encode_request_headers(req Request) []u8 {
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

	for key, value in req.headers {
		headers << HeaderField{
			name:  key.to_lower()
			value: value
		}
	}

	return c.qpack_encoder.encode(headers)
}

// flush_encoder_instructions sends any pending QPACK encoder instructions on the encoder stream.
fn (mut c Client) flush_encoder_instructions() {
	instructions := c.qpack_encoder.pending_instructions()
	if instructions.len > 0 && c.uni.encoder_stream_id >= 0 {
		c.quic_conn.send(u64(c.uni.encoder_stream_id), instructions) or {}
	}
}

fn (mut c Client) send_frame(stream_id u64, frame Frame) ! {
	mut data := []u8{}

	data << encode_varint(u64(frame.frame_type))!
	data << encode_varint(frame.length)!
	data << frame.payload

	c.quic_conn.send(stream_id, data)!
}

fn (mut c Client) read_response(stream_id u64) !Response {
	data := c.quic_conn.recv(stream_id)!
	headers, body := c.parse_response_frames(data)!

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

fn (mut c Client) parse_response_frames(data []u8) !([]HeaderField, []u8) {
	mut idx := 0
	mut headers := []HeaderField{}
	mut body := []u8{}

	for idx < data.len {
		frame_type_val, bytes_read := decode_varint(data[idx..])!
		idx += bytes_read

		frame_length, bytes_read2 := decode_varint(data[idx..])!
		idx += bytes_read2

		if idx + int(frame_length) > data.len {
			return error('incomplete frame')
		}

		payload := data[idx..idx + int(frame_length)]
		idx += int(frame_length)

		frame_type := frame_type_from_u64(frame_type_val) or { continue }

		match frame_type {
			.headers {
				headers = c.qpack_decoder.decode(payload)!
			}
			.data {
				body << payload
			}
			.goaway {
				if payload.len > 0 {
					goaway_id, _ := decode_varint(payload)!
					c.last_peer_goaway_stream_id = goaway_id
				}
				break
			}
			else {}
		}
	}

	return headers, body
}

// create_data_frames splits a request body into DATA frames respecting max_data_frame_size.
// Returns an empty-payload DATA frame when the body is empty to signal stream end.
fn create_data_frames(data string) []Frame {
	if data.len == 0 {
		return [
			Frame{
				frame_type: .data
				length:     0
				payload:    []u8{}
			},
		]
	}
	mut frames := []Frame{cap: data.len / max_data_frame_size + 1}
	mut offset := 0
	for offset < data.len {
		end := if offset + max_data_frame_size > data.len {
			data.len
		} else {
			offset + max_data_frame_size
		}
		chunk := data[offset..end].bytes()
		frames << Frame{
			frame_type: .data
			length:     u64(chunk.len)
			payload:    chunk
		}
		offset = end
	}
	return frames
}

// close shuts down the HTTP/3 client and QUIC connection.
pub fn (mut c Client) close() {
	c.send_goaway(c.next_stream_id) or {}
	c.quic_conn.close()
}

// send_settings sends an HTTP/3 SETTINGS frame on the control stream.
pub fn (mut c Client) send_settings() ! {
	if c.uni.control_stream_id < 0 {
		return error('control stream not opened')
	}
	ctrl_id := u64(c.uni.control_stream_id)

	mut data := []u8{}
	data << encode_varint(u64(FrameType.settings))!
	data << encode_varint(u64(0))!

	c.quic_conn.send(ctrl_id, data)!
}

// send_goaway sends a GOAWAY frame on the control stream.
pub fn (mut c Client) send_goaway(stream_id u64) ! {
	if c.uni.control_stream_id < 0 {
		return error('control stream not opened')
	}
	ctrl_id := u64(c.uni.control_stream_id)

	payload := encode_varint(stream_id)!

	mut data := []u8{}
	data << encode_varint(u64(FrameType.goaway))!
	data << encode_varint(u64(payload.len))!
	data << payload

	c.quic_conn.send(ctrl_id, data)!
}

// HeaderField represents a header name-value pair.
pub struct HeaderField {
pub:
	name  string
	value string
}

// frame_type_from_u64 converts a u64 to a FrameType.
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
