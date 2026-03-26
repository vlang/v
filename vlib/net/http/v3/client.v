module v3

// HTTP/3 client, types, and framing (RFC 9114).
import net.quic

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
	address           string
	quic_conn         quic.Connection
	settings          Settings
	next_stream_id    u64
	qpack_encoder     Encoder
	qpack_decoder     Decoder
	session_cache     quic.SessionCache
	zero_rtt_enabled  bool = true
	migration_enabled bool = true
	uni               UniStreamManager
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

// request sends an HTTP/3 request and returns the response.
pub fn (mut c Client) request(req Request) !Response {
	stream_id := c.next_stream_id
	c.next_stream_id += 4

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

	encoded_headers := c.qpack_encoder.encode(headers)

	headers_frame := Frame{
		frame_type: .headers
		length:     u64(encoded_headers.len)
		payload:    encoded_headers
	}

	c.send_frame(stream_id, headers_frame)!

	if req.data.len > 0 {
		data_frame := Frame{
			frame_type: .data
			length:     u64(req.data.len)
			payload:    req.data.bytes()
		}

		c.send_frame(stream_id, data_frame)!
	}

	return c.read_response(stream_id)!
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
				break
			}
			else {}
		}
	}

	return headers, body
}

// close shuts down the HTTP/3 client and QUIC connection.
pub fn (mut c Client) close() {
	c.send_goaway(c.next_stream_id) or {}
	c.quic_conn.close()
}

// send_settings sends an HTTP/3 SETTINGS frame on the control stream.
pub fn (mut c Client) send_settings() ! {
	ctrl_id := if c.uni.control_stream_id >= 0 {
		u64(c.uni.control_stream_id)
	} else {
		u64(2)
	}

	mut data := []u8{}
	data << encode_varint(u64(0x00))!
	data << encode_varint(u64(FrameType.settings))!
	data << encode_varint(u64(0))!

	c.quic_conn.send(ctrl_id, data)!
}

// send_goaway sends a GOAWAY frame on the control stream.
pub fn (mut c Client) send_goaway(stream_id u64) ! {
	ctrl_id := if c.uni.control_stream_id >= 0 {
		u64(c.uni.control_stream_id)
	} else {
		u64(2)
	}
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
