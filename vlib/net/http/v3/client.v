module v3

// HTTP/3 client (RFC 9114).
import net.quic

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
	control_reader             ControlStreamReader
	pool                       ?&ClientPool
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

	verify_alpn(&c.quic_conn.crypto_ctx) or {
		c.quic_conn.close()
		return err
	}

	c.setup_initial_streams()

	return c
}

// verify_alpn checks that the ALPN negotiated protocol is "h3" (RFC 9114 §3.3).
fn verify_alpn(crypto_ctx &quic.CryptoContext) ! {
	alpn := crypto_ctx.get_alpn_selected()
	if alpn != none {
		if alpn != 'h3' {
			return error('HTTP/3 ALPN mismatch: expected "h3", got "${alpn}"')
		}
	} else {
		$if debug {
			eprintln('warning: ALPN not available — cannot verify h3 negotiation')
		}
	}
}

// setup_initial_streams opens unidirectional streams and sends initial SETTINGS.
fn (mut c Client) setup_initial_streams() {
	c.uni.open_streams(mut c.quic_conn) or {
		$if debug {
			eprintln('warning: failed to open unidirectional streams: ${err}')
		}
	}

	c.send_settings() or {
		$if debug {
			eprintln('warning: failed to send initial SETTINGS: ${err}')
		}
	}
}

// start_control_reader spawns a background reader on the peer's control stream.
// Call this after setup_initial_streams to read the server's SETTINGS, GOAWAY,
// and other control frames. The reader applies peer settings to the QPACK encoder
// via apply_peer_settings and records GOAWAY stream IDs via apply_goaway.
pub fn (mut c Client) start_control_reader() {
	c.control_reader = new_control_reader()
	spawn read_peer_control_stream(mut c)
}

// request sends an HTTP/3 request and returns the response.
// After sending all frames (HEADERS + DATA), the QUIC layer signals
// end-of-stream (FIN) implicitly — ngtcp2 marks the stream write-side
// as complete when no more data is written before reading the response.
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
				validate_header_names_lowercase(headers)!
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

// close shuts down the HTTP/3 client and QUIC connection, sending
// H3_NO_ERROR (0x0100) as the application error code per RFC 9114 §5.2.
// If the client belongs to a connection pool, it releases back to the pool instead.
pub fn (mut c Client) close() {
	if mut pool := c.pool {
		pool.release(c.address)
		return
	}
	c.send_goaway(c.next_stream_id) or {}
	c.quic_conn.close_with_error(u64(H3ErrorCode.h3_no_error), '') or { c.quic_conn.close() }
}

// cancel_request cancels an in-flight HTTP/3 request by resetting its QUIC
// stream with H3_REQUEST_CANCELLED (RFC 9114 §4.1.1). The peer will receive
// a RESET_STREAM frame and should discard any partial response.
pub fn (mut c Client) cancel_request(stream_id u64) ! {
	c.quic_conn.reset_stream(stream_id, u64(H3ErrorCode.h3_request_cancelled))!
}

// send_settings sends an HTTP/3 SETTINGS frame with actual client
// settings (RFC 9114 §7.2.4) on the control stream.
pub fn (mut c Client) send_settings() ! {
	if c.uni.control_stream_id < 0 {
		return error('control stream not opened')
	}
	ctrl_id := u64(c.uni.control_stream_id)

	payload := build_settings_payload(c.settings)!

	mut data := []u8{}
	data << encode_varint(u64(FrameType.settings))!
	data << encode_varint(u64(payload.len))!
	data << payload

	c.quic_conn.send(ctrl_id, data)!
}

// send_goaway sends a GOAWAY frame on the control stream. The stream_id
// indicates the highest stream ID that might have been processed. Peers
// should use H3 error codes from H3ErrorCode when closing the connection.
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
