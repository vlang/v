module v3

// HTTP/3 frame types, request/response types, and shared structures (RFC 9114).

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
	max_field_section_size   u64 = 65536
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

// HeaderField represents a header name-value pair.
pub struct HeaderField {
pub:
	name  string
	value string
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

// frame_type_from_u64 converts a u64 to a FrameType, returning none for
// unknown types so callers can silently ignore them (RFC 9114 §7.2.8).
pub fn frame_type_from_u64(val u64) ?FrameType {
	return match val {
		0x0 { .data }
		0x1 { .headers }
		0x3 { .cancel_push }
		0x4 { .settings }
		0x5 { .push_promise }
		0x7 { .goaway }
		0xd { .max_push_id }
		else { none }
	}
}
