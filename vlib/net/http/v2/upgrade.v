module v2

// h2c Upgrade from HTTP/1.1 per RFC 7540 §3.2.
// Supports detecting HTTP/1.1 upgrade requests, building upgrade requests,
// and switching to HTTP/2 after the 101 Switching Protocols response.
import encoding.base64

// http_methods lists the HTTP methods that can appear in an upgrade request.
const http_methods = ['GET', 'POST', 'PUT', 'DELETE', 'PATCH', 'HEAD', 'OPTIONS']

// response_101 is the fixed HTTP/1.1 101 Switching Protocols response for h2c upgrade.
const response_101 = 'HTTP/1.1 101 Switching Protocols\r\nConnection: Upgrade\r\nUpgrade: h2c\r\n\r\n'

// UpgradeRequest holds the parsed fields from an HTTP/1.1 h2c upgrade request.
pub struct UpgradeRequest {
pub:
	method      string
	path        string
	headers     map[string]string
	h2_settings []u8
}

// detect_h2c_upgrade parses HTTP/1.1 request bytes and detects an h2c upgrade.
// Returns the parsed UpgradeRequest if the request contains valid `Upgrade: h2c`,
// `Connection: Upgrade, HTTP2-Settings`, and `HTTP2-Settings` headers.
// Returns none if the request is not an h2c upgrade.
pub fn detect_h2c_upgrade(data []u8) ?UpgradeRequest {
	text := data.bytestr()
	header_end := text.index('\r\n\r\n') or { return none }
	header_section := text[..header_end]
	lines := header_section.split('\r\n')
	if lines.len < 1 {
		return none
	}

	method, path := parse_request_line(lines[0]) or { return none }
	headers := parse_upgrade_headers(lines[1..])

	if !has_h2c_upgrade(headers) {
		return none
	}

	settings_value := headers['http2-settings'] or { return none }
	if settings_value == '' {
		return none
	}

	decoded_settings := base64.url_decode(settings_value)
	return UpgradeRequest{
		method:      method
		path:        path
		headers:     headers
		h2_settings: decoded_settings
	}
}

// send_101_response writes the HTTP/1.1 101 Switching Protocols response
// to the connection, completing the h2c upgrade handshake.
pub fn send_101_response(mut conn ServerConn) ! {
	conn.write(response_101.bytes()) or { return error('failed to send 101 response: ${err}') }
}

// apply_upgrade_settings parses the HTTP2-Settings payload (same format as
// a SETTINGS frame payload) and returns a Settings struct with decoded values.
pub fn apply_upgrade_settings(payload []u8) !Settings {
	mut settings := Settings{}
	if payload.len == 0 {
		return settings
	}
	pairs := parse_settings_payload(payload)!
	for pair in pairs {
		validate_setting_value(pair.id, pair.value)!
		apply_setting_to_settings(pair, mut settings)
	}
	return settings
}

// build_upgrade_request builds an HTTP/1.1 request string with h2c upgrade headers.
// The settings are base64url-encoded into the HTTP2-Settings header value.
pub fn build_upgrade_request(method string, path string, host string, settings Settings) string {
	payload := encode_settings_payload(settings)
	encoded := base64.url_encode(payload)
	return '${method} ${path} HTTP/1.1\r\n' + 'Host: ${host}\r\n' +
		'Connection: Upgrade, HTTP2-Settings\r\n' + 'Upgrade: h2c\r\n' +
		'HTTP2-Settings: ${encoded}\r\n' + '\r\n'
}

// encode_settings_payload encodes a Settings struct into the binary format
// used by SETTINGS frames (6 bytes per setting: 2-byte id + 4-byte value).
pub fn encode_settings_payload(s Settings) []u8 {
	mut payload := []u8{cap: 36}
	append_setting(mut payload, .header_table_size, s.header_table_size)
	append_setting(mut payload, .enable_push, if s.enable_push { u32(1) } else { u32(0) })
	append_setting(mut payload, .max_concurrent_streams, s.max_concurrent_streams)
	append_setting(mut payload, .initial_window_size, s.initial_window_size)
	append_setting(mut payload, .max_frame_size, s.max_frame_size)
	if s.max_header_list_size > 0 {
		append_setting(mut payload, .max_header_list_size, s.max_header_list_size)
	}
	return payload
}

// is_http1_request checks if data starts with a known HTTP/1.1 method,
// indicating it may be an HTTP/1.1 upgrade request rather than an HTTP/2 preface.
pub fn is_http1_request(data []u8) bool {
	text := data.bytestr()
	for m in http_methods {
		if text.starts_with(m) {
			return true
		}
	}
	return false
}

fn parse_request_line(line string) ?(string, string) {
	parts := line.split(' ')
	if parts.len < 3 {
		return none
	}
	return parts[0], parts[1]
}

fn parse_upgrade_headers(lines []string) map[string]string {
	mut headers := map[string]string{}
	for line in lines {
		colon := line.index(':') or { continue }
		key := line[..colon].trim_space().to_lower()
		value := line[colon + 1..].trim_space()
		headers[key] = value
	}
	return headers
}

fn has_h2c_upgrade(headers map[string]string) bool {
	upgrade := headers['upgrade'] or { return false }
	if upgrade.to_lower() != 'h2c' {
		return false
	}
	connection := headers['connection'] or { return false }
	lower_conn := connection.to_lower()
	return lower_conn.contains('upgrade') && lower_conn.contains('http2-settings')
}

fn apply_setting_to_settings(pair SettingPair, mut settings Settings) {
	match pair.id {
		.header_table_size { settings.header_table_size = pair.value }
		.enable_push { settings.enable_push = pair.value != 0 }
		.max_concurrent_streams { settings.max_concurrent_streams = pair.value }
		.initial_window_size { settings.initial_window_size = pair.value }
		.max_frame_size { settings.max_frame_size = pair.value }
		.max_header_list_size { settings.max_header_list_size = pair.value }
	}
}

// read_http1_headers reads remaining HTTP/1.1 header bytes from the connection,
// appending to initial_data until the header terminator `\r\n\r\n` is found.
// Returns the complete header data including initial_data.
pub fn read_http1_headers(mut conn ServerConn, initial_data []u8) ![]u8 {
	mut data := initial_data.clone()
	mut buf := []u8{len: 512}
	max_header_size := 8192

	for data.len < max_header_size {
		if data.bytestr().contains('\r\n\r\n') {
			return data
		}
		n := conn.read(mut buf) or { return error('reading HTTP/1.1 headers: ${err}') }
		if n == 0 {
			return error('connection closed while reading HTTP/1.1 headers')
		}
		data << buf[..n]
	}
	return error('HTTP/1.1 headers exceed maximum size')
}

fn append_setting(mut payload []u8, id SettingId, value u32) {
	payload << u8(u16(id) >> 8)
	payload << u8(u16(id))
	payload << u8(value >> 24)
	payload << u8(value >> 16)
	payload << u8(value >> 8)
	payload << u8(value)
}

// negotiate_protocol determines whether the connection uses HTTP/2 directly
// (prior knowledge) or starts with an HTTP/1.1 h2c upgrade request (RFC 7540 §3.2).
// For TLS connections, always expects the HTTP/2 preface directly.
// Returns a ServerRequest with stream_id > 0 if an upgrade was detected,
// or stream_id 0 if the connection uses HTTP/2 prior knowledge.
fn (mut s Server) negotiate_protocol(mut conn ServerConn) !ServerRequest {
	if s.tls {
		s.read_preface(mut conn)!
		return ServerRequest{}
	}

	mut initial_buf := []u8{len: preface.len}
	read_exact(mut conn, mut initial_buf, preface.len) or {
		return error('failed to read initial bytes: ${err}')
	}

	if initial_buf.bytestr() == preface {
		$if debug {
			eprintln('[HTTP/2] Preface received (prior knowledge)')
		}
		return ServerRequest{}
	}

	if !is_http1_request(initial_buf) {
		return error('invalid connection preface')
	}

	return s.perform_h2c_upgrade(mut conn, initial_buf)
}

// perform_h2c_upgrade completes the h2c upgrade handshake after detecting an
// HTTP/1.1 request. Sends the 101 response, reads the client's HTTP/2 preface,
// and returns the original request as stream 1.
fn (mut s Server) perform_h2c_upgrade(mut conn ServerConn, initial_buf []u8) !ServerRequest {
	full_data := read_http1_headers(mut conn, initial_buf)!
	upgrade_req := detect_h2c_upgrade(full_data) or {
		return error('HTTP/1.1 request without h2c upgrade')
	}

	$if debug {
		eprintln('[HTTP/2] h2c upgrade detected: ${upgrade_req.method} ${upgrade_req.path}')
	}

	apply_upgrade_settings(upgrade_req.h2_settings) or {
		return error('invalid HTTP2-Settings: ${err}')
	}

	send_101_response(mut conn) or { return error('failed to send 101: ${err}') }
	s.read_preface(mut conn)!

	return ServerRequest{
		method:    upgrade_req.method
		path:      upgrade_req.path
		headers:   upgrade_req.headers
		body:      []u8{}
		stream_id: 1
	}
}
