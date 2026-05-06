module common

// Shared types for unified HTTP/1.1, HTTP/2, and HTTP/3 server handling.
// Both net.http and net.http.v2/v3 import this module to break circular deps.

// ServerRequest represents an incoming HTTP request in the server handler.
// Used across all HTTP versions (1.1, 2, 3).
pub struct ServerRequest {
pub:
	method    Method
	path      string
	host      string
	header    Header
	body      []u8
	version   Version
	stream_id u64 // 0 for HTTP/1.1
pub mut:
	cookies map[string]string
}

// ServerResponse represents an outgoing HTTP response from the server handler.
// Used across all HTTP versions (1.1, 2, 3).
pub struct ServerResponse {
pub mut:
	status_code int = 200
	header      Header
	body        []u8
}

// body_text returns the request body as a string for text-based handlers.
pub fn (r ServerRequest) body_text() string {
	return r.body.bytestr()
}

// body_text returns the response body as a string for debugging and tests.
pub fn (r ServerResponse) body_text() string {
	return r.body.bytestr()
}

