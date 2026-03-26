module v2

// Shared types for HTTP/2 client and server: Method, Request, Response, Settings, ClientConfig.
import time

// Method represents HTTP methods for HTTP/2 requests.
// Duplicated from net.http.Method because V does not allow circular imports —
// net.http imports net.http.v2, so net.http.v2 cannot import net.http.
pub enum Method {
	get
	post
	put
	patch
	delete
	head
	options
}

// str returns the string representation of the HTTP method.
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

// Request represents a simplified HTTP/2 request.
pub struct Request {
pub:
	method  Method
	url     string
	host    string
	data    string
	headers map[string]string
}

// Response represents a simplified HTTP/2 response.
pub struct Response {
pub:
	status_code int
	headers     map[string]string
	body        string
}

// Settings holds HTTP/2 connection settings per RFC 7540 Section 6.5.
pub struct Settings {
pub mut:
	header_table_size      u32  = 4096
	enable_push            bool = true
	max_concurrent_streams u32  = 100
	initial_window_size    u32  = 65535
	max_frame_size         u32  = 16384
	max_header_list_size   u32
}

// ClientConfig holds configuration options for the HTTP/2 client.
pub struct ClientConfig {
pub:
	response_timeout time.Duration
}
