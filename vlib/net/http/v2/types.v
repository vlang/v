module v2

// Shared types for HTTP/2 client and server: Method, Request, Response, Settings, ClientConfig.
import time
import net.http.common

pub type Method = common.Method

// Request represents a simplified HTTP/2 client request.
pub struct Request {
pub:
	method  Method
	url     string
	host    string
	data    string
	header  common.Header
}

// Response represents a simplified HTTP/2 client response.
pub struct Response {
pub:
	status_code int
	header      common.Header
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
