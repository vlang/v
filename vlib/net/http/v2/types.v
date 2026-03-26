// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

import time

// Method represents HTTP methods. Duplicated from net.http.Method because
// net.http imports net.http.v2, creating a circular dependency if v2 were
// to import net.http back. Only the subset of methods needed for HTTP/2
// client requests is included here.
pub enum Method {
	get
	post
	put
	patch
	delete
	head
	options
}

// str returns the string representation of the HTTP method
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

// Request represents a simplified HTTP/2 request
pub struct Request {
pub:
	method  Method
	url     string
	host    string
	data    string
	headers map[string]string
}

// Response represents a simplified HTTP/2 response
pub struct Response {
pub:
	status_code int
	headers     map[string]string
	body        string
}

// Settings holds HTTP/2 connection settings per RFC 7540 Section 6.5
pub struct Settings {
pub mut:
	header_table_size      u32  = 4096
	enable_push            bool = true
	max_concurrent_streams u32  = 100
	initial_window_size    u32  = 65535
	max_frame_size         u32  = 16384
	max_header_list_size   u32 // 0 = unlimited
}

// ClientConfig holds configuration options for the HTTP/2 client
pub struct ClientConfig {
pub:
	// response_timeout is the maximum time to wait for a complete response.
	// Defaults to 30 seconds when zero.
	response_timeout time.Duration
}
