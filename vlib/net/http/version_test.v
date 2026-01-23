// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// Test automatic HTTP version negotiation

fn test_version_negotiation() {
	// Test unknown version defaults to auto-negotiation
	req := new_request(.get, 'https://example.com', '')
	assert req.version == .unknown
}

fn test_explicit_version() {
	// Test explicit version setting
	mut req := new_request(.get, 'https://example.com', '')
	req.version = .v1_1
	assert req.version == .v1_1

	req.version = .v2_0
	assert req.version == .v2_0
}

fn test_alpn_proto() {
	// Test ALPN protocol identifiers
	assert Version.v1_1.alpn_proto() == 'http/1.1'
	assert Version.v2_0.alpn_proto() == 'h2'
	assert Version.v3_0.alpn_proto() == 'h3'
	assert Version.unknown.alpn_proto() == ''
}

fn test_version_from_alpn() {
	// Test ALPN to Version conversion
	assert version_from_alpn('h2') == .v2_0
	assert version_from_alpn('h3') == .v3_0
	assert version_from_alpn('http/1.1') == .v1_1
	assert version_from_alpn('unknown') == .unknown
}

fn test_version_str() {
	// Test version string representation
	assert Version.v1_1.str() == 'HTTP/1.1'
	assert Version.v2_0.str() == 'HTTP/2.0'
	assert Version.v3_0.str() == 'HTTP/3.0'
	assert Version.unknown.str() == 'unknown'
}

fn test_version_from_str() {
	// Test string to version conversion
	assert version_from_str('HTTP/1.1') == .v1_1
	assert version_from_str('HTTP/2.0') == .v2_0
	assert version_from_str('HTTP/2') == .v2_0
	assert version_from_str('HTTP/3.0') == .v3_0
	assert version_from_str('HTTP/3') == .v3_0
	assert version_from_str('invalid') == .unknown
}
