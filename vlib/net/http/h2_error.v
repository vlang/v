// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

// H2ErrorCode is an HTTP/2 error code, as used in RST_STREAM and GOAWAY
// frames (RFC 7540 Section 7).
pub enum H2ErrorCode as u32 {
	no_error            = 0x0
	protocol_error      = 0x1
	internal_error      = 0x2
	flow_control_error  = 0x3
	settings_timeout    = 0x4
	stream_closed       = 0x5
	frame_size_error    = 0x6
	refused_stream      = 0x7
	cancel              = 0x8
	compression_error   = 0x9
	connect_error       = 0xa
	enhance_your_calm   = 0xb
	inadequate_security = 0xc
	http_1_1_required   = 0xd
}

// str returns the RFC name of the error code.
pub fn (e H2ErrorCode) str() string {
	return match e {
		.no_error { 'NO_ERROR' }
		.protocol_error { 'PROTOCOL_ERROR' }
		.internal_error { 'INTERNAL_ERROR' }
		.flow_control_error { 'FLOW_CONTROL_ERROR' }
		.settings_timeout { 'SETTINGS_TIMEOUT' }
		.stream_closed { 'STREAM_CLOSED' }
		.frame_size_error { 'FRAME_SIZE_ERROR' }
		.refused_stream { 'REFUSED_STREAM' }
		.cancel { 'CANCEL' }
		.compression_error { 'COMPRESSION_ERROR' }
		.connect_error { 'CONNECT_ERROR' }
		.enhance_your_calm { 'ENHANCE_YOUR_CALM' }
		.inadequate_security { 'INADEQUATE_SECURITY' }
		.http_1_1_required { 'HTTP_1_1_REQUIRED' }
	}
}
