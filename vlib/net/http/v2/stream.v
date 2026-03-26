// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module v2

// Stream represents an HTTP/2 stream with flow control
pub struct Stream {
pub mut:
	id          u32
	state       StreamState
	window_size i64 = 65535
	headers     []HeaderField
	data        []u8
	end_stream  bool
	end_headers bool
	// raw_header_block accumulates header block fragments when HEADERS arrives
	// without END_HEADERS; cleared once END_HEADERS is seen on a CONTINUATION frame.
	raw_header_block []u8
}

// StreamState represents HTTP/2 stream states per RFC 7540 Section 5.1
pub enum StreamState {
	idle
	reserved_local
	reserved_remote
	open
	half_closed_local
	half_closed_remote
	closed
}
