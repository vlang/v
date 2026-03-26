module v2

// HTTP/2 stream state and lifecycle (RFC 7540 §5.1).

// Stream represents an HTTP/2 stream with flow control.
pub struct Stream {
pub mut:
	id                 u32
	state              StreamState
	window_size        i64 = 65535
	headers            []HeaderField
	data               []u8
	end_stream         bool
	end_headers        bool
	raw_header_block   []u8
	continuation_count int
}

// StreamState represents HTTP/2 stream states per RFC 7540 Section 5.1.
pub enum StreamState {
	idle
	reserved_local
	reserved_remote
	open
	half_closed_local
	half_closed_remote
	closed
}
