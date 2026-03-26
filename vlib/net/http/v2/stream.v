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

// can_send returns whether the given frame type can be sent in this stream state
// per RFC 7540 §5.1.
pub fn (s StreamState) can_send(frame_type FrameType) bool {
	return match s {
		.idle {
			frame_type in [.headers, .priority]
		}
		.open {
			frame_type in [.data, .headers, .rst_stream, .window_update, .priority]
		}
		.half_closed_local {
			frame_type in [.rst_stream, .window_update, .priority]
		}
		.half_closed_remote {
			frame_type in [.data, .headers, .rst_stream, .window_update, .priority]
		}
		.reserved_local {
			frame_type in [.headers, .rst_stream, .priority]
		}
		.reserved_remote {
			frame_type in [.rst_stream, .window_update, .priority]
		}
		.closed {
			frame_type == .priority
		}
	}
}

// can_recv returns whether the given frame type can be received in this stream state
// per RFC 7540 §5.1.
pub fn (s StreamState) can_recv(frame_type FrameType) bool {
	return match s {
		.idle {
			frame_type in [.headers, .priority]
		}
		.open {
			frame_type in [.data, .headers, .rst_stream, .window_update, .priority, .continuation]
		}
		.half_closed_local {
			frame_type in [.data, .headers, .rst_stream, .window_update, .priority, .continuation]
		}
		.half_closed_remote {
			frame_type in [.rst_stream, .window_update, .priority]
		}
		.reserved_local {
			frame_type in [.rst_stream, .window_update, .priority]
		}
		.reserved_remote {
			frame_type in [.headers, .rst_stream, .priority]
		}
		.closed {
			frame_type in [.rst_stream, .window_update, .priority]
		}
	}
}

// next_on_send returns the new stream state after sending a frame of the given type.
// The end_stream flag indicates whether END_STREAM was set on the frame.
pub fn (s StreamState) next_on_send(frame_type FrameType, end_stream bool) StreamState {
	if frame_type == .rst_stream {
		return .closed
	}
	return match s {
		.idle {
			if frame_type == .headers {
				if end_stream {
					StreamState.half_closed_local
				} else {
					StreamState.open
				}
			} else {
				s
			}
		}
		.open {
			if end_stream {
				StreamState.half_closed_local
			} else {
				s
			}
		}
		.half_closed_remote {
			if end_stream {
				StreamState.closed
			} else {
				s
			}
		}
		.reserved_local {
			if frame_type == .headers {
				StreamState.half_closed_remote
			} else {
				s
			}
		}
		else {
			s
		}
	}
}

// next_on_recv returns the new stream state after receiving a frame of the given type.
// The end_stream flag indicates whether END_STREAM was set on the frame.
pub fn (s StreamState) next_on_recv(frame_type FrameType, end_stream bool) StreamState {
	if frame_type == .rst_stream {
		return .closed
	}
	return match s {
		.idle {
			if frame_type == .headers {
				if end_stream {
					StreamState.half_closed_remote
				} else {
					StreamState.open
				}
			} else {
				s
			}
		}
		.open {
			if end_stream {
				StreamState.half_closed_remote
			} else {
				s
			}
		}
		.half_closed_local {
			if end_stream {
				StreamState.closed
			} else {
				s
			}
		}
		.reserved_remote {
			if frame_type == .headers {
				StreamState.half_closed_local
			} else {
				s
			}
		}
		else {
			s
		}
	}
}
