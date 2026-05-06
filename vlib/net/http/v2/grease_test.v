module v2

// Tests for HTTP/2 GREASE (RFC 8701) frame type generation.

fn test_grease_frame_type() {
	// HTTP/2 GREASE frame types follow 0x0b + 0x1f * N, fit in u8
	ft := grease_frame_type()
	// Must not match any known HTTP/2 frame type
	known := frame_type_from_byte(ft)
	assert known == none, 'GREASE type 0x${ft:02x} collides with known frame type'
	// Must follow the GREASE pattern: (ft - 0x0b) % 0x1f == 0
	assert (ft - 0x0b) % 0x1f == 0, 'type 0x${ft:02x} does not match GREASE pattern'
}

fn test_generate_grease_frame() {
	frame := generate_grease_frame()
	// Frame type must be unknown to HTTP/2
	known := frame_type_from_byte(u8(frame.header.frame_type))
	assert known == none, 'GREASE frame type should be unknown to HTTP/2'
	// Stream ID should be 0 (connection-level)
	assert frame.header.stream_id == 0
	// Payload length must match header
	assert frame.header.length == u32(frame.payload.len)
	// Payload 0-16 bytes
	assert frame.payload.len <= 16
}
