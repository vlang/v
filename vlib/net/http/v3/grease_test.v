module v3

// Tests for GREASE (RFC 8701) frame and stream type generation.

fn test_grease_frame_type_pattern() {
	// GREASE frame types follow 0x1f * N + 0x21
	assert grease_frame_type(0) == 0x21
	assert grease_frame_type(1) == 0x40
	assert grease_frame_type(2) == 0x5f
	assert grease_frame_type(3) == 0x7e
	assert grease_frame_type(7) == 0xfa
}

fn test_grease_stream_type_pattern() {
	// GREASE stream types follow the same pattern: 0x1f * N + 0x21
	assert grease_stream_type(0) == 0x21
	assert grease_stream_type(1) == 0x40
	assert grease_stream_type(5) == 0xbc
}

fn test_is_grease_valid() {
	// Known GREASE values must return true
	assert is_grease(0x21) == true
	assert is_grease(0x40) == true
	assert is_grease(0x5f) == true
	assert is_grease(0x7e) == true
	assert is_grease(0xfa) == true
}

fn test_is_grease_invalid() {
	// Non-GREASE values must return false
	assert is_grease(0x00) == false // DATA
	assert is_grease(0x01) == false // HEADERS
	assert is_grease(0x04) == false // SETTINGS
	assert is_grease(0x20) == false // just below first GREASE
	assert is_grease(0x22) == false // just above first GREASE
}

fn test_generate_grease_frame() {
	frame := generate_grease_frame()
	// Frame type must match the GREASE pattern
	ftype := u64(frame.frame_type)
	assert is_grease(ftype), 'frame type 0x${ftype:x} is not a GREASE value'
	// Payload length must be 0-16 bytes
	assert frame.payload.len <= 16
	// frame.length must match payload
	assert frame.length == u64(frame.payload.len)
}

fn test_generate_grease_stream_type() {
	stype := generate_grease_stream_type()
	assert is_grease(stype), 'stream type 0x${stype:x} is not a GREASE value'
}
