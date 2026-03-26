module v3

// GREASE (Generate Random Extensions And Sustain Extensibility) support
// for HTTP/3 per RFC 8701 and RFC 9114 §9.
//
// GREASE values follow the pattern 0x1f * N + 0x21 for both frame types
// and stream types. Compliant peers must silently ignore unknown types.
import rand

// grease_frame_type generates a GREASE frame type value for a given N.
// The pattern is 0x1f * N + 0x21 per RFC 8701.
pub fn grease_frame_type(n u64) u64 {
	return 0x1f * n + 0x21
}

// grease_stream_type generates a GREASE stream type value for a given N.
// The pattern is 0x1f * N + 0x21 per RFC 8701.
pub fn grease_stream_type(n u64) u64 {
	return 0x1f * n + 0x21
}

// is_grease returns true if the value matches the GREASE pattern
// (0x1f * N + 0x21 for some non-negative integer N).
pub fn is_grease(value u64) bool {
	return value >= 0x21 && (value - 0x21) % 0x1f == 0
}

// generate_grease_frame creates a Frame with a random GREASE type
// and random 0–16 byte payload. Compliant peers silently ignore it.
pub fn generate_grease_frame() Frame {
	n := u64(rand.intn(8) or { 0 })
	ft := grease_frame_type(n)
	payload_len := rand.intn(17) or { 0 }
	payload := rand.bytes(payload_len) or { []u8{} }
	return Frame{
		frame_type: unsafe { FrameType(ft) }
		length:     u64(payload.len)
		payload:    payload
	}
}

// generate_grease_stream_type returns a random GREASE stream type value.
pub fn generate_grease_stream_type() u64 {
	n := u64(rand.intn(8) or { 0 })
	return grease_stream_type(n)
}
