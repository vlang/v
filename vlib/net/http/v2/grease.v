module v2

// GREASE (Generate Random Extensions And Sustain Extensibility) support
// for HTTP/2 per RFC 8701.
//
// HTTP/2 frame types are 8-bit. GREASE uses the pattern 0x0b + 0x1f * N
// to produce unknown frame types that compliant peers must ignore.
import rand

// grease_frame_type generates a GREASE frame type for HTTP/2.
// The pattern 0x0b + 0x1f * N produces values that do not collide
// with any defined HTTP/2 frame type (RFC 7540 §6).
pub fn grease_frame_type() u8 {
	n := rand.intn(8) or { 0 }
	return u8(0x0b + 0x1f * n)
}

// generate_grease_frame creates an HTTP/2 Frame with a random GREASE
// type and random 0–16 byte payload on stream 0.
pub fn generate_grease_frame() Frame {
	ft := grease_frame_type()
	payload_len := rand.intn(17) or { 0 }
	payload := rand.bytes(payload_len) or { []u8{} }
	return Frame{
		header:  FrameHeader{
			length:     u32(payload.len)
			frame_type: unsafe { FrameType(ft) }
			flags:      0
			stream_id:  0
		}
		payload: payload
	}
}
