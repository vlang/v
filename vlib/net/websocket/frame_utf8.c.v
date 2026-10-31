module websocket

import encoding.utf8

// ASCII is valid UTF-8. Inspect bounded, unaligned words without aliasing
// assumptions; any non-ASCII byte delegates the entire message to the existing
// validator, preserving all Unicode and invalid-sequence behavior.
fn frame_text_valid(payload []u8) bool {
	mut offset := 0
	for offset <= payload.len - 8 {
		mut word := u64(0)
		unsafe { C.memcpy(&word, &u8(payload.data) + offset, 8) }
		if word & u64(0x8080808080808080) != 0 {
			return utf8.validate(payload.data, payload.len)
		}
		offset += 8
	}
	for offset < payload.len {
		if payload[offset] & 0x80 != 0 {
			return utf8.validate(payload.data, payload.len)
		}
		offset++
	}
	return true
}
