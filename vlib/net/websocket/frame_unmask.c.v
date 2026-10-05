module websocket

// Fixed-size memcpy lets the C compiler load native words without alignment or
// strict-aliasing assumptions. Building the mask through bytes is endian-neutral.
fn frame_unmask(mut payload []u8, mask []u8) {
	assert mask.len == 4
	mut repeated := [8]u8{}
	for i in 0 .. 8 { repeated[i] = mask[i % 4] }
	mut key := u64(0)
	unsafe { C.memcpy(&key, &repeated[0], 8) }
	mut offset := 0
	for offset <= payload.len - 8 {
		mut word := u64(0)
		// offset + 8 is in bounds; memcpy also permits unaligned payloads.
		unsafe { C.memcpy(&word, &u8(payload.data) + offset, 8) }
		word ^= key
		unsafe { C.memcpy(&u8(payload.data) + offset, &word, 8) }
		offset += 8
	}
	for offset < payload.len {
		payload[offset] ^= mask[offset % 4]
		offset++
	}
}
