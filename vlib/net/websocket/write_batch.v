module websocket

// write_messages sends complete messages in order with one socket write. It
// does not wait for more messages or retain caller buffers after returning.
// The return value includes frame headers, as with write(). A failed write may
// have sent a prefix; close the connection instead of replaying the batch.
// Close frames are not accepted; use close() to perform the closing handshake.
pub fn (mut ws Client) write_messages(messages []Message) !int {
	if ws.get_state() != .open {
		return error('trying to write on a closed socket!')
	}
	mut size := i64(0)
	for message in messages {
		if message.opcode == .close {
			return error('close frames cannot be batched; use close() instead')
		}
		if message.opcode !in [.text_frame, .binary_frame, .ping, .pong] {
			return error('batch messages must be complete data or control frames')
		}
		if is_control_frame(message.opcode) && message.payload.len > 125 {
			return error('control frame payload exceeds 125 bytes')
		}
		size += i64(message.payload.len) + 2 + if message.payload.len < 126 {
			0
		} else if message.payload.len <= 65535 {
			2
		} else {
			8
		} + if ws.is_server { 0 } else { 4 }
		if size > 0x7fffffff {
			return error('batch too large')
		}
	}
	if size == 0 {
		return 0
	}
	mut wire := []u8{cap: int(size)}
	defer { unsafe { wire.free() } }
	for message in messages {
		wire << (u8(message.opcode) | 0x80)
		mask_bit := if ws.is_server { u8(0) } else { u8(0x80) }
		length := message.payload.len
		if length < 126 {
			wire << (u8(length) | mask_bit)
		} else if length <= 65535 {
			wire << [u8(126) | mask_bit, u8(length >> 8), u8(length)]
		} else {
			wire << (u8(127) | mask_bit)
			for shift := 56; shift >= 0; shift -= 8 {
				wire << u8(u64(length) >> shift)
			}
		}
		if ws.is_server {
			wire << message.payload
		} else {
			key := create_masking_key()
			wire << key
			for index, byte in message.payload {
				wire << (byte ^ key[index % 4])
			}
			unsafe { key.free() }
		}
	}
	return ws.socket_write(wire)
}
