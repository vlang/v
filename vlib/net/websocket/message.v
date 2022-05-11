module websocket

import encoding.utf8

const (
	header_len_offset           = 2 // offset for lengthpart of websocket header
	buffer_size                 = 256 // default buffer size
	extended_payload16_end_byte = 4 // header length with 16-bit extended payload
	extended_payload64_end_byte = 10 // header length with 64-bit extended payload
)

// Fragment represents a websocket data fragment
struct Fragment {
	data   []u8   // included data payload data in a fragment
	opcode OPCode // interpretation of the payload data
}

// Frame represents a data frame header
struct Frame {
mut:
	// length of the websocket header part
	header_len int = 2
	// size of total frame
	frame_size  int = 2
	fin         bool   // true if final fragment of message
	rsv1        bool   // reserved for future use in websocket RFC
	rsv2        bool   // reserved for future use in websocket RFC
	rsv3        bool   // reserved for future use in websocket RFC
	opcode      OPCode // interpretation of the payload data
	has_mask    bool   // true if the payload data is masked
	payload_len int    // payload length
	masking_key [4]u8  // all frames from client to server is masked with this key
}

const (
	invalid_close_codes = [999, 1004, 1005, 1006, 1014, 1015, 1016, 1100, 2000, 2999, 5000, 65536]
)

// validate_client validates client frame rules from RFC6455
pub fn (mut ws Client) validate_frame(frame &Frame) ? {
	if frame.rsv1 || frame.rsv2 || frame.rsv3 {
		ws.close(1002, 'rsv cannot be other than 0, not negotiated')?
		return error('rsv cannot be other than 0, not negotiated')
	}
	if (int(frame.opcode) >= 3 && int(frame.opcode) <= 7)
		|| (int(frame.opcode) >= 11 && int(frame.opcode) <= 15) {
		ws.close(1002, 'use of reserved opcode')?
		return error('use of reserved opcode')
	}
	if frame.has_mask && !ws.is_server {
		// server should never send masked frames
		// to client, close connection
		ws.close(1002, 'client got masked frame')?
		return error('client sent masked frame')
	}
	if is_control_frame(frame.opcode) {
		if !frame.fin {
			ws.close(1002, 'control message must not be fragmented')?
			return error('unexpected control frame with no fin')
		}
		if frame.payload_len > 125 {
			ws.close(1002, 'control frames must not exceed 125 bytes')?
			return error('unexpected control frame payload length')
		}
	}
	if frame.fin == false && ws.fragments.len == 0 && frame.opcode == .continuation {
		err_msg := 'unexecpected continuation, there are no frames to continue, $frame'
		ws.close(1002, err_msg)?
		return error(err_msg)
	}
}

// is_control_frame returns true if the frame is a control frame
fn is_control_frame(opcode OPCode) bool {
	return opcode !in [.text_frame, .binary_frame, .continuation]
}

// is_data_frame returns true if the frame is a control frame
fn is_data_frame(opcode OPCode) bool {
	return opcode in [.text_frame, .binary_frame]
}

// read_payload reads the message payload from the socket
fn (mut ws Client) read_payload(frame &Frame) ?[]u8 {
	if frame.payload_len == 0 {
		return []u8{}
	}
	mut buffer := []u8{cap: frame.payload_len}
	mut read_buf := [1]u8{}
	mut bytes_read := 0
	for bytes_read < frame.payload_len {
		len := ws.socket_read_ptr(&read_buf[0], 1)?
		if len != 1 {
			return error('expected read all message, got zero')
		}
		bytes_read += len
		buffer << read_buf[0]
	}
	if bytes_read != frame.payload_len {
		return error('failed to read payload')
	}
	if frame.has_mask {
		for i in 0 .. frame.payload_len {
			buffer[i] ^= frame.masking_key[i % 4] & 0xff
		}
	}
	return buffer
}

// validate_utf_8 validates payload for valid utf8 encoding
// - Future implementation needs to support fail fast utf errors for strict autobahn conformance
fn (mut ws Client) validate_utf_8(opcode OPCode, payload []u8) ? {
	if opcode in [.text_frame, .close] && !utf8.validate(payload.data, payload.len) {
		ws.logger.error('malformed utf8 payload, payload len: ($payload.len)')
		ws.send_error_event('Recieved malformed utf8.')
		ws.close(1007, 'malformed utf8 payload')?
		return error('malformed utf8 payload')
	}
}

// read_next_message reads 1 to n frames to compose a message
pub fn (mut ws Client) read_next_message() ?Message {
	for {
		frame := ws.parse_frame_header()?
		ws.validate_frame(&frame)?
		frame_payload := ws.read_payload(&frame)?
		if is_control_frame(frame.opcode) {
			// Control frames can interject other frames
			// and need to be returned immediately
			msg := Message{
				opcode: OPCode(frame.opcode)
				payload: frame_payload.clone()
			}
			unsafe { frame_payload.free() }
			return msg
		}
		// if the message is fragmented we just put it on fragments
		// a fragment is allowed to have zero size payload
		if !frame.fin {
			ws.fragments << &Fragment{
				data: frame_payload.clone()
				opcode: frame.opcode
			}
			unsafe { frame_payload.free() }
			continue
		}
		if ws.fragments.len == 0 {
			ws.validate_utf_8(frame.opcode, frame_payload) or {
				ws.logger.error('UTF8 validation error: $err, len of payload($frame_payload.len)')
				ws.send_error_event('UTF8 validation error: $err, len of payload($frame_payload.len)')
				return err
			}
			msg := Message{
				opcode: OPCode(frame.opcode)
				payload: frame_payload.clone()
			}
			unsafe { frame_payload.free() }
			return msg
		}
		defer {
			ws.fragments = []
		}
		if is_data_frame(frame.opcode) {
			ws.close(0, '')?
			return error('Unexpected frame opcode')
		}
		payload := ws.payload_from_fragments(frame_payload)?
		opcode := ws.opcode_from_fragments()
		ws.validate_utf_8(opcode, payload)?
		msg := Message{
			opcode: opcode
			payload: payload.clone()
		}
		unsafe {
			frame_payload.free()
			payload.free()
		}
		return msg
	}
	return none
}

// payload_from_fragments returs the whole paylaod from fragmented message
fn (ws Client) payload_from_fragments(fin_payload []u8) ?[]u8 {
	mut total_size := 0
	for f in ws.fragments {
		if f.data.len > 0 {
			total_size += f.data.len
		}
	}
	total_size += fin_payload.len
	if total_size == 0 {
		return []u8{}
	}
	mut total_buffer := []u8{cap: total_size}
	for f in ws.fragments {
		if f.data.len > 0 {
			total_buffer << f.data
		}
	}
	total_buffer << fin_payload
	return total_buffer
}

// opcode_from_fragments returns the opcode for message from the first fragment sent
fn (ws Client) opcode_from_fragments() OPCode {
	return OPCode(ws.fragments[0].opcode)
}

// parse_frame_header parses next message by decoding the incoming frames
pub fn (mut ws Client) parse_frame_header() ?Frame {
	mut buffer := [256]u8{}
	mut bytes_read := 0
	mut frame := Frame{}
	mut rbuff := [1]u8{}
	mut mask_end_byte := 0
	for ws.state == .open {
		read_bytes := ws.socket_read_ptr(&rbuff[0], 1)?
		if read_bytes == 0 {
			// this is probably a timeout or close
			continue
		}
		buffer[bytes_read] = rbuff[0]
		bytes_read++
		// parses the first two header bytes to get basic frame information
		if bytes_read == websocket.header_len_offset {
			frame.fin = (buffer[0] & 0x80) == 0x80
			frame.rsv1 = (buffer[0] & 0x40) == 0x40
			frame.rsv2 = (buffer[0] & 0x20) == 0x20
			frame.rsv3 = (buffer[0] & 0x10) == 0x10
			frame.opcode = OPCode(int(buffer[0] & 0x7F))
			frame.has_mask = (buffer[1] & 0x80) == 0x80
			frame.payload_len = buffer[1] & 0x7F
			// if has mask set the byte postition where mask ends
			if frame.has_mask {
				mask_end_byte = if frame.payload_len < 126 {
					websocket.header_len_offset + 4
				} else if frame.payload_len == 126 {
					websocket.header_len_offset + 6
				} else if frame.payload_len == 127 {
					websocket.header_len_offset + 12
				} else {
					0
				} // impossible
			}
			frame.payload_len = frame.payload_len
			frame.frame_size = frame.header_len + frame.payload_len
			if !frame.has_mask && frame.payload_len < 126 {
				break
			}
		}
		if frame.payload_len == 126 && bytes_read == websocket.extended_payload16_end_byte {
			frame.header_len += 2
			frame.payload_len = 0
			frame.payload_len |= int(u32(buffer[2]) << 8)
			frame.payload_len |= int(buffer[3])
			frame.frame_size = frame.header_len + frame.payload_len
			if !frame.has_mask {
				break
			}
		}
		if frame.payload_len == 127 && bytes_read == websocket.extended_payload64_end_byte {
			frame.header_len += 8
			// these shift operators needs 64 bit on clang with -prod flag
			mut payload_len := u64(0)
			payload_len |= u64(buffer[2]) << 56
			payload_len |= u64(buffer[3]) << 48
			payload_len |= u64(buffer[4]) << 40
			payload_len |= u64(buffer[5]) << 32
			payload_len |= u64(buffer[6]) << 24
			payload_len |= u64(buffer[7]) << 16
			payload_len |= u64(buffer[8]) << 8
			payload_len |= u64(buffer[9])
			frame.payload_len = int(payload_len)
			if !frame.has_mask {
				break
			}
		}
		if frame.has_mask && bytes_read == mask_end_byte {
			frame.masking_key[0] = buffer[mask_end_byte - 4]
			frame.masking_key[1] = buffer[mask_end_byte - 3]
			frame.masking_key[2] = buffer[mask_end_byte - 2]
			frame.masking_key[3] = buffer[mask_end_byte - 1]
			break
		}
	}
	return frame
}

// unmask_sequence unmask any given sequence
fn (f Frame) unmask_sequence(mut buffer []u8) {
	for i in 0 .. buffer.len {
		buffer[i] ^= f.masking_key[i % 4] & 0xff
	}
}
