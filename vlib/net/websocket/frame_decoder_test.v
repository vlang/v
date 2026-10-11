module websocket

import encoding.utf8

fn decoder_wire(payload []u8, opcode u8) []u8 {
	mut wire := [opcode]
	if payload.len < 126 {
		wire << u8(0x80 | payload.len)
	} else if payload.len <= 65535 {
		wire << [u8(0xfe), u8(payload.len >> 8), u8(payload.len)]
	} else {
		wire << u8(0xff)
		for shift := 56; shift >= 0; shift -= 8 { wire << u8(u64(payload.len) >> shift) }
	}
	mask := [u8(3), 7, 11, 13]
	wire << mask
	for i, byte in payload { wire << byte ^ mask[i % 4] }
	return wire
}

fn test_decoder_tcp_splits_and_length_boundaries() {
	for size in [0, 1, 2, 124, 125, 126, 127, 255, 1024, 65535, 65536] {
		payload := []u8{len: size, init: u8(index % 256)}
		wire := decoder_wire(payload, 0x82)
		for split in [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 13, wire.len / 2, wire.len - 1] {
			if split >= wire.len { continue }
			mut decoder := ServerFrameDecoder{ max_message_bytes: 65536 }
			mut input := wire[..split].clone()
			before := input.clone()
			assert decoder.decode(mut input).kind == .need_more
			assert input == before
			input << wire[split..]
			decoded := decoder.decode(mut input)
			assert decoded.kind == .message
			assert decoded.opcode == .binary_frame
			assert decoded.consumed == wire.len
			assert decoded.payload == payload
		}
	}
}

fn test_decoder_fragmented_utf8_and_interleaved_controls() {
	text := 'a 🌍 مرحبا café'.bytes()
	for split in 0 .. text.len + 1 {
		mut decoder := ServerFrameDecoder{}
		mut first := decoder_wire(text[..split], 0x01)
		assert decoder.decode(mut first).kind == .fragment
		mut ping := decoder_wire([u8(0xff), 0], 0x89)
		control := decoder.decode(mut ping)
		assert control.kind == .control && control.opcode == .ping
		assert control.payload == [u8(0xff), 0]
		mut last := decoder_wire(text[split..], 0x80)
		result := decoder.decode(mut last)
		assert result.kind == .message && result.payload == text
		mut following := decoder_wire('next'.bytes(), 0x81)
		assert decoder.decode(mut following).payload == 'next'.bytes()
	}
	mut bad := ServerFrameDecoder{}
	mut wire := decoder_wire([u8(0xed), 0xa0], 0x01)
	assert bad.decode(mut wire).close_code == 1007
}

fn test_decoder_declared_limits_and_terminal_failures() {
	mut decoder := ServerFrameDecoder{ max_message_bytes: 128 }
	mut oversize := [u8(0x81), 0xfe, 0, 129]
	assert decoder.decode(mut oversize).close_code == 1009
	mut valid := decoder_wire('valid'.bytes(), 0x81)
	assert decoder.decode(mut valid).close_code == 1009
	mut fragmented := ServerFrameDecoder{ max_message_bytes: 5 }
	mut first := decoder_wire('abc'.bytes(), 0x01)
	assert fragmented.decode(mut first).kind == .fragment
	mut next := [u8(0x80), 0x83]
	assert fragmented.decode(mut next).close_code == 1009
}

fn test_decoder_invalid_headers_closes_and_utf8() {
	mut cases := [][]u8{}
	cases << [u8(0x81), 1, `x`]
	cases << decoder_wire('x'.bytes(), 0xc1)
	cases << decoder_wire('x'.bytes(), 0x83)
	cases << decoder_wire('x'.bytes(), 0x09)
	cases << decoder_wire([]u8{len: 126}, 0x89)
	cases << decoder_wire('x'.bytes(), 0x80)
	cases << decoder_wire([u8(1)], 0x88)
	cases << decoder_wire([u8(3), 237], 0x88)
	cases << [u8(0x81), 0xfe, 0, 1]
	cases << [u8(0x81), 0xff, 128, 0, 0, 0, 0, 0, 0, 0]
	for wire in cases {
		mut input := wire.clone()
		mut decoder := ServerFrameDecoder{}
		assert decoder.decode(mut input).close_code == 1002
	}
	for payload in [[u8(0xff)], [u8(0xc0), 0xaf], [u8(0xf4), 0x90, 0x80, 0x80], [u8(0xe2), 0x82]] {
		mut decoder := ServerFrameDecoder{}
		mut wire := decoder_wire(payload, 0x81)
		assert decoder.decode(mut wire).close_code == 1007
	}
	for payload in [[]u8{}, [u8(3), 232], [u8(3), 232, `b`, `y`, `e`]] {
		mut decoder := ServerFrameDecoder{}
		mut wire := decoder_wire(payload, 0x88)
		assert decoder.decode(mut wire).payload == payload
	}
}

fn test_fragment_utf8_validator_matches_whole_string_validator() {
	mut seed := u64(91731)
	for size in 0 .. 25 {
		for _ in 0 .. 400 {
			mut bytes := []u8{len: size}
			for i in 0 .. size {
				seed = seed * u64(6364136223846793005) + 1
				bytes[i] = u8(seed >> 32)
			}
			mut state := FrameUtf8{}
			mut accepted := true
			for i in 0 .. bytes.len {
				if !state.accept(bytes[i..i + 1]) {
					accepted = false
					break
				}
			}
			assert (accepted && state.remaining == 0) == utf8.validate(bytes.data, bytes.len)
		}
	}
}

fn test_decoder_consumes_one_coalesced_frame_at_a_time() {
	frames := [decoder_wire([]u8{}, 0x81), decoder_wire('hello'.bytes(), 0x81),
		decoder_wire([]u8{}, 0x89), decoder_wire([u8(0xff), 0], 0x82), decoder_wire([]u8{}, 0x8a)]
	payloads := [[]u8{}, 'hello'.bytes(), []u8{}, [u8(0xff), 0], []u8{}]
	opcodes := [OPCode.text_frame, .text_frame, .ping, .binary_frame, .pong]
	mut input := []u8{}
	for frame in frames {
		input << frame
	}
	mut decoder := ServerFrameDecoder{}
	for i, frame in frames {
		before := input.clone()
		decoded := decoder.decode(mut input)
		expected_kind := if i in [2, 4] { FrameDecodeKind.control } else { FrameDecodeKind.message }
		assert decoded.kind == expected_kind
		assert decoded.consumed == frame.len
		assert decoded.opcode == opcodes[i]
		assert decoded.payload == payloads[i]
		assert input[decoded.consumed..] == before[decoded.consumed..]
		input = input[decoded.consumed..].clone()
	}
	assert input.len == 0
	assert decoder.decode(mut input).kind == .need_more
}

fn test_decoder_fragmented_binary_at_limit_with_empty_continuation() {
	mut decoder := ServerFrameDecoder{ max_message_bytes: 4 }
	mut first := decoder_wire([u8(0xff), 0], 0x02)
	assert decoder.decode(mut first).kind == .fragment
	mut empty := decoder_wire([]u8{}, 0x00)
	assert decoder.decode(mut empty).kind == .fragment
	wire := decoder_wire([u8(0xc0), 0xaf], 0x80)
	mut partial := wire[..wire.len - 1].clone()
	before := partial.clone()
	assert decoder.decode(mut partial).kind == .need_more
	assert partial == before
	partial << wire.last()
	decoded := decoder.decode(mut partial)
	assert decoded.kind == .message
	assert decoded.opcode == .binary_frame
	assert decoded.payload == [u8(0xff), 0, 0xc0, 0xaf]
	mut next := decoder_wire('four'.bytes(), 0x81)
	assert decoder.decode(mut next).payload == 'four'.bytes()
}

fn test_decoder_rejects_new_data_frames_during_fragmentation() {
	for opcode in [u8(0x01), 0x02, 0x81, 0x82] {
		mut decoder := ServerFrameDecoder{}
		mut first := decoder_wire('first'.bytes(), 0x01)
		assert decoder.decode(mut first).kind == .fragment
		mut next := decoder_wire('next'.bytes(), opcode)
		assert decoder.decode(mut next).close_code == 1002
	}
}

fn test_decoder_rejects_truncated_fragmented_utf8_and_invalid_close_reason() {
	mut decoder := ServerFrameDecoder{}
	mut first := decoder_wire([u8(0xe2), 0x82], 0x01)
	assert decoder.decode(mut first).kind == .fragment
	mut last := decoder_wire([]u8{}, 0x80)
	assert decoder.decode(mut last).close_code == 1007
	mut following := decoder_wire('valid'.bytes(), 0x81)
	assert decoder.decode(mut following).close_code == 1007

	mut close_decoder := ServerFrameDecoder{}
	mut close := decoder_wire([u8(3), 232, 0xff], 0x88)
	assert close_decoder.decode(mut close).close_code == 1007
}

fn test_decoder_rejects_noncanonical_64_bit_length_and_nonpositive_limits() {
	mut decoder := ServerFrameDecoder{}
	mut wire := [u8(0x82), 0xff, 0, 0, 0, 0, 0, 0, 0xff, 0xff]
	assert decoder.decode(mut wire).close_code == 1002
	for limit in [-1, 0] {
		mut invalid := ServerFrameDecoder{ max_message_bytes: limit }
		mut empty := decoder_wire([]u8{}, 0x81)
		assert invalid.decode(mut empty).close_code == 1009
	}
}
