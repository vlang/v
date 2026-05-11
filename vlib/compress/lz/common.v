module lz

const stream_magic = [u8(0x56), 0x4c, 0x5a, 0x31] // VLZ1

struct MatchProfile {
	window      int
	min_match   int
	max_match   int
	max_literal int
}

fn wrap_payload(format Format, source []u8, payload []u8) []u8 {
	mut out := []u8{cap: stream_magic.len + 8 + payload.len}
	out << stream_magic
	out << u8(format)
	encode_uvarint(mut out, u64(source.len))
	out << payload
	return out
}

fn unwrap_payload(data []u8, format Format) !([]u8, int) {
	if data.len < stream_magic.len + 2 {
		return error('invalid lz stream: too short')
	}
	if data[..stream_magic.len] != stream_magic {
		return error('invalid lz stream: bad magic')
	}
	wire_format := data[stream_magic.len]
	if wire_format != u8(format) {
		return error('invalid lz stream: format mismatch')
	}
	decoded_len, mut pos, ok := decode_uvarint(data, stream_magic.len + 1)
	if !ok {
		return error('invalid lz stream: bad length')
	}
	if decoded_len > u64(1 << 31) {
		return error('invalid lz stream: decoded length too large')
	}
	if pos > data.len {
		return error('invalid lz stream: truncated payload')
	}
	return data[pos..], int(decoded_len)
}

fn compress_with_profile(data []u8, profile MatchProfile, format Format) []u8 {
	if data.len == 0 {
		return wrap_payload(format, data, []u8{})
	}
	mut payload := []u8{cap: data.len}
	mut literals := []u8{cap: profile.max_literal}
	mut pos := 0
	for pos < data.len {
		offset, length := find_best_match(data, pos, profile)
		if length >= profile.min_match {
			flush_literals(mut payload, mut literals)
			emit_match(mut payload, offset, length, profile.min_match)
			pos += length
		} else {
			literals << data[pos]
			if literals.len == profile.max_literal {
				flush_literals(mut payload, mut literals)
			}
			pos++
		}
	}
	flush_literals(mut payload, mut literals)
	return wrap_payload(format, data, payload)
}

fn decompress_with_profile(data []u8, profile MatchProfile, format Format) ![]u8 {
	payload, expected_len := unwrap_payload(data, format)!
	mut out := []u8{cap: expected_len}
	mut pos := 0
	for pos < payload.len {
		control := payload[pos]
		pos++
		if control & 0x80 == 0 {
			literal_len := int(control & 0x7f) + 1
			if pos + literal_len > payload.len {
				return error('invalid lz stream: truncated literal')
			}
			out << payload[pos..pos + literal_len]
			pos += literal_len
			continue
		}
		match_len := int(control & 0x7f) + profile.min_match
		offset, next_pos, ok := decode_uvarint(payload, pos)
		if !ok {
			return error('invalid lz stream: truncated match offset')
		}
		pos = next_pos
		if offset == 0 || int(offset) > out.len {
			return error('invalid lz stream: bad match offset')
		}
		base := out.len - int(offset)
		for i in 0 .. match_len {
			out << out[base + i]
		}
	}
	if out.len != expected_len {
		return error('invalid lz stream: length mismatch')
	}
	return out
}

fn find_best_match(data []u8, pos int, profile MatchProfile) (int, int) {
	start := if pos > profile.window { pos - profile.window } else { 0 }
	max_len := if pos + profile.max_match < data.len { profile.max_match } else { data.len - pos }
	mut best_len := 0
	mut best_offset := 0
	for i := start; i < pos; i++ {
		mut current_len := 0
		for current_len < max_len && data[i + current_len] == data[pos + current_len] {
			current_len++
		}
		if current_len > best_len {
			best_len = current_len
			best_offset = pos - i
			if best_len == max_len {
				break
			}
		}
	}
	return best_offset, best_len
}

fn flush_literals(mut payload []u8, mut literals []u8) {
	if literals.len == 0 {
		return
	}
	payload << u8(literals.len - 1)
	payload << literals
	literals.clear()
}

fn emit_match(mut payload []u8, offset int, length int, min_match int) {
	payload << u8(0x80 | u8(length - min_match))
	encode_uvarint(mut payload, u64(offset))
}

fn encode_uvarint(mut out []u8, value u64) {
	mut v := value
	for v >= 0x80 {
		out << u8(v & 0x7f | 0x80)
		v >>= 7
	}
	out << u8(v)
}

fn decode_uvarint(data []u8, start int) (u64, int, bool) {
	mut value := u64(0)
	mut shift := u32(0)
	mut pos := start
	for pos < data.len && shift <= 63 {
		b := data[pos]
		pos++
		value |= u64(b & 0x7f) << shift
		if b & 0x80 == 0 {
			return value, pos, true
		}
		shift += 7
	}
	return 0, start, false
}
