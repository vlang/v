module lz

const stream_magic = [u8(0x56), 0x4c, 0x5a, 0x31] // VLZ1

struct MatchProfile {
	window      int
	min_match   int
	max_match   int
	max_literal int
}

const match_hash_bits = 16
const match_hash_size = 1 << match_hash_bits
const max_match_candidates = 64

fn wrap_payload(format Format, source []u8, payload []u8) []u8 {
	mut out := []u8{cap: stream_magic.len + 8 + payload.len}
	out << stream_magic
	out << u8(format)
	encode_uvarint(mut out, u64(source.len))
	out << payload
	return out
}

fn unwrap_payload(data []u8, format Format) !([]u8, i64) {
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
	decoded_len_u64, mut pos, ok := decode_uvarint(data, stream_magic.len + 1)
	if !ok {
		return error('invalid lz stream: bad length')
	}
	if decoded_len_u64 > u64(max_int) {
		return error('invalid lz stream: decoded length too large')
	}
	decoded_len := i64(decoded_len_u64)
	if pos > data.len {
		return error('invalid lz stream: truncated payload')
	}
	return data[pos..], decoded_len
}

fn compress_with_profile(data []u8, profile MatchProfile, format Format) []u8 {
	if data.len == 0 {
		return wrap_payload(format, data, []u8{})
	}
	mut payload := []u8{cap: data.len}
	mut literals := []u8{cap: profile.max_literal}
	mut last_match := []int{len: match_hash_size, init: -1}
	mut prev_match := []int{len: data.len, init: -1}
	mut pos := 0
	for pos < data.len {
		offset, length := find_best_match(data, pos, profile, last_match, prev_match)
		if length >= profile.min_match {
			flush_literals(mut payload, mut literals)
			emit_match(mut payload, offset, length, profile.min_match)
			for i := pos; i < pos + length; i++ {
				index_match_position(data, i, mut last_match, mut prev_match)
			}
			pos += length
		} else {
			literals << data[pos]
			if literals.len == profile.max_literal {
				flush_literals(mut payload, mut literals)
			}
			index_match_position(data, pos, mut last_match, mut prev_match)
			pos++
		}
	}
	flush_literals(mut payload, mut literals)
	return wrap_payload(format, data, payload)
}

fn decompress_with_profile(data []u8, profile MatchProfile, format Format) ![]u8 {
	payload, expected_len := unwrap_payload(data, format)!
	mut out := []u8{cap: int(expected_len)}
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
		if offset == 0 || offset > u64(max_i64) || i64(offset) > i64(out.len) {
			return error('invalid lz stream: bad match offset')
		}
		offset_int := int(offset)
		base := out.len - offset_int
		for i in 0 .. match_len {
			out << out[base + i]
		}
	}
	if i64(out.len) != expected_len {
		return error('invalid lz stream: length mismatch')
	}
	return out
}

fn find_best_match(data []u8, pos int, profile MatchProfile, last_match []int, prev_match []int) (int, int) {
	if pos + profile.min_match > data.len {
		return 0, 0
	}
	max_len := if pos + profile.max_match < data.len { profile.max_match } else { data.len - pos }
	mut best_len := 0
	mut best_offset := 0
	hash_idx := match_hash(data, pos)
	mut candidates_checked := 0
	mut i := last_match[hash_idx]
	for i >= 0 && candidates_checked < max_match_candidates {
		offset := pos - i
		if offset > profile.window {
			break
		}
		mut current_len := 0
		for current_len < max_len && data[i + current_len] == data[pos + current_len] {
			current_len++
		}
		if current_len > best_len {
			best_len = current_len
			best_offset = offset
			if best_len == max_len {
				break
			}
		}
		i = prev_match[i]
		candidates_checked++
	}
	return best_offset, best_len
}

fn index_match_position(data []u8, pos int, mut last_match []int, mut prev_match []int) {
	if pos + 2 >= data.len {
		return
	}
	hash_idx := match_hash(data, pos)
	prev_match[pos] = last_match[hash_idx]
	last_match[hash_idx] = pos
}

fn match_hash(data []u8, pos int) int {
	v := (u32(data[pos]) << 16) | (u32(data[pos + 1]) << 8) | u32(data[pos + 2])
	return int((v * u32(2654435761)) >> (32 - match_hash_bits))
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
