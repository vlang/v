module lz

// compress_lz78 compresses data using a pure-V LZ78 dictionary stream.
pub fn compress_lz78(data []u8) ![]u8 {
	mut payload := []u8{}
	mut dict := map[string]int{}
	mut next_index := 1
	mut word := []u8{}

	for b in data {
		mut candidate := word.clone()
		candidate << b
		candidate_key := candidate.bytestr()
		if candidate_key in dict {
			word = candidate.clone()
			continue
		}

		prefix_index := if word.len == 0 { 0 } else { dict[word.bytestr()] }
		encode_uvarint(mut payload, u64(prefix_index))
		payload << u8(1)
		payload << b
		dict[candidate_key] = next_index
		next_index++
		word.clear()
	}

	if word.len > 0 {
		final_index := dict[word.bytestr()]
		encode_uvarint(mut payload, u64(final_index))
		payload << u8(0)
	}

	return wrap_payload(.lz78, data, payload)
}

// decompress_lz78 decompresses data produced by compress_lz78.
pub fn decompress_lz78(data []u8) ![]u8 {
	payload, expected_len := unwrap_payload(data, .lz78)!
	mut out := []u8{cap: int(expected_len)}
	mut dict := map[int][]u8{}
	mut next_index := 1
	mut pos := 0

	for pos < payload.len {
		prefix, next_pos, ok := decode_uvarint(payload, pos)
		if !ok {
			return error('invalid lz78 stream: bad prefix index')
		}
		pos = next_pos
		if pos >= payload.len {
			return error('invalid lz78 stream: missing suffix flag')
		}
		has_suffix := payload[pos]
		pos++

		mut phrase := if prefix == 0 {
			[]u8{}
		} else {
			if int(prefix) !in dict {
				return error('invalid lz78 stream: unknown prefix index')
			}
			dict[int(prefix)].clone()
		}

		if has_suffix == 1 {
			if pos >= payload.len {
				return error('invalid lz78 stream: missing suffix byte')
			}
			phrase << payload[pos]
			pos++
		} else if has_suffix != 0 {
			return error('invalid lz78 stream: bad suffix flag')
		}

		out << phrase
		dict[next_index] = phrase
		next_index++
	}

	if i64(out.len) != expected_len {
		return error('invalid lz78 stream: length mismatch')
	}
	return out
}
