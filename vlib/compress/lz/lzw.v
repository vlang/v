module lz

// compress_lzw compresses data using a pure-V LZW dictionary stream.
pub fn compress_lzw(data []u8) ![]u8 {
	mut payload := []u8{}
	mut dict := map[string]int{}
	for i in 0 .. 256 {
		dict[[u8(i)].bytestr()] = i
	}
	mut next_code := 256
	mut word := ''

	for b in data {
		symbol := [b].bytestr()
		candidate := word + symbol
		if candidate in dict {
			word = candidate
			continue
		}
		if word != '' {
			encode_uvarint(mut payload, u64(dict[word]))
		}
		dict[candidate] = next_code
		next_code++
		word = symbol
	}

	if word != '' {
		encode_uvarint(mut payload, u64(dict[word]))
	}

	return wrap_payload(.lzw, data, payload)
}

// decompress_lzw decompresses data produced by compress_lzw.
pub fn decompress_lzw(data []u8) ![]u8 {
	payload, expected_len := unwrap_payload(data, .lzw)!
	if payload.len == 0 {
		if expected_len == i64(0) {
			return []u8{}
		}
		return error('invalid lzw stream: missing codes')
	}

	mut dict := map[int][]u8{}
	for i in 0 .. 256 {
		dict[i] = [u8(i)]
	}
	mut next_code := 256
	mut pos := 0

	first_code, next_pos, ok := decode_uvarint(payload, pos)
	if !ok {
		return error('invalid lzw stream: bad initial code')
	}
	pos = next_pos
	if int(first_code) !in dict {
		return error('invalid lzw stream: unknown initial code')
	}
	mut word := dict[int(first_code)].clone()
	mut out := word.clone()

	for pos < payload.len {
		code_u64, new_pos, ok_code := decode_uvarint(payload, pos)
		if !ok_code {
			return error('invalid lzw stream: bad code')
		}
		pos = new_pos
		code := int(code_u64)
		mut entry := []u8{}
		if code in dict {
			entry = dict[code].clone()
		} else if code == next_code {
			entry = word.clone()
			entry << word[0]
		} else {
			return error('invalid lzw stream: unknown code')
		}

		out << entry
		mut new_entry := word.clone()
		new_entry << entry[0]
		dict[next_code] = new_entry
		next_code++
		word = entry.clone()
	}

	if i64(out.len) != expected_len {
		return error('invalid lzw stream: length mismatch')
	}
	return out
}
