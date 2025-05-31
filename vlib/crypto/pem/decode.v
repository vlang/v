module pem

import encoding.base64

// decode_only reads `data` and returns the first parsed PEM Block. `none` is returned
//  when a header is expected, but not present or when a start of '-----BEGIN' or end of '-----END'
// can't be found.
//
// use decode if you still need the unparsed rest of the string.
@[inline]
pub fn decode_only(data string) ?Block {
	block, _ := decode_internal(data)?
	return block
}

// decode reads `data` and returns the first parsed PEM Block along with the rest of
// the string. `none` is returned when a header is expected, but not present
// or when a start of '-----BEGIN' or end of '-----END' can't be found.
//
// use decode_only if you do not need the unparsed rest of the string.
@[direct_array_access; inline]
pub fn decode(data string) ?(Block, string) {
	block, rest := decode_internal(data)?
	return block, rest[rest.index(pem_end)? + pem_end.len..].all_after_first(pem_eol)
}

// decode_internal allows `decode` variations to deal with the rest of the data as
// they want to. for example Block.decode could have hindered performance with the final
// indexing into `rest` that `decode_partial` does.
@[direct_array_access]
fn decode_internal(data string) ?(Block, string) {
	// direct_array_access safety: since we use the string.index method here,
	// we won't get an invalid index since it would otherwise return `none`
	mut rest := data[data.index(pem_begin)?..]
	mut block := Block.new(rest[pem_begin.len..].all_before(pem_eol))
	block.headers, rest = parse_headers(rest[pem_begin.len..].all_after(pem_eol).trim_left(' \n\t\v\f\r'))?

	block_end_index := rest.index(pem_end)?
	b64_data := rest[..block_end_index].replace_each(['\r', '', '\n', '', '\t', '', ' ', ''])
	block_data_len := block_end_index / 4 * 3
	block.data = []u8{len: block_data_len, cap: block_data_len + 3, init: 0}
	decoded_len := base64.decode_in_buffer(&b64_data, &block.data[0])
	block.data = block.data[..decoded_len]

	return block, rest
}

@[direct_array_access]
fn parse_headers(block string) ?(map[string][]string, string) {
	headers_str := block.all_before(pem_end).all_before('\n\n')

	// check that something was split or if it's empty
	if headers_str.len == block.all_before(pem_end).len || headers_str.len == 0 {
		return map[string][]string{}, block
	}

	// separate lines instead of iterating over them,
	// so that we can manually index them
	headers_separated := headers_str.split_into_lines()

	// index the key/value separator ':', otherwise
	// return none because it should exist
	// the initialisation of this function already tells us headers are present
	mut colon_index := headers_separated[0].index(colon) or { return none }

	mut headers := map[string][]string{}
	mut index := 0

	for index < headers_separated.len - 1 {
		line := headers_separated[index]
		if line.len == 0 {
			break
		}

		key := line[..colon_index].trim_space()
		mut val := line[colon_index + 1..].trim_space()

		for colon_index = 0; index < headers_separated.len - 1 && colon_index == 0; {
			index++
			colon_index = headers_separated[index].index(colon) or {
				val += headers_separated[index].trim_space()
				0
			}
		}

		headers[key] << val
	}

	return headers, block.all_after('\n\n')
}
