module pem

import encoding.base64

// decode_only reads `data` and returns the first parsed PEM Block. `none` is returned
//  when a header is expected, but not present or when a start of '-----BEGIN' or end of '-----END'
// can't be found, or when the '-----END' line names another type than the '-----BEGIN' line,
// or when the body is not valid, padded base64.
//
// use decode if you still need the unparsed rest of the string.
@[inline]
pub fn decode_only(data string) ?Block {
	block, _ := decode_internal(data)?
	return block
}

// decode reads `data` and returns the first parsed PEM Block along with the rest of
// the string. `none` is returned when a header is expected, but not present
// or when a start of '-----BEGIN' or end of '-----END' can't be found,
// or when the '-----END' line names another type than the '-----BEGIN' line,
// or when the body is not valid, padded base64.
//
// use decode_only if you do not need the unparsed rest of the string.
@[direct_array_access; inline]
pub fn decode(data string) ?(Block, string) {
	block, rest := decode_internal(data)?
	return block, rest
}

// decode_internal allows `decode` variations to deal with the rest of the data as
// they want to. The returned rest starts right after the '-----END TYPE-----' of the block.
@[direct_array_access]
fn decode_internal(data string) ?(Block, string) {
	// direct_array_access safety: since we use the string.index method here,
	// we won't get an invalid index since it would otherwise return `none`
	mut rest := data[data.index(pem_begin)?..]
	mut block := Block.new(rest[pem_begin.len..].all_before(pem_eol))
	block.headers, rest =
		parse_headers(rest[pem_begin.len..].all_after(pem_eol).trim_left(' \n\t\v\f\r'))?

	// index of the '-----END ' line: with an empty body it is the first line left,
	// without the newline in front of it that `pem_end` starts with
	mut block_end_index := 0
	if !rest.starts_with(pem_end[1..]) {
		block_end_index = rest.index(pem_end)? + 1
	}
	// the END line must name the same type as the BEGIN line
	end_type := rest[block_end_index + pem_end.len - 1..]
	if !end_type.starts_with(block.block_type)
		|| !end_type[block.block_type.len..].starts_with(pem_eol) {
		return none
	}
	b64_data := rest[..block_end_index].replace_each(['\r', '', '\n', '', '\t', '', ' ', ''])
	// base64.decode_in_buffer does not report errors: it decodes characters outside
	// the alphabet to garbage and drops an unpadded tail, so check the body first
	if !is_valid_base64(b64_data) {
		return none
	}
	block_data_len := block_end_index / 4 * 3
	block.data = []u8{len: block_data_len, cap: block_data_len + 3, init: 0}
	decoded_len := base64.decode_in_buffer(&b64_data, &block.data[0])
	block.data = block.data[..decoded_len]

	return block, end_type[block.block_type.len + pem_eol.len..]
}

// is_valid_base64 reports whether `s` is standard base64 with padding (RFC 4648 section 4):
// a multiple of 4 characters from the alphabet, with '=' only as the last one or two.
@[direct_array_access]
fn is_valid_base64(s string) bool {
	if s.len % 4 != 0 {
		return false
	}
	for i, c in s {
		if c == `=` {
			if i < s.len - 2 || (i == s.len - 2 && s[s.len - 1] != `=`) {
				return false
			}
		} else if !c.is_alnum() && c != `+` && c != `/` {
			return false
		}
	}
	return true
}

// is_header_section reports whether `section`, the text before a blank line that ends
// with CRLF, contains only headers. A line without a colon must continue a header value:
// it is indented, or it follows a header whose value is empty or ends with a comma.
fn is_header_section(section string) bool {
	if !section.all_before('\n').contains(colon) {
		return false
	}
	mut wrapped_value := false
	for line in section.split_into_lines() {
		if separator := line.index(colon) {
			// The encoder starts a wrapped value after an empty header line, and the
			// RFC 1421 examples wrap comma separated values after a comma.
			value := line[separator + 1..].trim_space()
			wrapped_value = value.len == 0 || value.ends_with(',')
		} else if line.len == 0 || (!wrapped_value && line[0] !in [` `, `\t`]) {
			// An unindented body line can precede a blank line in a block whose
			// headers were not followed by the usual blank separator.
			return false
		}
	}
	return true
}

@[direct_array_access]
fn parse_headers(block string) ?(map[string][]string, string) {
	section := block.all_before(pem_end)
	mut headers_str := section.all_before('\n\n')
	mut body := block.all_after('\n\n')
	// A blank line ending with CRLF also terminates folded headers. Keep an earlier LF
	// separator when a later CRLF blank line belongs to the base64 body.
	if separator := section.index('\n\r\n') {
		if separator < headers_str.len && is_header_section(section[..separator]) {
			headers_str = section[..separator]
			body = block[separator + 3..]
		}
	}

	if headers_str.len == section.len {
		// no blank line: the headers are the leading lines that contain a colon
		// (base64 never does), and the body starts at the first line without one
		mut header_end := 0
		for header_end < section.len {
			line_end := section.index_after('\n', header_end) or { section.len }
			if !section[header_end..line_end].contains(colon) {
				break
			}
			header_end = line_end + 1
		}
		if header_end > section.len {
			header_end = section.len
		}
		headers_str = section[..header_end]
		body = block[header_end..]
	}

	// Without a header key, any blank lines belong to the base64 body.
	if headers_str.len == 0 || !headers_str.contains(colon) {
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

	for index < headers_separated.len {
		line := headers_separated[index]
		if line.len == 0 {
			break
		}

		key := line[..colon_index].trim_space()
		mut val := line[colon_index + 1..].trim_space()

		// append the following lines without a colon to the value,
		// stopping at the next header or after the last line
		for colon_index = 0; colon_index == 0; {
			index++
			if index == headers_separated.len {
				break
			}
			colon_index = headers_separated[index].index(colon) or {
				val += headers_separated[index].trim_space()
				0
			}
		}

		if key !in headers {
			headers[key] = []string{}
		}
		headers[key] << val
	}

	return headers, body
}
