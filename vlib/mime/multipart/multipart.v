module multipart

// Part represents a single part in a multipart message.
pub struct Part {
pub:
	field_name   string
	file_name    string
	content_type string
	data         string
}

// parse returns complete multipart parts in wire order, preserving duplicate
// field names and body bytes. The boundary is the unquoted MIME parameter value.
// Invalid boundaries return no parts. Parsing stops at the first malformed or
// incomplete part and returns the preceding complete parts. Preamble and epilogue
// are ignored; transfer encodings and nested multipart bodies are not decoded.
pub fn parse(body string, boundary string) []Part {
	mut parts := []Part{}
	if !valid_boundary(boundary) {
		return parts
	}
	marker := '--${boundary}'
	mut current := find_delimiter(body, marker, 0) or { return parts }
	for !current.closing {
		next := find_delimiter(body, marker, current.after) or { break }
		// The CRLF immediately before a delimiter belongs to the delimiter.
		end := next.start - 2
		if end < current.after {
			break
		}
		part := parse_part(body[current.after..end]) or { break }
		parts << part
		current = next
	}
	return parts
}

struct Delimiter {
	start   int
	after   int
	closing bool
}

fn valid_boundary(boundary string) bool {
	if boundary.len == 0 || boundary.len > 70 || boundary.ends_with(' ') {
		return false
	}
	for b in boundary {
		if !b.is_alnum() && !"'()+_,-./:=? ".contains_u8(b) {
			return false
		}
	}
	return true
}

fn find_delimiter(body string, marker string, start int) ?Delimiter {
	mut offset := start
	for {
		pos := body.index_after(marker, offset) or { return none }
		offset = pos + marker.len
		if pos != 0 && (pos < 2 || body[pos - 2] != `\r` || body[pos - 1] != `\n`) {
			continue
		}
		mut end := offset
		closing := end <= body.len - 2 && body[end] == `-` && body[end + 1] == `-`
		if closing {
			end += 2
		}
		for end < body.len && body[end] in [` `, `\t`] {
			end++
		}
		if closing && end == body.len {
			return Delimiter{pos, end, true}
		}
		if end <= body.len - 2 && body[end] == `\r` && body[end + 1] == `\n` {
			return Delimiter{pos, end + 2, closing}
		}
	}
	return none
}

fn parse_part(raw string) ?Part {
	mut header_end := 0
	mut data_start := 2
	if !raw.starts_with('\r\n') {
		header_end = raw.index('\r\n\r\n') or { return none }
		data_start = header_end + 4
	}
	mut headers := map[string][]string{}
	mut last_name := ''
	if header_end > 0 {
		for line in raw[..header_end].split('\r\n') {
			for b in line {
				if (b < 32 && b != `\t`) || b == 127 {
					return none
				}
			}
			if line.starts_with(' ') || line.starts_with('\t') {
				if last_name == '' {
					return none
				}
				headers[last_name] << line.trim(' \t')
				continue
			}
			colon := line.index(':') or { return none }
			name := line[..colon].to_lower()
			if name.len == 0 {
				return none
			}
			for b in name {
				if !is_token_byte(b) {
					return none
				}
			}
			if name in ['content-disposition', 'content-type'] && name in headers {
				return none
			}
			headers[name] = [line[colon + 1..].trim(' \t')]
			last_name = name
		}
	}
	mut params := map[string]string{}
	if disposition := headers['content-disposition'] {
		params = disposition_parameters(disposition.join(' ')) or { return none }
	}
	return Part{
		field_name:   params['name']
		file_name:    params['filename']
		content_type: headers['content-type'].join(' ')
		data:         raw[data_start..]
	}
}

fn is_token_byte(b u8) bool {
	return b > 32 && b < 127 && !r'()<>@,;:\"/[]?='.contains_u8(b)
}

fn disposition_parameters(value string) ?map[string]string {
	mut params := map[string]string{}
	mut pos := value.index(';') or { value.len }
	disposition := value[..pos].trim(' \t')
	if disposition.len == 0 {
		return none
	}
	for b in disposition {
		if !is_token_byte(b) {
			return none
		}
	}
	for pos < value.len {
		pos++ // semicolon
		for pos < value.len && value[pos] in [` `, `\t`] {
			pos++
		}
		start := pos
		for pos < value.len && is_token_byte(value[pos]) {
			pos++
		}
		name := value[start..pos].to_lower()
		for pos < value.len && value[pos] in [` `, `\t`] {
			pos++
		}
		if name.len == 0 || name in params || pos >= value.len || value[pos] != `=` {
			return none
		}
		pos++
		for pos < value.len && value[pos] in [` `, `\t`] {
			pos++
		}
		mut bytes := []u8{}
		if pos < value.len && value[pos] == `"` {
			pos++
			for pos < value.len && value[pos] != `"` {
				if value[pos] == `\\` {
					pos++
					if pos == value.len {
						return none
					}
				}
				bytes << value[pos]
				pos++
			}
			if pos == value.len {
				return none
			}
			pos++
		} else {
			for pos < value.len && is_token_byte(value[pos]) {
				bytes << value[pos]
				pos++
			}
			if bytes.len == 0 {
				return none
			}
		}
		params[name] = bytes.bytestr()
		for pos < value.len && value[pos] in [` `, `\t`] {
			pos++
		}
		if pos < value.len && value[pos] != `;` {
			return none
		}
	}
	return params
}
