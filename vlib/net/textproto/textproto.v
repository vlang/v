module textproto

// canonical_mime_header_key returns the canonical form of a MIME header key:
// each hyphen-separated word is title-cased (e.g. "content-type" ->
// "Content-Type"). Empty names and names containing invalid MIME field-name
// bytes are returned unchanged, as in Go's textproto.CanonicalMIMEHeaderKey.
pub fn canonical_mime_header_key(s string) string {
	if !valid_mime_header_key(s) {
		return s
	}
	parts := s.split('-')
	mut out := []string{}
	for p in parts {
		if p == '' {
			out << ''
			continue
		}
		out << p[0..1].to_upper() + p[1..].to_lower()
	}
	return out.join('-')
}

// read_mime_header parses a block of MIME header text into a map from
// canonical key to its values. Continuation lines (starting with a space or
// tab) are folded into the previous value. A blank line terminates the
// headers. CRLF and LF line endings are accepted. Keys are canonicalized and
// only spaces and tabs are trimmed from each value line. Folding an empty
// initial value does not add leading whitespace. Malformed fields and orphan
// continuations are ignored; a malformed line also ends the previous field's
// continuation. This lenient helper does not report parsing errors.
pub fn read_mime_header(text string) map[string][]string {
	mut result := map[string][]string{}
	mut last_key := ''
	lines := text.split('\n')
	for i, raw_line in lines {
		line := if i + 1 < lines.len && raw_line.ends_with('\r') {
			raw_line[..raw_line.len - 1]
		} else {
			raw_line
		}
		if line == '' {
			break
		}
		if line.starts_with(' ') || line.starts_with('\t') {
			if !valid_mime_header_value(line) {
				last_key = ''
				continue
			}
			if last_key != '' {
				idx := result[last_key].len - 1
				result[last_key][idx] = (result[last_key][idx] + ' ' + line.trim(' \t')).trim_left(' \t')
			}
			continue
		}
		last_key = ''
		colon := line.index(':') or { continue }
		if !valid_mime_header_key(line[..colon]) || !valid_mime_header_value(line[colon + 1..]) {
			continue
		}
		key := canonical_mime_header_key(line[..colon])
		val := line[colon + 1..].trim(' \t')
		result[key] << val
		last_key = key
	}
	return result
}

fn valid_mime_header_key(key string) bool {
	if key.len == 0 {
		return false
	}
	for c in key {
		if !((c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`)
			|| c in [`!`, `#`, `$`, `%`, `&`, `'`, `*`, `+`, `-`, `.`, `^`, `_`, `\x60`, `|`, `~`]) {
			return false
		}
	}
	return true
}

fn valid_mime_header_value(value string) bool {
	for c in value {
		if (c < 32 && c != `\t`) || c == 127 {
			return false
		}
	}
	return true
}
