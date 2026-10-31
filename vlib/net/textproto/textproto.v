module textproto

// canonical_mime_header_key returns the canonical form of a MIME header key:
// each hyphen-separated word is title-cased (e.g. "content-type" ->
// "Content-Type"). This matches Go's textproto.CanonicalMIMEHeaderKey.
pub fn canonical_mime_header_key(s string) string {
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
// headers. Keys are canonicalized.
pub fn read_mime_header(text string) map[string][]string {
	mut result := map[string][]string{}
	mut last_key := ''
	for line in text.split_into_lines() {
		if line == '' {
			break
		}
		if line.starts_with(' ') || line.starts_with('\t') {
			if last_key != '' {
				if result[last_key].len > 0 {
					idx := result[last_key].len - 1
					result[last_key][idx] = result[last_key][idx] + ' ' + line.trim_space()
				}
			}
			continue
		}
		if !line.contains(':') {
			continue
		}
		parts := line.split_nth(':', 2)
		key := canonical_mime_header_key(parts[0].trim_space())
		val := if parts.len > 1 { parts[1].trim_space() } else { '' }
		result[key] << val
		last_key = key
	}
	return result
}
