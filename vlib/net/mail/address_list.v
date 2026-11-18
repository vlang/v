module mail

// parse_address_list parses a comma-separated list of addresses.
pub fn parse_address_list(s string) ![]Address {
	mut out := []Address{}
	for part in split_address_list(s) {
		p := part.trim_space()
		if p == '' {
			continue
		}
		out << parse_address(p)!
	}
	return out
}

// split_address_list splits on commas that separate mailboxes: commas
// inside quoted strings, RFC 5322 comments, or angle brackets never split.
fn split_address_list(s string) []string {
	mut parts := []string{}
	mut depth_angle := 0
	mut depth_comment := 0
	mut in_quote := false
	mut escaped := false
	mut start := 0
	for i := 0; i < s.len; i++ {
		c := s[i]
		if escaped {
			escaped = false
			continue
		}
		if c == `\\` && in_quote {
			escaped = true
			continue
		}
		if c == `"` && depth_comment == 0 {
			in_quote = !in_quote
			continue
		}
		if in_quote {
			continue
		}
		if c == `(` {
			depth_comment++
			continue
		}
		if c == `)` && depth_comment > 0 {
			depth_comment--
			continue
		}
		if depth_comment > 0 {
			continue
		}
		if c == `<` {
			depth_angle++
			continue
		}
		if c == `>` && depth_angle > 0 {
			depth_angle--
			continue
		}
		if c == `,` && depth_angle == 0 {
			parts << s[start..i]
			start = i + 1
		}
	}
	parts << s[start..]
	return parts
}
