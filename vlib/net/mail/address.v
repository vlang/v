module mail

pub struct Address {
pub:
	name    string
	address string
}

// parse_address parses a single RFC 5322 address.
pub fn parse_address(s string) !Address {
	line := s.trim_space()
	if line == '' {
		return error('mail: no address found')
	}
	open_idx, close_idx := find_angle_pair(line)
	if open_idx >= 0 {
		if close_idx < 0 || close_idx < open_idx {
			return error('mail: no address found')
		}
		addr := line[open_idx + 1..close_idx].trim_space()
		if !addr.contains('@') {
			return error('mail: no address found')
		}
		raw_name := line[..open_idx].trim_space()
		name := unquote_display_name(raw_name)
		return Address{ name: name, address: addr }
	}
	// A stray closing bracket without an opener is malformed.
	if close_idx >= 0 {
		return error('mail: no address found')
	}
	// no angle brackets
	if line.contains('@') {
		return Address{ name: '', address: line }
	}
	return error('mail: no address found')
}

// find_angle_pair locates the first '<' and its matching '>' outside
// quoted strings, reporting -1 for either when absent. Backslash escapes
// are honored so quoted pairs like `"A > B"` never delimit the mailbox.
fn find_angle_pair(s string) (int, int) {
	mut open_idx := -1
	mut close_idx := -1
	mut in_quote := false
	mut escaped := false
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
		if c == `"` {
			in_quote = !in_quote
			continue
		}
		if in_quote {
			continue
		}
		if c == `<` && open_idx < 0 {
			open_idx = i
		} else if c == `>` && open_idx >= 0 && close_idx < 0 {
			close_idx = i
			break
		}
	}
	return open_idx, close_idx
}

// unquote_display_name strips one pair of surrounding quotes and resolves
// RFC 5322 quoted-pair escapes (`\"` and `\\`).
fn unquote_display_name(s string) string {
	t := s.trim_space()
	if t.len >= 2 && t[0] == `"` && t[t.len - 1] == `"` {
		inner := t[1..t.len - 1]
		mut out := []u8{cap: inner.len}
		mut i := 0
		for i < inner.len {
			if inner[i] == `\\` && i + 1 < inner.len {
				out << inner[i + 1]
				i += 2
				continue
			}
			out << inner[i]
			i++
		}
		return out.bytestr()
	}
	return t
}

pub fn (a Address) string() string {
	if a.name == '' {
		return a.address
	}
	return '"${escape_display_name(a.name)}" <${a.address}>'
}

// escape_display_name escapes backslashes and quotes per RFC 5322 3.2.1,
// 3.2.4 and 3.4 so the rendered name round-trips through parse_address.
fn escape_display_name(s string) string {
	return s.replace('\\', '\\\\').replace('"', '\\"')
}
