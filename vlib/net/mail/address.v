module mail

import strings

pub struct Address {
pub:
	name    string
	address string
}

// parse_address parses a single RFC 5322 address.
pub fn parse_address(s string) !Address {
	line := s.trim_space()
	// find <...>
	if idx := line.index('<') {
		if end := line.index('>') {
			addr := line[idx + 1..end].trim_space()
			name := line[..idx].trim_space().trim('"')
			return Address{ name: name, address: addr }
		}
	}
	// no angle brackets
	if line.contains('@') {
		return Address{ name: '', address: line }
	}
	return error('mail: no address found')
}

pub fn (a Address) string() string {
	if a.name == '' {
		return a.address
	}
	return '"${a.name}" <${a.address}>'
}
