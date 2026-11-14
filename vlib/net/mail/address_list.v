module mail

import strings

// parse_address_list parses a comma-separated list of addresses.
pub fn parse_address_list(s string) ![]Address {
	mut out := []Address{}
	for part in s.split(',') {
		p := part.trim_space()
		if p == '' {
			continue
		}
		out << parse_address(p)!
	}
	return out
}
