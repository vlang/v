module mail

import time

// parse_date parses an RFC 2822 date.
pub fn parse_date(s string) !time.Time {
	return time.parse_rfc2822(s.trim_space())
}
