module time

// Helper to attempt RFC2822 parsing without propagating error
fn try_parse_rfc2822(s string) ?Time {
	return parse_rfc2822(s) or { return none }
}

// from_json_string implements a custom decoder for json2 (unix)
pub fn (mut t Time) from_json_number(raw_number string) ! {
	t = unix(raw_number.i64())
}

// from_json_string implements a custom decoder for json2 (iso8601/rfc3339/rfc2822/unix)
pub fn (mut t Time) from_json_string(raw_string string) ! {
	// Improved ISO8601 detection: check for date pattern (YYYY-MM-DD) and optional time separator
	if raw_string.len >= 10 && raw_string[4] == `-` && raw_string[7] == `-` {
		// Check for ISO8601 time separator 'T' or space, or date-only format
		if raw_string.len == 10 || (raw_string.len > 10 && (raw_string[10] == `T` || raw_string[10] == ` `)) {
			t = parse_iso8601(raw_string)!
			return
		}
	}

	// RFC3339 detection (subset of ISO8601 with 'Z' timezone)
	if raw_string.len == 24 && raw_string[23] == `Z` && raw_string[10] == `T` {
		t = parse_rfc3339(raw_string)!
		return
	}

	// RFC2822 detection: try parsing as RFC2822 if it looks like RFC2822 format
	// RFC2822 format: "Mon, 11 Mar 2022 13:54:25 +0000" or "Mon, 11 Mar 2022 13:54:25 GMT"
	// Simple heuristic: check for comma after 3 letters (day name)
	if raw_string.len >= 16 {
		// Check for day name abbreviation (3 letters) followed by comma
		has_day_name := raw_string.len >= 4 && raw_string[3] == `,` &&
			((raw_string[0] >= `A` && raw_string[0] <= `Z`) || (raw_string[0] >= `a` && raw_string[0] <= `z`)) &&
			((raw_string[1] >= `A` && raw_string[1] <= `Z`) || (raw_string[1] >= `a` && raw_string[1] <= `z`)) &&
			((raw_string[2] >= `A` && raw_string[2] <= `Z`) || (raw_string[2] >= `a` && raw_string[2] <= `z`))
		
		if has_day_name {
			// Try parsing as RFC2822 - attempt and use if successful
			if rfc2822_time := try_parse_rfc2822(raw_string) {
				t = rfc2822_time
				return
			}
			// If parsing failed, continue to other format checks
		}
	}

	// Unix timestamp detection: all digits (or negative sign + digits)
	mut is_unix_timestamp := true
	for c in raw_string {
		if c == `-` || (c >= `0` && c <= `9`) {
			continue
		}
		is_unix_timestamp = false
		break
	}
	if is_unix_timestamp {
		t = unix(raw_string.i64())
		return
	}

	// Fallback to generic time.parse() for other formats
	t = parse(raw_string)!
	return
}

// to_json implements a custom encoder for json2 (rfc3339)
pub fn (t Time) to_json() string {
	return '"' + t.format_rfc3339() + '"'
}
