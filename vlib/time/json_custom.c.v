module time

import strconv

// from_json_string implements a custom decoder for json2 (unix)
pub fn (mut t Time) from_json_number(raw_number string) ! {
	t = unix(raw_number.i64())
}

// from_json_string decodes iso8601/rfc3339/unix values, with or without JSON string quotes.
pub fn (mut t Time) from_json_string(raw_string string) ! {
	raw := if raw_string.len >= 2 && raw_string[0] == `"`
		&& raw_string[raw_string.len - 1] == `"` {
		raw_string[1..raw_string.len - 1]
	} else {
		raw_string
	}
	is_iso8601 := raw.len >= 10 && raw[4] == `-` && raw[7] == `-`
	if is_iso8601 {
		t = parse_iso8601(raw)!
		return
	}

	is_rfc3339 := raw.len == 24 && raw[23] == `Z` && raw[10] == `T`
	if is_rfc3339 {
		t = parse_rfc3339(raw)!
		return
	}

	start := if raw.len > 0 && raw[0] == `-` { 1 } else { 0 }
	mut is_unix_timestamp := raw.len > start
	for c in raw[start..] {
		if c >= `0` && c <= `9` {
			continue
		}
		is_unix_timestamp = false
		break
	}
	if is_unix_timestamp {
		t = unix(strconv.common_parse_int(raw, 10, 64, true, true)!)
		return
	}

	return error('Expected iso8601/rfc3339/unix time but got: ${raw_string}')
}

// to_json implements a custom encoder for json2 (rfc3339)
pub fn (t Time) to_json() string {
	return '"' + t.format_rfc3339() + '"'
}
