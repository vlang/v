module time

// from_json_string implements a custom decoder for json2 (unix)
pub fn (mut t Time) from_json_number(raw_number string) ! {
	t = unix(raw_number.i64())
}

// from_json_string implements a custom decoder for json2 (iso8601/rfc3339/unix)
pub fn (mut t Time) from_json_string(raw_string string) ! {
	is_iso8601 := raw_string[4] == `-` && raw_string[7] == `-`
	if is_iso8601 {
		t = parse_iso8601(raw_string)!
		return
	}

	is_rfc3339 := raw_string.len == 24 && raw_string[23] == `Z` && raw_string[10] == `T`
	if is_rfc3339 {
		t = parse_rfc3339(raw_string)!
		return
	}

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

	return error('Expected iso8601/rfc3339/unix time but got: ${raw_string}')
}

// to_json implements a custom encoder for json2 (rfc3339)
pub fn (t Time) to_json() string {
	return '"' + t.format_rfc3339() + '"'
}
