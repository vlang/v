module time

// from_json_string implements a custom decoder for json2
pub fn (mut t Time) from_json_string(raw_string string) ! {
	t = parse_rfc3339(raw_string) or { Time{} }
}
