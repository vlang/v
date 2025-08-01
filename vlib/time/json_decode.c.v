module time

pub fn (mut t Time) from_json_string(raw_string string) ! {
	t = parse_rfc3339(raw_string) or { Time{} }
}
