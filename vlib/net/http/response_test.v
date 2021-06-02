module http

fn test_parse_response_line() ? {
	if _ := parse_response_line('hello world') {
		panic('should be error')
	}
	mut version, mut status := parse_response_line('http/1.1 200 OK') or {
		panic('should not error')
	}
	assert version == .v1_1
	assert status == .ok

	version, status = parse_response_line('http/1.1 200 any string') or {
		panic('should not error')
	}
	assert version == .v1_1
	assert status == .ok

	// TODO: should this not parse?
	version, status = parse_response_line('abc 123') or {
		panic('should not error')
	}
	assert version == .unknown
	assert status == .unassigned
}
