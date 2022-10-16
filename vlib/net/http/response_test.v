module http

fn test_response_bytestr() {
	{
		resp := new_response(
			status: .ok
			text: 'Foo' // TODO: replace with `body` once deprecaped
		)
		assert resp.bytestr() == 'HTTP/1.1 200 OK\r\n' + 'Content-Length: 3\r\n' + '\r\n' + 'Foo'
	}
	{
		resp := new_response(
			status: .found
			body: 'Foo'
			header: new_header(key: .location, value: '/')
		)
		lines := resp.bytestr().split_into_lines()
		assert lines[0] == 'HTTP/1.1 302 Found'
		// header order is not guaranteed
		check_headers(['Location: /', 'Content-Length: 3'], lines[1..3])?
		assert lines[3] == ''
		assert lines[4] == 'Foo'
	}
}

// check_headers is a helper function for asserting all expected headers
// are found because rendered header order is not guaranteed. The check
// is O(n^2) which is fine for small lists.
fn check_headers(expected []string, found []string) ! {
	assert expected.len == found.len
	for header in expected {
		if !found.contains(header) {
			return error('expected header "$header" not in $found')
		}
	}
}
