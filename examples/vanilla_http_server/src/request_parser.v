module main

struct Slice {
	start int
	len   int
}

struct HttpRequest {
mut:
	buffer  []u8
	method  Slice
	path    Slice
	version Slice
}

@[direct_array_access]
fn parse_request_line(mut req HttpRequest) ! {
	mut i := 0
	// Parse HTTP method
	for i < req.buffer.len && req.buffer[i] != ` ` {
		i++
	}
	req.method = Slice{
		start: 0
		len:   i
	}
	i++

	// Parse path
	mut path_start := i
	for i < req.buffer.len && req.buffer[i] != ` ` {
		i++
	}
	req.path = Slice{
		start: path_start
		len:   i - path_start
	}
	i++

	// Parse HTTP version
	mut version_start := i
	for i < req.buffer.len && req.buffer[i] != `\r` {
		i++
	}
	req.version = Slice{
		start: version_start
		len:   i - version_start
	}

	// Move to the end of the request line
	if i + 1 < req.buffer.len && req.buffer[i] == `\r` && req.buffer[i + 1] == `\n` {
		i += 2
	} else {
		return error('Invalid HTTP request line')
	}
}

fn decode_http_request(buffer []u8) !HttpRequest {
	mut req := HttpRequest{
		buffer: buffer
	}

	parse_request_line(mut req)!

	return req
}

// Helper function to convert Slice to string for debugging
fn slice_to_string(buffer []u8, s Slice) string {
	return buffer[s.start..s.start + s.len].bytestr()
}
