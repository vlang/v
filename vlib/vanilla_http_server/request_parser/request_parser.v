module request_parser

pub struct Slice {
pub:
	start int
	len   int
}

pub struct HttpRequest {
pub mut:
	buffer  []u8
	method  Slice
	path    Slice
	version Slice
}

@[direct_array_access]
fn parse_http1_request_line(mut req HttpRequest) ! {
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

// decode_http_request decodes an HTTP request from a byte buffer.
// It parses the request line and populates the HttpRequest struct with method, path, and version.
pub fn decode_http_request(buffer []u8) !HttpRequest {
	mut req := HttpRequest{
		buffer: buffer
	}

	parse_http1_request_line(mut req)!

	return req
}

// Helper function to convert Slice to string for debugging
pub fn slice_to_string(buffer []u8, s Slice) string {
	return buffer[s.start..s.start + s.len].bytestr()
}

// get_header_value_slice retrieves the value of a header from the HTTP request buffer.
// It searches for the header name in the request buffer and returns its value as a Slice.
@[direct_array_access]
pub fn (req HttpRequest) get_header_value_slice(name string) ?Slice {
	mut pos := req.version.start + req.version.len + 2 // Start after request line (CRLF)
	if pos >= req.buffer.len {
		return none
	}

	for pos < req.buffer.len {
		if unsafe {
			vmemcmp(&req.buffer[pos], name.str, name.len)
		} == 0 {
			pos += name.len
			if req.buffer[pos] != `:` {
				return none
			}
			pos++
			for pos < req.buffer.len && (req.buffer[pos] == ` ` || req.buffer[pos] == `\t`) {
				pos++
			}
			if pos >= req.buffer.len {
				return none
			}
			mut start := pos
			for pos < req.buffer.len && req.buffer[pos] != `\r` {
				pos++
			}
			return Slice{
				start: start
				len:   pos - start
			}
		}
		if req.buffer[pos] == `\r` {
			pos++
			if pos < req.buffer.len && req.buffer[pos] == `\n` {
				pos++
			}
		} else {
			pos++
		}
	}

	return none
}
