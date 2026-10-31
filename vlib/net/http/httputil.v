module http

import strings

// dump_request returns a string representation of `req`, similar to Go's
// httputil.DumpRequest. When `body` is true the request body is included.
pub fn dump_request(req Request, body bool) string {
	mut sb := strings.new_builder(256)
	sb.write_string(req.method.str())
	sb.write_byte(` `)
	sb.write_string(if req.url != '' { req.url } else { '/' })
	sb.write_byte(` `)
	sb.write_string(req.version.str())
	sb.write_string('\r\n')
	sb.write_string(req.header.str())
	sb.write_string('\r\n')
	if body && req.data != '' {
		sb.write_string(req.data)
		if !req.data.ends_with('\n') {
			sb.write_string('\r\n')
		}
	}
	return sb.str()
}

// dump_response returns a string representation of `resp`, similar to Go's
// httputil.DumpResponse. When `body` is true the response body is included.
pub fn dump_response(resp Response, body bool) string {
	if body {
		return resp.bytestr()
	}
	mut sb := strings.new_builder(256)
	sb.write_string('HTTP/${resp.http_version}')
	sb.write_byte(` `)
	sb.write_string(resp.status_code.str())
	if resp.status_msg != '' {
		sb.write_byte(` `)
		sb.write_string(resp.status_msg)
	}
	sb.write_string('\r\n')
	sb.write_string(resp.header.str())
	sb.write_string('\r\n')
	return sb.str()
}
