module http

import strings

// dump_request returns an HTTP/1-style diagnostic representation of `req`.
// It uses the stored method, URL (or '/' when empty), version and headers;
// it does not apply transport defaults or rewrite the URL. When `body` is true,
// the stored body is included without changing its bytes. `req` is not modified.
pub fn dump_request(req Request, body bool) string {
	mut sb := strings.new_builder(256)
	sb.write_string(req.method.str())
	sb.write_byte(` `)
	sb.write_string(if req.url != '' { req.url } else { '/' })
	sb.write_byte(` `)
	sb.write_string(req.version.str())
	sb.write_string('\r\n')
	sb.write_string(req.header.render(version: req.version))
	sb.write_string('\r\n')
	if body {
		sb.write_string(req.data)
	}
	return sb.str()
}

// dump_response returns the same diagnostic representation as `resp.bytestr()`.
// When `body` is false, only the body is omitted; the status line and headers,
// including Content-Length, are unchanged. `resp` is not modified.
pub fn dump_response(resp Response, body bool) string {
	if body {
		return resp.bytestr()
	}
	mut without_body := resp
	without_body.body = ''
	return without_body.bytestr()
}
