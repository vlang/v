module http

import net.urllib

// redirect returns an empty HTTP/1.1 response with a Location header and the
// given status code (typically 301, 302, 303, 307 or 308).
pub fn redirect(url string, code int) Response {
	mut r := new_response(status: status_from_int(code))
	r.status_code = code
	r.header.set(.location, url)
	r.header.set(.content_length, '0')
	return r
}

// not_found returns an HTTP/1.1 404 response with a plain-text error body.
pub fn not_found() Response {
	return error_response('404 page not found', 404)
}

// error_response returns an HTTP/1.1 plain-text response with the given status
// code and message. The message is preserved verbatim, including any newlines.
pub fn error_response(msg string, code int) Response {
	mut r := new_response(status: status_from_int(code), body: msg)
	r.status_code = code
	r.header.set(.content_length, msg.len.str())
	r.header.set(.content_type, 'text/plain; charset=utf-8')
	r.header.set(.x_content_type_options, 'nosniff')
	return r
}

// StripPrefixHandler wraps a handler, stripping a literal request path prefix.
pub struct StripPrefixHandler {
pub:
	prefix string
pub mut:
	handler Handler
}

// handle removes the prefix from matching URL paths, preserving query strings
// and escaped path suffixes. Unmatched or malformed URLs receive a 404 response.
pub fn (mut s StripPrefixHandler) handle(req Request) Response {
	if s.prefix == '' {
		return s.handler.handle(req)
	}
	mut url := urllib.parse_request_uri(req.url) or { return not_found() }
	if !url.path.starts_with(s.prefix)
		|| (url.raw_path != '' && !url.raw_path.starts_with(s.prefix)) {
		return not_found()
	}
	url.path = url.path[s.prefix.len..]
	if url.raw_path != '' {
		url.raw_path = url.raw_path[s.prefix.len..]
	}
	if url.path == '' {
		url.path = '/'
	}
	mut r := req
	r.url = url.str()
	return s.handler.handle(r)
}

// strip_prefix wraps handler, removing prefix from matching request URL paths.
// An empty prefix passes requests through unchanged. An empty resulting path
// becomes "/". The prefix is literal, so use a trailing slash for a directory.
pub fn strip_prefix(prefix string, handler Handler) StripPrefixHandler {
	return StripPrefixHandler{ prefix: prefix, handler: handler }
}
