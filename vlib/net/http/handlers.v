module http

// redirect returns a redirect response to `url` with the given status code
// (typically 301, 302, 303, 307 or 308), similar to Go's http.Redirect.
pub fn redirect(url string, code int) Response {
	mut r := Response{}
	r.header.set(.location, url)
	r.set_status(status_from_int(code))
	return r
}

// not_found returns a 404 Not Found response, similar to Go's http.NotFound.
pub fn not_found() Response {
	mut r := Response{}
	r.set_status(.not_found)
	r.body = '404 page not found'
	return r
}

// error_response returns an error response with the given status code and
// message, similar to Go's http.Error.
pub fn error_response(msg string, code int) Response {
	mut r := Response{}
	r.set_status(status_from_int(code))
	r.body = msg
	r.header.set(.content_type, 'text/plain; charset=utf-8')
	return r
}

// StripPrefixHandler wraps a handler, stripping a prefix from the request URL.
// Mirrors Go's http.StripPrefix.
pub struct StripPrefixHandler {
pub:
	prefix  string
	handler Handler
}

pub fn (mut s StripPrefixHandler) handle(req Request) Response {
	mut r := req
	if r.url.starts_with(s.prefix) {
		r.url = r.url[s.prefix.len..]
		if r.url == '' {
			r.url = '/'
		}
	}
	return s.handler.handle(r)
}

// strip_prefix wraps handler, stripping prefix from request URLs.
pub fn strip_prefix(prefix string, handler Handler) StripPrefixHandler {
	return StripPrefixHandler{ prefix: prefix, handler: handler }
}
