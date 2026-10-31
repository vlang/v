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
