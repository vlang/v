module http

import net.urllib

// ReverseProxy is a basic HTTP reverse proxy, similar to Go's
// httputil.ReverseProxy. It forwards an incoming request to a target URL and
// returns the response.
pub struct ReverseProxy {
pub mut:
	target string // base URL of the upstream, e.g. "http://localhost:8080"
}

// new_reverse_proxy creates a reverse proxy targeting `target`.
pub fn new_reverse_proxy(target string) ReverseProxy {
	return ReverseProxy{ target: target }
}

// director rewrites `req` so it targets the proxy's upstream. It sets the
// host and url fields.
pub fn (p ReverseProxy) director(mut req Request) {
	u := urllib.parse(p.target) or { return }
	req.host = u.host
	if !req.url.starts_with('http') {
		mut path := req.url
		if path.ends_with('?') {
			path = path[..path.len - 1]
		}
		req.url = p.target.trim_right('/') + path
	}
}

// serve_http forwards `req` to the upstream and returns the response.
pub fn (p ReverseProxy) serve_http(req Request) !Response {
	mut r := req
	p.director(mut r)
	return r.do() // need method
}
