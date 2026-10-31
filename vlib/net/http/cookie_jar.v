module http

import net.urllib
import time

// CookieJar stores HTTP cookies and applies them to requests, following
// RFC 6265 (domain/path matching, expiry, Secure).
pub struct CookieJar {
mut:
	cookies []Cookie
}

// new_cookie_jar returns an empty cookie jar.
pub fn new_cookie_jar() CookieJar {
	return CookieJar{}
}

// host_of returns the host part of `raw_url` without a port number.
fn jar_host_of(raw_url string) string {
	u := urllib.parse(raw_url) or { return '' }
	mut host := u.host
	if host.starts_with('[') {
		if idx := host.index(']') {
			return host[..idx + 1].to_lower()
		}
		return host.to_lower()
	}
	if idx := host.last_index(':') {
		return host[..idx].to_lower()
	}
	return host.to_lower()
}

// path_of returns the path part of `raw_url`, defaulting to "/".
// The escaped form is used so encoded characters (especially encoded
// slashes) keep their meaning for cookie path matching (RFC 6265).
fn jar_path_of(raw_url string) string {
	u := urllib.parse(raw_url) or { return '/' }
	p := u.escaped_path()
	if p == '' {
		return '/'
	}
	return p
}

// scheme_of returns the scheme of `raw_url`.
fn jar_scheme_of(raw_url string) string {
	u := urllib.parse(raw_url) or { return '' }
	return u.scheme
}

// domain_matches reports whether `host` matches the cookie's domain.
fn domain_matches(host string, domain string) bool {
	if domain == '' {
		return true
	}
	d := domain.trim_left('.').to_lower()
	h := host.to_lower()
	if h == d {
		return true
	}
	return h.ends_with('.' + d)
}

// path_matches reports whether `request_path` matches the cookie's path.
fn path_matches(request_path string, cookie_path string) bool {
	cp := if cookie_path == '' { '/' } else { cookie_path }
	if request_path == cp {
		return true
	}
	if cp.ends_with('/') {
		return request_path.starts_with(cp)
	}
	if request_path.starts_with(cp) {
		// boundary: next char must be '/'
		if request_path.len > cp.len && request_path[cp.len] == `/` {
			return true
		}
	}
	return false
}

// set_cookies stores the cookies returned for `raw_url`. Existing cookies
// with the same name, domain and path are replaced.
pub fn (mut j CookieJar) set_cookies(raw_url string, cookies []Cookie) {
	host := jar_host_of(raw_url)
	req_path := jar_path_of(raw_url)
	now := time.now()
	for c in cookies {
		mut cookie := c
		// RFC 6265 §5.3 step 6: an explicit Domain must domain-match the
		// response host, otherwise the cookie is ignored (before any mutation).
		if cookie.domain != '' {
			if !domain_matches(host, cookie.domain) {
				continue
			}
		}
		if cookie.domain == '' {
			// host-only cookie: scope is the exact response host.
			cookie.domain = host
			cookie.host_only = true
		} else {
			// canonicalize: drop a leading dot, lowercase.
			cookie.domain = cookie.domain.trim_left('.').to_lower()
			cookie.host_only = false
		}
		if cookie.path == '' || !cookie.path.starts_with('/') {
			// default path: up to last '/' (RFC 6265 §5.2.4)
			if idx := req_path.last_index('/') {
				if idx == 0 {
					cookie.path = '/'
				} else {
					cookie.path = req_path[..idx]
				}
			} else {
				cookie.path = '/'
			}
		}
		// Max-Age takes precedence over Expires when present.
		if cookie.max_age > 0 {
			cookie.expires = now.add_seconds(cookie.max_age)
		}
		if cookie.max_age < 0 {
			// delete: remove matching cookies
			j.cookies = j.cookies.filter(!(it.name == cookie.name
				&& it.domain == cookie.domain && it.path == cookie.path))
			continue
		}
		// remove existing
		j.cookies = j.cookies.filter(!(it.name == cookie.name
			&& it.domain == cookie.domain && it.path == cookie.path))
		// secure check: only store secure cookies for https? Store anyway; filter on read.
		j.cookies << cookie
	}
	// drop expired
	j.cookies = j.cookies.filter(!(!it.expires.is_zero() && it.expires < now))
}

// cookies returns the cookies that should be sent with a request to `raw_url`.
pub fn (j CookieJar) cookies(raw_url string) []Cookie {
	host := jar_host_of(raw_url)
	req_path := jar_path_of(raw_url)
	scheme := jar_scheme_of(raw_url)
	now := time.now()
	mut out := []Cookie{}
	for c in j.cookies {
		if c.host_only {
			if host != c.domain {
				continue
			}
		} else if !domain_matches(host, c.domain) {
			continue
		}
		if !path_matches(req_path, c.path) {
			continue
		}
		if c.secure && scheme != 'https' {
			continue
		}
		if !c.expires.is_zero() && c.expires < now {
			continue
		}
		out << c
	}
	return out
}

// cookies_header returns a "Cookie" header value for `raw_url`.
pub fn (j CookieJar) cookies_header(raw_url string) string {
	cs := j.cookies(raw_url)
	mut parts := []string{}
	for c in cs {
		if !is_request_cookie_name_writable(c.name) {
			continue
		}
		parts << '${c.name}=${sanitize_cookie_value(c.value)}'
	}
	return parts.join('; ')
}
