module http

import net.urllib
import time

// CookieJar stores HTTP cookies with domain/path matching, expiry and Secure handling.
// It does not check a public suffix list.
pub struct CookieJar {
mut:
	cookies []Cookie
}

// new_cookie_jar returns an empty cookie jar.
pub fn new_cookie_jar() CookieJar {
	return CookieJar{}
}

// jar_host_of returns the canonical host of an absolute HTTP(S) URL, or an empty string.
fn jar_host_of(raw_url string) string {
	u := urllib.parse(raw_url) or { return '' }
	if u.scheme != 'http' && u.scheme != 'https' {
		return ''
	}
	host := u.hostname()
	if host.contains(':') {
		return jar_lower_host(host)
	}
	return host.trim_string_right('.').to_lower()
}

// jar_lower_host preserves the case-sensitive zone identifier of a scoped IPv6 address.
fn jar_lower_host(host string) string {
	if host.contains(':') {
		if zone := host.index('%') {
			return host[..zone].to_lower() + host[zone..]
		}
	}
	return host.to_lower()
}

// jar_path_of returns the path part of `raw_url`, defaulting to "/".
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

// jar_scheme_of returns the scheme of `raw_url`.
fn jar_scheme_of(raw_url string) string {
	u := urllib.parse(raw_url) or { return '' }
	return u.scheme
}

// domain_matches compares canonical hosts and domains; IP addresses never suffix-match.
fn domain_matches(host string, domain string) bool {
	if host == '' || domain == '' {
		return false
	}
	if host == domain {
		return true
	}
	return !is_ipv4_literal(host) && !host.contains_any(':%') && host.ends_with('.' + domain)
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
// Invalid, hostless and non-HTTP(S) URLs are ignored, as are invalid or unrelated domains.
pub fn (mut j CookieJar) set_cookies(raw_url string, cookies []Cookie) {
	host := jar_host_of(raw_url)
	if host == '' {
		return
	}
	req_path := jar_path_of(raw_url)
	now := time.now()
	for c in cookies {
		mut cookie := c
		if cookie.domain == '' {
			// host-only cookie: scope is the exact response host.
			cookie.domain = host
			cookie.host_only = true
		} else {
			// Remove only the single optional leading dot. Reject malformed domains
			// before replacement or deletion can mutate an existing cookie.
			cookie.domain = jar_lower_host(cookie.domain.trim_string_left('.'))
			if cookie.domain == '' || cookie.domain.starts_with('.') || cookie.domain.ends_with('.') {
				continue
			}
			if is_ipv4_literal(host) || host.contains(':') {
				if cookie.domain != host {
					continue
				}
				cookie.host_only = true
			} else {
				if !valid_cookie_domain(cookie.domain) || !domain_matches(host, cookie.domain) {
					continue
				}
				cookie.host_only = false
			}
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
		// Secure controls transmission; cookies are filtered when read.
		j.cookies << cookie
	}
	// drop expired
	j.cookies = j.cookies.filter(!(!it.expires.is_zero() && it.expires < now))
}

// cookies returns the cookies that should be sent with a request to `raw_url`.
// Invalid, hostless and non-HTTP(S) URLs return no cookies.
pub fn (j CookieJar) cookies(raw_url string) []Cookie {
	host := jar_host_of(raw_url)
	if host == '' {
		return []
	}
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
