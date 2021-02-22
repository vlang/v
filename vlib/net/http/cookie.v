// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module http

import time
import strings

pub struct Cookie {
pub mut:
	name        string
	value       string
	path        string // optional
	domain      string // optional
	expires     time.Time // optional
	raw_expires string // for reading cookies only. optional.
	// max_age=0 means no 'Max-Age' attribute specified.
	// max_age<0 means delete cookie now, equivalently 'Max-Age: 0'
	// max_age>0 means Max-Age attribute present and given in seconds
	max_age     int
	secure      bool
	http_only   bool
	same_site   SameSite
	raw         string
	unparsed    []string // Raw text of unparsed attribute-value pairs
}
// SameSite allows a server to define a cookie attribute making it impossible for
// the browser to send this cookie along with cross-site requests. The main
// goal is to mitigate the risk of cross-origin information leakage, and provide
// some protection against cross-site request forgery attacks.
//
// See https://tools.ietf.org/html/draft-ietf-httpbis-cookie-same-site-00 for details.
pub enum SameSite {
	same_site_default_mode = 1
	same_site_lax_mode
	same_site_strict_mode
	same_site_none_mode
}

// Parses all "Set-Cookie" values from the header `h` and
// returns the successfully parsed Cookies.
pub fn read_set_cookies(h map[string][]string) []&Cookie {
	cookies_s := h['Set-Cookie']
	cookie_count := cookies_s.len
	if cookie_count == 0 {
		return []
	}
	mut cookies := []&Cookie{}
	for _, line in cookies_s {
		mut parts := line.trim_space().split(';')
		if parts.len == 1 && parts[0] == '' {
			continue
		}
		parts[0] = parts[0].trim_space()
		keyval := parts[0].split('=')
		if keyval.len != 2 {
			continue
		}
		name := keyval[0]
		raw_value := keyval[1]
		if !is_cookie_name_valid(name) {
			continue
		}
		value := parse_cookie_value(raw_value, true) or {
			continue
		}
		mut c  := &Cookie{
			name: name,
			value: value,
			raw: line
		}
		for i, _ in parts {
			parts[i] = parts[i].trim_space()
			if parts[i].len == 0 {
				continue
			}
			mut attr := parts[i]
			mut raw_val := ''
			if attr.contains('=') {
				pieces := attr.split('=')
				attr = pieces[0]
				raw_val = pieces[1]
			}
			lower_attr := attr.to_lower()
			val := parse_cookie_value(raw_val, false) or {
				c.unparsed << parts[i]
				continue
			}
			match lower_attr {
				'samesite' {
					lower_val := val.to_lower()
					match lower_val {
						'lax' { c.same_site = .same_site_lax_mode }
						'strict' { c.same_site = .same_site_strict_mode }
						'none' { c.same_site = .same_site_none_mode }
						else { c.same_site = .same_site_default_mode }
					}
				}
				'secure' {
					c.secure = true
					continue
				}
				'httponly' {
					c.http_only = true
					continue
				}
				'domain' {
					c.domain = val
					continue
				}
				'max-age' {
					mut secs := val.int()
					if secs != 0 && val[0] != `0` {
						break
					}
					if secs <= 0 {
						secs = -1
					}
					c.max_age = secs
					continue
				}
				// TODO: Fix this once time works better
				// 'expires' {
				// 	c.raw_expires = val
				// 	mut exptime := time.parse_iso(val)
				// 	if exptime.year == 0 {
				// 		exptime = time.parse_iso('Mon, 02-Jan-2006 15:04:05 MST')
				// 	}
				// 	c.expires = exptime
				// 	continue
				// }
				'path' {
					c.path = val
					continue
				}
				else {
					c.unparsed << parts[i]
				}
			}
		}
		cookies << c
	}
	return cookies
}

// Parses all "Cookie" values from the header `h` and
// returns the successfully parsed Cookies.
//
// if `filter` isn't empty, only cookies of that name are returned
pub fn read_cookies(h map[string][]string, filter string) []&Cookie {
	lines := h['Cookie']
	if lines.len == 0 {
		return []
	}
	mut cookies := []&Cookie{}
	for _, line_ in lines {
		mut line := line_.trim_space()
		mut part := ''
		for line.len > 0 {
			if line.index_any(';') > 0 {
				line_parts := line.split(';')
				part = line_parts[0]
				line = line_parts[1]
			} else {
				part = line
				line = ''
			}
			part = part.trim_space()
			if part.len == 0 {
				continue
			}
			mut name := part
			mut val := ''
			if part.contains('=') {
				val_parts := part.split('=')
				name = val_parts[0]
				val = val_parts[1]
			}
			if !is_cookie_name_valid(name) {
				continue
			}
			if filter != '' && filter != name {
				continue
			}
			val = parse_cookie_value(val, true) or {
				continue
			}
			cookies << &Cookie{name: name, value: val}
		}
	}
	return cookies
}

// Returns the serialization of the cookie for use in a Cookie header
// (if only Name and Value are set) or a Set-Cookie response
// header (if other fields are set).
//
// If c.name is invalid, the empty string is returned.
pub fn (c &Cookie) str() string {
	if !is_cookie_name_valid(c.name) {
		return ''
	}
	// extra_cookie_length derived from typical length of cookie attributes
	// see RFC 6265 Sec 4.1.
	extra_cookie_length := 110
	mut b := strings.new_builder(c.name.len + c.value.len + c.domain.len + c.path.len + extra_cookie_length)
	b.write_string(c.name)
	b.write_string('=')
	b.write_string(sanitize_cookie_value(c.value))
	if c.path.len > 0 {
		b.write_string('; path=')
		b.write_string(sanitize_cookie_path(c.path))
	}
	if c.domain.len > 0 {
		if valid_cookie_domain(c.domain) {
			// A `domain` containing illegal characters is not
			// sanitized but simply dropped which turns the cookie
			// into a host-only cookie. A leading dot is okay
			// but won't be sent.
			mut d := c.domain
			if d[0] == `.` {
				d = d.substr(1, d.len)
			}
			b.write_string('; domain=')
			b.write_string(d)
		} else {
			// TODO: Log invalid cookie domain warning
		}
	}
	if c.expires.year > 1600 {
		e := c.expires
		time_str := '${e.weekday_str()}, ${e.day.str()} ${e.smonth()} ${e.year} ${e.hhmmss()} GMT'
		b.write_string('; expires=')
		b.write_string(time_str)
	}
	// TODO: Fix this. Techically a max age of 0 or less should be 0
	// We need a way to not have a max age.
	if c.max_age > 0 {
		b.write_string('; Max-Age=')
		b.write_string(c.max_age.str())
	} else if c.max_age < 0 {
		b.write_string('; Max-Age=0')
	}
	if c.http_only {
		b.write_string('; HttpOnly')
	}
	if c.secure {
		b.write_string('; Secure')
	}
	match c.same_site {
		.same_site_default_mode {
			b.write_string('; SameSite')
		}
		.same_site_none_mode {
			b.write_string('; SameSite=None')
		}
		.same_site_lax_mode {
			b.write_string('; SameSite=Lax')
		}
		.same_site_strict_mode {
			b.write_string('; SameSite=Strict')
		}
	}
	return b.str()
}

fn sanitize(valid fn(byte) bool, v string) string {
	mut ok := true
	for i in 0..v.len {
		if valid(v[i]) {
			continue
		}
		// TODO: Warn that we're dropping the invalid byte?
		ok = false
		break
	}
	if ok {
		return v.clone()
	}
	return v.bytes().filter(valid(it)).bytestr()
}

fn sanitize_cookie_name(name string) string {
	return name.replace_each(['\n', '-', '\r', '-'])
}

// https://tools.ietf.org/html/rfc6265#section-4.1.1
// cookie-value      = *cookie-octet / ( DQUOTE *cookie-octet DQUOTE )
// cookie-octet      = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
//           ; US-ASCII characters excluding CTLs,
//           ; whitespace DQUOTE, comma, semicolon,
//           ; and backslash
// We loosen this as spaces and commas are common in cookie values
// but we produce a quoted cookie-value in when value starts or ends
// with a comma or space.
pub fn sanitize_cookie_value(v string) string {
	val := sanitize(valid_cookie_value_byte, v)
	if v.len == 0 {
		return v
	}
	// Check for the existence of a space or comma
	if val.starts_with(' ') || val.ends_with(' ') || val.starts_with(',') || val.ends_with(',') {
		return '"$v"'
	}
	return v
}

fn sanitize_cookie_path(v string) string {
	return sanitize(valid_cookie_path_byte, v)
}

fn valid_cookie_value_byte(b byte) bool {
	return 0x20 <= b && b < 0x7f && b != `"` && b != `;` && b != `\\`
}

fn valid_cookie_path_byte(b byte) bool {
	return 0x20 <= b && b < 0x7f && b != `!`
}

fn valid_cookie_domain(v string) bool {
	if is_cookie_domain_name(v) {
		return true
	}
	// TODO
	// valid_ip := net.parse_ip(v) or {
	// 	false
	// }
	// if valid_ip {
	// 	return true
	// }
	return false
}

pub fn is_cookie_domain_name(_s string) bool {
	mut s := _s
	if s.len == 0 {
		return false
	}
	if s.len > 255 {
		return false
	}
	if s[0] == `.` {
		s = s.substr(1, s.len)
	}
	mut last := `.`
	mut ok := false
	mut part_len := 0
	for i, _ in s {
		c := s[i]
		if (`a` <= c && c <= `z`) || (`A` <= c && c <= `Z`) {
			// No '_' allowed here (in contrast to package net).
			ok = true
			part_len++
		} else if `0` <= c && c <= `9` {
			// fine
			part_len++
		} else if c == `-` {
			// Byte before dash cannot be dot.
			if last == `.` {
				return false
			}
			part_len++
		} else if c == `.` {
			// Byte before dot cannot be dot, dash.
			if last == `.` || last == `-` {
				return false
			}
			if part_len > 63 || part_len == 0 {
				return false
			}
			part_len = 0
		} else {
			 return false
		}
		last = c
	}
	if last == `-` || part_len > 63 {
		return false
	}
	return ok
}

fn parse_cookie_value(_raw string, allow_double_quote bool) ?string {
	mut raw := _raw
	// Strip the quotes, if present
	if allow_double_quote && raw.len > 1 && raw[0] == `"` && raw[raw.len - 1] == `"` {
		raw = raw.substr(1, raw.len - 1)
	}
	for i in 0..raw.len {
		if !valid_cookie_value_byte(raw[i]) {
			return error('http.cookie: invalid cookie value')
		}
	}
	return raw
}

fn is_cookie_name_valid(name string) bool {
	if name == '' {
		return false
	}
	for b in name {
		if b < 33 || b > 126 {
			return false
		}
	}
	return true
}
