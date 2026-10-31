import net.http
import time

// set_cookie returns the cookie that `Response.cookies()` reads from a response
// with the single field `Set-Cookie: <line>`.
fn set_cookie(line string) http.Cookie {
	response := http.parse_response('HTTP/1.1 200 OK\r\nSet-Cookie: ${line}\r\nContent-Length: 0\r\n\r\n') or {
		panic('parse_response failed for `${line}`: ${err}')
	}
	cookies := response.cookies()
	assert cookies.len == 1, line
	return cookies[0]
}

// assert_attributes_after_max_age_are_read checks the attributes that the test lines
// put after `Max-Age`: none of them may be lost, whatever the value of `Max-Age` is.
fn assert_attributes_after_max_age_are_read(c http.Cookie, line string) {
	assert c.name == 'a', line
	assert c.value == 'b', line
	assert c.http_only, line
	assert c.secure, line
	assert c.same_site == .same_site_lax_mode, line
}

fn test_set_cookie_max_age_is_read() {
	// A value of zero or less means "delete now", which `Cookie.max_age` stores as -1.
	expected := {
		'1':          1
		'3600':       3600
		'+5':         5
		'2147483647': 2147483647
		'0':          -1
		'00':         -1
		'-0':         -1
		'-1':         -1
		'-3600':      -1
	}
	for name in ['Max-Age', 'max-age', 'MAX-AGE'] {
		for val, max_age in expected {
			line := 'a=b; ${name}=${val}; HttpOnly; Secure; SameSite=Lax'
			c := set_cookie(line)
			assert c.max_age == max_age, line
			assert c.unparsed.len == 0, line
			assert_attributes_after_max_age_are_read(c, line)
		}
	}
}

fn test_set_cookie_max_age_that_is_not_an_integer_stays_unparsed() {
	// `03`: a number other than zero must not start with `0`.
	for val in ['03', '007', 'abc', '12abc', 'abc12', '', '+', '-', '--5', '1_000', '0x10', '3.5',
		'1e3', '5 0', '99999999999999999999', '-99999999999999999999'] {
		line := 'a=b; Max-Age=${val}; HttpOnly; Secure; SameSite=Lax'
		c := set_cookie(line)
		assert c.max_age == 0, line
		assert c.unparsed == ['Max-Age=${val}'], line
		assert_attributes_after_max_age_are_read(c, line)
	}
	line := 'a=b; Max-Age; HttpOnly; Secure; SameSite=Lax'
	c := set_cookie(line)
	assert c.max_age == 0
	assert c.unparsed == ['Max-Age']
	assert_attributes_after_max_age_are_read(c, line)
}

fn test_set_cookie_max_age_wider_than_32_bits_is_not_clamped() {
	c := set_cookie('a=b; Max-Age=3153600000; Secure')
	assert c.secure
	if sizeof(int) > 4 {
		assert i64(c.max_age) == i64(3153600000)
		assert c.unparsed.len == 0
	} else {
		assert c.max_age == 0
		assert c.unparsed == ['Max-Age=3153600000']
	}
}

fn test_set_cookie_max_age_at_any_position() {
	for line in [
		'a=b; Max-Age=60; Path=/x; Domain=example.com; HttpOnly; Secure; SameSite=Strict',
		'a=b; Path=/x; Domain=example.com; Max-Age=60; HttpOnly; Secure; SameSite=Strict',
		'a=b; Path=/x; Domain=example.com; HttpOnly; Secure; SameSite=Strict; Max-Age=60',
		'a=b;Max-Age=60;Path=/x;Domain=example.com;HttpOnly;Secure;SameSite=Strict',
	] {
		c := set_cookie(line)
		assert c.max_age == 60, line
		assert c.path == '/x', line
		assert c.domain == 'example.com', line
		assert c.http_only, line
		assert c.secure, line
		assert c.same_site == .same_site_strict_mode, line
		assert c.unparsed.len == 0, line
	}
}

fn test_set_cookie_repeated_max_age() {
	later := set_cookie('a=b; Max-Age=60; Max-Age=120')
	assert later.max_age == 120
	assert later.unparsed.len == 0
	// A value that cannot be read does not replace one that was.
	invalid := set_cookie('a=b; Max-Age=60; Max-Age=bogus; Secure')
	assert invalid.max_age == 60
	assert invalid.secure
	assert invalid.unparsed == ['Max-Age=bogus']
}

fn test_set_cookie_named_like_an_attribute_sets_no_attribute() {
	values := {
		'path':     'zzz'
		'Path':     '/'
		'domain':   'example.com'
		'secure':   '1'
		'httponly': 'x'
		'samesite': 'lax'
		'max-age':  '5'
		'Max-Age':  '0'
		'expires':  'Tue, 10 Nov 2009 23:00:00 GMT'
	}
	for name, value in values {
		line := '${name}=${value}'
		c := set_cookie(line)
		assert c.name == name, line
		assert c.value == value, line
		assert c.path == '', line
		assert c.domain == '', line
		assert c.expires.year == 0, line
		assert c.raw_expires == '', line
		assert c.max_age == 0, line
		assert !c.secure, line
		assert !c.http_only, line
		assert c.same_site == .same_site_not_set, line
		assert c.unparsed.len == 0, line
	}
}

fn test_set_cookie_named_like_an_attribute_keeps_its_real_attributes() {
	c := set_cookie('path=zzz; Path=/real; Max-Age=7; Secure')
	assert c.name == 'path'
	assert c.value == 'zzz'
	assert c.path == '/real'
	assert c.max_age == 7
	assert c.secure
	assert !c.http_only
	assert c.unparsed.len == 0
}

fn test_set_cookie_unparsed_holds_only_what_was_not_read() {
	assert set_cookie('a=b').unparsed.len == 0
	// The value of the cookie itself may be quoted; that is not an unread attribute.
	quoted := set_cookie('a="b c"; Secure')
	assert quoted.value == 'b c'
	assert quoted.unparsed.len == 0
	known :=
		set_cookie('a=b; Path=/; Domain=example.com; Expires=Tue, 10 Nov 2009 23:00:00 GMT; Max-Age=1; HttpOnly; Secure; SameSite=None')
	assert known.unparsed.len == 0
	unknown := set_cookie('a=b; Path=/; foo=bar; Secure; baz; Max-Age=x; Domain="quoted"')
	assert unknown.path == '/'
	assert unknown.secure
	assert unknown.domain == ''
	assert unknown.unparsed == ['foo=bar', 'baz', 'Max-Age=x', 'Domain="quoted"']
}

fn test_cookie_str_is_read_back_by_response_cookies() {
	for same_site in [http.SameSite.same_site_lax_mode, .same_site_strict_mode, .same_site_none_mode] {
		for max_age in [3600, 1, 0, -1] {
			c := http.Cookie{
				name:      'sid'
				value:     'abc123'
				path:      '/app'
				domain:    'example.com'
				expires:   time.unix(1257894000)
				max_age:   max_age
				secure:    true
				http_only: true
				same_site: same_site
			}
			line := c.str()
			parsed := set_cookie(line)
			assert parsed.name == c.name, line
			assert parsed.value == c.value, line
			assert parsed.path == c.path, line
			assert parsed.domain == c.domain, line
			assert parsed.expires.unix() == c.expires.unix(), line
			assert parsed.max_age == c.max_age, line
			assert parsed.secure, line
			assert parsed.http_only, line
			assert parsed.same_site == c.same_site, line
			assert parsed.unparsed.len == 0, line
			assert parsed.str() == line
		}
	}
	// Every negative `max_age` is written as `Max-Age=0`, which is read back as -1.
	negative := http.Cookie{
		name:    'sid'
		value:   'x'
		max_age: -3600
	}
	expired := set_cookie(negative.str())
	assert expired.max_age == -1
	assert expired.unparsed.len == 0
}
