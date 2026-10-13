import net.http
import time

// RFC 6265 section 4.1.1 spells the Set-Cookie attributes `Path`, `Domain` and `Expires`,
// next to `Max-Age`, `HttpOnly` and `Secure`. Attribute names are case-insensitive for a
// reader (section 5.2), so `Response.cookies` has to accept any spelling.

fn parse_set_cookie(line string) !http.Cookie {
	response :=
		http.parse_response('HTTP/1.1 200 OK\r\nSet-Cookie: ${line}\r\nContent-Length: 0\r\n\r\n')!
	cookies := response.cookies()
	assert cookies.len == 1, line
	return cookies[0]
}

fn test_cookie_str_writes_every_attribute_in_the_rfc_6265_spelling() {
	cookie := http.Cookie{
		name:      'sid'
		value:     'abc'
		path:      '/p'
		domain:    'example.com'
		expires:   time.unix(1257894000)
		max_age:   3600
		http_only: true
		secure:    true
		same_site: .same_site_lax_mode
	}
	assert cookie.str() == 'sid=abc; Path=/p; Domain=example.com; Expires=Tue, 10 Nov 2009 23:00:00 GMT; Max-Age=3600; HttpOnly; Secure; SameSite=Lax'
}

fn test_cookie_str_writes_path_domain_and_expires_alone() {
	path_cookie := http.Cookie{
		name: 'c8'
		path: '/p'
	}
	assert path_cookie.str() == 'c8=; Path=/p'
	domain_cookie := http.Cookie{
		name:   'c8'
		domain: 'example.com'
	}
	assert domain_cookie.str() == 'c8=; Domain=example.com'
	expires_cookie := http.Cookie{
		name:    'c8'
		expires: time.unix(1257894000)
	}
	assert expires_cookie.str() == 'c8=; Expires=Tue, 10 Nov 2009 23:00:00 GMT'
}

fn test_response_cookies_read_path_domain_and_expires_in_any_case() {
	for attribute in ['Path', 'path', 'PATH'] {
		cookie := parse_set_cookie('a=b; ${attribute}=/p')!
		assert cookie.path == '/p', attribute
		assert '${attribute}=/p' !in cookie.unparsed, attribute
	}
	for attribute in ['Domain', 'domain', 'DOMAIN'] {
		cookie := parse_set_cookie('a=b; ${attribute}=example.com')!
		assert cookie.domain == 'example.com', attribute
		assert '${attribute}=example.com' !in cookie.unparsed, attribute
	}
	for attribute in ['Expires', 'expires', 'EXPIRES'] {
		cookie := parse_set_cookie('a=b; ${attribute}=Tue, 10 Nov 2009 23:00:00 GMT')!
		assert cookie.expires.unix() == 1257894000, attribute
		assert cookie.raw_expires == 'Tue, 10 Nov 2009 23:00:00 GMT', attribute
		assert '${attribute}=Tue, 10 Nov 2009 23:00:00 GMT' !in cookie.unparsed, attribute
	}
}

fn test_response_cookies_read_back_path_domain_and_expires_written_by_str() {
	cookie := http.Cookie{
		name:    'sid'
		value:   'abc'
		path:    '/p'
		domain:  'example.com'
		expires: time.unix(1257894000)
	}
	for line in [cookie.str(),
		'sid=abc; path=/p; domain=example.com; expires=Tue, 10 Nov 2009 23:00:00 GMT',
		'sid=abc; PATH=/p; DOMAIN=example.com; EXPIRES=Tue, 10 Nov 2009 23:00:00 GMT'] {
		parsed := parse_set_cookie(line)!
		assert parsed.name == 'sid', line
		assert parsed.value == 'abc', line
		assert parsed.path == '/p', line
		assert parsed.domain == 'example.com', line
		assert parsed.expires.unix() == 1257894000, line
	}
}
