module http

import time

fn test_cookie_jar_empty() {
	j := new_cookie_jar()
	assert j.cookies('https://example.com/') == []
	assert j.cookies_header('https://example.com/') == ''
}

fn test_cookie_jar_store_and_retrieve() {
	mut j := new_cookie_jar()
	j.set_cookies('https://example.com/path', [
		Cookie{ name: 'a', value: '1' },
		Cookie{ name: 'b', value: '2', path: '/path' },
	])
	cs := j.cookies('https://example.com/path')
	assert cs.len == 2
	assert j.cookies_header('https://example.com/path') == 'a=1; b=2'
}

fn test_cookie_jar_domain_matching() {
	mut j := new_cookie_jar()
	j.set_cookies('https://www.example.com/', [
		Cookie{ name: 'id', value: 'x', domain: 'example.com' },
	])
	// subdomain matches
	assert j.cookies('https://api.example.com/').len == 1
	// unrelated host does not
	assert j.cookies('https://other.com/').len == 0
}

fn test_cookie_jar_path_matching() {
	mut j := new_cookie_jar()
	j.set_cookies('https://example.com/foo/bar', [
		Cookie{ name: 'p', value: 'v', path: '/foo' },
	])
	assert j.cookies('https://example.com/foo').len == 1
	assert j.cookies('https://example.com/foo/bar').len == 1
	assert j.cookies('https://example.com/foobar').len == 0
	assert j.cookies('https://example.com/').len == 0
}

fn test_cookie_jar_secure() {
	mut j := new_cookie_jar()
	j.set_cookies('https://example.com/', [
		Cookie{ name: 's', value: 'v', secure: true },
	])
	assert j.cookies('https://example.com/').len == 1
	assert j.cookies('http://example.com/').len == 0
}

fn test_cookie_jar_expiry_max_age() {
	mut j := new_cookie_jar()
	j.set_cookies('https://example.com/', [
		Cookie{ name: 'e', value: 'v', max_age: -1 },
	])
	// max_age < 0 deletes immediately
	assert j.cookies('https://example.com/').len == 0
}

fn test_cookie_jar_replace() {
	mut j := new_cookie_jar()
	j.set_cookies('https://example.com/', [Cookie{ name: 'a', value: '1' }])
	j.set_cookies('https://example.com/', [Cookie{ name: 'a', value: '2' }])
	cs := j.cookies('https://example.com/')
	assert cs.len == 1
	assert cs[0].value == '2'
}

fn test_cookie_jar_expired_dropped() {
	mut j := new_cookie_jar()
	j.set_cookies('https://example.com/', [
		Cookie{ name: 'old', value: 'v', expires: time.unix(1) },
	])
	assert j.cookies('https://example.com/').len == 0
}

fn cookie_jar_response_cookies(lines []string) []Cookie {
	fields := lines.map('Set-Cookie: ${it}\r\n').join('')
	response := parse_response('HTTP/1.1 200 OK\r\n${fields}Content-Length: 0\r\n\r\n') or {
		panic(err)
	}
	cookies := response.cookies()
	assert cookies.len == lines.len
	return cookies
}

fn test_cookie_jar_host_only_and_explicit_domain_scope() {
	mut j := new_cookie_jar()
	j.set_cookies('https://EXAMPLE.COM/', cookie_jar_response_cookies([
		'host=secret; Path=/',
		'domain=shared; Domain=.EXAMPLE.COM; Path=/',
	]))
	assert j.cookies_header('https://example.com/') == 'host=secret; domain=shared'
	assert j.cookies_header('https://example.com:8443/') == 'host=secret; domain=shared'
	assert j.cookies_header('https://sub.example.com/') == 'domain=shared'
	assert j.cookies('https://notexample.com/').len == 0
}

fn test_cookie_jar_foreign_domain_cannot_insert_replace_or_delete() {
	for line in [
		'injected=yes; Domain=victim.test; Path=/',
		'sid=attacker; Domain=victim.test; Path=/',
		'sid=; Domain=victim.test; Path=/; Max-Age=0',
		'sid=; Domain=victim.test; Path=/; Expires=Tue, 10 Nov 2009 23:00:00 GMT',
	] {
		mut j := new_cookie_jar()
		j.set_cookies('https://victim.test/', cookie_jar_response_cookies([
			'sid=legitimate; Path=/',
		]))
		j.set_cookies('https://attacker.test/', cookie_jar_response_cookies([line]))
		assert j.cookies_header('https://victim.test/') == 'sid=legitimate', line
		assert j.cookies('https://attacker.test/').len == 0, line
	}
}

fn test_cookie_jar_canonical_domain_replacement_and_deletion() {
	mut j := new_cookie_jar()
	j.set_cookies('https://EXAMPLE.COM/', cookie_jar_response_cookies([
		'sid=first; Domain=.EXAMPLE.COM; Path=/',
	]))
	j.set_cookies('https://example.com/', cookie_jar_response_cookies([
		'sid=second; Domain=example.com; Path=/',
	]))
	assert j.cookies_header('https://example.com/') == 'sid=second'
	assert j.cookies_header('https://sub.example.com/') == 'sid=second'
	j.set_cookies('https://Example.Com/', cookie_jar_response_cookies([
		'sid=; Domain=.EXAMPLE.COM; Path=/; Max-Age=0',
	]))
	assert j.cookies('https://example.com/').len == 0
	assert j.cookies('https://sub.example.com/').len == 0
}

fn test_cookie_jar_quoted_values_survive_a_header_round_trip() {
	for value in [' a', 'a ', ' ', ',', 'a,b', ' a, b '] {
		mut j := new_cookie_jar()
		j.set_cookies('https://example.com/', cookie_jar_response_cookies([
			'sid="${value}"; Path=/; Secure; HttpOnly',
		]))
		header_value := j.cookies_header('https://example.com/')
		assert header_value == 'sid="${value}"'
		mut header := Header{}
		header.add(.cookie, header_value)
		cookies := read_cookies(header, '')
		assert cookies.len == 1
		assert cookies[0].name == 'sid'
		assert cookies[0].value == value
	}
}

fn test_cookie_jar_header_omits_unsafe_names() {
	for name in ['', 'bad name', 'a;b', 'a=b', 'x\r\nInjected:1'] {
		mut j := new_cookie_jar()
		j.set_cookies('https://example.com/', [
			Cookie{ name: 'safe', value: 'ok' },
			Cookie{ name: name, value: 'injected' },
		])
		assert j.cookies_header('https://example.com/') == 'safe=ok', name.bytes().str()
	}
}

fn test_cookie_jar_header_sanitizes_values_without_adding_cookies() {
	for value, expected in {
		'a\r\nInjected:1': 'aInjected:1'
		'a; admin=1':      'a admin=1'
		'a"b\\c\x00':      'abc'
	} {
		mut j := new_cookie_jar()
		j.set_cookies('https://example.com/', [Cookie{ name: 'sid', value: value }])
		header_value := j.cookies_header('https://example.com/')
		assert !header_value.contains_any('\r\n'), value.bytes().str()
		mut header := Header{}
		header.add(.cookie, header_value)
		cookies := read_cookies(header, '')
		assert cookies.len == 1, value.bytes().str()
		assert cookies[0].name == 'sid'
		assert cookies[0].value == expected, value.bytes().str()
	}
}

fn test_cookie_jar_matches_escaped_paths() {
	mut j := new_cookie_jar()
	j.set_cookies('https://example.com/foo%20bar/login', cookie_jar_response_cookies([
		'sid=value; Path=/foo%20bar',
	]))
	assert j.cookies_header('https://example.com/foo%20bar') == 'sid=value'
	assert j.cookies_header('https://example.com/foo%20bar/account') == 'sid=value'
	assert j.cookies('https://example.com/foo%2520bar').len == 0
	assert j.cookies('https://example.com/foo%20barista').len == 0
}

fn test_cookie_jar_encoded_slash_is_not_a_path_boundary() {
	mut j := new_cookie_jar()
	j.set_cookies('https://example.com/a/login', cookie_jar_response_cookies([
		'sid=value; Path=/a',
	]))
	assert j.cookies_header('https://example.com/a/b') == 'sid=value'
	assert j.cookies('https://example.com/a%2Fb').len == 0
}

fn test_cookie_jar_default_path_preserves_encoded_slashes() {
	mut j := new_cookie_jar()
	j.set_cookies('https://example.com/a%2Fb/login', cookie_jar_response_cookies(['sid=value']))
	assert j.cookies_header('https://example.com/a%2Fb/account') == 'sid=value'
	assert j.cookies('https://example.com/a/b/account').len == 0
	assert j.cookies('https://example.com/a%2Fbc/account').len == 0
	// The encoded slash cannot turn a single path segment into a directory.
	mut root := new_cookie_jar()
	root.set_cookies('https://example.com/a%2Fb', cookie_jar_response_cookies(['sid=value']))
	assert root.cookies_header('https://example.com/elsewhere') == 'sid=value'
}

fn test_cookie_jar_missing_empty_and_relative_paths_use_the_default() {
	for path_attribute in ['', '; Path=', '; Path=relative'] {
		mut j := new_cookie_jar()
		j.set_cookies('https://example.com/app/login', cookie_jar_response_cookies([
			'sid=value${path_attribute}',
		]))
		assert j.cookies_header('https://example.com/app') == 'sid=value', path_attribute
		assert j.cookies_header('https://example.com/app/account') == 'sid=value', path_attribute
		assert j.cookies('https://example.com/application').len == 0, path_attribute
		assert j.cookies('https://example.com/').len == 0, path_attribute
	}
}

fn test_cookie_jar_malformed_domains_cannot_insert_replace_or_delete() {
	for domain in ['.', '..', '..example.com', 'example.com.'] {
		origin := if domain in ['.', '..'] {
			'https://evil.example./'
		} else {
			'https://example.com./'
		}
		for operation in ['sid=attacker', 'sid=; Max-Age=0',
			'sid=; Expires=Tue, 10 Nov 2009 23:00:00 GMT'] {
			mut j := new_cookie_jar()
			j.set_cookies(origin, cookie_jar_response_cookies(['sid=legitimate; Path=/']))
			line := '${operation}; Domain=${domain}; Path=/'
			j.set_cookies(origin, cookie_jar_response_cookies([line]))
			assert j.cookies_header(origin) == 'sid=legitimate', line
			assert j.cookies('https://victim.test/').len == 0, line
		}
	}
}

fn test_cookie_jar_ipv4_domains_require_an_exact_match() {
	mut j := new_cookie_jar()
	j.set_cookies('https://127.0.0.1:8443/', cookie_jar_response_cookies([
		'host=only; Path=/',
		'exact=only; Domain=127.0.0.1; Path=/',
		'suffix=rejected; Domain=0.1; Path=/',
	]))
	assert j.cookies_header('https://127.0.0.1/') == 'host=only; exact=only'
	assert j.cookies('https://sub.127.0.0.1/').len == 0
	assert j.cookies('https://10.0.0.1/').len == 0
}

fn test_cookie_jar_rejects_invalid_and_non_http_urls() {
	for raw_url in ['', '/', '/relative/path', 'example.com', 'ftp://example.com/',
		'https:///missing-host', 'https:example.com', 'http://', 'https://example.com:bad/',
		'https://example.com/%zz', 'https://[::1'] {
		mut j := new_cookie_jar()
		j.set_cookies('https://example.com/', cookie_jar_response_cookies(['sid=legitimate; Path=/']))
		j.set_cookies(raw_url, cookie_jar_response_cookies([
			'sid=attacker; Domain=example.com; Path=/',
			'injected=yes; Path=/',
		]))
		assert j.cookies_header('https://example.com/') == 'sid=legitimate', raw_url
		assert j.cookies(raw_url).len == 0, raw_url
		assert j.cookies('https://victim.test/').len == 0, raw_url
		j.set_cookies(raw_url, cookie_jar_response_cookies([
			'sid=; Domain=example.com; Path=/; Max-Age=0',
		]))
		assert j.cookies_header('https://example.com/') == 'sid=legitimate', raw_url
	}
}

fn test_cookie_jar_ipv6_domains_ignore_brackets_and_ports() {
	mut j := new_cookie_jar()
	j.set_cookies('http://[::1]:8080/', cookie_jar_response_cookies([
		'host=only; Path=/',
		'exact=only; Domain=::1; Path=/',
	]))
	assert j.cookies_header('http://[::1]:9090/') == 'host=only; exact=only'
	assert j.cookies_header('https://[::1]/') == 'host=only; exact=only'
	assert j.cookies('http://[::2]:8080/').len == 0
}

fn test_cookie_jar_ipv6_zone_identifiers_remain_case_sensitive() {
	mut j := new_cookie_jar()
	j.set_cookies('http://[FE80::1%25ETH0]:8080/', cookie_jar_response_cookies([
		'host=only; Path=/',
		'exact=only; Domain=fe80::1%ETH0; Path=/',
	]))
	assert j.cookies_header('http://[fe80::1%25ETH0]/') == 'host=only; exact=only'
	assert j.cookies('http://[fe80::1%25eth0]/').len == 0
}

fn test_cookie_jar_normalizes_one_trailing_hostname_dot() {
	for origin in ['https://EXAMPLE.COM./', 'https://example.com/'] {
		mut j := new_cookie_jar()
		j.set_cookies(origin, cookie_jar_response_cookies([
			'host=only; Path=/',
			'domain=shared; Domain=example.com; Path=/',
		]))
		for target in ['https://example.com/', 'https://EXAMPLE.COM./', 'http://example.com.:8080/'] {
			assert j.cookies_header(target) == 'host=only; domain=shared', '${origin} -> ${target}'
		}
		assert j.cookies_header('https://sub.example.com./') == 'domain=shared'
	}
}
