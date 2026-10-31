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
