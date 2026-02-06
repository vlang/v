import net.http

fn test_https_get() {
	$if !network ? {
		return
	}
	assert http.get_text('https://vlang.io/version') == '0.1.5'
	println('https ok')
}

fn test_http_get_from_vlang_utc_now() {
	$if !network ? {
		return
	}
	url := 'http://vlang.io/utc_now'
	println('Test getting current time from HTTP ${url} by http.get')
	res := http.get(url) or { panic(err) }
	assert res.status() == .ok
	assert res.body != ''
	assert res.body.int() > 1566403696
	println('Current time is: ${res.body.int()}')
}

fn test_https_get_from_vlang_utc_now() {
	$if !network ? {
		return
	}
	url := 'https://vlang.io/utc_now'
	println('Test getting current time from HTTPS ${url} by http.get')
	res := http.get(url) or { panic(err) }
	assert res.status() == .ok
	assert res.body != ''
	assert res.body.int() > 1566403696
	println('Current time is: ${res.body.int()}')
}

fn test_http_public_servers() {
	$if !network ? {
		return
	}
	urls := [
		'http://github.com/robots.txt',
		'http://google.com/robots.txt',
		// 'http://yahoo.com/robots.txt',
	]
	for url in urls {
		println('Testing http.get on public HTTP url: ${url} ')
		res := http.get(url) or { panic(err) }
		assert res.status() == .ok
		assert res.body != ''
	}
}

fn test_https_public_servers() {
	$if !network ? {
		return
	}
	urls := [
		'https://github.com/robots.txt',
		'https://google.com/robots.txt',
		// 'https://yahoo.com/robots.txt',
	]
	for url in urls {
		println('Testing http.get on public HTTPS url: ${url} ')
		res := http.get(url) or { panic(err) }
		assert res.status() == .ok
		assert res.body != ''
	}
}

fn test_relative_redirects() {
	$if !network ? {
		return
	}
	res := http.get('https://httpbin.org/relative-redirect/3?abc=xyz') or { panic(err) }
	assert res.status() == .ok
	assert res.body != ''
	assert res.body.contains('"abc": "xyz"')
}

fn test_default_user_agent() {
	$if !network ? {
		return
	}
	res := http.get('https://httpbin.org/user-agent') or { panic(err) }
	assert res.status() == .ok
	assert res.body != ''
	assert res.body.contains('"user-agent": "v.http"')
}

fn test_custom_user_agent() {
	$if !network ? {
		return
	}
	ua := 'V http test for UA'
	mut req := http.new_request(.get, 'https://httpbin.org/user-agent', '')
	req.user_agent = ua
	res := req.do() or { panic(err) }
	assert res.status() == .ok
	assert res.body != ''
	assert res.body.contains('"user-agent": "${ua}"')
}
