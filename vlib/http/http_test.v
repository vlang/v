// import net.urllib
import http

fn test_escape_unescape() {
/*
  original := 'те ст: т\\%'
  escaped := urllib.query_escape(original) or { assert false return}
  assert escaped == '%D1%82%D0%B5%20%D1%81%D1%82%3A%20%D1%82%5C%25'
  unescaped := urllib.query_unescape(escaped) or { assert false return }
  assert unescaped == original
*/
}

fn test_http_get() {
	assert http.get_text('https://vlang.io/version') == '0.1.5'
	println('http ok')
}

fn test_http_get_from_vlang_utc_now() {	
	urls := ['http://vlang.io/utc_now', 'https://vlang.io/utc_now']
	for url in urls {
		println('Test getting current time from $url by http.get')
		res := http.get(url) or { panic(err) }
		assert 200 == res.status_code
		assert res.text.len > 0
		assert res.text.int() > 1566403696
		println('Current time is: ${res.text.int()}')
	}
}

fn test_public_servers() {
	urls := [
		'http://github.com/robots.txt',
		'http://google.com/robots.txt',
		'http://yahoo.com/robots.txt',
		'https://github.com/robots.txt',
		'https://google.com/robots.txt',
		'https://yahoo.com/robots.txt',
	]
	for url in urls {
		println('Testing http.get on public url: $url ')
		res :=  http.get( url ) or { panic(err) }
		assert 200 == res.status_code
		assert res.text.len > 0
	}
}
