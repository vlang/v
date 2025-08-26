module http

import encoding.base64
import net
import net.urllib

const sample_proxy_url = 'https://localhost'
const sample_auth_proxy_url = 'http://user:pass@localhost:8888'

const sample_host = '127.0.0.1:1337'
const sample_request = &Request{
	url: 'http://${sample_host}'
}
const sample_path = '/'

fn test_proxy_fields() ? {
	sample_proxy := new_http_proxy(sample_proxy_url)!
	sample_auth_proxy := new_http_proxy(sample_auth_proxy_url)!

	assert sample_proxy.scheme == 'https'
	assert sample_proxy.host == 'localhost:443'
	assert sample_proxy.hostname == 'localhost'
	assert sample_proxy.port == 443
	assert sample_proxy.url == sample_proxy_url
	assert sample_auth_proxy.scheme == 'http'
	assert sample_auth_proxy.username == 'user'
	assert sample_auth_proxy.password == 'pass'
	assert sample_auth_proxy.host == 'localhost:8888'
	assert sample_auth_proxy.hostname == 'localhost'
	assert sample_auth_proxy.port == 8888
	assert sample_auth_proxy.url == sample_auth_proxy_url
}

fn test_proxy_headers() ? {
	sample_proxy := new_http_proxy(sample_proxy_url)!
	headers := sample_proxy.build_proxy_headers(sample_host)

	assert headers == 'CONNECT 127.0.0.1:1337 HTTP/1.1\r\n' + 'Host: 127.0.0.1\r\n' +
		'Proxy-Connection: Keep-Alive\r\n\r\n'
}

fn test_proxy_headers_authenticated() ? {
	sample_proxy := new_http_proxy(sample_auth_proxy_url)!
	headers := sample_proxy.build_proxy_headers(sample_host)

	auth_token := base64.encode(('${sample_proxy.username}:' + '${sample_proxy.password}').bytes())

	assert headers == 'CONNECT 127.0.0.1:1337 HTTP/1.1\r\n' + 'Host: 127.0.0.1\r\n' +
		'Proxy-Connection: Keep-Alive\r\nProxy-Authorization: Basic ${auth_token}\r\n\r\n'
}

fn test_http_do() {
	// params
	host := urllib.URL{
		scheme: 'http'
		host:   'httpbin.org'
	}
	path := '/get'
	req := &Request{
		method: Method.get
		url:    'http://httpbin.org/get'
	}

	assert host.scheme == 'http'
	assert host.host == 'httpbin.org'

	host_name, port := net.split_address(host.hostname())!

	assert host_name == 'httpbin.org'
	assert port == 80 || port == 0

	full_url := if host.scheme == 'http' || host.scheme == 'https' {
		port_part := if port == 80 || port == 0 { '' } else { ':${port}' }
		'${host.scheme}://${host.host}${port_part}${path}'
	} else {
		'${host.scheme}://${host.host}${path}'
	}

	assert full_url.starts_with('http') || full_url.starts_with('https')
		|| full_url.starts_with('/')

	s_proxy := new_http_proxy('http://8.8.8.8:8080')!
	assert s_proxy.host == '8.8.8.8:8080'
	assert s_proxy.hostname == '8.8.8.8'
	assert s_proxy.port == 8080
	assert s_proxy.url == 'http://8.8.8.8:8080'

	s_fetch := prepare(FetchConfig{
		url:   'http://httpbin.org/get'
		proxy: s_proxy
	})!

	str := req.build_request_headers(req.method, host_name, port, full_url)

	// Test the request headers string was built correctly
	println(s_fetch)
	println(str)
	assert str.len > 0
	assert str.contains('GET') && str.contains('HTTP/1.1')
	assert str.contains(s_fetch.url)
	assert str.contains('Host: httpbin.org')
	assert str.contains('User-Agent:')

	if host.scheme == 'http' || host.scheme == 'https' {
		$if windows {
			println('Running on Windows. Test would skip SSL connection.')
			return
		}

		mock_response_text := 'HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 32\r\n\r\n{"origin": "127.0.0.1", "url": "http://httpbin.org/get"}'
		mock_result := parse_response(mock_response_text)!

		assert mock_result.status_code == 200
		assert mock_result.body.contains('"origin"')
		assert str.starts_with('GET')
		assert str.ends_with('\r\n\r\n')

		assert req.read_timeout > 0
		assert req.write_timeout > 0

		// req.on_finish(req, u64(mock_response_text.len)) or { panic('on_finish callback failed') }

		assert mock_result.body.len > 0
	} else {
		assert false, 'Should not reach here. Need to add more test cases'
	}
}
