module http

import encoding.base64
import net.urllib
import os

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

fn test_http_proxy_do() {
	env := os.environ()
	mut env_proxy := ''

	for envvar in ['http_proxy', 'HTTP_PROXY', 'https_proxy', 'HTTPS_PROXY'] {
		prox_val := env[envvar] or { continue }
		if prox_val != '' {
			env_proxy = env[envvar]
		}
	}
	if env_proxy != '' {
		println('Has usable proxy env vars')
		proxy := new_http_proxy(env_proxy)!
		mut header := new_header(key: .user_agent, value: 'vlib')
		header.add_custom('X-Vlang-Test', 'proxied')!
		res := proxy.http_do(urllib.parse('http://httpbin.org/headers')!, Method.get,
			'/headers', &Request{ proxy: proxy, header: header })!
		println(res.status_code)
		println('he4aders ${res.header}')
		assert res.status_code == 200
		// assert res.header.data['X-Vlang-Test'] == 'proxied'
	} else {
		println('Proxy env vars (HTTP_PROXY or HTTPS_PROXY) not set. Skipping test.')
	}
}
