module http

import encoding.base64

const (
	sample_proxy_url      = 'https://localhost'
	sample_auth_proxy_url = 'http://user:pass@localhost:8888'

	sample_host           = '127.0.0.1:1337'
	sample_request        = &Request{
		url: 'http://${sample_host}'
	}
	sample_path           = '/'
)

fn test_proxy_fields() ? {
	sample_proxy := new_http_proxy(http.sample_proxy_url)!
	sample_auth_proxy := new_http_proxy(http.sample_auth_proxy_url)!

	assert sample_proxy.scheme == 'https'
	assert sample_proxy.host == 'localhost:443'
	assert sample_proxy.hostname == 'localhost'
	assert sample_proxy.port == 443
	assert sample_proxy.url == http.sample_proxy_url
	assert sample_auth_proxy.scheme == 'http'
	assert sample_auth_proxy.username == 'user'
	assert sample_auth_proxy.password == 'pass'
	assert sample_auth_proxy.host == 'localhost:8888'
	assert sample_auth_proxy.hostname == 'localhost'
	assert sample_auth_proxy.port == 8888
	assert sample_auth_proxy.url == http.sample_auth_proxy_url
}

fn test_proxy_headers() ? {
	sample_proxy := new_http_proxy(http.sample_proxy_url)!
	headers := sample_proxy.build_proxy_headers(http.sample_request, http.sample_host,
		http.sample_path)

	assert headers == 'GET ${http.sample_request.url}${http.sample_path} HTTP/1.1\r\n' +
		'Host: ${http.sample_host}\r\n' + 'User-Agent: ${http.sample_request.user_agent}\r\n' +
		'Upgrade-Insecure-Requests: 1\r\n\r\n'
}

fn test_proxy_headers_authenticated() ? {
	sample_proxy := new_http_proxy(http.sample_auth_proxy_url)!
	headers := sample_proxy.build_proxy_headers(http.sample_request, http.sample_host,
		http.sample_path)

	auth_token := base64.encode(('${sample_proxy.username}:' + '${sample_proxy.password}').bytes())

	assert headers == 'GET ${http.sample_request.url}${http.sample_path} HTTP/1.1\r\n' +
		'Host: ${http.sample_host}\r\n' + 'User-Agent: ${http.sample_request.user_agent}\r\n' +
		'Upgrade-Insecure-Requests: 1\r\n' + 'Proxy-Authorization: Basic ${auth_token}\r\n\r\n'
}
