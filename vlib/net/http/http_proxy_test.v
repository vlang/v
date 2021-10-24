module http

import net.http
import os
import encoding.base64

const (
	sample_proxy_url      = 'https://localhost'
	sample_auth_proxy_url = 'http://user:pass@localhost:8888'

	sample_host           = '127.0.0.1:1337'
	sample_request        = &http.Request{
		url: 'http://$sample_host'
	}
)

fn test_proxy_fields() ? {
	sample_proxy := new_http_proxy(http.sample_proxy_url) ?
	sample_auth_proxy := new_http_proxy(http.sample_auth_proxy_url) ?

	assert sample_proxy.scheme == 'https'
	assert sample_proxy.userinfo.password_set == false
	assert sample_proxy.host == 'localhost:443'
	assert sample_proxy.hostname == 'localhost'
	assert sample_proxy.port == 443
	assert sample_proxy.url == http.sample_proxy_url

	assert sample_auth_proxy.scheme == 'http'
	assert sample_auth_proxy.userinfo.password_set == true
	assert sample_auth_proxy.userinfo.username == 'user'
	assert sample_auth_proxy.userinfo.password == 'pass'
	assert sample_auth_proxy.host == 'localhost:8888'
	assert sample_auth_proxy.hostname == 'localhost'
	assert sample_auth_proxy.port == 8888
	assert sample_auth_proxy.url == http.sample_auth_proxy_url
}

fn test_proxy_headers() ? {
	sample_proxy := new_http_proxy(http.sample_proxy_url) ?
	headers := sample_proxy.build_proxy_headers(http.sample_request, http.sample_host)

	assert headers == 'CONNECT $http.sample_host HTTP/1.1\r\n' + 'Host: $http.sample_host\r\n' +
		'User-Agent: $http.sample_request.user_agent\r\n' + 'Proxy-Connection: Keep-Alive\r\n\r\n'
}

fn test_proxy_headers_authenticated() ? {
	sample_proxy := new_http_proxy(http.sample_auth_proxy_url) ?
	headers := sample_proxy.build_proxy_headers(http.sample_request, http.sample_host)

	auth_token := base64.encode(('$sample_proxy.userinfo.username:' +
		'$sample_proxy.userinfo.password').bytes())

	assert headers == 'CONNECT $http.sample_host HTTP/1.1\r\n' + 'Host: $http.sample_host\r\n' +
		'User-Agent: $http.sample_request.user_agent\r\n' + 'Proxy-Connection: Keep-Alive\r\n' +
		'Proxy-Authorization: Basic $auth_token\r\n\r\n'
}
