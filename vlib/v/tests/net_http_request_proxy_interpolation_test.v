import net.http

fn test_request_proxy_can_be_interpolated_outside_net_http() {
	req := http.Request{}
	assert '${req.proxy}' == '&nil'
}

fn test_http_proxy_can_be_interpolated_outside_net_http() ! {
	proxy := http.new_http_proxy('http://user:pass@localhost:8888')!
	req := http.Request{
		proxy: proxy
	}
	assert '${req.proxy}' == '&http://user:pass@localhost:8888'
}
