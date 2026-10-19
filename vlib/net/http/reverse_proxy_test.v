module http

import net.urllib
import time

fn test_join_url_path() {
	assert join_url_path('', '') == '/'
	assert join_url_path('', 'test') == '/test'
	assert join_url_path('', '/test') == '/test'
	assert join_url_path('/', '') == '/'
	assert join_url_path('/', 'test') == '/test'
	assert join_url_path('/', '/test') == '/test'
	assert join_url_path('/api', '') == '/api'
	assert join_url_path('/api', 'users') == '/api/users'
	assert join_url_path('/api', '/users') == '/api/users'
	assert join_url_path('/api/', 'users') == '/api/users'
	assert join_url_path('/api/', '/users') == '/api/users'
}

fn test_clean_hop_by_hop_headers() {
	mut h := new_header()
	h.add(.connection, 'close, X-Custom-Hop')
	h.add(.keep_alive, 'timeout=5')
	h.add(.proxy_authenticate, 'Basic')
	h.add(.transfer_encoding, 'chunked')
	h.add(.upgrade, 'websocket')
	h.add(.content_type, 'application/json')
	h.add_custom('X-Custom-Hop', 'custom_value') or {}
	h.add_custom('X-Keep-This', 'keep_value') or {}

	clean_hop_by_hop_headers(mut h)

	assert !h.contains(.connection)
	assert !h.contains(.keep_alive)
	assert !h.contains(.proxy_authenticate)
	assert !h.contains(.transfer_encoding)
	assert !h.contains(.upgrade)
	assert h.get_custom('X-Custom-Hop') == none
	assert (h.get(.content_type) or { '' }) == 'application/json'
	assert (h.get_custom('X-Keep-This') or { '' }) == 'keep_value'
}

fn test_reverse_proxy_forwarded_headers() {
	mut rp := new_single_host_reverse_proxy('http://127.0.0.1:9090/backend')!
	assert rp.config.target.host == '127.0.0.1:9090'
	assert rp.config.target.path == '/backend'

	// Mock incoming request
	mut req := Request{
		url:         '/test?query=1'
		remote_addr: '203.0.113.195:54321'
		host:        'frontend.example.com'
	}
	req.header.add(.connection, 'keep-alive')
	req.header.add(.x_forwarded_for, '198.51.100.1')

	// We test URL rewrites and header transformations
	target := rp.config.target
	target_query := target.raw_query
	mut req_url := urllib.parse(req.url) or { panic(err) }
	joined_path := join_url_path(target.path, req_url.path)
	req_url.scheme = target.scheme
	req_url.host = target.host
	req_url.path = joined_path
	if target_query != '' && req_url.raw_query != '' {
		req_url.raw_query = '${target_query}&${req_url.raw_query}'
	} else if target_query != '' {
		req_url.raw_query = target_query
	}
	req.url = req_url.str()

	if !rp.config.preserve_host {
		req.host = target.host
		req.header.set(.host, target.host)
	}
	clean_hop_by_hop_headers(mut req.header)

	client_ip := req.remote_ip()
	if client_ip != '' {
		if prev_xff := req.header.get(.x_forwarded_for) {
			req.header.set(.x_forwarded_for, '${prev_xff}, ${client_ip}')
		} else {
			req.header.set(.x_forwarded_for, client_ip)
		}
	}

	assert req.url == 'http://127.0.0.1:9090/backend/test?query=1'
	assert req.host == '127.0.0.1:9090'
	assert (req.header.get(.x_forwarded_for) or { '' }) == '198.51.100.1, 203.0.113.195'
	assert !req.header.contains(.connection)
}

fn test_reverse_proxy_error_handler_on_unreachable_upstream() {
	mut rp := new_single_host_reverse_proxy('http://127.0.0.1:1')! // Unreachable port
	req := Request{
		url: '/unreachable'
	}
	resp := rp.handle(req)
	assert resp.status_code == 502
	assert resp.status_msg == 'Bad Gateway'
	assert resp.body.contains('Bad Gateway')
}

struct UpstreamBackendHandler {}

fn (mut h UpstreamBackendHandler) handle(req Request) Response {
	xff := req.header.get(.x_forwarded_for) or { 'none' }
	custom := req.header.get_custom('X-Injected-By-Director') or { 'none' }
	return Response{
		status_code: 200
		header:      new_header(
			key:   .content_type
			value: 'application/json'
		)
		body:        '{"path":"${req.url}","xff":"${xff}","custom":"${custom}"}'
	}
}

fn test_reverse_proxy_end_to_end() {
	// Start an upstream backend server on an ephemeral port
	mut upstream := &Server{
		addr:                 '127.0.0.1:0'
		handler:              UpstreamBackendHandler{}
		show_startup_message: false
	}
	spawn upstream.listen_and_serve()
	time.sleep(50 * time.millisecond)

	upstream_addr := upstream.addr
	assert upstream_addr != ''

	// Configure ReverseProxy pointing to the upstream server
	mut rp := new_single_host_reverse_proxy('http://${upstream_addr}/backend')!
	rp.config.director = fn (mut r Request) {
		r.add_custom_header('X-Injected-By-Director', 'director_val') or {}
	}
	rp.config.modify_response = fn (mut resp Response) ! {
		resp.header.add_custom('X-Added-By-Proxy', 'modified')!
	}

	mut client_req := Request{
		url:         '/hello?param=world'
		remote_addr: '192.0.2.1:12345'
	}

	resp := rp.handle(client_req)
	assert resp.status_code == 200
	assert (resp.header.get_custom('X-Added-By-Proxy') or { '' }) == 'modified'
	assert resp.body.contains('"xff":"192.0.2.1"')
	assert resp.body.contains('"custom":"director_val"')
	assert resp.body.contains('/backend/hello?param=world')

	upstream.close()
}

fn test_reverse_proxy_invalid_config() {
	if _ := new_single_host_reverse_proxy('ftp://example.com') {
		assert false
	} else {
		assert err.msg().contains('scheme must be http or https')
	}

	if _ := new_single_host_reverse_proxy('http://') {
		assert false
	} else {
		assert err.msg().contains('target host cannot be empty')
	}
}

fn test_reverse_proxy_custom_error_handler() {
	mut rp := new_single_host_reverse_proxy('http://127.0.0.1:1')!
	rp.config.error_handler = fn (mut req Request, err IError) Response {
		return Response{
			status_code: 504
			status_msg:  'Gateway Timeout'
			body:        'custom error'
		}
	}
	resp := rp.handle(Request{ url: '/test' })
	assert resp.status_code == 504
	assert resp.status_msg == 'Gateway Timeout'
	assert resp.body == 'custom error'
}
