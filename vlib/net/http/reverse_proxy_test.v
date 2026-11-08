module http

import compress.gzip
import net
import time

fn test_join_url_path() {
	assert join_url_path('', '') == '/'
	assert join_url_path('', 'test') == '/test'
	assert join_url_path('', '/test') == '/test'
	assert join_url_path('/', '') == '/'
	assert join_url_path('/', 'test') == '/test'
	assert join_url_path('/', '/test') == '/test'
	assert join_url_path('/api', '') == '/api'
	assert join_url_path('/api', '/') == '/api/'
	assert join_url_path('/api/', '/') == '/api/'
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
	h.add_custom('Proxy-Connection', 'keep-alive') or {}
	h.add_custom('X-Keep-This', 'keep_value') or {}

	clean_hop_by_hop_headers(mut h)

	assert !h.contains(.connection)
	assert !h.contains(.keep_alive)
	assert !h.contains(.proxy_authenticate)
	assert !h.contains(.transfer_encoding)
	assert !h.contains(.upgrade)
	assert h.get_custom('X-Custom-Hop') == none
	assert h.get_custom('Proxy-Connection') == none
	assert (h.get(.content_type) or { '' }) == 'application/json'
	assert (h.get_custom('X-Keep-This') or { '' }) == 'keep_value'
}

fn test_reverse_proxy_forwarded_headers() ! {
	mut rp := new_single_host_reverse_proxy('http://127.0.0.1:1/backend%2Froot?base=1')!
	mut req := Request{
		url:         'https://frontend.example.com/test%2Fitem?query=1'
		remote_addr: '203.0.113.195:54321'
		host:        'frontend.example.com'
		version:     .v2_0
	}
	req.header.add(.connection, 'keep-alive')
	req.header.add(.connection, 'X-Custom-Hop')
	req.header.add_custom('X-Custom-Hop', 'remove me')!
	req.header.add(.x_forwarded_for, '198.51.100.1')
	req.header.add(.x_forwarded_for, '198.51.100.2')
	if _ := rp.proxy(mut req) {
		assert false, 'the reserved closed upstream should refuse the request'
	}
	assert req.url == 'http://127.0.0.1:1/backend%2Froot/test%2Fitem?base=1&query=1'
	assert req.host == '127.0.0.1:1'
	assert (req.header.get(.host) or { '' }) == '127.0.0.1:1'
	assert (req.header.get(.x_forwarded_host) or { '' }) == 'frontend.example.com'
	assert (req.header.get(.x_forwarded_proto) or { '' }) == 'https'
	assert (req.header.get(.x_forwarded_for) or { '' }) == '198.51.100.1, 198.51.100.2, 203.0.113.195'
	assert req.version == .v1_1
	assert req.validate
	assert !req.header.contains(.connection)
	assert req.header.get_custom('X-Custom-Hop') == none
}

fn test_reverse_proxy_preserves_host_and_uses_incoming_protocol() ! {
	mut rp := new_single_host_reverse_proxy('https://127.0.0.1:1/base')!
	rp.config.preserve_host = true
	mut req := Request{
		url:  '/test'
		host: 'frontend.example.com'
	}
	req.header.add(.host, req.host)
	req.header.add(.connection, 'Host')
	if _ := rp.proxy(mut req) {
		assert false
	}
	assert req.host == 'frontend.example.com'
	assert (req.header.get(.host) or { '' }) == 'frontend.example.com'
	assert (req.header.get(.x_forwarded_host) or { '' }) == 'frontend.example.com'
	assert (req.header.get(.x_forwarded_proto) or { '' }) == 'http'
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
	if req.url.contains('/compressed') {
		body := gzip.compress('compressed response'.bytes()) or { panic(err) }
		return Response{
			status_code: 200
			header:      new_header(key: .content_encoding, value: 'gzip')
			body:        body.bytestr()
		}
	}
	if req.url.contains('/redirect') {
		return Response{
			status_code: 302
			header:      new_header(key: .location, value: '/unexpected')
		}
	}
	xff := req.header.get(.x_forwarded_for) or { 'none' }
	custom := req.header.get_custom('X-Injected-By-Director') or { 'none' }
	return Response{
		status_code: 200
		header:      new_header(
			key:   .content_type
			value: 'application/json'
		)
		body:        '{"path":"${req.url}","xff":"${xff}","custom":"${custom}","data":"${req.data}"}'
	}
}

@[unsafe]
fn test_reverse_proxy_end_to_end() {
	// Start an upstream backend server on an ephemeral port
	mut upstream := &Server{
		listener:             net.listen_tcp(.ip, '127.0.0.1:0')!
		handler:              UpstreamBackendHandler{}
		show_startup_message: false
		worker_num:           1
		accept_timeout:       50 * time.millisecond
	}
	upstream_addr := upstream.listener.addr()!.str()
	spawn upstream.listen_and_serve()
	defer { upstream.close() }
	assert upstream_addr != ''

	// Configure ReverseProxy pointing to the upstream server
	mut rp := new_single_host_reverse_proxy('http://${upstream_addr}/backend')!
	rp.config.director = fn (mut r Request) {
		r.add_custom_header('X-Injected-By-Director', 'director_val') or {}
		r.data = 'changed body'
	}
	rp.config.modify_response = fn (mut resp Response) ! {
		resp.header.add_custom('X-Added-By-Proxy', 'modified')!
		resp.body += ' changed'
	}

	mut client_req := Request{
		url:         '/hello?param=world'
		remote_addr: '192.0.2.1:12345'
		method:      .post
		data:        'original'
	}
	client_req.header.add(.content_length, '8')

	resp := rp.handle(client_req)
	assert resp.status_code == 200
	assert (resp.header.get_custom('X-Added-By-Proxy') or { '' }) == 'modified'
	assert resp.body.contains('"xff":"192.0.2.1"')
	assert resp.body.contains('"custom":"director_val"')
	assert resp.body.contains('"data":"changed body"')
	assert resp.body.contains('/backend/hello?param=world')
	assert resp.body.ends_with(' changed')
	assert !resp.header.contains(.content_length)
	redirect := rp.handle(Request{ url: '/redirect', version: .v1_0 })
	assert redirect.status_code == 302
	assert redirect.http_version == '1.0'
	assert (redirect.header.get(.location) or { '' }) == '/unexpected'
	rp.config.modify_response = unsafe { nil }
	compressed := rp.handle(Request{ url: '/compressed' })
	assert compressed.status_code == 200
	assert (compressed.header.get(.content_encoding) or { '' }) == 'gzip'
	assert (compressed.header.get(.content_length) or { '' }).int() == compressed.body.len
	assert gzip.decompress(compressed.body.bytes())!.bytestr() == 'compressed response'
	assert parse_response(compressed.bytestr())!.body == 'compressed response'
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
