module http

import net
import net.urllib
import time

struct RedirectTestReply {
	status   int = 200
	location string
	body     string = 'ok'
}

fn test_redirect_origin_uses_scheme_host_and_effective_port() ! {
	assert same_origin(urllib.parse('HTTP://Example.COM/start')!, urllib.parse('http://example.com:80/next')!)
	assert same_origin(urllib.parse('http://example.com/start')!, urllib.parse('http://example.com:080/next')!)
	assert same_origin(urllib.parse('https://Example.COM/start')!, urllib.parse('https://example.com:443/next')!)
	assert !same_origin(urllib.parse('http://example.com/start')!, urllib.parse('https://example.com/next')!)
	assert !same_origin(urllib.parse('http://example.com/start')!, urllib.parse('http://example.com:81/next')!)
	assert !same_origin(urllib.parse('http://localhost/start')!, urllib.parse('http://127.0.0.1/next')!)
	assert same_origin(urllib.parse('http://[fe80::1%25en0]/start')!, urllib.parse('http://[fe80::1%25en0]/next')!)
	assert same_origin(urllib.parse('http://[fe80::A%25en0]/start')!, urllib.parse('http://[fe80::a%25en0]/next')!)
	assert !same_origin(urllib.parse('http://[fe80::1%25en0]/start')!, urllib.parse('http://[fe80::1%25EN0]/next')!)
}

fn test_cross_origin_redirect_removes_all_authorization_fields() ! {
	for status in [Status.moved_permanently, .found, .see_other, .temporary_redirect,
		.permanent_redirect] {
		check_cross_origin_redirect_removes_authorization(status)!
	}
}

fn check_cross_origin_redirect_removes_authorization(status Status) ! {
	mut first_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	mut target_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	first_port := first_listener.addr()!.port()!
	target_port := target_listener.addr()!.port()!
	first_requests := chan string{cap: 1}
	target_requests := chan string{cap: 1}
	first_thread := spawn serve_redirect_test_responses(mut first_listener, [RedirectTestReply{
		status:   int(status)
		location: 'http://127.0.0.1:${target_port}/capture'
		body:     ''
	}], first_requests)
	target_thread := spawn serve_redirect_test_responses(mut target_listener, [RedirectTestReply{}],
		target_requests)
	mut header := new_header(key: .authorization, value: 'Bearer canonical')
	header.add_custom('authorization', 'Bearer lowercase')!
	header.add_custom('AUTHORIZATION', 'Bearer uppercase')!
	response := Request{
		method: .post
		url:    'http://127.0.0.1:${first_port}/start'
		data:   'payload'
		header: header
	}.do()!
	first_request := <-first_requests
	target_request := <-target_requests
	first_thread.wait()
	target_thread.wait()
	assert response.status() == .ok
	assert authorization_values(first_request) == ['Bearer canonical, Bearer lowercase, Bearer uppercase']
	assert authorization_values(target_request).len == 0
	expected_method := if status in [Status.moved_permanently, .found, .see_other] {
		'GET'
	} else {
		'POST'
	}
	assert target_request.starts_with('${expected_method} /capture HTTP/1.1\r\n')
	first_listener.close() or {}
	target_listener.close() or {}
}

fn test_same_origin_redirect_preserves_authorization() ! {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	port := listener.addr()!.port()!
	requests := chan string{cap: 2}
	server_thread := spawn serve_redirect_test_responses(mut listener, [
		RedirectTestReply{
			status:   int(Status.found)
			location: '/next'
			body:     ''
		},
		RedirectTestReply{},
	], requests)
	mut header := new_header(key: .authorization, value: 'Bearer same-origin')
	response := Request{
		url:    'http://127.0.0.1:${port}/start'
		header: header
	}.do()!
	first_request := <-requests
	second_request := <-requests
	server_thread.wait()
	assert response.status() == .ok
	assert authorization_values(first_request) == ['Bearer same-origin']
	assert authorization_values(second_request) == ['Bearer same-origin']
	listener.close() or {}
}

fn test_cross_origin_redirect_chain_does_not_restore_authorization() ! {
	mut first_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	mut second_listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	first_port := first_listener.addr()!.port()!
	second_port := second_listener.addr()!.port()!
	first_requests := chan string{cap: 2}
	second_requests := chan string{cap: 1}
	first_thread := spawn serve_redirect_test_responses(mut first_listener, [
		RedirectTestReply{
			status:   int(Status.found)
			location: 'http://127.0.0.1:${second_port}/middle'
			body:     ''
		},
		RedirectTestReply{},
	], first_requests)
	second_thread := spawn serve_redirect_test_responses(mut second_listener, [
		RedirectTestReply{
			status:   int(Status.found)
			location: 'http://127.0.0.1:${first_port}/final'
			body:     ''
		},
	], second_requests)
	response := Request{
		url:    'http://127.0.0.1:${first_port}/start'
		header: new_header(key: .authorization, value: 'Bearer chain')
	}.do()!
	first_request := <-first_requests
	second_request := <-second_requests
	final_request := <-first_requests
	first_thread.wait()
	second_thread.wait()
	assert response.status() == .ok
	assert authorization_values(first_request) == ['Bearer chain']
	assert authorization_values(second_request).len == 0
	assert authorization_values(final_request).len == 0
	first_listener.close() or {}
	second_listener.close() or {}
}

fn serve_redirect_test_responses(mut listener net.TcpListener, replies []RedirectTestReply, requests chan string) {
	listener.set_accept_timeout(5 * time.second)
	for reply in replies {
		mut conn := listener.accept() or { return }
		conn.set_read_timeout(5 * time.second)
		request := read_redirect_test_request(mut conn)
		location_header := if reply.location == '' { '' } else { 'Location: ${reply.location}\r\n' }
		response := 'HTTP/1.1 ${reply.status} Test\r\nContent-Length: ${reply.body.len}\r\nConnection: close\r\n${location_header}\r\n${reply.body}'
		conn.write(response.bytes()) or { return }
		requests <- request
		conn.close() or {}
	}
	listener.close() or {}
}

fn read_redirect_test_request(mut conn net.TcpConn) string {
	mut request := ''
	mut buffer := []u8{len: 2048}
	for !request.contains('\r\n\r\n') {
		n := conn.read(mut buffer) or { return request }
		if n <= 0 {
			return request
		}
		request += buffer[..n].bytestr()
	}
	return request
}

fn authorization_values(request string) []string {
	mut values := []string{}
	for line in request.split('\r\n') {
		if line.to_lower().starts_with('authorization:') {
			values << line.all_after(':').trim_space()
		}
	}
	return values
}
