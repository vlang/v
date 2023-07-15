module http

import net
import time

fn test_server_stop() {
	mut server := &Server{
		accept_timeout: 1 * time.second
	}
	t := spawn server.listen_and_serve()
	time.sleep(250 * time.millisecond)
	mut watch := time.new_stopwatch()
	server.stop()
	assert server.status() == .stopped
	assert watch.elapsed() < 100 * time.millisecond
	t.wait()
	assert watch.elapsed() < 999 * time.millisecond
}

fn test_server_close() {
	mut server := &Server{
		accept_timeout: 1 * time.second
		handler: MyHttpHandler{}
	}
	t := spawn server.listen_and_serve()
	time.sleep(250 * time.millisecond)
	mut watch := time.new_stopwatch()
	server.close()
	assert server.status() == .closed
	assert watch.elapsed() < 100 * time.millisecond
	t.wait()
	assert watch.elapsed() < 999 * time.millisecond
}

fn test_server_custom_listener() {
	listener := net.listen_tcp(.ip6, ':8081')!
	mut server := &Server{
		accept_timeout: 1 * time.second
		listener: listener
	}
	t := spawn server.listen_and_serve()
	time.sleep(250 * time.millisecond)
	mut watch := time.new_stopwatch()
	server.close()
	assert server.status() == .closed
	assert watch.elapsed() < 100 * time.millisecond
	t.wait()
	assert watch.elapsed() < 999 * time.millisecond
}

struct MyHttpHandler {
mut:
	counter    int
	oks        int
	not_founds int
}

fn (mut handler MyHttpHandler) handle(req Request) Response {
	handler.counter++
	// eprintln('$time.now() | counter: $handler.counter | $req.method $req.url\n$req.header\n$req.data - 200 OK\n')
	mut r := Response{
		body: req.data + ', ${req.url}'
		header: req.header
	}
	match req.url.all_before('?') {
		'/endpoint', '/another/endpoint' {
			r.set_status(.ok)
			handler.oks++
		}
		else {
			r.set_status(.not_found)
			handler.not_founds++
		}
	}
	r.set_version(req.version)
	return r
}

const cport = 8198

fn test_server_custom_handler() {
	mut handler := MyHttpHandler{}
	mut server := &Server{
		accept_timeout: 1 * time.second
		handler: handler
		port: http.cport
	}
	t := spawn server.listen_and_serve()
	for server.status() != .running {
		time.sleep(10 * time.millisecond)
	}
	x := fetch(url: 'http://localhost:${http.cport}/endpoint?abc=xyz', data: 'my data')!
	assert x.body == 'my data, /endpoint?abc=xyz'
	assert x.status_code == 200
	assert x.status_msg == 'OK'
	assert x.http_version == '1.1'
	y := fetch(url: 'http://localhost:${http.cport}/another/endpoint', data: 'abcde')!
	assert y.body == 'abcde, /another/endpoint'
	assert y.status_code == 200
	assert x.status_msg == 'OK'
	assert y.status() == .ok
	assert y.http_version == '1.1'
	//
	fetch(url: 'http://localhost:${http.cport}/something/else')!
	server.stop()
	t.wait()
	assert handler.counter == 3
	assert handler.oks == 2
	assert handler.not_founds == 1
}
