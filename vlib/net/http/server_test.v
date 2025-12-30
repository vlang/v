// vtest retry: 5
import log
import net
import net.http
import time

const atimeout = 500 * time.millisecond

fn testsuite_begin() {
	log.info(@FN)
}

fn testsuite_end() {
	log.info(@FN)
}

fn test_server_stop() {
	log.warn('${@FN} started')
	defer {
		log.warn('${@FN} finished')
	}
	mut server := &http.Server{
		accept_timeout: atimeout
	}
	t := spawn server.listen_and_serve()
	server.wait_till_running()!
	mut watch := time.new_stopwatch()
	server.stop()
	assert server.status() == .stopped
	assert watch.elapsed() < 100 * time.millisecond
	t.wait()
	assert watch.elapsed() < 999 * time.millisecond
}

fn test_server_close() {
	log.warn('${@FN} started')
	defer {
		log.warn('${@FN} finished')
	}
	mut server := &http.Server{
		accept_timeout:       atimeout
		handler:              MyHttpHandler{}
		show_startup_message: false
	}
	t := spawn server.listen_and_serve()
	server.wait_till_running()!
	mut watch := time.new_stopwatch()
	server.close()
	assert server.status() == .closed
	assert watch.elapsed() < 100 * time.millisecond
	t.wait()
	assert watch.elapsed() < 999 * time.millisecond
}

fn test_server_custom_listener() {
	log.warn('${@FN} started')
	defer {
		log.warn('${@FN} finished')
	}
	listener := net.listen_tcp(.ip6, ':8081')!
	mut server := &http.Server{
		accept_timeout:       atimeout
		listener:             listener
		show_startup_message: false
	}
	t := spawn server.listen_and_serve()
	server.wait_till_running()!
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
	redirects  int
}

fn (mut handler MyHttpHandler) handle(req http.Request) http.Response {
	handler.counter++
	// eprintln('$time.now() | counter: $handler.counter | $req.method $req.url\n$req.header\n$req.data - 200 OK\n')
	mut r := http.Response{
		body:   req.data + ', ${req.url}'
		header: req.header
	}
	match req.url.all_before('?') {
		'/endpoint', '/another/endpoint' {
			r.set_status(.ok)
			handler.oks++
		}
		'/redirect_to_big' {
			r.header = http.new_header(key: .location, value: '/big')
			r.status_msg = 'Moved permanently'
			r.status_code = 301
			handler.redirects++
		}
		'/big' {
			r.body = 'xyz def '.repeat(5_000)
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

fn test_server_custom_handler() {
	log.warn('${@FN} started')
	defer {
		log.warn('${@FN} finished')
	}
	mut handler := MyHttpHandler{}
	mut server := &http.Server{
		accept_timeout: atimeout
		handler:        handler
		addr:           '127.0.0.1:18197'
	}
	t := spawn server.listen_and_serve()
	server.wait_till_running()!
	x := http.fetch(url: 'http://${server.addr}/endpoint?abc=xyz', data: 'my data')!
	assert x.body == 'my data, /endpoint?abc=xyz'
	assert x.status_code == 200
	assert x.status_msg == 'OK'
	assert x.http_version == '1.1'
	y := http.fetch(url: 'http://${server.addr}/another/endpoint', data: 'abcde')!
	assert y.body == 'abcde, /another/endpoint'
	assert y.status_code == 200
	assert x.status_msg == 'OK'
	assert y.status() == .ok
	assert y.http_version == '1.1'

	http.fetch(url: 'http://${server.addr}/something/else')!

	big_url := 'http://${server.addr}/redirect_to_big'
	mut progress_calls := &ProgressCalls{}
	z := http.fetch(
		url:         big_url
		user_ptr:    progress_calls
		on_redirect: fn (req &http.Request, nredirects int, new_url string) ! {
			mut progress_calls := unsafe { &ProgressCalls(req.user_ptr) }
			eprintln('>>>>>>>> on_redirect, req.url: ${req.url} | new_url: ${new_url} | nredirects: ${nredirects}')
			progress_calls.redirected_to << new_url
		}
		on_progress: fn (req &http.Request, chunk []u8, read_so_far u64) ! {
			mut progress_calls := unsafe { &ProgressCalls(req.user_ptr) }
			eprintln('>>>>>>>> on_progress, req.url: ${req.url} | got chunk.len: ${chunk.len:5}, read_so_far: ${read_so_far:8}, chunk: ${chunk#[0..30].bytestr()}')
			progress_calls.chunks << chunk.clone()
			progress_calls.reads << read_so_far
		}
		on_finish:   fn (req &http.Request, final_size u64) ! {
			mut progress_calls := unsafe { &ProgressCalls(req.user_ptr) }
			eprintln('>>>>>>>> on_finish, req.url: ${req.url}, final_size: ${final_size}')
			progress_calls.finished_was_called = true
			progress_calls.final_size = final_size
		}
	)!
	assert z.status_code == 200
	assert z.body.starts_with('xyz')
	assert z.body.len > 10000
	assert progress_calls.final_size > 40_000
	assert progress_calls.finished_was_called
	assert progress_calls.chunks.len > 1
	assert progress_calls.reads.len > 1
	assert progress_calls.chunks[0].bytestr().starts_with('HTTP/1.1 301 Moved permanently')
	assert progress_calls.chunks[1].bytestr().starts_with('HTTP/1.1 200 OK')
	assert progress_calls.chunks.last().bytestr().contains('xyz def')
	assert progress_calls.redirected_to == ['http://${server.addr}/big']

	server.stop()
	t.wait()

	assert handler.counter == 5
	assert handler.oks == 3
	assert handler.not_founds == 1
	assert handler.redirects == 1
}

struct ProgressCalls {
mut:
	chunks              [][]u8
	reads               []u64
	finished_was_called bool
	redirected_to       []string
	final_size          u64
}

//

struct MyCountingHandler {
mut:
	counter int
}

fn (mut handler MyCountingHandler) handle(req http.Request) http.Response {
	handler.counter++
	mut r := http.Response{
		body:   req.data + ', ${req.url}, counter: ${handler.counter}'
		header: req.header
	}
	match req.url.all_before('?') {
		'/count' {
			r.set_status(.ok)
		}
		else {
			r.set_status(.not_found)
		}
	}
	r.set_version(req.version)
	return r
}

fn test_my_counting_handler_on_random_port() {
	log.warn('${@FN} started')
	defer {
		log.warn('${@FN} finished')
	}
	mut server := &http.Server{
		show_startup_message: false
		addr:                 ''
		accept_timeout:       atimeout
		handler:              MyCountingHandler{}
		on_running:           fn (mut server http.Server) {
			spawn fn (mut server http.Server) {
				log.warn('server started')
				url := 'http://${server.addr}/count'
				log.info('fetching from url: ${url}')
				for _ in 0 .. 5 {
					x := http.fetch(url: url, data: 'my data') or { panic(err) }
					log.info(x.body)
				}
				server.stop()
				log.warn('server stopped')
			}(mut server)
		}
	}
	server.listen_and_serve()
	if mut server.handler is MyCountingHandler {
		dump(server.handler.counter)
		assert server.handler.counter == 5
	}
	assert true
}

//

struct MyCustomHttpHostHandler {}

fn (mut handler MyCustomHttpHostHandler) handle(req http.Request) http.Response {
	dump(req.header)
	return http.Response{
		body: 'Host was: ${req.header.get(.host) or { '-' }}'
	}
}

fn test_host_header_sent_to_server() {
	ip := '127.0.0.1'
	port := 54671
	log.warn('${@FN} started')
	defer { log.warn('${@FN} finished') }
	mut server := &http.Server{
		handler: MyCustomHttpHostHandler{}
		addr:    '${ip}:${port}'
	}
	t := spawn server.listen_and_serve()
	server.wait_till_running() or {
		estr := err.str()
		if estr == 'maximum retries reached' {
			log.error('>>>> Skipping test ${@FN} since its server could not start, err: ${err}')
			return
		}
		log.fatal(estr)
	}
	defer { server.stop() }
	dump(server.addr)
	x := http.get('http://${server.addr}/')!
	dump(x)
	assert x.body.ends_with('${ip}:${port}')
}

//

struct KeepAliveHandler {
mut:
	request_count int
}

fn (mut handler KeepAliveHandler) handle(req http.Request) http.Response {
	handler.request_count++
	mut r := http.Response{
		body: 'request #${handler.request_count}'
	}
	r.set_status(.ok)
	r.set_version(req.version)
	// Echo back the Connection header from the request if present
	if conn := req.header.get(.connection) {
		r.header.set(.connection, conn)
	}
	return r
}

fn test_server_keep_alive() {
	log.warn('${@FN} started')
	defer { log.warn('${@FN} finished') }
	mut handler := KeepAliveHandler{}
	mut server := &http.Server{
		accept_timeout:       atimeout
		handler:              handler
		addr:                 '127.0.0.1:18198'
		show_startup_message: false
	}
	t := spawn server.listen_and_serve()
	server.wait_till_running() or {
		estr := err.str()
		if estr == 'maximum retries reached' {
			log.error('>>>> Skipping test ${@FN} since its server could not start, err: ${err}')
			return
		}
		log.fatal(estr)
	}

	// Test keep-alive by sending multiple requests over a single TCP connection
	mut conn := net.dial_tcp('127.0.0.1:18198')!
	defer { conn.close() or {} }
	conn.set_read_timeout(5 * time.second)
	conn.set_write_timeout(5 * time.second)

	// Send first request with Connection: keep-alive
	request1 := 'GET /test1 HTTP/1.1\r\nHost: 127.0.0.1:18198\r\nConnection: keep-alive\r\n\r\n'
	conn.write(request1.bytes())!
	mut resp1 := read_http_response(mut conn)!
	log.info('Response 1: ${resp1}')
	assert resp1.contains('request #1')
	assert resp1.to_lower().contains('connection: keep-alive')

	// Send second request on the same connection
	request2 := 'GET /test2 HTTP/1.1\r\nHost: 127.0.0.1:18198\r\nConnection: keep-alive\r\n\r\n'
	conn.write(request2.bytes())!
	mut resp2 := read_http_response(mut conn)!
	log.info('Response 2: ${resp2}')
	assert resp2.contains('request #2')
	assert resp2.to_lower().contains('connection: keep-alive')

	// Send third request with Connection: close to end the connection
	request3 := 'GET /test3 HTTP/1.1\r\nHost: 127.0.0.1:18198\r\nConnection: close\r\n\r\n'
	conn.write(request3.bytes())!
	mut resp3 := read_http_response(mut conn)!
	log.info('Response 3: ${resp3}')
	assert resp3.contains('request #3')
	assert resp3.to_lower().contains('connection: close')

	server.stop()
	t.wait()

	// Verify all 3 requests were handled
	assert handler.request_count == 3
}

fn read_http_response(mut conn net.TcpConn) !string {
	mut response := []u8{}
	mut buf := []u8{len: 1024}
	mut content_length := -1
	mut headers_end := -1

	for {
		n := conn.read(mut buf) or { break }
		if n <= 0 {
			break
		}
		response << buf[..n]

		// Check if we have received all headers
		response_str := response.bytestr()
		if headers_end == -1 {
			headers_end = response_str.index('\r\n\r\n') or { -1 }
			if headers_end != -1 {
				// Parse Content-Length from headers
				headers := response_str[..headers_end]
				for line in headers.split('\r\n') {
					if line.to_lower().starts_with('content-length:') {
						content_length = line.all_after(':').trim_space().int()
						break
					}
				}
			}
		}

		// Check if we have received the full response
		if headers_end != -1 && content_length >= 0 {
			body_start := headers_end + 4
			body_received := response.len - body_start
			if body_received >= content_length {
				break
			}
		}
	}

	return response.bytestr()
}
