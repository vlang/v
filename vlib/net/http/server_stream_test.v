import net
import net.http
import time

struct StreamingHandler {}

fn (h StreamingHandler) handle(req http.Request) http.Response {
	if req.url == '/plain' {
		return http.Response{
			body:        'plain'
			status_code: 200
		}
	}
	if req.url.starts_with('/status/') {
		return http.Response{
			status_code: req.url.all_after_last('/').int()
			stream:      fn (mut writer http.StreamWriter) ! {
				writer.write_string('hello')!
			}
		}
	}
	mut header := http.new_header(key: .content_type, value: 'text/event-stream')
	return http.Response{
		status_code: 200
		header:      header
		stream:      fn (mut writer http.StreamWriter) ! {
			writer.write_event(http.SSEEvent{ event: 'first', data: 'one' })!
			// Held open: another connection is answered meanwhile.
			time.sleep(300 * time.millisecond)
			writer.write_event(http.SSEEvent{ event: 'second', data: 'two\nlines' })!
		}
	}
}

fn read_all(mut conn net.TcpConn) string {
	mut all := []u8{}
	mut buffer := []u8{len: 1024}
	for {
		n := conn.read(mut buffer) or { break }
		if n <= 0 {
			break
		}
		all << buffer[..n]
	}
	return all.bytestr()
}

fn start_server() !(&http.Server, thread, string) {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	address := listener.addr()!.str()
	mut server := &http.Server{
		addr:                 address
		listener:             listener
		handler:              StreamingHandler{}
		worker_num:           2
		accept_timeout:       100 * time.millisecond
		show_startup_message: false
	}
	serving := spawn server.listen_and_serve()
	for server.status() != .running {
		time.sleep(10 * time.millisecond)
	}
	return server, serving, address
}

fn request(address string, raw string) !string {
	mut conn := net.dial_tcp(address)!
	defer {
		conn.close() or {}
	}
	conn.set_read_timeout(5 * time.second)
	conn.write_string(raw)!
	return read_all(mut conn)
}

fn test_a_handler_streams_its_body_on_its_connection() {
	mut server, serving, address := start_server()!
	defer {
		server.stop()
		serving.wait()
	}
	mut conn := net.dial_tcp(address)!
	conn.set_read_timeout(5 * time.second)
	conn.write_string('GET /events HTTP/1.1\r\nHost: test\r\n\r\n')!
	started := time.new_stopwatch()
	plain := http.get('http://${address}/plain')!
	assert plain.body == 'plain'
	assert started.elapsed() < 250 * time.millisecond, 'another connection was answered while the stream was open'
	text := read_all(mut conn)
	head := text.all_before('\r\n\r\n').to_lower()
	body := text.all_after('\r\n\r\n')
	assert head.starts_with('http/1.1 200 ok')
	assert head.contains('content-type: text/event-stream')
	assert head.contains('connection: close')
	assert !head.contains('content-length')
	assert body == 'event: first\ndata: one\n\nevent: second\ndata: two\ndata: lines\n\n'
}

fn test_a_response_without_a_body_is_its_head_alone() {
	mut server, serving, address := start_server()!
	defer {
		server.stop()
		serving.wait()
	}
	started := time.new_stopwatch()
	head_text := request(address, 'HEAD /events HTTP/1.1\r\nHost: test\r\n\r\n')!
	assert started.elapsed() < 250 * time.millisecond, 'HEAD started the stream'
	head := head_text.all_before('\r\n\r\n').to_lower()
	assert head.starts_with('http/1.1 200 ok')
	assert head.contains('content-type: text/event-stream')
	assert head.contains('connection: close')
	assert !head.contains('content-length')
	assert head_text.all_after('\r\n\r\n') == ''

	// 100 and 199 cover the whole informational class, not only 101 to 103.
	for code in [100, 199, 204, 304] {
		text := request(address, 'GET /status/${code} HTTP/1.1\r\nHost: test\r\n\r\n')!
		status_head := text.all_before('\r\n\r\n').to_lower()
		assert status_head.starts_with('http/1.1 ${code}'), 'status ${code}'
		assert !status_head.contains('content-length'), 'status ${code}'
		assert text.all_after('\r\n\r\n') == '', 'status ${code} sent a body'
	}
}
