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

fn test_a_handler_streams_its_body_on_its_connection() {
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
