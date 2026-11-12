module test_utils

import io
import net
import net.http
import time

// MockHttpServer serves a test handler on a private loopback port.
// Its worker owns the listener and all connections until close joins it.
pub struct MockHttpServer {
pub:
	base_url string
mut:
	stop   chan bool
	worker thread
}

// start_mock_http_server binds before starting its worker, so callers can send
// requests immediately without polling mutable server state.
pub fn start_mock_http_server(handler http.Handler) !MockHttpServer {
	mut listener := net.listen_tcp(.ip, '127.0.0.1:0')!
	address := listener.addr()!.str()
	listener.set_accept_timeout(20 * time.millisecond)
	stop := chan bool{cap: 1}
	worker := spawn serve_mock_http(mut listener, handler, stop)
	return MockHttpServer{
		base_url: 'http://${address}'
		stop:     stop
		worker:   worker
	}
}

// close stops the worker and waits for it to release its listener and connections.
pub fn (mut server MockHttpServer) close() {
	server.stop <- true
	server.worker.wait()
	server.stop.close()
}

fn serve_mock_http(mut listener net.TcpListener, handler http.Handler, stop chan bool) {
	defer { listener.close() or {} }
	mut current_handler := handler
	for {
		select {
			_ := <-stop {
				return
			}
			else {
			}
		}
		mut conn := listener.accept() or {
			if err.code() == net.err_timed_out_code {
				continue
			}
			panic(err)
		}
		serve_mock_http_request(mut conn, mut current_handler)
	}
}

fn serve_mock_http_request(mut conn net.TcpConn, mut handler http.Handler) {
	defer { conn.close() or {} }
	conn.set_read_timeout(2 * time.second)
	conn.set_write_timeout(2 * time.second)
	mut reader := io.new_buffered_reader(reader: conn)
	defer { unsafe { reader.free() } }
	request := http.parse_request(mut reader) or { panic(err) }
	mut response := handler.handle(request)
	if response.status_code == 0 {
		response.set_status(.ok)
	}
	if response.status_msg == '' {
		response.status_msg = http.status_from_int(response.status_code).str()
	}
	response.set_version(request.version)
	response.header.set(.content_length, response.body.len.str())
	response.header.set(.connection, 'close')
	conn.write(response.bytes()) or { panic(err) }
}
