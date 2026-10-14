// vtest build: !windows // fasthttp.Server.run is not implemented on windows yet
import io
import net
import net.http
import time
import veb

const port = 13071
const exit_after = 10 * time.second

pub struct Context {
	veb.Context
}

pub struct App {
mut:
	started chan bool
}

// before_accept_loop tells the test that the server is listening.
pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

// index answers with the number of header fields that the request has.
pub fn (app &App) index(mut ctx Context) veb.Result {
	return ctx.text('fields:${ctx.req.header.keys().len}')
}

fn testsuite_begin() {
	mut app := &App{}
	spawn veb.run_at[App, Context](mut app, port: port, family: .ip, timeout_in_seconds: 5)
	_ := <-app.started
	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()
}

// get sends a request that has `nfields` header fields, and returns the raw response.
fn get(nfields int) !string {
	mut lines := ['GET / HTTP/1.1', 'Host: localhost', 'Connection: close']
	for i in 2 .. nfields {
		lines << 'X-Field-${i}: v'
	}
	mut conn := net.dial_tcp('127.0.0.1:${port}')!
	defer {
		conn.close() or {}
	}
	conn.set_read_timeout(2 * time.second)
	conn.set_write_timeout(2 * time.second)
	conn.write_string(lines.join('\r\n') + '\r\n\r\n')!
	return io.read_all(reader: conn)!.bytestr()
}

// A request can fill http.Header completely: veb stores the fields that the
// request has, and none of its own.
fn test_request_with_max_headers_fields_is_served() {
	response := get(http.max_headers)!
	assert response.starts_with('HTTP/1.1 200 OK'), response
	assert response.ends_with('fields:${http.max_headers}'), response
}

fn test_request_with_too_many_header_fields_gets_431_and_the_server_keeps_running() {
	for nfields in [http.max_headers + 1, 4 * http.max_headers] {
		rejected := get(nfields)!
		assert rejected.starts_with('HTTP/1.1 431 Request Header Fields Too Large'), rejected
		// the next client is still answered
		served := get(2)!
		assert served.starts_with('HTTP/1.1 200 OK'), served
		assert served.ends_with('fields:2'), served
	}
}
