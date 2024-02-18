// vtest flaky: true
// vtest retry: 3
import time
import x.vweb
import net
import net.http

const port = 13020
const exit_after = time.second * 20

pub struct Context {
	vweb.Context
}

pub struct App {
mut:
	started chan bool
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

pub fn (mut app App) index(mut ctx Context) vweb.Result {
	return ctx.text('Hello V!')
}

fn testsuite_begin() {
	mut app := &App{}
	spawn vweb.run_at[App, Context](mut app,
		port: port
		timeout_in_seconds: 15
		config: vweb.Config{
			max_nr_of_incomplete_requests: 1
		}
	)
	_ := <-app.started

	spawn fn () {
		time.sleep(exit_after)
		assert false, 'timeout reached'
		exit(1)
	}()
}

fn test_incomplete_request() {
	// simulate a slow network where the request headers are received in parts
	// we set the maximum number of incomplete requests on a connection to 1.
	// This means that when we send another incomplete request we expect vweb
	// to close the connection.

	mut conn := net.dial_tcp('127.0.0.1:${port}')!
	defer {
		conn.close() or {}
	}

	conn.write_string('GET / HTTP/1.1\r
User-Agent: V\r
Acc')!

	// simulate network delay
	time.sleep(time.second * 2)
	conn.write_string('ept: */*\r\n\r\n')!

	// simulate network delay
	time.sleep(time.second * 2)
	mut buf := []u8{len: 86}
	conn.read(mut buf)!
	resp := http.parse_response(buf.bytestr())!
	assert resp.status() == .ok
	assert resp.body == 'Hello V!'

	// do second request

	// simulate network delay
	time.sleep(time.second * 2)
	conn.write_string('GET / HTTP/1.1\r
User-Agent: V\r
Acc')!

	// simulate network delay
	time.sleep(time.second * 2)

	buf = []u8{len: 103}
	conn.read(mut buf)!
	bad_resp := http.parse_response(buf.bytestr())!
	assert bad_resp.status() == .bad_request
}
