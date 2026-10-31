// vtest build: !windows // fasthttp.Server.run is not implemented on windows yet
import io
import net
import net.http
import time
import veb

const limited_port = 13352
const default_port = 13353
const raised_port = 13354
const unlimited_port = 13355
const rejected_port = 13356

const mib = 1024 * 1024
const default_limit = 64 * mib
const exit_after = 20 * time.second

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

// upload answers with the size of the request body.
@['/upload'; put]
pub fn (app &App) upload(mut ctx Context) veb.Result {
	return ctx.text('got ${ctx.req.data.len} bytes')
}

fn testsuite_begin() {
	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()
	mut limited := &App{}
	spawn veb.run_at[App, Context](mut limited,
		port:                  limited_port
		family:                .ip
		max_request_body_size: mib
	)
	_ := <-limited.started
	mut unset := &App{}
	spawn veb.run_at[App, Context](mut unset, port: default_port, family: .ip)
	_ := <-unset.started
	mut raised := &App{}
	spawn veb.run_at[App, Context](mut raised,
		port:                  raised_port
		family:                .ip
		max_request_body_size: 2 * default_limit
	)
	_ := <-raised.started
	mut unlimited := &App{}
	spawn veb.run_at[App, Context](mut unlimited,
		port:                  unlimited_port
		family:                .ip
		max_request_body_size: 0
	)
	_ := <-unlimited.started
}

// put sends a request that has a body of `size` bytes.
fn put(port int, size int) !http.Response {
	return http.fetch(
		url:    'http://127.0.0.1:${port}/upload'
		method: .put
		data:   'x'.repeat(size)
	)
}

// announce sends the head of a request that has a body of `size` bytes, without the body,
// then stops sending, and returns the raw response. The server decides from the head: it
// answers `413` to a body over its limit, else it waits for the body, which does not come,
// and answers `400`. That shows where the limit is, without sending bodies of many MiB.
fn announce(port int, size int) !string {
	mut conn := net.dial_tcp('127.0.0.1:${port}')!
	defer {
		conn.close() or {}
	}
	conn.set_read_timeout(5 * time.second)
	conn.set_write_timeout(5 * time.second)
	conn.write_string('PUT /upload HTTP/1.1\r\nHost: localhost\r\nContent-Length: ${size}\r\n\r\n')!
	net.shutdown(conn.sock.handle, how: .write)
	return io.read_all(reader: conn)!.bytestr()
}

fn test_a_body_up_to_the_limit_is_accepted() {
	res := put(limited_port, mib)!
	assert res.status_code == 200
	assert res.body == 'got ${mib} bytes'
}

fn test_a_body_over_the_limit_gets_413_and_the_server_keeps_running() {
	rejected := announce(limited_port, mib + 1)!
	assert rejected.starts_with('HTTP/1.1 413 '), rejected
	assert rejected.contains('Connection: close'), rejected
	// the next client is still answered
	res := put(limited_port, 10)!
	assert res.status_code == 200
	assert res.body == 'got 10 bytes'
}

fn test_the_default_limit_is_64_mib() {
	assert announce(default_port, default_limit)!.starts_with('HTTP/1.1 400 ')
	assert announce(default_port, default_limit + 1)!.starts_with('HTTP/1.1 413 ')
}

fn test_a_limit_above_the_default_is_honoured() {
	assert announce(raised_port, default_limit + 1)!.starts_with('HTTP/1.1 400 ')
	assert announce(raised_port, 2 * default_limit)!.starts_with('HTTP/1.1 400 ')
	assert announce(raised_port, 2 * default_limit + 1)!.starts_with('HTTP/1.1 413 ')
}

fn test_a_limit_of_0_accepts_a_body_of_any_size() {
	assert announce(unlimited_port, 16 * default_limit)!.starts_with('HTTP/1.1 400 ')
	res := put(unlimited_port, 2 * mib)!
	assert res.status_code == 200
	assert res.body == 'got ${2 * mib} bytes'
}

fn test_a_negative_limit_is_an_error() {
	mut app := &App{}
	veb.run_at[App, Context](mut app, port: rejected_port, family: .ip, max_request_body_size: -1) or {
		assert err.msg().contains('invalid max_request_body_size `-1`'), err.msg()
		return
	}
	assert false, 'veb.run_at should have returned an error'
}
