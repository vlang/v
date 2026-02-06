import net
import net.http
import io
import os
import time
import veb

const exit_after = time.second * 10
const port = 13009
const localserver = 'localhost:${port}'
const tcp_r_timeout = 2 * time.second
const tcp_w_timeout = 2 * time.second
const max_retries = 4

const default_request = 'GET / HTTP/1.1
User-Agent: VTESTS
Accept: */*
\r\n'

const response_body = 'intact!'

pub struct Context {
	veb.Context
}

pub struct App {
mut:
	started chan bool
	counter int
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	app.counter++
	return ctx.text('${response_body}:${app.counter}')
}

pub fn (mut app App) reset(mut ctx Context) veb.Result {
	app.counter = 0
	return ctx.ok('')
}

fn testsuite_begin() {
	os.chdir(os.dir(@FILE))!
	mut app := &App{}

	spawn veb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 5)
	_ := <-app.started

	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()
}

fn test_conn_remains_intact() {
	http.get('http://${localserver}/reset')!

	mut conn := simple_tcp_client()!
	conn.write_string(default_request)!

	mut read := io.read_all(reader: conn)!
	mut response := read.bytestr()
	assert response.contains('Connection: close') == false, '`Connection` header should NOT be present!'
	assert response.ends_with('${response_body}:1') == true, 'read response: ${response}'

	// send request again over the same connection
	conn.write_string(default_request)!

	read = io.read_all(reader: conn)!
	response = read.bytestr()
	assert response.contains('Connection: close') == false, '`Connection` header should NOT be present!'
	assert response.ends_with('${response_body}:2') == true, 'read response: ${response}'

	conn.close() or {}
}

fn test_support_http_1() {
	http.get('http://${localserver}/reset')!
	// HTTP 1.0 always closes the connection after each request, so the client must
	// send the Connection: close header. If that header is present the connection
	// needs to be closed and a `Connection: close` header needs to be send back
	mut x := http.fetch(http.FetchConfig{
		url:    'http://${localserver}/'
		header: http.new_header_from_map({
			.connection: 'close'
		})
	})!
	assert x.status() == .ok
	if conn_header := x.header.get(.connection) {
		assert conn_header == 'close'
	} else {
		assert false, '`Connection: close` header should be present!'
	}
}

// utility code:

fn simple_tcp_client() !&net.TcpConn {
	mut client := &net.TcpConn(unsafe { nil })
	mut tries := 0
	for tries < max_retries {
		tries++
		eprintln('> client retries: ${tries}')
		client = net.dial_tcp(localserver) or {
			eprintln('dial error: ${err.msg()}')
			if tries > max_retries {
				return err
			}
			time.sleep(100 * time.millisecond)
			continue
		}
		break
	}
	if client == unsafe { nil } {
		eprintln('could not create a tcp client connection to http://${localserver} after ${max_retries} retries')
		exit(1)
	}
	client.set_read_timeout(tcp_r_timeout)
	client.set_write_timeout(tcp_w_timeout)
	return client
}
