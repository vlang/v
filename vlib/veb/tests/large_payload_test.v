// vtest retry: 3
import veb
import net.http
import time
import os

const port = 13002

const localserver = 'http://127.0.0.1:${port}'

const exit_after = time.second * 10

const tmp_file = os.join_path(os.vtmp_dir(), 'veb_large_payload.txt')

pub struct App {
mut:
	started chan bool
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	return ctx.text('Hello V!')
}

@[post]
pub fn (mut app App) post_request(mut ctx Context) veb.Result {
	return ctx.text(ctx.req.data)
}

pub fn (app &App) file(mut ctx Context) veb.Result {
	return ctx.file(tmp_file)
}

pub struct Context {
	veb.Context
}

fn testsuite_begin() {
	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()

	mut app := &App{}
	spawn veb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 2, family: .ip)
	// app startup time
	_ := <-app.started
}

fn test_large_request_body() {
	// string of a's of 8.96mb send over the connection
	// veb reads a maximum of 4096KB per picoev loop cycle
	// this test tests if veb is able to do multiple of these
	// cycles and updates the response body each cycle
	mut buf := []u8{len: veb.max_read * 10, init: `a`}

	str := buf.bytestr()
	mut x := http.post('${localserver}/post_request', str)!

	assert x.body.len == veb.max_read * 10
}

fn test_large_request_header() {
	// same test as test_large_request_body, but then with a large header,
	// which is parsed separately
	mut buf := []u8{len: veb.max_read * 2, init: `a`}

	str := buf.bytestr()
	// make 1 header longer than vebs max read limit
	mut x := http.fetch(http.FetchConfig{
		url:    localserver
		header: http.new_custom_header_from_map({
			'X-Overflow-Header': str
		})!
	})!

	assert x.status() == .request_entity_too_large
}

fn test_bigger_content_length() {
	data := '123456789'
	mut x := http.fetch(http.FetchConfig{
		method: .post
		url:    '${localserver}/post_request'
		header: http.new_header_from_map({
			.content_length: '10'
		})
		data:   data
	})!

	// Content-length is larger than the data sent, so the request should timeout
	assert x.status() == .request_timeout
}

fn test_smaller_content_length() {
	data := '123456789'
	mut x := http.fetch(http.FetchConfig{
		method: .post
		url:    '${localserver}/post_request'
		header: http.new_header_from_map({
			.content_length: '5'
		})
		data:   data
	})!

	assert x.status() == .bad_request
	assert x.body == 'Mismatch of body length and Content-Length header'
}

fn test_sendfile() {
	mut buf := []u8{len: veb.max_write * 10, init: `a`}
	os.write_file(tmp_file, buf.bytestr())!

	x := http.get('${localserver}/file')!

	assert x.body.len == veb.max_write * 10
}

fn testsuite_end() {
	os.rm(tmp_file)!
}
