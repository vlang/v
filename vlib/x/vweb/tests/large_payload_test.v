// vtest flaky: true
// vtest retry: 3
import x.vweb
import net.http
import time
import os

const port = 13002

const localserver = 'http://127.0.0.1:${port}'

const exit_after = time.second * 10

const tmp_file = os.join_path(os.vtmp_dir(), 'vweb_large_payload.txt')

pub struct App {}

pub fn (mut app App) index(mut ctx Context) vweb.Result {
	return ctx.text('Hello V!')
}

@[post]
pub fn (mut app App) post_request(mut ctx Context) vweb.Result {
	return ctx.text(ctx.req.data)
}

pub fn (app &App) file(mut ctx Context) vweb.Result {
	return ctx.file(tmp_file)
}

pub struct Context {
	vweb.Context
}

fn testsuite_begin() {
	spawn fn () {
		mut app := &App{}
		vweb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 2, family: .ip) or {
			panic('could not start vweb app')
		}
	}()

	// app startup time
	time.sleep(time.millisecond * 500)

	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()
}

fn test_large_request_body() {
	// string of a's of 8.96mb send over the connection
	// vweb reads a maximum of 4096KB per picoev loop cycle
	// this test tests if vweb is able to do multiple of these
	// cycles and updates the response body each cycle
	mut buf := []u8{len: vweb.max_read * 10, init: `a`}

	str := buf.bytestr()
	mut x := http.post('${localserver}/post_request', str)!

	assert x.body.len == vweb.max_read * 10
}

fn test_large_request_header() {
	// same test as test_large_request_body, but then with a large header,
	// which is parsed seperately
	mut buf := []u8{len: vweb.max_read * 2, init: `a`}

	str := buf.bytestr()
	// make 1 header longer than vwebs max read limit
	mut x := http.fetch(http.FetchConfig{
		url: localserver
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
		url: '${localserver}/post_request'
		header: http.new_header_from_map({
			.content_length: '10'
		})
		data: data
	})!

	assert x.status() == .bad_request
	assert x.body == 'Mismatch of body length and Content-Length header'
}

fn test_smaller_content_length() {
	data := '123456789'
	mut x := http.fetch(http.FetchConfig{
		method: .post
		url: '${localserver}/post_request'
		header: http.new_header_from_map({
			.content_length: '5'
		})
		data: data
	})!

	assert x.status() == .bad_request
	assert x.body == 'Mismatch of body length and Content-Length header'
}

fn test_sendfile() {
	mut buf := []u8{len: vweb.max_write * 10, init: `a`}
	os.write_file(tmp_file, buf.bytestr())!

	x := http.get('${localserver}/file')!

	assert x.body.len == vweb.max_write * 10
}

fn testsuite_end() {
	os.rm(tmp_file)!
}
