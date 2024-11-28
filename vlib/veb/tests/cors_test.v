import veb
import net.http
import os
import time

const port = 13012
const localserver = 'http://localhost:${port}'
const exit_after = time.second * 10
const allowed_origin = 'https://vlang.io'
const cors_options = veb.CorsOptions{
	origins:         [allowed_origin]
	allowed_methods: [.get, .head]
}

pub struct Context {
	veb.Context
}

pub struct App {
	veb.Middleware[Context]
mut:
	started chan bool
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

pub fn (app &App) index(mut ctx Context) veb.Result {
	return ctx.text('index')
}

@[post]
pub fn (app &App) post(mut ctx Context) veb.Result {
	return ctx.text('post')
}

fn testsuite_begin() {
	os.chdir(os.dir(@FILE))!
	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()

	mut app := &App{}
	app.use(veb.cors[Context](cors_options))

	spawn veb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 2)
	// app startup time
	_ := <-app.started
}

fn test_valid_cors() {
	x := http.fetch(http.FetchConfig{
		url:    localserver
		method: .get
		header: http.new_header_from_map({
			.origin: allowed_origin
		})
	})!

	assert x.status() == .ok
	assert x.body == 'index'
}

fn test_preflight() {
	x := http.fetch(http.FetchConfig{
		url:    localserver
		method: .options
		header: http.new_header_from_map({
			.origin: allowed_origin
		})
	})!
	assert x.status() == .ok
	assert x.body == 'ok'

	assert x.header.get(.access_control_allow_origin)! == allowed_origin
	if _ := x.header.get(.access_control_allow_credentials) {
		assert false, 'Access-Control-Allow-Credentials should not be present the value is `false`'
	}
	assert x.header.get(.access_control_allow_methods)! == 'GET, HEAD'
}

fn test_invalid_origin() {
	x := http.fetch(http.FetchConfig{
		url:    localserver
		method: .get
		header: http.new_header_from_map({
			.origin: 'https://google.com'
		})
	})!

	assert x.status() == .forbidden
}

fn test_invalid_method() {
	x := http.fetch(http.FetchConfig{
		url:    '${localserver}/post'
		method: .post
		header: http.new_header_from_map({
			.origin: allowed_origin
		})
	})!

	assert x.status() == .method_not_allowed
}
