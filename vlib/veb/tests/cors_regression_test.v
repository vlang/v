import veb
import net.http
import time
import os

const base_port = 13013
const exit_after = time.second * 10
const allowed_origin = 'https://vlang.io'

fn get_port_and_url(test_number int) (int, string) {
	p := base_port + test_number
	return p, 'http://localhost:${p}'
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

fn setup(port int, o veb.CorsOptions) ! {
	os.chdir(os.dir(@FILE))!
	go fn () {
		time.sleep(exit_after)
		assert false, 'timeout reached!'
		exit(1)
	}()

	mut app := &App{}
	app.use(veb.cors[Context](o))

	go veb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 2)
	// app startup time
	_ := <-app.started
}

fn test_no_user_provided_allowed_headers() {
	port, localserver := get_port_and_url(1)
	setup(port, veb.CorsOptions{
		origins: [allowed_origin]
	})!

	x := http.fetch(http.FetchConfig{
		url:    localserver
		method: http.Method.options
		header: http.new_header_from_map({
			http.CommonHeader.origin: allowed_origin
		})
	})!

	assert x.status() == http.Status.ok
	if header := x.header.get(.access_control_allow_headers) {
		assert false, 'Header should not be set'
	}
}

fn test_user_provided_allowed_header() {
	port, localserver := get_port_and_url(2)
	setup(port, veb.CorsOptions{
		origins:         [allowed_origin]
		allowed_headers: ['content-type']
	})!

	x := http.fetch(http.FetchConfig{
		url:    localserver
		method: http.Method.options
		header: http.new_header_from_map({
			http.CommonHeader.origin: allowed_origin
		})
	})!

	assert x.status() == http.Status.ok
	if header := x.header.get(.access_control_allow_headers) {
		assert header == 'content-type'
	} else {
		assert false, 'Header not set'
	}
}

fn test_user_provided_allowed_header_wildcard() {
	port, localserver := get_port_and_url(3)
	setup(port, veb.CorsOptions{
		origins:         [allowed_origin]
		allowed_headers: ['*']
	})!

	x := http.fetch(http.FetchConfig{
		url:    localserver
		method: http.Method.options
		header: http.new_header_from_map({
			http.CommonHeader.origin: allowed_origin
		})
	})!

	assert x.status() == http.Status.ok
	if header := x.header.get(.access_control_allow_headers) {
		assert header == '*'
	} else {
		assert false, 'Header not set'
	}
}

fn test_request_has_access_control_request_headers() {
	port, localserver := get_port_and_url(4)
	setup(port, veb.CorsOptions{
		origins: [allowed_origin]
	})!

	x := http.fetch(http.FetchConfig{
		url:    localserver
		method: http.Method.options
		header: http.new_header_from_map({
			http.CommonHeader.origin:                         allowed_origin
			http.CommonHeader.access_control_request_headers: 'any-value'
		})
	})!

	assert x.status() == http.Status.ok
	if header := x.header.get(http.CommonHeader.access_control_allow_headers) {
		assert header == veb.cors_safelisted_response_headers
	} else {
		assert false, 'Header not set'
	}
}

fn test_allow_credentials_non_preflight() {
	port, localserver := get_port_and_url(5)
	setup(port, veb.CorsOptions{
		origins:           [allowed_origin]
		allowed_methods:   [http.Method.get]
		allow_credentials: true
	})!

	x := http.fetch(http.FetchConfig{
		url:    localserver
		header: http.new_header_from_map({
			http.CommonHeader.origin: allowed_origin
		})
	})!

	assert x.status() == http.Status.ok
	if header := x.header.get(http.CommonHeader.access_control_allow_credentials) {
		assert header == 'true'
	} else {
		assert false, 'Header not set'
	}
}
