module main

import vweb
import time
import os

struct App {
	vweb.Context
	vweb.Controller
	timeout int
}

struct Admin {
	vweb.Context
}

struct Other {
	vweb.Context
}

struct OtherHidedByOther {
	vweb.Context
}

fn exit_after_timeout(timeout_in_ms int) {
	time.sleep(timeout_in_ms * time.millisecond)
	println('>> webserver: pid: ${os.getpid()}, exiting ...')
	exit(0)
}

fn main() {
	if os.args.len != 3 {
		panic('Usage: `controller_test_server.exe PORT TIMEOUT_IN_MILLISECONDS`')
	}
	http_port := os.args[1].int()
	assert http_port > 0
	timeout := os.args[2].int()
	spawn exit_after_timeout(timeout)

	mut app := &App{
		timeout:     timeout
		controllers: [
			vweb.controller('/admin', &Admin{}),
			vweb.controller('/other', &Other{}),
			vweb.controller('/other/hide', &OtherHidedByOther{}),
		]
	}

	eprintln('>> webserver: pid: ${os.getpid()}, started on http://localhost:${http_port}/ , with maximum runtime of ${app.timeout} milliseconds.')
	vweb.run_at(app, host: 'localhost', port: http_port, family: .ip)!
}

@['/']
pub fn (mut app App) home() vweb.Result {
	return app.text('App')
}

@['/path']
pub fn (mut app App) app_path() vweb.Result {
	return app.text('App path')
}

pub fn (mut app App) not_found() vweb.Result {
	app.set_status(404, 'Not Found')
	return app.text('404 From App')
}

@['/']
pub fn (mut app Admin) admin_home() vweb.Result {
	return app.text('Admin')
}

@['/path']
pub fn (mut app Admin) admin_path() vweb.Result {
	return app.text('Admin path')
}

pub fn (mut app Admin) not_found() vweb.Result {
	app.set_status(404, 'Not Found')
	return app.text('404 From Admin')
}

@['/']
pub fn (mut app Other) other_home() vweb.Result {
	return app.text('Other')
}

@['/path']
pub fn (mut app Other) other_path() vweb.Result {
	return app.text('Other path')
}

@['/']
pub fn (mut app OtherHidedByOther) other_home() vweb.Result {
	return app.text('Other')
}

@['/path']
pub fn (mut app OtherHidedByOther) other_path() vweb.Result {
	return app.text('Other path')
}

// utility functions:

pub fn (mut app App) shutdown() vweb.Result {
	session_key := app.get_cookie('skey') or { return app.not_found() }
	if session_key != 'superman' {
		return app.not_found()
	}
	spawn app.exit_gracefully()
	return app.ok('good bye')
}

fn (mut app App) exit_gracefully() {
	eprintln('>> webserver: exit_gracefully')
	time.sleep(100 * time.millisecond)
	exit(0)
}
