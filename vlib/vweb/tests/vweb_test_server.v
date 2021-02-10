module main

import os
import vweb
import time

const (
	known_users = ['bilbo', 'kent']
)

struct App {
	vweb.Context
	port    int
	timeout int
}

fn exit_after_timeout(timeout_in_ms int) {
	time.sleep_ms(timeout_in_ms)
	// eprintln('webserver is exiting ...')
	exit(0)
}

fn main() {
	if os.args.len != 3 {
		panic('Usage: `vweb_test_server.exe PORT TIMEOUT_IN_MILLISECONDS`')
	}
	http_port := os.args[1].int()
	assert http_port > 0
	timeout := os.args[2].int()
	assert timeout > 0
	go exit_after_timeout(timeout)
	//
	mut app := App{
		port: http_port
		timeout: timeout
	}
	vweb.run_app<App>(mut app, http_port)
}

pub fn (mut app App) init() {
}

pub fn (mut app App) init_once() {
	eprintln('>> webserver: started on http://127.0.0.1:$app.port/ , with maximum runtime of $app.timeout milliseconds.')
}

pub fn (mut app App) index() vweb.Result {
	return app.text('Welcome to VWeb')
}

pub fn (mut app App) simple() vweb.Result {
	return app.text('A simple result')
}

pub fn (mut app App) html_page() vweb.Result {
	return app.html('<h1>ok</h1>')
}

pub fn (mut app App) chunk() vweb.Result {
	app.enable_chunked_transfer(20)
	return app.html('Lorem ipsum dolor sit amet, consetetur sadipscing')
}

// the following serve custom routes
['/:user/settings']
pub fn (mut app App) settings(username string) vweb.Result {
	if username !in known_users {
		return app.not_found()
	}
	return app.html('username: $username')
}

['/:user/:repo/settings']
pub fn (mut app App) user_repo_settings(username string, repository string) vweb.Result {
	if username !in known_users {
		return app.not_found()
	}
	return app.html('username: $username | repository: $repository')
}

[post]
['/json_echo']
pub fn (mut app App) json_echo() vweb.Result {
	eprintln('>>>>> received http request at /json_echo is: $app.req')
	app.set_content_type(app.req.headers['Content-Type'])
	return app.ok(app.req.data)
}

// Make sure [post] works without the path
[post]
pub fn (mut app App) json() vweb.Result {
	eprintln('>>>>> received http request at /json is: $app.req')
	app.set_content_type(app.req.headers['Content-Type'])
	return app.ok(app.req.data)
}

pub fn (mut app App) shutdown() vweb.Result {
	session_key := app.get_cookie('skey') or { return app.not_found() }
	if session_key != 'superman' {
		return app.not_found()
	}
	go app.gracefull_exit()
	return app.ok('good bye')
}

fn (mut app App) gracefull_exit() {
	eprintln('>> webserver: gracefull_exit')
	time.sleep_ms(100)
	exit(0)
}
