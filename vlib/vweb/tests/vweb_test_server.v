module main

import os
import vweb
import time

const (
	known_users = ['bilbo', 'kent']
)

struct App {
	vweb.Context
	port          int
	timeout       int
	global_config shared Config
}

struct Config {
	max_ping int
}

fn exit_after_timeout(timeout_in_ms int) {
	time.sleep(timeout_in_ms * time.millisecond)
	println('>> webserver: pid: $os.getpid(), exiting ...')
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
	spawn exit_after_timeout(timeout)
	//
	shared config := &Config{
		max_ping: 50
	}
	app := &App{
		port: http_port
		timeout: timeout
		global_config: config
	}
	eprintln('>> webserver: pid: $os.getpid(), started on http://localhost:$app.port/ , with maximum runtime of $app.timeout milliseconds.')
	vweb.run_at(app, host: 'localhost', port: http_port, family: .ip)?
}

// pub fn (mut app App) init_server() {
//}

pub fn (mut app App) index() vweb.Result {
	rlock app.global_config {
		assert app.global_config.max_ping == 50
	}
	return app.text('Welcome to VWeb')
}

pub fn (mut app App) simple() vweb.Result {
	return app.text('A simple result')
}

pub fn (mut app App) html_page() vweb.Result {
	return app.html('<h1>ok</h1>')
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

['/json_echo'; post]
pub fn (mut app App) json_echo() vweb.Result {
	// eprintln('>>>>> received http request at /json_echo is: $app.req')
	app.set_content_type(app.req.header.get(.content_type) or { '' })
	return app.ok(app.req.data)
}

['/form_echo'; post]
pub fn (mut app App) form_echo() vweb.Result {
	app.set_content_type(app.req.header.get(.content_type) or { '' })
	return app.ok(app.form['foo'])
}

// Make sure [post] works without the path
[post]
pub fn (mut app App) json() vweb.Result {
	// eprintln('>>>>> received http request at /json is: $app.req')
	app.set_content_type(app.req.header.get(.content_type) or { '' })
	return app.ok(app.req.data)
}

pub fn (mut app App) shutdown() vweb.Result {
	session_key := app.get_cookie('skey') or { return app.not_found() }
	if session_key != 'superman' {
		return app.not_found()
	}
	spawn app.gracefull_exit()
	return app.ok('good bye')
}

fn (mut app App) gracefull_exit() {
	eprintln('>> webserver: gracefull_exit')
	time.sleep(100 * time.millisecond)
	exit(0)
}
