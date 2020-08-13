module main

import os
import vweb
import time

struct App {
	port    int
	timeout int
pub mut:
	vweb    vweb.Context
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
	eprintln('Started webserver on http://127.0.0.1:$app.port/ , with maximum runtime of $app.timeout milliseconds.')
}

pub fn (mut app App) index() {
	app.vweb.text('Welcome to VWeb')
}

pub fn (mut app App) simple() vweb.Result {
	app.vweb.text('A simple result')
	return vweb.Result{}
}

pub fn (mut app App) html_page() vweb.Result {
	app.vweb.html('<h1>ok</h1>')
	return vweb.Result{}
}

// the following serve custom routes
['/:user/settings']
pub fn (mut app App) settings(username string) vweb.Result {
	app.vweb.html('username: $username')
	return vweb.Result{}
}

['/:user/:repo/settings']
pub fn (mut app App) user_repo_settings(username, repository string) vweb.Result {
	app.vweb.html('username: $username | repository: $repository')
	return vweb.Result{}
}
