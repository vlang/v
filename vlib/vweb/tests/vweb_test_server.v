module main

import os
import vweb
import time

const (
	known_users = ['bilbo', 'kent']
)

struct App {
	port    int
	timeout int
}

fn exit_after_timeout(timeout_in_ms int) {
	time.sleep(timeout_in_ms * time.millisecond)
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
	eprintln('>> webserver: started on http://127.0.0.1:$app.port/ , with maximum runtime of $app.timeout milliseconds.')
	vweb.run_app<App>(mut app, port: http_port)
}

pub fn (mut app App) index(mut c vweb.Context) vweb.Result {
	return c.text('Welcome to VWeb')
}

pub fn (mut app App) simple(mut c vweb.Context) vweb.Result {
	return c.text('A simple result')
}

pub fn (mut app App) html_page(mut c vweb.Context) vweb.Result {
	return c.html('<h1>ok</h1>')
}

pub fn (mut app App) chunk(mut c vweb.Context) vweb.Result {
	c.enable_chunked_transfer(20)
	return c.html('Lorem ipsum dolor sit amet, consetetur sadipscing')
}

// the following serve custom routes
['/:user/settings']
pub fn (mut app App) settings(mut c vweb.Context, username string) vweb.Result {
	if username !in known_users {
		return c.not_found()
	}
	return c.html('username: $username')
}

['/:user/:repo/settings']
pub fn (mut app App) user_repo_settings(mut c vweb.Context, username string, repository string) vweb.Result {
	if username !in known_users {
		return c.not_found()
	}
	return c.html('username: $username | repository: $repository')
}

['/json_echo'; post]
pub fn (mut app App) json_echo(mut c vweb.Context) vweb.Result {
	// eprintln('>>>>> received http request at /json_echo is: $c.req')
	c.set_content_type(c.req.headers['Content-Type'])
	return c.ok(c.req.data)
}

// Make sure [post] works without the path
[post]
pub fn (mut app App) json(mut c vweb.Context) vweb.Result {
	// eprintln('>>>>> received http request at /json is: $c.req')
	c.set_content_type(c.req.headers['Content-Type'])
	return c.ok(c.req.data)
}

pub fn (mut app App) shutdown(mut c vweb.Context) vweb.Result {
	session_key := c.get_cookie('skey') or { return c.not_found() }
	if session_key != 'superman' {
		return c.not_found()
	}
	go app.gracefull_exit()
	return c.ok('good bye')
}

fn (mut app App) gracefull_exit() {
	eprintln('>> webserver: gracefull_exit')
	time.sleep(100 * time.millisecond)
	exit(0)
}
