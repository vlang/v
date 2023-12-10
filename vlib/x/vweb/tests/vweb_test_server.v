module main

import os
import x.vweb
import time

const known_users = ['bilbo', 'kent']

struct ServerContext {
	vweb.Context
}

// Custom 404 page
pub fn (mut ctx ServerContext) not_found() vweb.Result {
	ctx.res.set_status(.not_found)
	return ctx.html('404 on "${ctx.req.url}"')
}

pub struct ServerApp {
	port          int
	timeout       int
	global_config Config
}

struct Config {
	max_ping int
}

fn exit_after_timeout(timeout_in_ms int) {
	time.sleep(timeout_in_ms * time.millisecond)
	println('>> webserver: pid: ${os.getpid()}, exiting ...')
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
	mut app := &ServerApp{
		port: http_port
		timeout: timeout
		global_config: Config{
			max_ping: 50
		}
	}
	eprintln('>> webserver: pid: ${os.getpid()}, started on http://localhost:${app.port}/ , with maximum runtime of ${app.timeout} milliseconds.')
	vweb.run_at[ServerApp, ServerContext](mut app, host: 'localhost', port: http_port, family: .ip)!
}

// pub fn (mut app ServerApp) init_server() {
//}

pub fn (mut app ServerApp) index(mut ctx ServerContext) vweb.Result {
	assert app.global_config.max_ping == 50
	return ctx.text('Welcome to VWeb')
}

pub fn (mut app ServerApp) simple(mut ctx ServerContext) vweb.Result {
	return ctx.text('A simple result')
}

pub fn (mut app ServerApp) html_page(mut ctx ServerContext) vweb.Result {
	return ctx.html('<h1>ok</h1>')
}

// the following serve custom routes
@['/:user/settings']
pub fn (mut app ServerApp) settings(mut ctx ServerContext, username string) vweb.Result {
	if username !in known_users {
		return ctx.not_found()
	}
	return ctx.html('username: ${username}')
}

@['/:user/:repo/settings']
pub fn (mut app ServerApp) user_repo_settings(mut ctx ServerContext, username string, repository string) vweb.Result {
	if username !in known_users {
		return ctx.not_found()
	}
	return ctx.html('username: ${username} | repository: ${repository}')
}

@['/json_echo'; post]
pub fn (mut app ServerApp) json_echo(mut ctx ServerContext) vweb.Result {
	// eprintln('>>>>> received http request at /json_echo is: $app.req')
	ctx.set_content_type(ctx.req.header.get(.content_type) or { '' })
	return ctx.ok(ctx.req.data)
}

@['/login'; post]
pub fn (mut app ServerApp) login_form(mut ctx ServerContext, username string, password string) vweb.Result {
	return ctx.html('username: x${username}x | password: x${password}x')
}

@['/form_echo'; post]
pub fn (mut app ServerApp) form_echo(mut ctx ServerContext) vweb.Result {
	ctx.set_content_type(ctx.req.header.get(.content_type) or { '' })
	return ctx.ok(ctx.form['foo'])
}

@['/file_echo'; post]
pub fn (mut app ServerApp) file_echo(mut ctx ServerContext) vweb.Result {
	if 'file' !in ctx.files {
		ctx.res.set_status(.internal_server_error)
		return ctx.text('no file')
	}

	return ctx.text(ctx.files['file'][0].data)
}

// Make sure [post] works without the path
@[post]
pub fn (mut app ServerApp) json(mut ctx ServerContext) vweb.Result {
	// eprintln('>>>>> received http request at /json is: $app.req')
	ctx.set_content_type(ctx.req.header.get(.content_type) or { '' })
	return ctx.ok(ctx.req.data)
}

@[host: 'example.com']
@['/with_host']
pub fn (mut app ServerApp) with_host(mut ctx ServerContext) vweb.Result {
	return ctx.ok('')
}

pub fn (mut app ServerApp) shutdown(mut ctx ServerContext) vweb.Result {
	session_key := ctx.get_cookie('skey') or { return ctx.not_found() }
	if session_key != 'superman' {
		return ctx.not_found()
	}
	spawn app.exit_gracefully()
	return ctx.ok('good bye')
}

fn (mut app ServerApp) exit_gracefully() {
	eprintln('>> webserver: exit_gracefully')
	time.sleep(100 * time.millisecond)
	exit(0)
}
