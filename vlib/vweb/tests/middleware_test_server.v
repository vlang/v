module main

import vweb
import time
import os

struct App {
	vweb.Context
	timeout       int
	global_config shared Config
	middlewares   map[string][]vweb.Middleware
}

struct Config {
pub mut:
	middleware_text string
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
	spawn exit_after_timeout(timeout)

	shared config := &Config{}

	app := &App{
		timeout:       timeout
		global_config: config
		middlewares:   {
			'/single':        [middleware1]
			'/single_post':   [middleware1]
			'/multiple':      [middleware1, middleware2]
			'/multiple_post': [middleware1, middleware2]
			'/combined':      [middleware1, middleware2]
			'/combined_post': [middleware1, middleware2]
			'/admin/':        [middleware1]
			'/other/':        [middleware1, middleware2]
			'/redirect':      [middleware_redirect]
			'/with-context':  [context1]
		}
	}
	eprintln('>> webserver: pid: ${os.getpid()}, started on http://localhost:${http_port}/ , with maximum runtime of ${app.timeout} milliseconds.')
	vweb.run_at(app, host: 'localhost', port: http_port, family: .ip)!
}

// normal routes:

@[middleware: app_middleware]
@['/']
pub fn (mut app App) index() vweb.Result {
	mut result := ''

	rlock app.global_config {
		result = app.global_config.middleware_text
	}

	return app.text('${result}index')
}

@['/single']
pub fn (mut app App) single() vweb.Result {
	mut result := ''

	rlock app.global_config {
		result = app.global_config.middleware_text
	}

	return app.text('${result}single')
}

@['/multiple']
pub fn (mut app App) multiple() vweb.Result {
	mut result := ''

	rlock app.global_config {
		result = app.global_config.middleware_text
	}

	return app.text('${result}multiple')
}

@[middleware: app_middleware]
@['/combined']
pub fn (mut app App) combined() vweb.Result {
	mut result := ''

	rlock app.global_config {
		result = app.global_config.middleware_text
	}

	return app.text('${result}combined')
}

@['/admin/nested']
pub fn (mut app App) nested() vweb.Result {
	mut result := ''

	rlock app.global_config {
		result = app.global_config.middleware_text
	}

	return app.text('${result}nested')
}

// above routes + post

@[middleware: app_middleware]
@['/index_post'; post]
pub fn (mut app App) index_post() vweb.Result {
	mut result := ''

	rlock app.global_config {
		result = app.global_config.middleware_text
	}

	return app.text('${result}index_post:${app.req.data}')
}

@['/single_post'; post]
pub fn (mut app App) single_post() vweb.Result {
	mut result := ''

	rlock app.global_config {
		result = app.global_config.middleware_text
	}

	return app.text('${result}single_post:${app.req.data}')
}

@['/multiple_post'; post]
pub fn (mut app App) multiple_post() vweb.Result {
	mut result := ''

	rlock app.global_config {
		result = app.global_config.middleware_text
	}

	return app.text('${result}multiple_post:${app.req.data}')
}

@[middleware: app_middleware]
@['/combined_post'; post]
pub fn (mut app App) combined_post() vweb.Result {
	mut result := ''

	rlock app.global_config {
		result = app.global_config.middleware_text
	}

	return app.text('${result}combined_post:${app.req.data}')
}

@['/admin/nested_post'; post]
pub fn (mut app App) nested_post() vweb.Result {
	mut result := ''

	rlock app.global_config {
		result = app.global_config.middleware_text
	}

	return app.text('${result}nested_post:${app.req.data}')
}

// dynamic routes

@['/admin/:dynamic']
pub fn (mut app App) admin_dynamic(dynamic string) vweb.Result {
	mut result := ''

	rlock app.global_config {
		result = app.global_config.middleware_text
	}

	return app.text('${result}admin_dynamic:${dynamic}')
}

@[middleware: app_middleware]
@['/other/:dynamic']
pub fn (mut app App) combined_dynamic(dynamic string) vweb.Result {
	mut result := ''

	rlock app.global_config {
		result = app.global_config.middleware_text
	}

	return app.text('${result}combined_dynamic:${dynamic}')
}

// redirect routes:

@[middleware: app_redirect]
@['/app_redirect']
pub fn (mut app App) app_redirect_route() vweb.Result {
	mut result := ''

	rlock app.global_config {
		result = app.global_config.middleware_text
	}

	return app.text('${result}should_never_reach!')
}

@['/redirect']
pub fn (mut app App) redirect_route() vweb.Result {
	mut result := ''

	rlock app.global_config {
		result = app.global_config.middleware_text
	}

	return app.text('${result}should_never_reach!')
}

@['/with-context']
pub fn (mut app App) with_context() vweb.Result {
	a := app.get_value[string]('a') or { 'none' }
	return app.text(a)
}

// middleware functions:

pub fn (mut app App) before_request() {
	lock app.global_config {
		app.global_config.middleware_text = '0'
	}
}

pub fn (mut app App) app_middleware() bool {
	lock app.global_config {
		app.global_config.middleware_text += 'app_middleware'
	}
	return true
}

pub fn (mut app App) app_redirect() bool {
	app.redirect('/')
	return false
}

fn middleware1(mut ctx vweb.Context) bool {
	ctx.conn.write_string('m1') or { panic(err) }
	return true
}

fn middleware2(mut ctx vweb.Context) bool {
	ctx.conn.write_string('m2') or { panic(err) }
	return true
}

fn middleware_redirect(mut ctx vweb.Context) bool {
	ctx.conn.write_string('m_redirect') or { panic(err) }
	ctx.redirect('/')
	return false
}

fn context1(mut ctx vweb.Context) bool {
	ctx.set_value('a', 'b')
	return true
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
