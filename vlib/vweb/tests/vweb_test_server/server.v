module vweb_test_server

import os
import log
import vweb
import time

@[if verbose_vweb_server ?]
fn linfo(s string) {
	log.info(s)
}

pub fn start_in_background(http_port int, timeout time.Duration) ! {
	linfo('>> ${@FN}, http_port: ${http_port}, timeout: ${timeout}')
	assert http_port > 0
	assert timeout > 0

	spawn fn (timeout time.Duration) {
		time.sleep(timeout)
		linfo('>> webserver: pid: ${os.getpid()}, exiting cleanly after ${timeout.milliseconds()}ms ...')
		exit(0)
	}(timeout)
	shared config := &Config{
		max_ping: 50
	}
	app := &App{
		port:          http_port
		global_config: config
	}
	linfo('>> webserver: pid: ${os.getpid()}, started on http://localhost:${app.port}/ , with maximum runtime of ${timeout.milliseconds()} ms.')
	spawn fn (app &App, http_port int) {
		vweb.run_at(app, host: 'localhost', port: http_port, family: .ip) or {}
	}(app, http_port)
	_ := <-app.is_ready
	log.info('>> ${@FN} finished, http_port: ${http_port}, timeout: ${timeout}')
}

const known_users = ['bilbo', 'kent']

struct App {
	vweb.Context
	port          int
	global_config shared Config
	is_ready      chan bool
}

struct Config {
	max_ping int
}

pub fn (mut app App) before_accept_loop() {
	linfo('>>>>> ${@LOCATION}')
	app.is_ready <- true
}

pub fn (mut app App) index() vweb.Result {
	linfo('>>>>> ${@LOCATION}')
	rlock app.global_config {
		assert app.global_config.max_ping == 50
	}
	return app.text('Welcome to VWeb')
}

pub fn (mut app App) simple() vweb.Result {
	linfo('>>>>> ${@LOCATION}')
	return app.text('A simple result')
}

pub fn (mut app App) html_page() vweb.Result {
	linfo('>>>>> ${@LOCATION}')
	return app.html('<h1>ok</h1>')
}

// the following serve custom routes
@['/:user/settings']
pub fn (mut app App) settings(username string) vweb.Result {
	linfo('>>>>> ${@LOCATION}, username: ${username}')
	if username !in known_users {
		return app.not_found()
	}
	return app.html('username: ${username}')
}

@['/:user/:repo/settings']
pub fn (mut app App) user_repo_settings(username string, repository string) vweb.Result {
	linfo('>>>>> ${@LOCATION}, username: ${username}, repository: ${repository}')
	if username !in known_users {
		return app.not_found()
	}
	return app.html('username: ${username} | repository: ${repository}')
}

@['/json_echo'; post]
pub fn (mut app App) json_echo() vweb.Result {
	linfo('>>>>> ${@LOCATION} received http request at /json_echo is: ${app.req}')
	app.set_content_type(app.req.header.get(.content_type) or { '' })
	return app.ok(app.req.data)
}

@['/login'; post]
pub fn (mut app App) login_form(username string, password string) vweb.Result {
	linfo('>>>>> ${@LOCATION}, username: ${username}, password: ${password}')
	return app.html('username: x${username}x | password: x${password}x')
}

@['/form_echo'; post]
pub fn (mut app App) form_echo() vweb.Result {
	linfo('>>>>> ${@LOCATION}')
	app.set_content_type(app.req.header.get(.content_type) or { '' })
	return app.ok(app.form['foo'])
}

@['/file_echo'; post]
pub fn (mut app App) file_echo() vweb.Result {
	linfo('>>>>> ${@LOCATION}')
	if 'file' !in app.files {
		app.set_status(500, '')
		return app.text('no file')
	}

	return app.text(app.files['file'][0].data)
}

// Make sure [post] works without the path
@[post]
pub fn (mut app App) json() vweb.Result {
	linfo('>>>>> ${@LOCATION} received http request is: ${app.req}')
	app.set_content_type(app.req.header.get(.content_type) or { '' })
	return app.ok(app.req.data)
}

// Custom 404 page
pub fn (mut app App) not_found() vweb.Result {
	linfo('>>>>> ${@LOCATION}')
	app.set_status(404, 'Not Found')
	return app.html('404 on "${app.req.url}"')
}

@[host: 'example.com']
@['/with_host']
pub fn (mut app App) with_host() vweb.Result {
	linfo('>>>>> ${@LOCATION}')
	return app.ok('')
}

pub fn (mut app App) shutdown() vweb.Result {
	linfo('>>>>> ${@LOCATION}')
	session_key := app.get_cookie('skey') or { return app.not_found() }
	if session_key != 'superman' {
		return app.not_found()
	}
	spawn app.exit_gracefully()
	return app.ok('good bye')
}

fn (mut app App) exit_gracefully() {
	linfo('>>>>> ${@LOCATION}')
	time.sleep(100 * time.millisecond)
	exit(0)
}
