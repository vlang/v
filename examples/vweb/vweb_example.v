module main

import vweb
import rand

const port = 8082

struct State {
mut:
	cnt int
}

struct App {
	vweb.Context
mut:
	state shared State
}

pub fn (app &App) before_request() {
	$if trace_before_request ? {
		eprintln('[vweb] before_request: ${app.req.method} ${app.req.url}')
	}
}

['/users/:user']
pub fn (mut app App) user_endpoint(user string) vweb.Result {
	id := rand.intn(100) or { 0 }
	return app.json({
		user: id
	})
}

pub fn (mut app App) index() vweb.Result {
	mut c := 0
	lock app.state {
		app.state.cnt++
		c = app.state.cnt
		//
		$if trace_address_of_app_state_cnt ? {
			dump(ptr_str(app.state.cnt))
		}
	}
	show := true
	hello := 'Hello world from vweb, request number: ${c}'
	numbers := [1, 2, 3]
	return $vweb.html()
}

pub fn (mut app App) show_text() vweb.Result {
	return app.text('Hello world from vweb')
}

pub fn (mut app App) cookie() vweb.Result {
	app.set_cookie(name: 'cookie', value: 'test')
	return app.text('Response Headers\n${app.header}')
}

[post]
pub fn (mut app App) post() vweb.Result {
	return app.text('Post body: ${app.req.data}')
}

fn main() {
	println('vweb example')
	vweb.run(&App{}, port)
}
