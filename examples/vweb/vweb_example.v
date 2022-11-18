module main

import vweb
import rand

const (
	port = 8082
)

struct App {
	vweb.Context
mut:
	state shared State
}

struct State {
mut:
	cnt int
}

pub fn (app App) before_request() {
	println('[Vweb] ${app.Context.req.method} ${app.Context.req.url}')
}

fn main() {
	println('vweb example')
	vweb.run(&App{}, port)
}

['/users/:user']
pub fn (mut app App) user_endpoint(user string) vweb.Result {
	id := rand.intn(100) or { 0 }
	return app.json({
		user: id
	})
}

pub fn (mut app App) index() vweb.Result {
	lock app.state {
		app.state.cnt++
	}
	show := true
	hello := 'Hello world from vweb'
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
