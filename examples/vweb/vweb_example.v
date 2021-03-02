module main

import vweb
import rand

const (
	port = 8082
)

struct App {
	vweb.Context
mut:
	cnt int
}

fn main() {
	println('vweb example')
	vweb.run<App>(port)
}

pub fn (mut app App) init_once() {
	app.handle_static('.', false)
}

['/users/:user']
pub fn (mut app App) user_endpoint(user string) vweb.Result {
	id := rand.intn(100)
	return app.json('{"$user": $id}')
}

pub fn (mut app App) index() vweb.Result {
	app.cnt++
	show := true
	// app.text('Hello world from vweb')
	hello := 'Hello world from vweb'
	numbers := [1, 2, 3]
	app.enable_chunked_transfer(40)
	return $vweb.html()
}

pub fn (mut app App) show_text() vweb.Result {
	return app.text('Hello world from vweb')
}

pub fn (mut app App) cookie() vweb.Result {
	app.set_cookie(name: 'cookie', value: 'test')
	return app.text('Headers: $app.headers')
}

[post]
pub fn (mut app App) post() vweb.Result {
	return app.text('Post body: $app.req.data')
}
