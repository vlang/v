module main

import vweb

const (
	port = 8082
)

struct App {
	vweb.Context
pub mut:
	cnt  int
}

fn main() {
	println('vweb example')
	vweb.run<App>(port)
}

pub fn (mut app App) init_once() {
	app.handle_static('.')
}

pub fn (mut app App) init() {
}

pub fn (mut app App) json_endpoint() vweb.Result {
	return app.json('{"a": 3}')
}

pub fn (mut app App) index() vweb.Result {
	app.cnt++
	show := true
	// app.vweb.text('Hello world from vweb')
	hello := 'Hello world from vweb'
	numbers := [1, 2, 3]
	return $vweb.html()
}

pub fn (mut app App) show_text() vweb.Result {
	return app.text('Hello world from vweb')
}

pub fn (mut app App) cookie() vweb.Result {
	app.set_cookie({
		name: 'cookie'
		value: 'test'
	})
	return app.text('Headers: $app.headers')
}
