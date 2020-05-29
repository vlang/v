module main

import vweb

const (
	port = 8082
)

struct App {
pub mut:
	vweb vweb.Context // TODO embed
	cnt int
}

fn main() {
	println('vweb example')
	vweb.run<App>(port)
}

pub fn (app App) init() {
	app.vweb.handle_static('.')
}

pub fn (mut app App) json_endpoint() {
	app.vweb.json('{"a": 3}')
}

pub fn (mut app App) index() {
	app.cnt++
	$vweb.html()
}

pub fn (mut app App) reset() {
}

pub fn (mut app App) text() {
	app.vweb.text('Hello world')
}

pub fn (mut app App) cookie() {
	app.vweb.set_cookie('cookie', 'test')
	app.vweb.text('Headers: $app.vweb.headers')
}
