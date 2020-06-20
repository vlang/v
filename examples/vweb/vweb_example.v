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

pub fn (mut app App) init_once() {
	app.vweb.handle_static('.')
}

pub fn (mut app App) init() {
}

pub fn (mut app App) json_endpoint() {
	app.vweb.json('{"a": 3}')
}

pub fn (mut app App) index() vweb.Result {
	app.cnt++
	show := true
	//app.vweb.text('Hello world from vweb')
	hello := 'Hello world from vweb'
	numbers := [1,2,3]
	return $vweb.html()
}

pub fn (mut app App) text() {
	app.vweb.text('Hello world from vweb')
}

pub fn (mut app App) cookie() {
	app.vweb.set_cookie('cookie', 'test')
	app.vweb.text('Headers: $app.vweb.headers')
}
