module main

import vweb

const (
	Port = 8082
)

struct App {
pub mut:
        vweb vweb.Context // TODO embed
}

fn main() {
        println('Running vweb test on http://localhost:$Port')
        vweb.run<App>(Port)
}

pub fn (app mut App) init() {
	app.vweb.serve_static('/css.css', 'static/css.css')
	app.vweb.serve_static('/jquery.js', 'static/jquery.js')
}

pub fn (app mut App) json_endpoint() {
        app.vweb.json('{"a": 3}')
}

pub fn (app mut App) index() {
        app.vweb.text('hello world')
}
