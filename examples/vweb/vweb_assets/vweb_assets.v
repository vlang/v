module main

import vweb
// import vweb.assets
import time

const (
	port = 8081
)

struct App {
	vweb.Context
}

fn main() {
	vweb.run<App>(port)
}

pub fn (mut app App) init_once() {
	// Arbitary mime type.
	app.serve_static('/favicon.ico', 'favicon.ico', 'img/x-icon')
	// Automatically make available known static mime types found in given directory.
	// app.handle_static('assets')
	// This would make available all known static mime types from current
	// directory and below.
	app.handle_static('.', false)
}

pub fn (mut app App) init() {
}

pub fn (mut app App) index() vweb.Result {
	// We can dynamically specify which assets are to be used in template.
	// 	mut am := assets.new_manager()
	// 	am.add_css('assets/index.css')
	// 	css := am.include_css(false)
	title := 'VWeb Assets Example'
	subtitle := 'VWeb can serve static assets too!'
	message := 'It also has an Assets Manager that allows dynamically specifying which CSS and JS files to be used.'
	return $vweb.html()
}

fn (mut app App) text() vweb.Result {
	return app.Context.text('Hello, world from vweb!')
}

fn (mut app App) time() vweb.Result {
	return app.Context.text(time.now().format())
}
