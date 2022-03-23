module main

import os
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
	mut app := &App{}
	app.serve_static('/favicon.ico', 'favicon.ico')
	// Automatically make available known static mime types found in given directory.
	os.chdir(os.dir(os.executable())) ?
	app.handle_static('assets', true)
	vweb.run(app, port)
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
