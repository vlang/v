module main

import os
import veb
// import vweb.assets
import time

const port = 8081

pub struct Context {
	veb.Context
}

pub struct App {
	veb.StaticHandler
}

fn main() {
	mut app := &App{}
	app.serve_static('/favicon.ico', 'favicon.ico')!
	// Automatically make available known static mime types found in given directory.
	os.chdir(os.dir(os.executable()))!
	app.handle_static('assets', true)!
	veb.run[App, Context](mut app, 8080)
}

pub fn (mut app App) index() veb.Result {
	// We can dynamically specify which assets are to be used in template.
	// 	mut am := assets.new_manager()
	// 	am.add_css('assets/index.css')
	// 	css := am.include_css(false)
	title := 'Veb Assets Example'
	subtitle := 'Veb can serve static assets too!'
	message := 'It also has an Assets Manager that allows dynamically specifying which CSS and JS files to be used.'
	return $veb.html()
}

fn (mut app App) text() veb.Result {
	return ctx.text('Hello, world from veb!')
}

fn (mut app App) time() veb.Result {
	return ctx.text(time.now().format())
}
