module main

import vweb
import vweb.assets
import time

const (
	port = 8081
)

pub struct App {
mut:
	vweb vweb.Context
}

fn main() {
	vweb.run<App>(port)
}

pub fn (mut app App) init() {
	// Arbitary mime type.
	app.vweb.serve_static('/favicon.ico', 'favicon.ico', 'img/x-icon')
	// Automatically make available known static mime types found in given directory.
	app.vweb.handle_static('assets')
	// This would make available all known static mime types from current
	// directory and below.
	//app.vweb.handle_static('.')
}

pub fn (mut app App) reset() {}

fn (mut app App) index() {
	// We can dynamically specify which assets are to be used in template.
	mut am := assets.new_manager()
	am.add_css('assets/index.css')

	css := am.include_css(false)
	title := 'VWeb Assets Example'
	subtitle := 'VWeb can serve static assets too!'
	message := 'It also has an Assets Manager that allows dynamically specifying which CSS and JS files to be used.'

	$vweb.html()
}

fn (mut app App) text() {
	app.vweb.text('Hello, world from vweb!')
}

fn (mut app App) time() {
	app.vweb.text(time.now().format())
}
