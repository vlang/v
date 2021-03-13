module main

import vweb
//import vweb.assets
import time

struct App {}

fn main() {
	mut conf := vweb.Config{
		port: 8081
	}
	// Arbitary mime type.
	conf.serve_static('/favicon.ico', 'favicon.ico', 'img/x-icon')
	// Automatically make available known static mime types found in given directory.
	// app.handle_static('assets')
	// This would make available all known static mime types from current
	// directory and below.
	conf.handle_static('.', false)
	vweb.run<App>(conf)
}

pub fn (mut app App) index(mut c vweb.Context) vweb.Result {
	// We can dynamically specify which assets are to be used in template.
	// 	mut am := assets.new_manager()
	// 	am.add_css('assets/index.css')
	// 	css := am.include_css(false)
	title := 'VWeb Assets Example'
	subtitle := 'VWeb can serve static assets too!'
	message := 'It also has an Assets Manager that allows dynamically specifying which CSS and JS files to be used.'
	return $vweb.html()
}

fn (mut app App) text(mut c vweb.Context) vweb.Result {
	return c.text('Hello, world from vweb!')
}

fn (mut app App) time(mut c vweb.Context) vweb.Result {
	return c.text(time.now().format())
}
