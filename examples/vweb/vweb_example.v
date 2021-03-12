module main

import vweb

struct App {
mut:
	cnt int
}

fn main() {
	println('vweb example')

	mut conf := vweb.Config{
		port: 8082
	}
	conf.handle_static('.', true)

	mut app := App{cnt: 100}
	vweb.run_app(mut app, conf)
}

['/users/:user']
pub fn (mut app App) user_endpoint(mut c vweb.Context, user string) vweb.Result {
	return c.json('{"$user": $app.cnt}')
}

pub fn (mut app App) index(mut c vweb.Context) vweb.Result {
	app.cnt++
	show := true
	// app.text('Hello world from vweb')
	hello := 'Hello world from vweb'
	numbers := [1, 2, 3]
	c.enable_chunked_transfer(40)
	return $vweb.html()
}

pub fn (mut app App) show_text(mut c vweb.Context) vweb.Result {
	return c.text('Hello world from vweb')
}

pub fn (mut app App) cookie(mut c vweb.Context) vweb.Result {
	c.set_cookie(name: 'cookie', value: 'test')
	return c.text('Headers: $c.headers')
}

[post]
pub fn (mut app App) post(mut c vweb.Context) vweb.Result {
	return c.text('Post body: $c.req.data')
}
