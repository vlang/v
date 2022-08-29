module main

import vweb
import vweb.csrf

struct App {
	csrf.App
}

fn main() {
	vweb.run_at(&App{}, vweb.RunParams{
		port: 8080
	}) or { panic(err) }
}

fn (mut app App) index() vweb.Result {
	app.set_csrf_cookie()
	return app.text('Csrf-Token set!')
}

fn (mut app App) foo() vweb.Result {
	app.csrf_protect()
	return app.text('Checked and passed csrf-guard')
}
