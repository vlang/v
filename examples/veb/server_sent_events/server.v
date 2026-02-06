module main

import os
import rand
import time
import veb
import veb.sse

struct App {
	veb.StaticHandler
}

struct Context {
	veb.Context
}

fn main() {
	mut app := &App{}
	app.serve_static('/favicon.ico', 'favicon.ico')!
	app.mount_static_folder_at(os.resource_abs_path('.'), '/')!
	veb.run[App, Context](mut app, 8081)
}

// XTODO template broken (@)
pub fn (mut app App) index() veb.Result {
	title := 'SSE Example'
	return $veb.html()
}

fn (mut app App) sse() veb.Result {
	mut session := sse.start_connection(mut ctx.Context)
	// Note: you can setup session.write_timeout and session.headers here
	// session.start() or { return app.server_error(501) }
	session.send_message(data: 'ok') or { return ctx.server_error_with_status(.not_implemented) }
	for {
		data := '{"time": "${time.now().str()}", "random_id": "${rand.ulid()}"}'
		session.send_message(event: 'ping', data: data) or {
			return ctx.server_error_with_status(.not_implemented)
		}
		println('> sent event: ${data}')
		time.sleep(1 * time.second)
	}
	return ctx.server_error_with_status(.not_implemented)
}
