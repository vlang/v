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
	ctx.takeover_conn()
	spawn handle_sse_conn(mut ctx)
	return veb.no_result()
}

fn handle_sse_conn(mut ctx Context) {
	mut session := sse.start_connection(mut ctx.Context)
	session.send_message(data: 'ok') or { return }
	for {
		data := '{"time": "${time.now().str()}", "random_id": "${rand.ulid()}"}'
		session.send_message(event: 'ping', data: data) or { break }
		println('> sent event: ${data}')
		time.sleep(1 * time.second)
	}
	session.close()
}
