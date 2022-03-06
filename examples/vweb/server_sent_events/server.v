module main

import os
import rand
import time
import vweb
import vweb.sse

struct App {
	vweb.Context
}

fn main() {
	mut app := &App{}
	app.serve_static('/favicon.ico', 'favicon.ico')
	app.mount_static_folder_at(os.resource_abs_path('.'), '/')
	vweb.run(app, 8081)
}

pub fn (mut app App) index() vweb.Result {
	title := 'SSE Example'
	return $vweb.html()
}

fn (mut app App) sse() vweb.Result {
	mut session := sse.new_connection(app.conn)
	// Note: you can setup session.write_timeout and session.headers here
	session.start() or { return app.server_error(501) }
	session.send_message(data: 'ok') or { return app.server_error(501) }
	for {
		data := '{"time": "$time.now().str()", "random_id": "$rand.ulid()"}'
		session.send_message(event: 'ping', data: data) or { return app.server_error(501) }
		println('> sent event: $data')
		time.sleep(1 * time.second)
	}
	return app.server_error(501)
}
