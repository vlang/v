module main

import os
import rand
import time
import vweb
import vweb.sse

struct App {}

fn main() {
	mut conf := vweb.Config{
		port: 8081
	}
	conf.serve_static('/favicon.ico', 'favicon.ico', 'img/x-icon')
	conf.mount_static_folder_at(os.resource_abs_path('.'), '/')
	vweb.run<App>(conf)
}

pub fn (mut app App) index(mut c vweb.Context) vweb.Result {
	title := 'SSE Example'
	return $vweb.html()
}

fn (mut app App) sse(mut c vweb.Context) vweb.Result {
	mut session := sse.new_connection(c.conn)
	// NB: you can setup session.write_timeout and session.headers here
	session.start() or { return c.server_error(501) }
	session.send_message(data: 'ok') or { return c.server_error(501) }
	for {
		data := '{"time": "$time.now().str()", "random_id": "$rand.ulid()"}'
		session.send_message(event: 'ping', data: data) or { return c.server_error(501) }
		println('> sent event: $data')
		time.sleep(1 * time.second)
	}
	return c.server_error(501)
}
