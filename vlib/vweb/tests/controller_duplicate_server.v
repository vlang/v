module main

import vweb
import time
import os

struct App {
	vweb.Context
	vweb.Controller
}

struct Admin {
	vweb.Context
}

@['/admin/duplicate']
pub fn (mut app App) duplicate() vweb.Result {
	return app.text('duplicate')
}

fn exit_after_timeout(timeout_in_ms int) {
	time.sleep(timeout_in_ms * time.millisecond)
	println('>> webserver: pid: ${os.getpid()}, exiting ...')
	exit(0)
}

fn main() {
	if os.args.len != 3 {
		panic('Usage: `controller_test_server.exe PORT TIMEOUT_IN_MILLISECONDS`')
	}
	http_port := os.args[1].int()
	assert http_port > 0
	mut app_dup := &App{
		controllers: [
			vweb.controller('/admin', &Admin{}),
		]
	}

	vweb.run_at(app_dup, host: 'localhost', port: http_port, family: .ip) or { panic(err) }
}
