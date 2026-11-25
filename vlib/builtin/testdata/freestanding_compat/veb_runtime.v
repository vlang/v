module main

import os
import time
import veb

pub struct App {
	veb.StaticHandler
mut:
	server &veb.Server = unsafe { nil }
}

pub struct Context {
	veb.Context
}

struct Reply {
	ok   bool
	name string
}

// init_server retains the server handle for the shutdown route.
pub fn (mut app App) init_server(server &veb.Server) { app.server = server }

// before_request marks responses so the probe can check middleware.
pub fn (mut ctx Context) before_request() {
	ctx.set_custom_header('X-Runtime-Probe', 'before-request') or { exit(1) }
}

// index returns the probe greeting.
@['/']
pub fn (mut app App) index(mut ctx Context) veb.Result { return ctx.text('veb syscall runtime') }

// hello exercises route parameters and JSON serialization.
@['/hello/:name']
pub fn (mut app App) hello(mut ctx Context, name string) veb.Result {
	return ctx.json(Reply{ ok: true, name: name })
}

// echo returns the request body unchanged.
@['/echo'; post]
pub fn (mut app App) echo(mut ctx Context) veb.Result { return ctx.text(ctx.req.data) }

fn stop_server(server &veb.Server) {
	time.sleep(20 * time.millisecond)
	server.shutdown(timeout: 2 * time.second) or {
		eprintln(err)
		exit(1)
	}
}

// stop requests graceful shutdown after responding.
@['/stop']
pub fn (mut app App) stop(mut ctx Context) veb.Result {
	spawn stop_server(app.server)
	return ctx.text('stopping')
}

fn main() {
	if os.args.len != 3 { exit(2) }
	mut app := &App{}
	app.serve_static('/asset.txt', os.args[2]) or {
		eprintln(err)
		exit(1)
	}
	veb.run_at[App, Context](mut app,
		family:             .ip
		host:               '127.0.0.1'
		port:               os.args[1].int()
		nr_workers:         1
		timeout_in_seconds: 2
	) or {
		eprintln(err)
		exit(1)
	}
	println('veb graceful shutdown passed')
}
