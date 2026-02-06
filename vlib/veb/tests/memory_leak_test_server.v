module main

import os
import veb
import time

struct Context {
	veb.Context
}

struct App {}

fn exit_after_timeout(timeout_in_ms int) {
	time.sleep(timeout_in_ms * time.millisecond)
	exit(0)
}

fn main() {
	if os.args.len != 3 {
		panic('Usage: memory_leak_test_server.exe PORT TIMEOUT_IN_MILLISECONDS')
	}
	http_port := os.args[1].int()
	assert http_port > 0
	timeout := os.args[2].int()
	assert timeout > 0
	spawn exit_after_timeout(timeout)

	mut app := &App{}
	veb.run_at[App, Context](mut app,
		host:               'localhost'
		port:               http_port
		family:             .ip
		timeout_in_seconds: 10
	)!
}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	return ctx.text('OK')
}

@['/heap']
pub fn (mut app App) heap(mut ctx Context) veb.Result {
	usage := gc_heap_usage()
	return ctx.text('${usage.heap_size}')
}

@['/gc']
pub fn (mut app App) gc_collect(mut ctx Context) veb.Result {
	gc_collect()
	return ctx.text('GC collected')
}

@['/large']
pub fn (mut app App) large(mut ctx Context) veb.Result {
	data := 'X'.repeat(100 * 1024)
	return ctx.text(data)
}
