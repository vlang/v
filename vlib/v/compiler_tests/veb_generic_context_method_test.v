import os
import v.tempname

const veb_generic_context_source = 'module main

import os
import rand
import veb

pub struct Context {
	veb.Context
}

pub struct App {}

pub struct SomeData {
	id string
}

fn (mut ctx Context) handle_ok[T](payload T) veb.Result {
	ctx.res.set_status(.ok)
	return ctx.json(payload)
}

@["/some_data"; get]
pub fn (mut app App) some_data(mut ctx Context) veb.Result {
	return ctx.handle_ok(SomeData{ id: rand.hex(16) })
}

// Keep App.some_data and its payload specialization reachable through the router alone.
pub struct ResponseApp {}

pub struct ResponseData {
	id string
}

@["/some_data"; get]
pub fn (mut app ResponseApp) some_data(mut ctx Context) veb.Result {
	return ctx.handle_ok(ResponseData{ id: rand.hex(16) })
}

fn main() {
	// Compile the complete router while keeping the test run free of listeners.
	if os.args.contains("--serve") {
		mut app := App{}
		veb.run[App, Context](mut app, 8080)
		return
	}

	mut ctx := Context{}
	ctx.res.set_status(.bad_request)
	ctx.handle_ok(ResponseData{ id: "payload" })
	assert ctx.res.status_code == 200
	assert ctx.res.body == \'{"id":"payload"}\'
	assert (ctx.res.header.get(.content_type) or { "" }) == "application/json"

	mut explicit_ctx := Context{}
	explicit_ctx.handle_ok[ResponseData](ResponseData{ id: "explicit" })
	assert explicit_ctx.res.status_code == 200
	assert explicit_ctx.res.body == \'{"id":"explicit"}\'

	mut text_ctx := Context{}
	text_ctx.handle_ok("hello")
	assert text_ctx.res.status_code == 200
	assert text_ctx.res.body == \'"hello"\'

	mut route_ctx := Context{}
	mut app := ResponseApp{}
	app.some_data(mut route_ctx)
	assert route_ctx.res.status_code == 200
	assert route_ctx.res.body.starts_with(\'{"id":"\')
	assert route_ctx.res.body.ends_with(\'"}\')
	println("ok")
}
'

fn test_veb_generic_user_context_method_with_router_and_response() {
	root := os.join_path(os.vtmp_dir(), 'veb_generic_context_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'main.v')
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'veb_generic_context' }\n")!
	os.write_file(source, veb_generic_context_source)!
	program := os.join_path(root, 'app' + $if windows { '.exe' } $else { '' })
	compile := os.exec([@VEXE, '-nocache', '-o', program, source])
	assert compile.exit_code == 0, compile.output
	assert os.is_file(program), compile.output
	result := os.exec([program])
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'ok', result.output
}
