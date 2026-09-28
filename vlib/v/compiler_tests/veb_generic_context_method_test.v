import os

const veb_generic_context_source = 'module main

import veb
import rand

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

fn main() {
	mut app := App{}
	veb.run[App, Context](mut app, 8080)
}
'

fn veb_generic_context_project(name string, source string) !string {
	root := os.join_path(os.vtmp_dir(), 'veb_generic_context_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'veb_generic_context' }\n")!
	os.write_file(os.join_path(root, 'main.v'), source)!
	return root
}

fn veb_generic_context_compile(root string) string {
	program := os.join_path(root, 'app' + $if windows { '.exe' } $else { '' })
	source := os.join_path(root, 'main.v')
	// An automatic retry with the compatibility compiler would hide this regression.
	result := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -o ${os.quoted_path(program)} ${os.quoted_path(source)}')
	assert result.exit_code == 0, result.output
	assert os.is_file(program), result.output
	return program
}

fn test_veb_generic_user_context_method_compiles_with_router() {
	root := veb_generic_context_project('router', veb_generic_context_source)!
	defer {
		os.rmdir_all(root) or {}
	}
	// Compile the complete router, but do not start a server or reserve a port.
	veb_generic_context_compile(root)
}

fn test_veb_generic_user_context_method_preserves_response() {
	source := veb_generic_context_source.replace('veb.run[App, Context](mut app, 8080)', '
	mut ctx := Context{}
	ctx.res.set_status(.bad_request)
	ctx.handle_ok(SomeData{ id: "payload" })
	assert ctx.res.status_code == 200
	assert ctx.res.body == \'{"id":"payload"}\'
	assert (ctx.res.header.get(.content_type) or { "" }) == "application/json"

	mut explicit_ctx := Context{}
	explicit_ctx.handle_ok[SomeData](SomeData{ id: "explicit" })
	assert explicit_ctx.res.status_code == 200
	assert explicit_ctx.res.body == \'{"id":"explicit"}\'

	mut text_ctx := Context{}
	text_ctx.handle_ok("hello")
	assert text_ctx.res.status_code == 200
	assert text_ctx.res.body == \'"hello"\'

	mut route_ctx := Context{}
	app.some_data(mut route_ctx)
	assert route_ctx.res.status_code == 200
	assert route_ctx.res.body.starts_with(\'{"id":"\')
	assert route_ctx.res.body.ends_with(\'"}\')
	println("ok")
')
	root := veb_generic_context_project('response', source)!
	defer {
		os.rmdir_all(root) or {}
	}
	program := veb_generic_context_compile(root)
	result := os.execute(os.quoted_path(program))
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'ok', result.output
}
