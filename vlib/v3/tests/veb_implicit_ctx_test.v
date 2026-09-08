import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_veb_ctx_test')
	build := os.execute('${vexe} -gc none -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

// A `veb.Result` method that receives the implicit `Context` parameter can be
// called by another handler without passing ctx explicitly. The call must
// type-check (not report a missing argument) and forward the enclosing `ctx`,
// not a zero/default value.
fn test_veb_implicit_ctx_forwarded_at_call_site() {
	v3_bin := build_v3()
	src := '
import veb

pub struct Context {
	veb.Context
}

pub struct App {
mut:
	n int
}

pub fn (app &App) index() veb.Result {
	app.show(5)
	return app.helper()
}

pub fn (app &App) helper() veb.Result {
	return veb.Result{}
}

pub fn (app &App) show(id int) veb.Result {
	return veb.Result{}
}

fn main() {
	mut app := &App{}
	_ := app.index()
}
'
	src_file := os.join_path(os.temp_dir(), 'v3_veb_ctx.v')
	os.write_file(src_file, src) or { panic(err) }
	c_out := os.join_path(os.temp_dir(), 'v3_veb_ctx.c')
	os.rm(c_out) or {}
	compile := os.execute('${v3_bin} -no-memory-limit ${src_file} -o ${c_out}')
	assert compile.exit_code == 0, compile.output
	c_code := os.read_file(c_out) or { '' }
	// No-arg delegation forwards the enclosing ctx in the ctx slot.
	assert c_code.contains('App__helper(app, ctx)'), c_code
	// Delegation that also passes a real argument keeps ctx at its slot,
	// so the explicit argument still lines up with its parameter.
	assert c_code.contains('App__show(app, ctx, 5)'), c_code
}

// A route parameter whose imported type leaf is `Context` but which is not a veb
// context (here `other.Context` aliases `string`) must not be mistaken for the
// request context. The handler still receives the hidden `Context`, so its
// implicit `ctx` use resolves.
fn test_veb_imported_context_alias_param_still_gets_hidden_ctx() {
	v3_bin := build_v3()
	root := os.join_path(os.temp_dir(), 'v3_veb_imported_ctx_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'other')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'vebimportedctx' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'other', 'other.v'),
		'module other\n\npub type Context = string\n') or { panic(err) }
	main_src := "module main

import veb
import other

pub struct Context {
	veb.Context
}

pub struct App {}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	return ctx.text('ok')
}

pub fn (mut app App) show(slug other.Context) veb.Result {
	return ctx.text('slug=\${slug}')
}

fn main() {}
"
	os.write_file(os.join_path(root, 'main.v'), main_src) or { panic(err) }
	out := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} -no-memory-limit ${os.join_path(root, 'main.v')} -o ${out}')
	assert compile.exit_code == 0, compile.output
}

// A program Context can embed veb.Context and declare the same hook name. The
// generated router must call the program method with the complete Context, not
// route its receiver to the embedded framework field just because both types
// share the short name `Context`.
fn test_veb_program_context_hook_keeps_complete_receiver() {
	v3_bin := build_v3()
	src := '
import veb

pub struct Context {
	veb.Context
}

pub struct App {}

pub fn (ctx &Context) before_request() {}

pub fn (mut app App) index(mut ctx Context) veb.Result {
	return ctx.text("ok")
}

fn main() {
	mut app := &App{}
	veb.run_at[App, Context](mut app, port: 0) or { panic(err) }
}
'
	src_file := os.join_path(os.temp_dir(), 'v3_veb_complete_context_receiver.v')
	os.write_file(src_file, src) or { panic(err) }
	c_out := os.join_path(os.temp_dir(), 'v3_veb_complete_context_receiver.c')
	os.rm(c_out) or {}
	compile := os.execute('${v3_bin} -no-memory-limit ${src_file} -o ${c_out}')
	assert compile.exit_code == 0, compile.output
	c_code := os.read_file(c_out) or { '' }
	assert c_code.contains('Context__before_request(user_context)'), c_code
	assert !c_code.contains('Context__before_request(&user_context->veb__Context)'), c_code
}
