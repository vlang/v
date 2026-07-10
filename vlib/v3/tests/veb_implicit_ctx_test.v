import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_veb_ctx_test')
	build := os.execute('${vexe} -o ${v3_bin} ${v3_src}')
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
	compile := os.execute('${v3_bin} ${src_file} -o ${c_out}')
	assert compile.exit_code == 0, compile.output
	c_code := os.read_file(c_out) or { '' }
	// No-arg delegation forwards the enclosing ctx in the ctx slot.
	assert c_code.contains('App__helper(app, ctx)'), c_code
	// Delegation that also passes a real argument keeps ctx at its slot,
	// so the explicit argument still lines up with its parameter.
	assert c_code.contains('App__show(app, ctx, 5)'), c_code
}
