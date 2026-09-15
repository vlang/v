import os

const scoped_monomorph_vexe = @VEXE
const scoped_monomorph_tests_dir = os.dir(@FILE)
const scoped_monomorph_v3_dir = os.dir(scoped_monomorph_tests_dir)
const scoped_monomorph_vlib_dir = os.dir(scoped_monomorph_v3_dir)
const scoped_monomorph_v3_src = os.join_path(scoped_monomorph_v3_dir, 'v.v')

fn scoped_monomorph_v3_bin() string {
	bin := os.join_path(os.temp_dir(), 'v3_scoped_monomorphize_closure_test')
	if os.exists(bin) {
		return bin
	}
	build := os.execute('${scoped_monomorph_vexe} -gc none -path "${scoped_monomorph_vlib_dir}|@vlib|@vmodules" -o ${bin} ${scoped_monomorph_v3_src}')
	assert build.exit_code == 0, build.output
	return bin
}

fn testsuite_begin() {
	os.rm(scoped_monomorph_v3_bin()) or {}
}

// Compiler builds use `-prealloc`, and the memory-bounded monomorphize path (the
// fix for vlang/v#28564) runs for every non-empty specialization batch there.
// That path used to give two different lifted closures the same `__anon_fn_N`
// name - the module-keyed signature table then mixed their signatures up and the
// generated C did not compile - and merged specialization arguments as shallow
// `[]string` copies that still pointed into the released worker arena, so a later
// pass read freed arguments (bogus `unknown function` diagnostics or a crash).
// This is the small `veb` program from vlang/v#28489, which exercises closures
// lifted while specializing a generic helper.
fn test_scoped_monomorphize_keeps_closure_signatures_and_args() {
	v3_bin := scoped_monomorph_v3_bin()
	dir := os.join_path(os.temp_dir(), 'v3_scoped_monomorphize_closure')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	os.write_file(os.join_path(dir, 'main.v'), "module main

import veb

pub struct Ctx {
	veb.Context
}

pub struct ModelApp {
	veb.Middleware[Ctx]
	veb.Controller
}

pub struct Item {
	ModelApp
}

pub struct MainApp {
	veb.Middleware[Ctx]
	veb.Controller
}

fn mw() veb.MiddlewareOptions[Ctx] {
	return veb.MiddlewareOptions[Ctx]{
		handler: fn (mut ctx Ctx) bool {
			return true
		}
	}
}

fn (mut app MainApp) common_middleware[T](mut ctrl T) {
	ctrl.use(mw())
}

fn (mut app MainApp) register_routes_no_auth[T, U](mut ctrl T, url_path string) {
	app.common_middleware[T](mut ctrl)
	app.register_controller[T, U](url_path, mut ctrl) or { panic(err) }
	ctrl.route_use('/item/*', veb.encode_auto[Ctx]())
}

fn main() {
	mut app := &MainApp{}
	app.register_routes_no_auth[Item, Ctx](mut &Item{}, '/item')
	veb.run_at[MainApp, Ctx](mut app, port: 9001) or { panic(err) }
}
") or { panic(err) }
	out := os.join_path(dir, 'app')
	compile := os.execute('${v3_bin} -nocache -o ${out} ${dir}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	assert os.is_file(out), 'the compile produced no binary'
}
