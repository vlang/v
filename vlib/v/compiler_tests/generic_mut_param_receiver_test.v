import os
import v.tempname

// A method promoted through the embedded fields of a generic `mut ctrl T`
// parameter is reached with an `&` on the embedded field. The parameter's C
// storage is a pointer (it is a `mut` parameter), but the V type of the ident is
// the value type, so the receiver used to be emitted as `ctrl.field` and the
// generated C did not compile. The shape needs the embedding chain to cross
// modules: the controller embeds an app struct that embeds a generic middleware
// struct declared in a third module.
fn test_generic_mut_param_promoted_method_through_embedded_fields() {
	dir := os.join_path(os.vtmp_dir(), 'v3_generic_mut_param_receiver_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	os.mkdir(os.join_path(dir, 'veblite'))!
	os.mkdir(os.join_path(dir, 'model'))!
	os.mkdir(os.join_path(dir, 'route'))!
	os.write_file(os.join_path(dir, 'v.mod'), 'Module{\n\tname: "mut_param_receiver"\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'veblite', 'mw.v'), "module veblite

pub struct Mw[T] {
mut:
	used int
}

pub struct Opts[T] {
pub:
	handler fn (mut ctx T) bool
}

pub fn (mut m Mw[T]) use(o Opts[T]) {
	if o.handler != unsafe { nil } {
		m.used++
		println('use')
	}
}
") or { panic(err) }
	os.write_file(os.join_path(dir, 'model', 'app.v'), 'module model

import veblite

pub struct Ctx {}

pub struct App {
	veblite.Mw[Ctx]
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'route', 'route.v'), 'module route

import model
import veblite

pub struct App {
	model.App
}

pub struct Api {
	model.App
}

fn (mut app App) common_middleware[T](mut ctrl T, mut ctx model.Ctx) {
	ctrl.use(veblite.Opts[model.Ctx]{
		handler: fn (mut ctx model.Ctx) bool {
			return true
		}
	})
}

pub fn (mut app App) register_routes[T](mut ctrl T, path string, mut ctx model.Ctx) {
	app.common_middleware[T](mut ctrl, mut ctx)
	(ctrl).use(veblite.Opts[model.Ctx]{
		handler: fn (mut ctx model.Ctx) bool {
			return true
		}
	})
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), "module main

import model
import route

fn main() {
	mut app := &route.App{}
	mut ctx := model.Ctx{}
	app.register_routes[route.Api](mut &route.Api{}, '/api', mut ctx)
	println('ok')
}
") or { panic(err) }
	out := os.join_path(dir, 'mut_param_receiver')
	compile := os.exec([@VEXE, '-nocache', '-o', out, dir])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'use\nuse\nok', run.output
}
