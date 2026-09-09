import os

const promoted_assignment_vexe = @VEXE
const promoted_assignment_tests_dir = os.dir(@FILE)
const promoted_assignment_v3_dir = os.dir(promoted_assignment_tests_dir)
const promoted_assignment_vlib_dir = os.dir(promoted_assignment_v3_dir)
const promoted_assignment_v3_source = os.join_path(promoted_assignment_v3_dir, 'v3.v')

fn promoted_assignment_tmp_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_promoted_assignment_${name}_${os.getpid()}')
}

fn test_generic_promoted_field_assignment_keeps_program_struct_identity() {
	root := promoted_assignment_tmp_path('project')
	v3_bin := promoted_assignment_tmp_path('compiler')
	output := os.join_path(root, 'app')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'helper')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
		os.rm(v3_bin) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'promotedassignment' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'helper', 'helper.v'), 'module helper

pub struct Request {
pub mut:
	data int
}

pub struct Context {
pub mut:
	req Request
}

pub struct Options[T] {
pub:
	handler fn (mut T) bool
}

fn produce() !int {
	return 1
}

pub fn make[T]() Options[T] {
	return Options[T]{
		handler: fn [T](mut ctx T) bool {
			before := ctx.req.data
			if before == 0 {
				changed := produce() or { return false }
				ctx.req.data = changed
			}
			return ctx.req.data == before + 1
		}
	}
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

import helper

struct Context {
	helper.Context
}

fn main() {
	mut ctx := Context{}
	options := helper.make[Context]()
	assert options.handler(mut ctx)
	println(ctx.req.data)
}
') or {
		panic(err)
	}
	build :=
		os.execute('"${promoted_assignment_vexe}" -gc none -prealloc -path "${promoted_assignment_vlib_dir}|@vlib|@vmodules" -o "${v3_bin}" "${promoted_assignment_v3_source}"')
	assert build.exit_code == 0, build.output
	compile := os.execute('"${v3_bin}" -prealloc -o "${output}" "${os.join_path(root, 'main.v')}"')
	assert compile.exit_code == 0, compile.output
	run := os.execute('"${output}"')
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '1'
}
