import os
import v.tempname

fn test_generic_promoted_field_assignment_keeps_program_struct_identity() {
	root := os.join_path(os.vtmp_dir(), 'v3_promoted_assignment_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	os.mkdir(os.join_path(root, 'helper'))!
	output := os.join_path(root, 'app')
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
	compile := os.exec([@VEXE, '-prealloc', '-o', output, os.join_path(root, 'main.v')])
	assert compile.exit_code == 0, compile.output
	run := os.exec([output])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '1'
}
