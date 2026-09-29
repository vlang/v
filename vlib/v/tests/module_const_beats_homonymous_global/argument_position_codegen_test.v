module main

import os

// Known checker residual: the checker binds the bare name to the foreign
// `__global` while the emitted symbol is the const's, so an argument-position
// call passes an incompatible pointer; V's default `-w` hides the C warning.

fn testsuite_begin() {
	os.setenv('VCOLORS', 'never', true)
}

fn setup_argument_position_fixture() string {
	workspace := os.join_path(os.vtmp_dir(), 'argument_position_codegen_${os.getpid()}')
	os.rmdir_all(workspace) or {}
	os.mkdir_all(os.join_path(workspace, 'api')) or { panic(err) }
	os.mkdir_all(os.join_path(workspace, 'consumer')) or { panic(err) }
	os.write_file(os.join_path(workspace, 'v.mod'), "Module {\n\tname: 'argument_position_codegen'\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(workspace, 'api', 'api.v'), '@[has_globals]
module api

pub struct GlobalType {
pub:
	n int
}

pub struct ConstType {
pub:
	n int
}

__global default_logger &GlobalType

fn init() {
	default_logger = &GlobalType{
		n: 7
	}
}
') or { panic(err) }
	os.write_file(os.join_path(workspace, 'consumer', 'consumer.v'), 'module consumer

import api

pub const default_logger = &api.ConstType{
	n: 99
}

fn read_global(logger &api.GlobalType) int {
	return logger.n
}

pub fn through_argument() int {
	return read_global(default_logger)
}
') or { panic(err) }
	os.write_file(os.join_path(workspace, 'main.v'), 'import consumer

fn main() {
	println(consumer.through_argument())
}
') or { panic(err) }
	return workspace
}

fn test_argument_position_emits_const_symbol() {
	workspace := setup_argument_position_fixture()
	defer {
		os.rmdir_all(workspace) or {}
	}
	c_path := os.join_path(workspace, 'out.c')
	gen := os.execute('${os.quoted_path(@VEXE)} -enable-globals -o ${os.quoted_path(c_path)} ${os.quoted_path(workspace)}')
	assert gen.exit_code == 0, gen.output
	c := os.read_file(c_path) or { panic(err) }
	assert c.contains('consumer__read_global(consumer__default_logger)'), c
	assert !c.contains('consumer__read_global(api__default_logger)'), c
}
