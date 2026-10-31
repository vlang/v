module main

import os

// A param or local named like a const-owned ident and a foreign `__global` is a shadow error,
// but its body must still see the param/local — a regression bound it to the const (`cannot use &api.ConstType as type int`).

fn testsuite_begin() {
	os.setenv('VCOLORS', 'never', true)
}

fn write_file(path string, content string) {
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, content) or { panic(err) }
}

fn setup_precedence_fixture() string {
	workspace := os.join_path(os.vtmp_dir(), 'param_shadows_const_${os.getpid()}')
	os.rmdir_all(workspace) or {}
	write_file(os.join_path(workspace, 'v.mod'), "Module {\n\tname: 'param_shadows_const'\n}\n")
	write_file(os.join_path(workspace, 'api', 'api.v'), '@[has_globals]
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
')
	write_file(os.join_path(workspace, 'consumer', 'consumer.v'), 'module consumer

import api

pub const default_logger = &api.ConstType{
	n: 99
}

pub fn param_wins(default_logger int) int {
	return default_logger
}

pub fn local_wins() int {
	default_logger := 5
	return default_logger
}
')
	write_file(os.join_path(workspace, 'main.v'), 'import consumer

fn main() {
	println(consumer.param_wins(3))
	println(consumer.local_wins())
}
')
	return workspace
}

fn test_param_and_local_win_over_const_and_global() {
	workspace := setup_precedence_fixture()
	defer {
		os.rmdir_all(workspace) or {}
	}
	res := os.exec([@VEXE, '-enable-globals', '-check', '${workspace}'])
	assert res.output.contains('variable `default_logger` shadows a global variable'), res.output
	assert !res.output.contains('ConstType'), res.output
	assert !res.output.contains('cannot use'), res.output
}
