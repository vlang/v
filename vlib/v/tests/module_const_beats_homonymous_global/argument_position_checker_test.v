module main

import os

// Pins the declaring-module const out-ranking the homonymous foreign `__global` in argument position.

fn testsuite_begin() {
	os.setenv('VCOLORS', 'never', true)
}

fn write_file(path string, content string) {
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, content) or { panic(err) }
}

fn setup_argument_position_fixture() string {
	workspace := os.join_path(os.vtmp_dir(), 'argument_position_checker_${os.getpid()}')
	os.rmdir_all(workspace) or {}
	write_file(os.join_path(workspace, 'v.mod'), "Module {\n\tname: 'argument_position_checker'\n}\n")
	write_file(os.join_path(workspace, 'api', 'api.v'), '@[has_globals]
module api

pub struct GlobalType {
pub:
	n      int
	secret int
}

pub struct ConstType {
pub:
	n int
}

__global default_logger &GlobalType

fn init() {
	default_logger = &GlobalType{
		n:      7
		secret: 777
	}
}
')
	write_file(os.join_path(workspace, 'consumer', 'consumer.v'), 'module consumer

import api

pub const default_logger = &api.ConstType{
	n: 99
}

fn read_global(logger &api.GlobalType) int {
	return logger.secret
}

pub fn through_argument() int {
	return read_global(default_logger)
}
')
	write_file(os.join_path(workspace, 'main.v'), 'import consumer

fn main() {
	println(consumer.through_argument())
}
')
	return workspace
}

fn test_incompatible_argument_rejected_by_v_checker() {
	workspace := setup_argument_position_fixture()
	defer {
		os.rmdir_all(workspace) or {}
	}
	res := os.exec([@VEXE, '-enable-globals', '-check', '${workspace}'])
	assert res.exit_code != 0, res.output
	assert res.output.contains('cannot use `&api.ConstType` as `&api.GlobalType` in argument 1'), res.output
}
