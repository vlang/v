module main

import os

fn testsuite_begin() {
	os.setenv('VCOLORS', 'never', true)
}

fn write_file(path string, content string) {
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, content) or { panic(err) }
}

fn setup_qualified_fixture(helper string, probe string) string {
	workspace := os.join_path(os.vtmp_dir(), 'qualified_name_const_${os.getpid()}')
	os.rmdir_all(workspace) or {}
	write_file(os.join_path(workspace, 'v.mod'), "Module {\n\tname: 'qualified_name_const'\n}\n")
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

${helper}

pub fn probe() int {
	return ${probe}
}
')
	write_file(os.join_path(workspace, 'main.v'), 'import consumer

fn main() {
	println(consumer.probe())
}
')
	return workspace
}

fn test_qualified_name_resolves_to_const() {
	workspace := setup_qualified_fixture('fn want_const(logger &api.ConstType) int {
	return logger.n
}', 'want_const(consumer.default_logger)')
	defer {
		os.rmdir_all(workspace) or {}
	}
	res := os.exec([@VEXE, '-enable-globals', 'run', '${workspace}'])
	assert res.exit_code == 0, res.output
	assert res.output.trim_space() == '99', res.output
}

fn test_qualified_name_is_not_the_foreign_global() {
	workspace := setup_qualified_fixture('fn want_global(logger &api.GlobalType) int {
	return logger.n
}', 'want_global(consumer.default_logger)')
	defer {
		os.rmdir_all(workspace) or {}
	}
	res := os.exec([@VEXE, '-enable-globals', '-check', '${workspace}'])
	assert res.exit_code != 0, res.output
	assert res.output.contains('cannot use `&api.ConstType` as `&api.GlobalType` in argument 1'), res.output
}
