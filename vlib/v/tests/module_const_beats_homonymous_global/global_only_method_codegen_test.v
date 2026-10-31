module main

import os

// A const-owned bare name resolves against the const's type only; a global-only method is rejected
// by the V checker instead of silently pairing the foreign global's method.

fn testsuite_begin() {
	os.setenv('VCOLORS', 'never', true)
}

fn write_file(path string, content string) {
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, content) or { panic(err) }
}

fn api_source(methods string) string {
	return '@[has_globals]
module api

pub struct GlobalType {
pub:
	n      int
	secret int
}

${methods}

pub struct ConstType {
pub:
	n int
}

pub fn (c &ConstType) shared() int {
	return c.n
}

__global default_logger &GlobalType

fn init() {
	default_logger = &GlobalType{
		n:      7
		secret: 777
	}
}
'
}

fn setup_method_fixture(name string, consumer string, methods string) string {
	workspace := os.join_path(os.vtmp_dir(), '${name}_${os.getpid()}')
	os.rmdir_all(workspace) or {}
	write_file(os.join_path(workspace, 'v.mod'), "Module {\n\tname: '${name}'\n}\n")
	write_file(os.join_path(workspace, 'api', 'api.v'), api_source(methods))
	write_file(os.join_path(workspace, 'consumer', 'consumer.v'), consumer)
	write_file(os.join_path(workspace, 'main.v'), 'import consumer

fn main() {
	println(consumer.shared())
}
')
	return workspace
}

fn test_shared_method_resolves_to_const_symbol() {
	workspace := setup_method_fixture('global_only_method_codegen', 'module consumer

import api

pub const default_logger = &api.ConstType{
	n: 99
}

pub fn shared() int {
	return default_logger.shared()
}
', 'pub fn (g &GlobalType) shared() int {
	return g.n
}
')
	defer {
		os.rmdir_all(workspace) or {}
	}
	c_path := os.join_path(workspace, 'out.c')
	gen := os.exec([@VEXE, '-enable-globals', '-o', c_path, '${workspace}'])
	assert gen.exit_code == 0, gen.output
	c := os.read_file(c_path) or { panic(err) }
	assert c.contains('api__ConstType__shared(consumer__default_logger)'), c
	assert !c.contains('api__GlobalType__shared(consumer__default_logger)'), c
}

fn test_global_only_method_rejected_by_v_checker() {
	workspace := setup_method_fixture('global_only_method_checker', 'module consumer

import api

pub const default_logger = &api.ConstType{
	n: 99
}

pub fn shared() int {
	return default_logger.shared()
}

pub fn only_global() int {
	return default_logger.only_global()
}
', 'pub fn (g &GlobalType) shared() int {
	return g.n
}

pub fn (g &GlobalType) only_global() int {
	return g.secret
}
')
	defer {
		os.rmdir_all(workspace) or {}
	}
	res := os.exec([@VEXE, '-enable-globals', '-check', '${workspace}'])
	assert res.exit_code != 0, res.output
	assert res.output.contains('unknown method or field: `ConstType.only_global`'), res.output
	assert !res.output.contains('api__GlobalType__only_global('), res.output
}
