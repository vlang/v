module main

import os

// A global-only method must not pair with the const's C symbol — that mix reads
// garbage. The const owns the name, so resolution is against the const's type
// only and a global-only method fails closed instead of silently mispairing.

fn testsuite_begin() {
	os.setenv('VCOLORS', 'never', true)
}

fn setup_global_only_method_fixture() string {
	workspace := os.join_path(os.vtmp_dir(), 'global_only_method_codegen_${os.getpid()}')
	os.rmdir_all(workspace) or {}
	os.mkdir_all(os.join_path(workspace, 'api')) or { panic(err) }
	os.mkdir_all(os.join_path(workspace, 'consumer')) or { panic(err) }
	os.write_file(os.join_path(workspace, 'v.mod'), "Module {\n\tname: 'global_only_method_codegen'\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(workspace, 'api', 'api.v'), '@[has_globals]
module api

pub struct GlobalType {
pub:
	n int
	secret int
}

pub fn (g &GlobalType) shared() int {
	return g.n
}

pub fn (g &GlobalType) only_global() int {
	return g.secret
}

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
		n: 7
		secret: 777
	}
}
') or { panic(err) }
	os.write_file(os.join_path(workspace, 'consumer', 'consumer.v'), 'module consumer

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
') or { panic(err) }
	os.write_file(os.join_path(workspace, 'main.v'), 'import consumer

fn main() {
	println(consumer.shared())
	println(consumer.only_global())
}
') or { panic(err) }
	return workspace
}

fn test_global_only_method_does_not_pair_with_const_symbol() {
	workspace := setup_global_only_method_fixture()
	defer {
		os.rmdir_all(workspace) or {}
	}
	c_path := os.join_path(workspace, 'out.c')
	gen := os.execute('${os.quoted_path(@VEXE)} -enable-globals -o ${os.quoted_path(c_path)} ${os.quoted_path(workspace)}')
	assert gen.exit_code == 0, gen.output
	c := os.read_file(c_path) or { panic(err) }
	assert c.contains('api__ConstType__shared(consumer__default_logger)'), c
	// The bad pair must not appear: global method on the const's symbol.
	assert !c.contains('api__GlobalType__only_global(consumer__default_logger)'), c
	// Fail closed: resolved against the const's type only; the build rejects it.
	assert c.contains('api__ConstType__only_global('), c
	build := os.execute('${os.quoted_path(@VEXE)} -enable-globals -o ${os.quoted_path(os.join_path(workspace, 'app'))} ${os.quoted_path(workspace)}')
	assert build.exit_code != 0, build.output
	assert !build.output.contains('api__GlobalType__only_global(consumer__default_logger)'), build.output
}
