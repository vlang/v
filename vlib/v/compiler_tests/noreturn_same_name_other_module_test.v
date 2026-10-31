import os

const vexe = @VEXE

// build compiles and runs a project with the local modules in modules (a map
// of module name to source) and main_source, returning the exit code and output.
fn build(name string, modules map[string]string, main_source string) (int, string) {
	return compile(name, 'run', modules, main_source)
}

// compile runs `v -new-compiler <command>` on a project laid out like build's.
fn compile(name string, command string, modules map[string]string, main_source string) (int, string) {
	dir := os.join_path(os.vtmp_dir(), 'v3_noreturn_${name}_${os.getpid()}')
	os.rmdir_all(dir) or {}
	defer {
		os.rmdir_all(dir) or {}
	}
	for mod, source in modules {
		os.mkdir_all(os.join_path(dir, mod)) or { panic(err) }
		os.write_file(os.join_path(dir, mod, '${mod}.v'), source) or { panic(err) }
	}
	os.write_file(os.join_path(dir, 'v.mod'), "Module{ name: '${name}' }\n") or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), main_source) or { panic(err) }
	res := os.exec([vexe, '-new-compiler', '${command}', dir])
	return res.exit_code, res.output
}

const logger_source = "module logger

pub struct Logger {
pub:
	level int
}

pub fn panic() Logger {
	return Logger{
		level: 5
	}
}

pub fn same_module() Logger {
	return panic()
}

fn may() !Logger {
	return error('no')
}

pub fn in_or_block() Logger {
	return may() or { panic() }
}
"

fn test_module_fn_named_like_a_noreturn_builtin_returns_normally() {
	code, output := build('panic_fn', {
		'logger': logger_source
	}, 'module main

import logger

fn main() {
	println(logger.panic().level)
	println(logger.same_module().level)
	println(logger.in_or_block().level)
}
')
	assert code == 0, output
	assert output.trim_space().split_into_lines() == ['5', '5', '5'], output
}

fn test_noreturn_attrib_does_not_leak_to_same_named_functions() {
	code, output := build('leak', {
		'a': 'module a

@[noreturn]
pub fn fail() {
	exit(3)
}
'
		'b': 'module b

pub fn fail() int {
	return 1
}
'
	}, 'module main

import a
import b

fn fail() int {
	return 2
}

fn main() {
	println(b.fail() + fail())
	if b.fail() > 1 {
		a.fail()
	}
}
')
	assert code == 0, output
	assert output.trim_space() == '3', output
}

fn test_missing_return_after_module_panic_call_is_reported() {
	code, output := build('missing_return', {
		'logger': 'module logger

pub struct Logger {}

pub fn panic() Logger {
	return Logger{}
}

pub fn falls_through() int {
	panic()
}
'
	}, 'module main

import logger

fn main() {
	println(logger.falls_through())
}
')
	assert code != 0, output
	assert output.contains('missing return at end of function'), output
}

fn test_module_fn_named_exit_returns_normally() {
	code, output := build('exit_fn', {
		'runner': "module runner

pub fn exit() int {
	return 4
}

pub fn same_module() int {
	return exit()
}

fn may() !int {
	return error('no')
}

pub fn in_or_block() int {
	return may() or { exit() }
}
"
	}, 'module main

import runner

fn main() {
	println(runner.exit())
	println(runner.same_module())
	println(runner.in_or_block())
}
')
	assert code == 0, output
	assert output.trim_space().split_into_lines() == ['4', '4', '4'], output
}

fn test_missing_return_after_module_exit_call_is_reported() {
	code, output := build('exit_missing_return', {
		'runner': 'module runner

pub fn exit() int {
	return 4
}

pub fn falls_through() int {
	exit()
}
'
	}, 'module main

import runner

fn main() {
	println(runner.falls_through())
}
')
	assert code != 0, output
	assert output.contains('missing return at end of function'), output
}

fn test_noreturn_module_fn_with_return_is_still_rejected() {
	code, output := build('real_noreturn', {
		'a': 'module a

@[noreturn]
pub fn fail() {
	return
}
'
	}, 'module main

import a

fn main() {
	a.fail()
}
')
	assert code != 0, output
	assert output.contains('[noreturn] functions cannot use return statements'), output
}

const bridge_source = "module bridge

@[noreturn]
fn C.foreign_halt()

pub fn stop() int {
	C.foreign_halt()
}

@[noreturn]
pub fn halt() {
	C.foreign_halt()
}

pub fn wrapped() int {
	halt()
}

fn may() !int {
	return error('no')
}

pub fn in_or_block() int {
	return may() or { C.foreign_halt() }
}
"

fn test_noreturn_foreign_fn_declared_in_a_module_terminates_calls() {
	// -check only: the foreign function is never defined, so nothing is linked.
	code, output := compile('foreign', '-check', {
		'bridge': bridge_source
	}, 'module main

import bridge

fn main() {
	println(bridge.stop())
	println(bridge.wrapped())
	println(bridge.in_or_block())
}
')
	assert code == 0, output
}

fn test_noreturn_foreign_fn_does_not_leak_to_same_named_module_fn() {
	code, output := compile('foreign_leak', '-check', {
		'bridge': 'module bridge

@[noreturn]
fn C.foreign_halt()

pub fn foreign_halt() int {
	return 1
}

pub fn falls_through() int {
	foreign_halt()
}
'
	}, 'module main

import bridge

fn main() {
	println(bridge.foreign_halt())
	println(bridge.falls_through())
}
')
	assert code != 0, output
	assert output.contains('missing return at end of function `falls_through`'), output
	assert !output.contains('[noreturn] functions cannot use return statements'), output
}
