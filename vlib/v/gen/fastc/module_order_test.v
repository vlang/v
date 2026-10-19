module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_cyclic_modules_keep_cross_calls_and_order_lifecycle_once() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_module_cycle_${os.getpid()}')
	for dir in [root, os.join_path(root, 'alpha'), os.join_path(root, 'beta'),
		os.join_path(root, 'dependency'), os.join_path(root, 'outside'),
		os.join_path(root, 'callbacks')] {
		os.mkdir_all(dir) or { panic(err) }
	}
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "module_cycle" }') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), r'module main
import alpha
import beta
import outside as _
import callbacks as _
@[c_extern]
fn C.fastc_cycle_emit(character i32)
fn init() { C.fastc_cycle_emit(73) }
fn cleanup() { C.fastc_cycle_emit(105) }
fn main() {
	assert alpha.total() == 32
	assert beta.total() == 30
	C.fastc_cycle_emit(77)
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'alpha', 'alpha.v'), r'module alpha
import beta
import dependency
@[c_extern]
fn C.fastc_cycle_emit(character i32)
fn init() { C.fastc_cycle_emit(65) }
fn cleanup() { C.fastc_cycle_emit(97) }
pub fn value() int { return 20 }
pub fn total() int { return beta.value() + dependency.value() }
') or { panic(err) }
	os.write_file(os.join_path(root, 'beta', 'beta.v'), r'module beta
import alpha
import dependency
@[c_extern]
fn C.fastc_cycle_emit(character i32)
fn init() { C.fastc_cycle_emit(66) }
fn cleanup() { C.fastc_cycle_emit(98) }
pub fn value() int { return 22 }
pub fn total() int { return alpha.value() + dependency.value() }
') or { panic(err) }
	os.write_file(os.join_path(root, 'dependency', 'dependency.v'), r'module dependency
@[c_extern]
fn C.fastc_cycle_emit(character i32)
fn init() { C.fastc_cycle_emit(68) }
fn cleanup() { C.fastc_cycle_emit(100) }
pub fn value() int { return 10 }
') or { panic(err) }
	os.write_file(os.join_path(root, 'outside', 'outside.v'), r'module outside
import dependency as _
@[c_extern]
fn C.fastc_cycle_emit(character i32)
fn init() { C.fastc_cycle_emit(69) }
fn cleanup() { C.fastc_cycle_emit(101) }
') or { panic(err) }
	mut prefs := pref.new_preferences()
	os.write_file(os.join_path(root, 'callbacks', 'callbacks.v'), r'module callbacks
@[has_globals]
__global (
	init fn () = unsafe { nil }
	cleanup fn () = unsafe { nil }
)
') or { panic(err) }
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	source := generate_files([root], prefs) or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, source + '\nvoid fastc_cycle_emit(i32 character) { putchar(character); }\n') or {
		panic(err)
	}
	build := cmdexec.run('cc', ['-std=gnu11', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	// dependency, beta, alpha, outside, main; cleanup reverses that order.
	assert run.output == 'DBAEIMieabd', run.output
}
