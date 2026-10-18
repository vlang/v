module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_borrowed_function_alias_casts_preserve_calls_and_results() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_borrowed_callback_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'callbacks')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'borrowed_callback' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'callbacks', 'callbacks.v'), r'module callbacks
pub type ScoreCallback = fn (voidptr, int) int
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import callbacks
type VoidCallback = fn ()
type ProbeCallback = fn (int) ?int
type ParseCallback = fn (bool) !int
struct State {
mut:
	value int
}
@[export: "vinix_call_void_fn"]
pub fn call_void_fn(callback voidptr) {
	unsafe { VoidCallback(callback)() }
}
fn call_score(callback voidptr, data voidptr, delta int) int {
	return unsafe { callbacks.ScoreCallback(callback)(data, delta) }
}
fn call_probe(callback voidptr, value int) ?int {
	return unsafe { ProbeCallback(callback)(value) }
}
fn call_parse(callback voidptr, fail bool) !int {
	return unsafe { ParseCallback(callback)(fail) }
}
fn score(data voidptr, delta int) int {
	mut state := unsafe { &State(data) }
	state.value += delta
	return state.value
}
fn probe(value int) ?int {
	if value < 0 { return none }
	return value + 1
}
fn parse(fail bool) !int {
	if fail { return error("borrowed callback failed") }
	return 42
}
@[c_extern]
fn C.probe_borrowed_callback()
fn main() {
	C.probe_borrowed_callback()
	mut state := State{ value: 7 }
	assert call_score(unsafe { voidptr(score) }, voidptr(&state), 35) == 42
	assert state.value == 42
	assert unsafe { (callbacks.ScoreCallback(voidptr(score)))(voidptr(&state), -7) } == 35
	assert state.value == 35
	assert (call_probe(unsafe { voidptr(probe) }, 41) or { -1 }) == 42
	assert (call_probe(unsafe { voidptr(probe) }, -1) or { -2 }) == -2
	assert (unsafe { ProbeCallback(voidptr(probe))(41) } or { -1 }) == 42
	assert (unsafe { ProbeCallback(voidptr(probe))(-1) } or { -2 }) == -2
	assert (call_parse(unsafe { voidptr(parse) }, false) or { -1 }) == 42
	mut failures := 0
	unsafe {
		ParseCallback(voidptr(parse))(true) or {
			assert err.msg() == "borrowed callback failed"
			failures++
			0
		}
	}
	assert failures == 1
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, source + r'
static int borrowed_calls;
static void native_borrowed_callback(void) { ++borrowed_calls; }
void probe_borrowed_callback(void) {
	vinix_call_void_fn((voidptr)native_borrowed_callback);
	if (borrowed_calls != 1) abort();
	vinix_call_void_fn((voidptr)native_borrowed_callback);
	if (borrowed_calls != 2) abort();
}
') or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=incompatible-pointer-types', '-o', program,
		c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
