module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_returned_callbacks_preserve_arguments_and_optional_results() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_returned_callback_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'handlers')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'returned_callback' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'handlers', 'handlers.v'), r'module handlers
pub struct State {
pub mut:
	value int
}
pub struct Slot {
pub:
	handler fn (voidptr)
	probe fn (int) ?int
	parse fn (bool) !int
}
__global (
	global_handler fn (voidptr) = unsafe { nil }
	global_probe fn (int) ?int = unsafe { nil }
)
fn handle(data voidptr) {
	mut state := unsafe { &State(data) }
	state.value += 5
}
pub fn get_timer_handler() fn (voidptr) { return handle }
fn score(value int) int { return value * 2 }
pub fn get_scorer() fn (int) int { return score }
fn probe(value int) ?int {
	if value < 0 { return none }
	return value + 1
}
pub fn get_probe() fn (int) ?int { return probe }
fn parse(fail bool) !int {
	if fail { return error("callback failed") }
	return 42
}
pub fn get_parser() fn (bool) !int { return parse }
pub fn slot() Slot { return Slot{ handler: handle, probe: probe, parse: parse } }
pub fn install_global() {
	global_handler = handle
	global_probe = probe
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import handlers
fn invoke(handler fn (voidptr), data voidptr) { handler(data) }
fn main() {
	mut state := handlers.State{ value: 7 }
	timer_handler := handlers.get_timer_handler()
	timer_handler(voidptr(&state))
	assert state.value == 12
	copied := timer_handler
	copied(voidptr(&state))
	assert state.value == 17
	invoke(handlers.get_timer_handler(), voidptr(&state))
	assert state.value == 22
	score := handlers.get_scorer()
	assert score(21) == 42
	probe := handlers.get_probe()
	assert (probe(41) or { -1 }) == 42
	assert (probe(-1) or { -2 }) == -2
	parse := handlers.get_parser()
	assert (parse(false) or { -1 }) == 42
	mut failures := 0
	parse(true) or {
		assert err.msg() == "callback failed"
		failures++
		0
	}
	assert failures == 1
	slots := [handlers.slot()]
	from_value := slots[0].handler
	from_value(voidptr(&state))
	assert state.value == 27
	copied_field := from_value
	copied_field(voidptr(&state))
	assert state.value == 32
	pointer_slots := [&handlers.Slot{ handler: timer_handler, probe: probe, parse: parse }]
	from_pointer := pointer_slots[0].handler
	from_pointer(voidptr(&state))
	assert state.value == 37
	field_probe := pointer_slots[0].probe
	assert (field_probe(41) or { -1 }) == 42
	assert (field_probe(-1) or { -2 }) == -2
	field_parser := slots[0].parse
	assert (field_parser(false) or { -1 }) == 42
	field_parser(true) or {
		assert err.msg() == "callback failed"
		failures++
		0
	}
	assert failures == 2
	handlers.install_global()
	global_handler(voidptr(&state))
	global_copy := global_handler
	global_copy(voidptr(&state))
	assert state.value == 47
	assert (global_probe(41) or { -1 }) == 42
	assert (global_probe(-1) or { -2 }) == -2
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.enable_globals = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=incompatible-pointer-types', '-o', program,
		c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
