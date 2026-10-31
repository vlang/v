module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_fixed_array_condition_braces_preserve_following_statements() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_fixed_array_condition_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct State {
mut:
	mac [6]u8
	ready bool
	fault bool
}
fn update(mut state State) {
	if state.mac == [6]u8{} || state.mac[0] & 1 != 0 {
		state.fault = true
		return
	}
	state.ready = true
}
fn main() {
	mut zero := State{}
	update(mut zero)
	assert zero.fault && !zero.ready
	mut multicast := State{}
	multicast.mac[0] = 1
	update(mut multicast)
	assert multicast.fault && !multicast.ready
	mut valid := State{}
	valid.mac[0] = 2
	update(mut valid)
	assert valid.ready && !valid.fault
	mut words := [2]u16{}
	mut comparisons := 0
	if [2]u16{} == words { comparisons++ }
	words[1] = 3
	if words != [2]u16{} && words[1] == 3 { comparisons++ }
	assert comparisons == 2
	mut iterations := 0
	for words != [2]u16{} {
		words[1] = 0
		iterations++
	}
	assert iterations == 1 && words == [2]u16{}
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
