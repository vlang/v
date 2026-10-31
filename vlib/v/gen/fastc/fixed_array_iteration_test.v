module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_fixed_array_literals_and_integer_storage_keep_v_width() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_fixed_array_iteration_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Values {
mut:
	items [2]int
}
__global items [2]int
fn update(mut values [2]int, amount int) {
	assert values.len == 2 && values.cap == 2
	values[1] += amount
}
fn first(values &[2]int) int {
	assert values.len == 2 && values.cap == 2
	return values[0]
}
fn change_first(values &[2]int, amount int) {
	unsafe { values[0] += amount }
}
fn main() {
	mut matches := 0
	for value in ["console", "pointer", "mouse"]! {
		if value == "pointer" { matches++ }
	}
	assert matches == 1
	large := int(1) << 40
	mut fixed := [large, large + 7]!
	mut total := int(0)
	for value in fixed { total += value }
	assert total == large * 2 + 7
	pointer := &fixed[0]
	unsafe { *pointer += 3 }
	assert fixed[0] == large + 3
	mut state := Values{}
	state.items[0] = large
	state.items[1] = large + 1
	for mut value in state.items { value += 2 }
	assert state.items[0] == large + 2 && state.items[1] == large + 3
	update(mut state.items, large)
	assert state.items[1] == large * 2 + 3
	assert first(&state.items) == large + 2
	change_first(&state.items, 5)
	assert state.items[0] == large + 7
	mut parenthesized := int(0)
	for value in (state.items) { parenthesized += value }
	assert parenthesized == large * 3 + 10
	items[0] = large
	items[1] = large + 4
	mut copied := items
	for mut value in copied { value += 1 }
	assert copied[0] == large + 1 && items[0] == large
	mut dynamic := [large, large + 9]
	for mut value in dynamic { value += 1 }
	assert dynamic[0] == large + 1 && dynamic[1] == large + 10
	counts := {large: large + 11, large + 1: large + 12}
	assert counts[large] == large + 11 && counts[large + 1] == large + 12
	mut map_total := int(0)
	for key, value in counts { map_total += key + value }
	assert map_total == large * 4 + 24
	mut initialized := []int{len: 2, init: large}
	initialized[1] += 3
	assert initialized[0] == large && initialized[1] == large + 3
	assert sizeof(state.items) == 16 && sizeof(fixed) == 16
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.enable_globals = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=incompatible-pointer-types', '-O2', '-o',
		program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
