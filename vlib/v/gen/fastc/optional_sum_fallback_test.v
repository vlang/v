module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_optional_sum_fallback_preserves_none_and_present_payloads() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_optional_sum_fallback_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Record { name string }
type Value = Record | int
fn lookup(values map[int]Value, index int) ?Value {
	return values[index] or { none }
}
fn forward(values map[int]Value, index int) ?Value {
	return lookup(values, index) or { none }
}
fn main() {
	values := {1: Value(Record{"present"}), 2: Value(42)}
	for key in [0, 1, 2] {
		if value := forward(values, key) {
			match value {
				Record { assert key == 1; assert value.name == "present" }
				int { assert key == 2; assert value == 42 }
			}
		} else { assert key == 0 }
	}
	if _ := lookup(map[int]Value{}, 7) { assert false }
	println("optional sum fallbacks work")
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'optional sum fallbacks work', run.output
}
