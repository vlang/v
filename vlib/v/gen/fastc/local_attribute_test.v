module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_local_trailing_attributes_preserve_following_array_mutations() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_local_attribute_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Group {
mut:
	value int
}
struct Holder {
mut:
	items []int
}
fn main() {
	mut groups := []&Group{cap: 16} @[freed]
	groups.flags |= .noslices
	mut count := 0
	count++
	groups << &Group{ value: count }
	groups[0].value += 41
	assert groups.len == 1 && groups.cap >= 16
	assert groups.flags.has(.noslices)
	assert count == 1 && groups[0].value == 42
	mut values := []int{len: 2, cap: 4, init: 5} @[freed]
	values[1]++
	assert values.len == 2 && values.cap >= 4
	assert values[0] == 5 && values[1] == 6
	values = []int{len: 2, init: 7} @[freed]
	values[0]++
	assert values[0] == 8 && values[1] == 7
	mut holder := Holder{}
	holder.items = []int{len: 2, init: 9} @[freed]
	holder.items[1] += 3
	assert holder.items[0] == 9 && holder.items[1] == 12
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
	os.write_file(c_path, source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=incompatible-pointer-types', '-o', program,
		c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
