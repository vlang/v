module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_option_guards_infer_new_bindings_before_shadowed_names() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_option_guard_shadow_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	os.write_file(path, r'module main
struct Lookup {}
struct Item { value int }
fn c_name(value string) string { return value }
fn pointer() int { return 7 }
fn apply(callback fn (string) string, value string) string {
	return callback(value)
}
fn (lookup Lookup) name(present bool) ?string {
	if !present { return none }
	return "answer"
}
fn (lookup Lookup) find_pointer(present bool, value voidptr) ?voidptr {
	if !present { return none }
	return value
}
fn main() {
	lookup := Lookup{}
	assert apply(c_name, "callback") == "callback"
	assert pointer() == 7
	mut branches := 0
	if c_name := lookup.name(true) {
		assert c_name == "answer"
		branches++
	} else { assert false }
	if c_name := lookup.name(false) {
		assert false
	} else { branches++ }
	item := &Item{ value: 42 }
	if pointer := lookup.find_pointer(true, item) {
		assert pointer == voidptr(item)
		branches++
	} else { assert false }
	if pointer := lookup.find_pointer(false, item) {
		assert false
	} else { branches++ }
	existing := 17
	if existing := lookup.name(true) {
		assert existing == "answer"
		branches++
	} else { assert false }
	assert existing == 17
	inline_name := if c_name := lookup.name(true) { c_name } else { "missing" }
	assert inline_name == "answer"
	assert branches == 5
	println("guard bindings work")
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
	build := cmdexec.run('cc', ['-std=gnu11', '-o', program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'guard bindings work', run.output
}
