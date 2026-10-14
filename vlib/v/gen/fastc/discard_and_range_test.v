module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_repeated_discard_declarations_preserve_side_effects() {
	prefs := pref.new_preferences()
	c_source := generate('module main
fn step(mut count int) int { count++; return count }
fn main() {
	mut count := 0
	_ := step(mut count)
	_ := step(mut count)
	_ := step(mut count)
	println(count)
}
', 'discard_declarations.v', prefs) or { panic(err) }
	assert !c_source.contains(' _ ='), c_source
	root := os.join_path(os.vtmp_dir(), 'fastc_discard_declarations_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	c_file := os.join_path(root, 'program.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compiled := cmdexec.run(tcc, ['-std=gnu11', '-o', program, c_file])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(program, [])
	assert executed.exit_code == 0, executed.output
	assert executed.output == '3\n'
}

const nested_range_source = r'module main

fn invalid_octal(text string, i int) bool {
	next := text[i + 1]
	return next == `$` || (next == `0`
		&& (i + 3 >= text.len || text[i + 2] !in `0` .. `8` || text[i + 3] !in `0` .. `8`))
}

fn counted(mut calls int, value int) int {
	calls += 1
	return value
}

fn main() {
	assert invalid_octal("x$", 0)
	assert invalid_octal("x00", 0)
	assert invalid_octal("x008", 0)
	assert !invalid_octal("x007", 0)
	assert !invalid_octal("x100", 0)
	assert 0 in 0 .. 8
	assert 7 in 0 .. 8
	assert 8 !in 0 .. 8
	assert -1 !in 0 .. 8
	assert u8(255) in 0 .. 256
	mut calls := 0
	assert counted(mut calls, 3) in counted(mut calls, 0) .. counted(mut calls, 8)
	assert calls == 3
	println("range membership")
}
'

fn test_fastc_range_membership_lowers_inside_nested_boolean_expressions() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate(nested_range_source, 'nested_range_membership.v', prefs) or {
		panic(err)
	}
	assert !c_source.contains('!in'), c_source
	root := os.join_path(os.vtmp_dir(), 'fastc_range_membership_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), nested_range_source) or { panic(err) }
	program := os.join_path(root, 'program')
	compiled := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-no-prealloc', '-nocache', '-o',
		program, root])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(program, [])
	assert executed.exit_code == 0, executed.output
	assert executed.output == 'range membership\n'
}
