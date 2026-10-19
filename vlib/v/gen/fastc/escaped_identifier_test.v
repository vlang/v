module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_escaped_keyword_fields_and_locals_preserve_pseudo_values() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_escaped_fields_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Lock {
mut:
	count int
}
struct Event {
mut:
	@lock Lock
	@type int
}
fn (mut gate Lock) acquire() { gate.count++ }
fn main() {
	mut events := [Event{}, Event{}]
	events[1].@lock.acquire()
	events[1].@type = 7
	assert events[0].@lock.count == 0
	assert events[1].@lock.count == 1 && events[1].@type == 7
	mut @type := 41
	@type++
	assert @type == 42
	assert @FN == "main"
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
	build := cmdexec.run('cc', ['-std=gnu11', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	mut unsupported := ''
	_ := generate('module main\nfn main() { println(@UNKNOWN) }', 'unknown_pseudo.v', prefs) or {
		unsupported = err.msg()
		''
	}
	assert unsupported.contains('compile-time pseudo value `@UNKNOWN`'), unsupported
}
