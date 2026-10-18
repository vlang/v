module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_same_line_grouped_globals_preserve_types_initializers_and_callbacks() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_grouped_globals_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Lock { value int }
__global (controller_lock Lock io_epoch u64 = 1)
__global (first int = 7 second u64 = u64(9) state Lock = Lock{ value: 11 } callback fn (int) ?int = unsafe { nil } last int = 13)
__global (enabled bool = false disabled bool = !enabled computed int = first + last trailing int = 29)
@[cinit]
__global (volatile static_first u64 = 17 volatile static_second u64 = 19)
fn probe(value int) ?int {
	if value < 0 { return none }
	return value + 1
}
fn main() {
	assert controller_lock.value == 0 && io_epoch == 1
	assert first == 7 && second == 9 && state.value == 11 && last == 13
	assert !enabled && disabled && computed == 20 && trailing == 29
	assert static_first == 17 && static_second == 19
	assert callback == unsafe { nil }
	callback = probe
	assert (callback(41) or { -1 }) == 42
	assert (callback(-1) or { -2 }) == -2
	controller_lock.value = 23
	io_epoch++
	assert controller_lock.value == 23 && io_epoch == 2
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
