// vtest retry: 3
import os

const vexe = os.getenv('VEXE')
const crun_folder = os.join_path(os.vtmp_dir(), 'crun_folder')
const vprogram_file = os.join_path(crun_folder, 'vprogram.vv')

fn testsuite_begin() {
	os.setenv('VCACHE', crun_folder, true)
	os.rmdir_all(crun_folder) or {}
	os.mkdir_all(crun_folder) or {}
	assert os.is_dir(crun_folder)
}

fn testsuite_end() {
	os.chdir(os.wd_at_startup) or {}
	os.rmdir_all(crun_folder) or {}
	assert !os.is_dir(crun_folder)
}

fn test_crun_reuses_executable_for_unchanged_v_program() {
	os.write_file(vprogram_file, 'print("hello")')!
	first := vcrun(vprogram_file)
	assert first.output == 'hello'

	cached := vcrun(vprogram_file, '-show-timings')
	assert cached.output.split_into_lines().any(it.starts_with('hello')), cached.output
	assert cached.output.contains('run (cached)'), cached.output
}

fn test_crun_rebuilds_when_local_c_source_changes() {
	module_dir := os.join_path(crun_folder, 'c_source_module')
	main_file := os.join_path(module_dir, 'code_tests.v')
	os.mkdir_all(module_dir)!
	os.write_file(os.join_path(module_dir, 'v.mod'), "Module {\n\tname: 'c_source_module'\n}\n")!
	os.write_file(main_file, [
		'module main',
		'',
		'#include "@VMODROOT/code.c"',
		'',
		'@[keep_args_alive]',
		'fn C.foo(arg [4]int)',
		'',
		'fn main() {',
		'\tC.foo([1, 2, 3, 4]!)',
		'}',
	].join('\n'))!
	write_c_source_module(module_dir, 'OLD', 2)!
	first := vcrun(module_dir)
	assert first.output == 'OLD:0:1\nOLD:1:2\n'
	write_c_source_module(module_dir, 'NEW', 4)!
	second := vcrun(module_dir)
	assert second.output == 'NEW:0:1\nNEW:1:2\nNEW:2:3\nNEW:3:4\n'
}

fn write_c_source_module(module_dir string, prefix string, count int) ! {
	os.write_file(os.join_path(module_dir, 'code.c'), [
		'#include <stdio.h>',
		'',
		'void foo(int arg[4]) {',
		'\tfor (int i = 0; i < ${count}; ++i) {',
		'\t\tprintf("${prefix}:%d:%d\\n", i, arg[i]);',
		'\t}',
		'}',
	].join('\n'))!
}

fn vcrun(target string, flags ...string) os.Result {
	res := os.exec([vexe, ...flags, 'crun', target])
	assert res.exit_code == 0, res.output
	return res
}
