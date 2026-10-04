// vtest retry: 3
import os
import time

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

fn test_saving_simple_v_program() {
	os.write_file(vprogram_file, '#include <stddef.h>\nprint("hello")')!
	assert true
}

fn test_crun_simple_v_program_several_times() {
	mut binary := vprogram_file.all_before_last('.')
	$if windows {
		binary += '.exe'
	}
	first := vcrun(vprogram_file)
	assert first.output == 'hello', first.output
	assert os.is_file(binary)
	for _ in 0 .. 3 {
		cache_stamp := os.file_last_mod_unix(binary) + 3600
		os.utime(binary, cache_stamp, cache_stamp)!
		result := vcrun(vprogram_file)
		assert result.output == 'hello', result.output
		assert os.file_last_mod_unix(binary) != cache_stamp
	}
	$if !windows {
		os.system_args(['ls', '-la', '${crun_folder}'])
		os.system_args(['find', '${crun_folder}'])
	}
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

fn vcrun(target string) os.Result {
	cmd := '${os.quoted_path(vexe)} crun ${os.quoted_path(target)}'
	eprintln('now: ${time.now().format_ss_milli()} | cmd: ${cmd}')
	res := os.exec([vexe, 'crun', '${target}'])
	assert res.exit_code == 0
	return res
}

fn test_crun_simple_v_program_output() {
	res := vcrun(vprogram_file)
	assert res.output == 'hello'
}
