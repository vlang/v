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
	os.write_file(vprogram_file, 'print("hello")')!
	assert true
}

fn test_crun_simple_v_program_several_times() {
	mut sw := time.new_stopwatch()
	mut times := []i64{}
	for i in 0 .. 10 {
		vcrun(vprogram_file)
		times << sw.elapsed().microseconds()
		time.sleep(50 * time.millisecond)
		sw.restart()
	}
	dump(times)
	assert times.first() > times.last() * 2 // cruns compile just once, if the source file is not changed
	$if !windows {
		os.system('ls -la ${crun_folder}')
		os.system('find ${crun_folder}')
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
	// `crun` cache invalidation uses second-resolution mtimes.
	time.sleep(1100 * time.millisecond)
	first := vcrun(module_dir)
	assert first.output == 'OLD:0:1\nOLD:1:2\n'
	time.sleep(1100 * time.millisecond)
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
	res := os.execute(cmd)
	assert res.exit_code == 0
	return res
}

fn test_crun_simple_v_program_output() {
	res := vcrun(vprogram_file)
	assert res.output == 'hello'
}
