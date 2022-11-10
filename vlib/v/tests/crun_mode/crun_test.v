import os
import time

const crun_folder = os.join_path(os.vtmp_dir(), 'v', 'crun_folder')

const vprogram_file = os.join_path(crun_folder, 'vprogram.vv')

const vexe = os.getenv('VEXE')

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
	os.write_file(vprogram_file, 'print("hello")')?
	assert true
}

fn test_crun_simple_v_program_several_times() {
	mut sw := time.new_stopwatch()
	mut times := []i64{}
	for i in 0 .. 10 {
		vcrun()
		times << sw.elapsed().microseconds()
		time.sleep(50 * time.millisecond)
		sw.restart()
	}
	dump(times)
	assert times.first() > times.last() * 4 // cruns compile just once, if the source file is not changed
	$if !windows {
		os.system('ls -la $crun_folder')
		os.system('find $crun_folder')
	}
}

fn vcrun() {
	cmd := '${os.quoted_path(vexe)} crun ${os.quoted_path(vprogram_file)}'
	eprintln('now: $time.now().format_ss_milli() | cmd: $cmd')
	res := os.execute(cmd)
	assert res.exit_code == 0
	assert res.output == 'hello'
}
