module main

import os
import dl

const (
	vexe              = os.real_path(os.getenv('VEXE'))
	cfolder           = os.dir(@FILE)
	so_ext            = dl.dl_ext
	library_file_name = os.join_path(cfolder, dl.get_libname('library'))
)

fn test_vexe() {
	eprintln('vexe: $vexe')
	assert vexe != ''
	eprintln('os.executable: ' + os.executable())
	eprintln('@FILE: ' + @FILE)
	eprintln('cfolder: $cfolder')
	eprintln('so_ext: $so_ext')
	eprintln('library_file_name: $library_file_name')
}

fn test_can_compile_library() {
	os.chdir(cfolder)
	os.rm(library_file_name) or { }
	res := v_compile('-d no_backtrace -o library -shared library.v')
	eprintln('res: $res')
	assert os.is_file(library_file_name)
}

fn test_can_compile_main_program() {
	os.chdir(cfolder)
	assert os.is_file(library_file_name)
	result := v_compile('run use.v')
	eprintln('result: $result')
	assert result.output.contains('res: 4')
	os.rm(library_file_name) or { }
}

fn v_compile(vopts string) os.Result {
	cmd := '"$vexe" -showcc $vopts'
	eprintln('>>> v_compile cmd: $cmd')
	res := os.exec(cmd) or { panic(err) }
	eprintln('>>> v_compile res: $res')
	// assert res.exit_code == 0
	$if !windows {
		os.system('ls -al $cfolder')
	} $else {
		os.system('dir $cfolder /a')
	}
	return res
}
