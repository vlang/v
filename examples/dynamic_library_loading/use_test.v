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
	// dump(vexe)
	assert vexe != ''
	// dump(os.executable())
	// dump(@FILE)
	// dump(cfolder)
	// dump(so_ext)
	// dump(library_file_name)
}

fn test_can_compile_library() {
	os.chdir(cfolder) or {}
	os.rm(library_file_name) or {}
	v_compile('-d no_backtrace -o library -shared modules/library/library.v')
	assert os.is_file(library_file_name)
}

fn test_can_compile_main_program() {
	os.chdir(cfolder) or {}
	assert os.is_file(library_file_name)
	result := v_compile('run use.v')
	// dump(result)
	assert result.output.contains('res: 4')
	os.rm(library_file_name) or {}
}

fn test_can_compile_and_use_library_with_skip_unused() {
	os.chdir(cfolder) or {}
	os.rm(library_file_name) or {}
	v_compile('-skip-unused -d no_backtrace -o library -shared modules/library/library.v')
	assert os.is_file(library_file_name)
	result := v_compile('run use.v')
	assert result.output.contains('res: 4')
	os.rm(library_file_name) or {}
}

fn v_compile(vopts string) os.Result {
	cmd := '${os.quoted_path(vexe)} -showcc ${vopts}'
	// dump(cmd)
	res := os.execute_or_exit(cmd)
	// dump(res)
	assert res.exit_code == 0
	return res
}
