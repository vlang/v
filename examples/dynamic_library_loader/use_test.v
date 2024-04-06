module main

import os
import dl

const vexe = os.real_path(os.getenv('VEXE'))
const so_ext = dl.dl_ext

fn test_vexe() {
	// dump(vexe)
	assert vexe != ''
	// dump(os.executable())
	// dump(@FILE)
	// dump(cfolder)
	// dump(so_ext)
}

fn test_can_compile_library() {
	os.chdir(cfolder) or {}
	library_file_path := os.join_path(cfolder, dl.get_libname('library'))
	os.rm(library_file_path) or {}
	v_compile('-d no_backtrace -o library -shared modules/library/library.v')
	assert os.is_file(library_file_path)
}

fn test_can_compile_main_program() {
	os.chdir(cfolder) or {}
	library_file_path := os.join_path(cfolder, dl.get_libname('library'))
	assert os.is_file(library_file_path)
	result := v_compile('run use_shared_library.v')
	// dump(result)
	assert result.output.contains('hello from add_1 , num = 4')
	assert result.output.contains('res: 4')
	os.rm(library_file_path) or {}
}

fn test_can_compile_and_use_library_with_skip_unused_home_dir() {
	os.chdir(cfolder) or {}
	library_file_path := os.join_path(cfolder, dl.get_libname('library'))
	os.rm(library_file_path) or {}
	v_compile('-skip-unused -d no_backtrace -o library -shared modules/library/library.v')
	assert os.is_file(library_file_path)
	result := v_compile('run use_shared_library.v')
	assert result.output.contains('res: 4')
	os.rm(library_file_path) or {}
}

fn test_can_compile_and_use_library_with_skip_unused_location1_dir() {
	os.chdir(cfolder) or {}
	library_file_path := os.join_path(cfolder, 'location1', dl.get_libname('library'))
	os.rm(library_file_path) or {}
	os.mkdir('location1') or {}
	v_compile('-skip-unused -d no_backtrace -o location1/library -shared modules/library/library.v')
	assert os.is_file(library_file_path)
	result := v_compile('run use_shared_library.v')
	assert result.output.contains('res: 4')
	os.rm(library_file_path) or {}
}

fn v_compile(vopts string) os.Result {
	cmd := '${os.quoted_path(vexe)} -showcc ${vopts}'
	// dump(cmd)
	res := os.execute_or_exit(cmd)
	// dump(res)
	assert res.exit_code == 0
	return res
}
