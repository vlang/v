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
	dump(vexe)
	assert vexe != ''
	dump(os.executable())
	dump(@FILE)
	dump(cfolder)
	dump(so_ext)
	dump(library_file_name)
}

fn test_can_compile_library() {
	os.chdir(cfolder)
	os.rm(library_file_name) or { }
	res := v_compile('-d no_backtrace -o library -shared modules/library/library.v')
	dump(res)
	assert os.is_file(library_file_name)
}

fn test_can_compile_main_program() {
	os.chdir(cfolder)
	assert os.is_file(library_file_name)
	result := v_compile('run use.v')
	dump(result)
	assert result.output.contains('res: 4')
	os.rm(library_file_name) or { }
}

fn v_compile(vopts string) os.Result {
	cmd := '"$vexe" -showcc $vopts'
	dump(cmd)
	res := os.execute_or_panic(cmd)
	dump(res)
	// assert res.exit_code == 0
	$if windows {
		os.system('dir $cfolder /a')
	} $else {
		os.system('ls -al $cfolder')
	}
	return res
}
