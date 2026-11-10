module fastc

import os
import v.cmdexec

fn test_struct_spread_from_result_call_with_named_arguments_and_fallback() ! {
	root := os.join_path(os.vtmp_dir(), 'fastc_spread_result_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Box { value int enabled bool }
@[params]
struct Config { value int }
fn make(config Config) !Box {
	if config.value < 0 { return error("negative") }
	return Box{value: config.value, enabled: true}
}
fn recover() Box {
	return Box{
		...make(value: -1) or { Box{value: 19} }
		enabled: true
	}
}
fn main() {
	value := Box{
		...make(value: 7) or { panic(err) }
		enabled: false
	}
	assert value.value == 7
	assert !value.enabled
	assert recover() == Box{value: 19, enabled: true}
}
')!
	for compiler in ['tcc', 'clang'] {
		if compiler == 'clang' && os.find_abs_path_of_executable(compiler) == none { continue }
		program := os.join_path(root, compiler)
		built := cmdexec.run(@VEXE, ['-b', 'fastc', '-cc', compiler, '-nocache', '-o', program,
			root])
		assert built.exit_code == 0, built.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
}
