module fastc

import os
import v.cmdexec

fn test_runtime_constants_observe_global_optional_defaults() ! {
	root := os.join_path(os.vtmp_dir(), 'fastc_global_defaults_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Defaults { item ?&int values []int }
__global empty = Defaults{}
const ready = initialized()
__global dependent = ready
fn initialized() bool {
	assert empty.item == none
	assert empty.values.len == 0
	return true
}
fn main() { assert ready && dependent }
')!
	program := os.join_path(root, 'program')
	built := cmdexec.run(@VEXE, ['-b', 'fastc', '-cc', 'tcc', '-enable-globals', '-nocache', '-o',
		program, root])
	assert built.exit_code == 0, built.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
