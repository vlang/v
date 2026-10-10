module c

import os
import v.cmdexec

fn test_global_and_function_with_the_same_name_compile_and_run() {
	root := os.join_path(os.vtmp_dir(), 'global_function_symbols_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	for fixture, source in {
		'main':   'module main
__global callback = 17
fn callback(value int) int { return value * 2 }
fn main() {
	assert callback == 17
	callback += 2
	assert callback == 19
}
'
		'module': 'module main
import local
fn main() { assert local.result() == 23 }
'
		'extern': 'module main
import local as alias
fn main() {
	assert alias.counter == 17
	assert alias.result() == 17
}
'
	} {
		dir := os.join_path(root, fixture)
		os.mkdir_all(os.join_path(dir, 'local')) or { panic(err) }
		main_file := os.join_path(dir, 'main.v')
		os.write_file(main_file, source) or { panic(err) }
		if fixture == 'module' {
			os.write_file(os.join_path(dir, 'local', 'local.v'), 'module local
__global callback = 17
fn callback(value int) int { return value * 2 }
pub fn result() int { return callback + local.callback(3) }
') or { panic(err) }
		} else if fixture == 'extern' {
			header := os.join_path(dir, 'local', 'counter.h')
			os.write_file(header, 'long long counter = 17;\n') or { panic(err) }
			os.write_file(os.join_path(dir, 'local', 'local.v'), 'module local
#insert "${header}"
@[c_extern]
pub __global counter int
pub fn result() int { return local.counter }
') or { panic(err) }
		}
		result := cmdexec.run(@VEXE, ['-b', 'c', '-cc', 'clang', '-enable-globals', 'run', main_file])
		assert result.exit_code == 0, '${fixture}: ${result.output}'
	}
}
