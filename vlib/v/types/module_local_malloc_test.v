module types

import os

fn test_a_module_calls_its_own_malloc_before_builtins() {
	root := os.join_path(os.vtmp_dir(), 'module_local_malloc_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'mem'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'local_malloc' }\n")!
	os.write_file(os.join_path(root, 'mem', 'mem.v'), 'module mem
pub fn malloc(size u64) voidptr {
	return voidptr(size)
}
pub fn bench() u64 {
	return u64(malloc(64))
}
')!
	os.write_file(os.join_path(root, 'main.v'), 'module main
import mem
fn main() {
	println(mem.bench())
}
')!
	result := os.exec([@VEXE, 'run', root])
	assert result.exit_code == 0, result.output
	// builtin malloc would have returned a heap address, not the size.
	assert result.output.trim_space().ends_with('64'), result.output
}
