import os
import term

fn test_all() {
	$if !linux {
		println('Valgrind tests can only be run on Linux.')
		exit(0)
	}
	exe := os.executable()
	dir := os.dir(exe)
	println(dir)
	println(111)
	// files := os.ls('$dir/vlib/v/tests/valgrind/') or {
	files := os.ls(dir) or {
		panic(err)
	}
	tests := files.filter(it.ends_with('.vv'))
	println(tests)
}
