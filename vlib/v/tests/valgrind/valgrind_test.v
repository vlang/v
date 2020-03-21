import os
import term

fn test_all() {
	$if !linux {
		println('Valgrind tests can only be run on Linux.')
		exit(1)
	}
	exe := os.executable()
	dir := os.dir(exe)
	files := os.ls('$dir/vlib/v/tests/valgrind/') or {
		panic(err)
	}
	println(files)
}
