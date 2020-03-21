import os
import term

fn test_all() {
	$if !linux {
		println('Valgrind tests can only be run on Linux.')
		exit(0)
	}
	exe := os.executable()
	dir := os.dir(exe)
	vexe := os.dir(os.dir(os.dir(os.dir(dir)))) + '/v'
	println(vexe)
	println(dir)
	println(111)
	// files := os.ls('$dir/vlib/v/tests/valgrind/') or {
	files := os.ls(dir) or {
		panic(err)
	}
	tests := files.filter(it.ends_with('.vv'))
	for test in tests {
		os.system('cp $dir/$test x.v') // cant run .vv file
		println(test)
		res := os.exec('$vexe x.v') or {
			println('valgrind $test failed')
			assert false
			continue
		}
		println(res.output)
		os.exec('valgrind ./x') or {
			println('valgrind $test failed')
			assert false
			continue
		}
		println(res.output)
		if res.exit_code != 0 {
			println('valgrind $test failed')
			assert false
		}
	}
	println(tests)
}
