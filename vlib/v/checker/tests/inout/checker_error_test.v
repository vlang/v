import os
import term

fn test_all() {
	mut total_errors := 0
	vexe := os.getenv('VEXE')
	// vroot := os.dir(vexe)
	dir := 'vlib/v/checker/tests/inout'
	files := os.ls(dir) or {
		panic(err)
	}
	tests := files.filter(it.ends_with('.vv'))
	if tests.len == 0 {
		println('no compiler tests found')
		assert false
	}
	for test in tests {
		path := os.join_path(dir, test).replace('\\', '/')
		print(test + ' ')
		program := path.replace('.vv', '.v')
		os.cp(path, program) or {
			panic(err)
		}
		// -prod so that warn are errors
		res := os.exec('$vexe -prod $program') or {
			panic(err)
		}
		mut expected := os.read_file(program.replace('.v', '') + '.out') or {
			panic(err)
		}
		expected = expected.trim_space().replace(' \n', '\n').replace(' \r\n', '\n').replace('\r\n',
			'\n').trim('\n')
		found := res.output.trim_space().replace(' \n', '\n').replace(' \r\n', '\n').replace('\r\n',
			'\n').trim('\n')
		if expected != found {
			println(term.red('FAIL'))
			println('============')
			println('expected:')
			println(expected)
			println('============')
			println('found:')
			println(found)
			println('============\n')
			total_errors++
		} else {
			println(term.green('OK'))
		}
	}
	assert total_errors == 0
}
