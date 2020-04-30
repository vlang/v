import os
import term

fn clean_line_endings(s string) string {
	return s.trim_space().replace(' \n', '\n').replace(' \r\n', '\n').replace('\r\n', '\n').trim('\n')
}

fn test_all() {
	mut total_errors := 0
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot)
	dir := 'vlib/v/checker/tests'
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
		program := path.replace('.vv', '.v')
		print(program + ' ')
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
		expected = clean_line_endings(expected)
		found := clean_line_endings(res.output)
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
			os.rm( program )
		}
	}
	assert total_errors == 0
}
