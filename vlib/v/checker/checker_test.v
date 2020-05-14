import os
import term

fn test_all() {
	mut total_errors := 0
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot)
	dir := 'vlib/v/checker/tests'
	files := os.ls(dir) or {
		panic(err)
	}
	mut tests := files.filter(it.ends_with('.vv'))
	if tests.len == 0 {
		println('no compiler tests found')
		assert false
	}
	tests.sort()
	for test in tests {
		// -prod so that warns are errors
		total_errors += check_path(vexe, dir, test, '-prod', '.out')
	}
	total_errors += check_path(vexe, dir, 'globals_error.vv', '--enable-globals run', '.run.out')
	assert total_errors == 0
}

fn check_path(vexe, dir, test, voptions, result_extension string) int {
	path := os.join_path(dir, test).replace('\\', '/')
	program := path.replace('.vv', '.v')
	print(program + ' ')
	os.cp(path, program) or {
		panic(err)
	}
	res := os.exec('$vexe $voptions $program') or {
		panic(err)
	}
	mut expected := os.read_file(program.replace('.v', '') + result_extension) or {
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
		return 1
	} else {
		println(term.green('OK'))
		os.rm(program)
	}
	return 0
}

fn clean_line_endings(s string) string {
	return s.trim_space().replace(' \n', '\n').replace(' \r\n', '\n').replace('\r\n', '\n').trim('\n')
}
