import os
import term

fn test_all() {
	mut total_errors := 0
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot)
	classic_dir := 'vlib/v/checker/tests'
	classic_tests := get_tests_in_dir(classic_dir)
	global_dir := '$classic_dir/globals'
	global_tests := get_tests_in_dir(global_dir)
	run_dir := '$classic_dir/run'
	run_tests := get_tests_in_dir(run_dir)	
	// -prod so that warns are errors
	total_errors += check_path(vexe, classic_dir, '-prod', '.out', classic_tests)
	total_errors += check_path(vexe, global_dir, '--enable-globals', '.out', global_tests)
	total_errors += check_path(vexe, classic_dir, '--enable-globals run', '.run.out', ['globals_error.vv'])
	total_errors += check_path(vexe, run_dir,   'run', '.run.out', run_tests)
	assert total_errors == 0
}

fn get_tests_in_dir(dir string) []string {
	files := os.ls(dir) or {
		panic(err)
	}
	mut tests := files.filter(it.ends_with('.vv'))
	tests.sort()
	return tests
}

fn check_path(vexe, dir, voptions, result_extension string, tests []string) int {
	mut nb_fail := 0
	for test in tests {
		path := os.join_path(dir, test).replace('\\', '/')
		program := path.replace('.vv', '.v')
		print(path + ' ')
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
			nb_fail += 1
		} else {
			println(term.green('OK'))
			os.rm(program)
		}
	}
	return nb_fail
}

fn clean_line_endings(s string) string {
	return s.trim_space().replace(' \n', '\n').replace(' \r\n', '\n').replace('\r\n', '\n').trim('\n')
}
