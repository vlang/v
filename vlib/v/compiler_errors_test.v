import os
import term
import v.util

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
	parser_dir := 'vlib/v/parser/tests'
	parser_tests := get_tests_in_dir(parser_dir)
	// -prod so that warns are errors
	total_errors += check_path(vexe, classic_dir, '-prod', '.out', classic_tests)
	total_errors += check_path(vexe, global_dir, '--enable-globals', '.out', global_tests)
	total_errors += check_path(vexe, classic_dir, '--enable-globals run', '.run.out',
		['globals_error.vv'])
	total_errors += check_path(vexe, run_dir, 'run', '.run.out', run_tests)
	total_errors += check_path(vexe, parser_dir, '-prod', '.out', parser_tests)
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
	vtest_only := os.getenv('VTEST_ONLY').split(',')
	mut nb_fail := 0
	mut paths := []string{}
	for test in tests {
		path := os.join_path(dir, test).replace('\\', '/')
		if vtest_only.len > 0 {
			mut found := 0
			for substring in vtest_only {
				if path.contains(substring) {
					found++
					break
				}
			}
			if found == 0 {
				continue
			}
		}
		paths << path
	}
	for path in paths {
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
			diff_content(expected, found)
			nb_fail++
		} else {
			println(term.green('OK'))
			os.rm(program)
		}
	}
	return nb_fail
}

fn clean_line_endings(s string) string {
	mut res := s.trim_space()
	res = res.replace(' \n', '\n')
	res = res.replace(' \r\n', '\n')
	res = res.replace('\r\n', '\n')
	res = res.trim('\n')
	return res
}

fn diff_content(s1, s2 string) {
	diff_cmd := util.find_working_diff_command() or {
		return
	}
	println('diff: ')
	println(util.color_compare_strings(diff_cmd, s1, s2))
	println('============\n')
}
