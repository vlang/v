import os
import term
import v.util.vtest
import v.util.diff

fn test_vet() {
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	os.chdir(vroot)!
	test_dir := 'cmd/tools/vvet/tests'
	tests := get_tests_in_dir(test_dir)
	fails := check_path(vexe, test_dir, tests)
	assert fails == 0
}

fn get_tests_in_dir(dir string) []string {
	files := os.ls(dir) or { panic(err) }
	mut tests := files.filter(it.ends_with('.vv'))
	tests.sort()
	return tests
}

fn check_path(vexe string, dir string, tests []string) int {
	mut nb_fail := 0
	paths := vtest.filter_vtest_only(tests, basepath: dir)
	for path in paths {
		program := path
		print(path + ' ')
		res := os.execute('${os.quoted_path(vexe)} vet -nocolor ${os.quoted_path(program)}')
		if res.exit_code < 0 {
			panic(res.output)
		}
		mut expected := os.read_file(program.replace('.vv', '') + '.out') or { panic(err) }
		expected = clean_line_endings(expected)
		found := clean_line_endings(res.output)
		if expected != found {
			println(term.red('FAIL'))
			println('============')
			if diff_ := diff.compare_text(expected, found) {
				println('diff:')
				println(diff_)
			} else {
				println('expected:')
				println(expected)
				println('============')
				println('found:')
				println(found)
			}
			println('============\n')
			nb_fail++
		} else {
			println(term.green('OK'))
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
