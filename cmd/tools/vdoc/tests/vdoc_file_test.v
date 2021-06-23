import os
import rand
import term
import v.util.vtest
import v.util.diff

const vexe = @VEXE

const vroot = @VMODROOT

const diff_cmd = find_diff_cmd()

fn find_diff_cmd() string {
	return diff.find_working_diff_command() or { '' }
}

fn test_vet() {
	os.setenv('VCOLORS', 'never', true)
	os.chdir(vroot)
	test_dir := 'cmd/tools/vdoc/tests/testdata'
	main_files := get_main_files_in_dir(test_dir)
	fails := check_path(vexe, test_dir, main_files)
	assert fails == 0
}

fn get_main_files_in_dir(dir string) []string {
	mut mfiles := os.walk_ext(dir, '.v')
	mfiles.sort()
	return mfiles
}

fn check_path(vexe string, dir string, tests []string) int {
	mut nb_fail := 0
	paths := vtest.filter_vtest_only(tests, basepath: vroot)
	for path in paths {
		program := path
		print(path + ' ')
		res := os.execute('$vexe doc $program')
		if res.exit_code < 0 {
			panic(res.output)
		}
		mut expected := os.read_file(program.replace('main.v', 'main.out')) or { panic(err) }
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
			println('diff:')
			println(diff.color_compare_strings(diff_cmd, rand.ulid(), found, expected))
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
