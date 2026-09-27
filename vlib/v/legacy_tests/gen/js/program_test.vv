import os
import term
import v.util.vtest
import v.util.diff

const vexe = @VEXE

const vroot = @VMODROOT

const github_job = os.getenv('GITHUB_JOB')

@[noreturn]
fn exit_because(msg string) {
	eprintln('${msg}, tests will not run')
	exit(0)
}

fn test_node_exists() {
	res := os.execute('node --version')
	if res.exit_code != 0 {
		exit_because('node does not exist')
	}
	if !res.output.starts_with('v') {
		exit_because('invalid node version')
	}
	version := res.output.trim_left('v').int()
	if version < 10 {
		exit_because('node should be at least version 10, but is currently version: ${version}')
	}
	println('Using node version: ${version}')
	// if github_job != '' && os.user_os() == 'macos' {
	//	exit_because('program_test.v is flaky on macos')
	//}
}

fn test_running_programs_compiled_with_the_js_backend() {
	os.setenv('VCOLORS', 'never', true)
	os.chdir(vroot) or {}
	test_dir := 'vlib/v/gen/js/tests/testdata'
	main_files := get_main_files_in_dir(test_dir)
	fails := check_path(test_dir, main_files)!
	assert fails == 0
}

fn get_main_files_in_dir(dir string) []string {
	mut mfiles := os.walk_ext(dir, '.v')
	mfiles.sort()
	return mfiles
}

fn check_path(dir string, tests []string) !int {
	mut nb_fail := 0
	paths := vtest.filter_vtest_only(tests, basepath: vroot)
	for path in paths {
		program := path.replace(vroot + os.path_separator, '')
		program_out := program.replace('.v', '.out')
		if !os.exists(program_out) {
			os.write_file(program_out, '')!
		}
		print(program + ' ')
		res := os.execute('${os.quoted_path(vexe)} -b js_node run ${os.quoted_path(program)}')
		if res.exit_code < 0 {
			panic(res.output)
		}
		mut expected := os.read_file(program_out)!
		expected = clean_line_endings(expected)
		found := clean_line_endings(res.output)
		if expected != found {
			println(term.red('FAIL'))
			println('============')
			if diff_ := diff.compare_text(expected, found) {
				println('diff:')
				println(diff_)
			} else {
				println('expected ${program_out} content:')
				println(expected)
				println('============')
				println('found:')
				println(found)
			}
			println('============\n')
			nb_fail++
		} else {
			println(term.green('OK'))
			assert true
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
