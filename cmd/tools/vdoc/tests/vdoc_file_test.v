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
	os.chdir(vroot)!
	test_dir := 'cmd/tools/vdoc/tests/testdata'
	main_files := get_main_files_in_dir(test_dir)
	fails := check_path(vexe, test_dir, main_files)
	assert fails == 0
}

fn test_run_examples_good() {
	os.setenv('VCOLORS', 'never', true)
	os.chdir(vroot)!
	res := os.execute('${os.quoted_path(vexe)} doc -comments -run-examples cmd/tools/vdoc/tests/testdata/run_examples_good/main.v')
	assert res.exit_code == 0
	assert res.output.contains('module main'), res.output
	assert res.output.contains('fn abc()'), res.output
	assert res.output.contains("abc just prints 'xyz'"), res.output
	assert res.output.contains('and should succeed'), res.output
	assert res.output.contains('Example: assert 5 * 5 == 25'), res.output
}

fn test_run_examples_bad() {
	os.setenv('VCOLORS', 'never', true)
	os.chdir(vroot)!
	res := os.execute('${os.quoted_path(vexe)} doc -comments -run-examples cmd/tools/vdoc/tests/testdata/run_examples_bad/main.v')
	assert res.exit_code != 0
	assert res.output.contains('error in documentation example'), res.output
	assert res.output.contains(' left value: 5 * 5 = 25'), res.output
	assert res.output.contains('right value: 77'), res.output
	assert res.output.contains('V panic: Assertion failed...'), res.output
	assert res.output.contains('module main'), res.output
	assert res.output.contains('Example: assert 5 * 5 == 77'), res.output
}

fn get_main_files_in_dir(dir string) []string {
	mut mfiles := os.walk_ext(dir, '.v')
	mfiles.sort()
	return mfiles
}

fn check_path(vexe string, dir string, tests []string) int {
	mut total_fails := 0
	paths := vtest.filter_vtest_only(tests, basepath: vroot)
	for path in paths {
		mut fails := 0
		program := path
		print(path + ' ')
		fails += check_output(
			program: program
			cmd: '${os.quoted_path(vexe)} doc ${os.quoted_path(program)}'
			out_filename: 'main.out'
		)
		fails += check_output(
			program: program
			cmd: '${os.quoted_path(vexe)} doc -comments ${os.quoted_path(program)}'
			out_filename: 'main.unsorted.out'
			should_sort: false
		)
		fails += check_output(
			program: program
			cmd: '${os.quoted_path(vexe)} doc -comments ${os.quoted_path(program)}'
			out_filename: 'main.comments.out'
		)
		fails += check_output(
			program: program
			cmd: '${os.quoted_path(vexe)} doc -readme -comments ${os.quoted_path(program)}'
			out_filename: 'main.readme.comments.out'
		)
		total_fails += fails
		if fails == 0 {
			println(term.green('OK'))
		}
		flush_stdout()
	}
	return total_fails
}

fn print_compare(expected string, found string) {
	println(term.red('FAIL'))
	println('============')
	println('expected:')
	println(expected)
	println('============')
	println('found:')
	println(found)
	println('============\n')
	println('diff:')
	println(diff.color_compare_strings(diff_cmd, rand.ulid(), expected, found))
	println('============\n')
}

fn clean_line_endings(s string) string {
	mut res := s.trim_space()
	res = res.replace(' \n', '\n')
	res = res.replace(' \r\n', '\n')
	res = res.replace('\r\n', '\n')
	res = res.trim('\n')
	return res
}

@[params]
struct CheckOutputParams {
	program       string = 'some/dir/main.v'
	cmd           string = 'v doc'
	main_filename string = 'main.v'
	out_filename  string = 'main.out'
	should_sort   bool   = true
}

fn check_output(params CheckOutputParams) int {
	out_file_path := params.program.replace(params.main_filename, params.out_filename)
	if !os.exists(out_file_path) {
		return 0
	}
	mut fails := 0
	mut expected := os.read_file(out_file_path) or { panic(err) }
	expected = clean_line_endings(expected)

	os.setenv('VDOC_SORT', params.should_sort.str(), true)
	res := os.execute(params.cmd)

	if res.exit_code < 0 {
		panic(res.output)
	}
	found := clean_line_endings(res.output)
	if expected != found {
		print_compare(expected, found)
		eprintln('>>> out_file_path: ${out_file_path}')
		eprintln('>>>           cmd: VDOC_SORT=${params.should_sort} ${params.cmd}')
		fails++
	}
	return fails
}
