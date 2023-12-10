import os
import rand
import term
import v.util.vtest
import v.util.diff

const vexe = os.quoted_path(@VEXE)

const vroot = @VMODROOT

const diff_cmd = find_diff_cmd()

const should_autofix = os.getenv('VAUTOFIX') != ''

fn find_diff_cmd() string {
	return diff.find_working_diff_command() or { '' }
}

fn test_vet() {
	os.setenv('VCOLORS', 'never', true)
	os.chdir(vroot)!
	test_dir := 'cmd/tools/vdoc/tests/testdata'
	main_files := get_main_files_in_dir(test_dir)
	fails := check_path(test_dir, main_files)
	assert fails == 0
}

fn test_run_examples_good() {
	os.setenv('VCOLORS', 'never', true)
	os.chdir(vroot)!
	res := os.execute('${vexe} doc -comments -run-examples cmd/tools/vdoc/tests/testdata/run_examples_good/main.v')
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
	res := os.execute('${vexe} doc -comments -run-examples cmd/tools/vdoc/tests/testdata/run_examples_bad/main.v')
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

fn check_path(dir string, tests []string) int {
	mut total_fails := 0
	paths := vtest.filter_vtest_only(tests, basepath: vroot)
	for path in paths {
		mut fails := 0
		qpath := os.quoted_path(path)
		print(path + ' ')
		fails += check_output(
			program: path
			cmd: '${vexe} doc ${qpath}'
			out_filename: 'main.out'
		)
		fails += check_output(
			program: path
			cmd: '${vexe} doc -comments ${qpath}'
			out_filename: 'main.unsorted.out'
			should_sort: false
		)
		fails += check_output(
			program: path
			cmd: '${vexe} doc -comments ${qpath}'
			out_filename: 'main.comments.out'
		)
		fails += check_output(
			program: path
			cmd: '${vexe} doc -readme -comments ${qpath}'
			out_filename: 'main.readme.comments.out'
		)
		// test the main 3 different formats:
		program_dir := os.quoted_path(if os.is_file(path) { os.dir(path) } else { path })
		fails += check_output(
			program: path
			cmd: '${vexe} doc -f html -o - -html-only-contents -readme -comments ${program_dir}'
			out_filename: 'main.html'
		)
		fails += check_output(
			program: path
			cmd: '${vexe} doc -f ansi -o - -html-only-contents -readme -comments ${program_dir}'
			out_filename: 'main.ansi'
		)
		fails += check_output(
			program: path
			cmd: '${vexe} doc -f text -o - -html-only-contents -readme -comments ${program_dir}'
			out_filename: 'main.text'
		)
		//
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
		eprintln('>>>           cmd: VDOC_SORT=${params.should_sort} ${params.cmd}')
		eprintln('>>> out_file_path: `${out_file_path}`')
		eprintln('>>>           fix: VDOC_SORT=${params.should_sort} ${params.cmd} > ${out_file_path}')
		fails++
	}
	if should_autofix {
		os.write_file(out_file_path, res.output) or {}
	}
	return fails
}
