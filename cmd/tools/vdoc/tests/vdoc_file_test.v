import os
import rand
import term
import v.util.vtest
import v.util.diff

const vexe_path = @VEXE
const vexe = os.quoted_path(vexe_path)
const vroot = os.dir(vexe_path)
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

fn test_out_path() {
	test_path := os.join_path(os.vtmp_dir(), 'vdoc_test_${rand.ulid()}')
	os.mkdir_all(test_path) or {}
	defer {
		os.rmdir_all(test_path) or {}
	}
	os.chdir(test_path)!

	// Copy a *small* vlib module for the test.
	mod := 'coroutines'
	os.cp_all(os.join_path(vroot, 'vlib', mod), os.join_path(test_path, mod), true) or {}

	// Relative input with default output path.
	os.execute_opt('${vexe} doc -f html -m ${mod}')!
	assert os.exists(os.join_path(test_path, mod, '_docs', '${mod}.html'))

	// Custom out path (no `_docs` subdir).
	out_dir := os.join_path(os.vtmp_dir(), 'docs_test')
	os.execute_opt('${vexe} doc -f html -m -o ${out_dir} ${mod}')!
	assert os.exists(os.join_path(out_dir, '${mod}.html'))
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
		path_no_ext := path.all_before_last('.')
		print(path + ' ')
		fails += check_output('${vexe} doc ${qpath}', path_no_ext + '.out')
		fails += check_output('${vexe} doc -comments ${qpath}', '${path_no_ext}.unsorted.out',
			should_sort: false
		)
		fails += check_output('${vexe} doc -comments ${qpath}', '${path_no_ext}.comments.out')
		fails += check_output('${vexe} doc -readme -comments ${qpath}', '${path_no_ext}.readme.comments.out')
		// test the main 3 different formats:
		program_dir := os.quoted_path(if os.is_file(path) { os.dir(path) } else { path })
		fails += check_output('${vexe} doc -f html -o - -html-only-contents -readme -comments ${program_dir}',
			'${path_no_ext}.html')
		fails += check_output('${vexe} doc -f ansi -o - -html-only-contents -readme -comments ${program_dir}',
			'${path_no_ext}.ansi')
		fails += check_output('${vexe} doc -f text -o - -html-only-contents -readme -comments ${program_dir}',
			'${path_no_ext}.text')
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
	should_sort bool = true
}

fn check_output(cmd string, out_path string, opts CheckOutputParams) int {
	if !os.exists(out_path) {
		return 0
	}
	mut fails := 0
	mut expected := os.read_file(out_path) or { panic(err) }
	expected = clean_line_endings(expected)

	os.setenv('VDOC_SORT', opts.should_sort.str(), true)
	res := os.execute(cmd)

	if res.exit_code < 0 {
		panic(res.output)
	}
	found := clean_line_endings(res.output)
	if expected != found {
		print_compare(expected, found)
		eprintln('>>>           cmd: VDOC_SORT=${opts.should_sort} ${cmd}')
		eprintln('>>> out_file_path: `${out_path}`')
		eprintln('>>>           fix: VDOC_SORT=${opts.should_sort} ${cmd} > ${out_path}')
		fails++
	}
	if should_autofix {
		os.write_file(out_path, res.output) or {}
	}
	return fails
}
