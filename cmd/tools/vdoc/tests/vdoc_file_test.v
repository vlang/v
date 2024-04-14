import os
import rand
import term
import v.util.vtest
import v.util.diff

const vexe_path = @VEXE
const vexe = os.quoted_path(vexe_path)
const vroot = os.dir(vexe_path)
const should_autofix = os.getenv('VAUTOFIX') != ''

fn test_output() {
	os.setenv('VCOLORS', 'never', true)
	os.chdir(vroot)!
	mut total_fails := 0
	test_files := vtest.filter_vtest_only(os.walk_ext('cmd/tools/vdoc/tests/testdata',
		'.v'), basepath: vroot)
	for path in test_files {
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
		program_dir := os.quoted_path(if os.is_dir(path) { path } else { os.dir(path) })
		for fmt in ['html', 'ansi', 'text'] {
			fails += check_output('${vexe} doc -f ${fmt} -o - -html-only-contents -readme -comments ${program_dir}',
				'${path_no_ext}.${fmt}')
		}
		if fails == 0 {
			println(term.green('OK'))
		} else {
			total_fails += fails
		}
		flush_stdout()
	}
	assert total_fails == 0
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
	// Work around CI issues covering v doc generation for relative input paths in tmp dir.
	// Instead just generate documentation in the v source dir.
	if os.getenv('CI') == 'true' {
		mod := 'coroutines'
		// Default output path.
		os.execute_opt('${vexe} doc -f html -m vlib/${mod}')!
		assert os.exists(os.join_path(vroot, 'vlib', 'coroutines', '_docs', '${mod}.html'))

		// Custom out path (no `_docs` subdir).
		out_dir := os.join_path(vroot, 'vlib', 'coroutines', 'docs')
		os.execute_opt('${vexe} doc -f html -m -o ${out_dir} ${mod}')!
		assert os.exists(os.join_path(out_dir, '${mod}.html'))

		return
	}

	test_path := os.join_path(os.vtmp_dir(), 'vdoc_test_${rand.ulid()}')
	os.mkdir_all(test_path)!
	os.chdir(test_path)!
	defer {
		os.rmdir_all(test_path) or {}
	}

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

fn print_compare(expected string, found string) {
	diff_cmd := diff.find_working_diff_command() or { return }
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

@[params]
struct CheckOutputParams {
	should_sort bool = true
}

fn check_output(cmd string, out_path string, opts CheckOutputParams) int {
	if !os.exists(out_path) {
		return 0
	}
	mut fails := 0
	os.setenv('VDOC_SORT', opts.should_sort.str(), true)
	expected := os.read_file(out_path) or { panic(err) }.replace('\r\n', '\n').trim_space()
	res := os.execute_opt(cmd) or { panic(err) }
	found := res.output.replace('\r\n', '\n').trim_space()
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
