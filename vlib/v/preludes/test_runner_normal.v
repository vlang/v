module main

import os
import term

///////////////////////////////////////////////////////////
// This file gets compiled as part of the main program, for
// each _test.v file. It implements the default/normal test
// output for `v run file_test.v`
// See also test_runner.v .
///////////////////////////////////////////////////////////

fn vtest_init() {
	change_test_runner(&TestRunner(new_normal_test_runner()))
}

struct NormalTestRunner {
pub mut:
	fname              string
	use_color          bool
	use_relative_paths bool
	all_assertsions    []&VAssertMetaInfo
	//
mut:
	file_test_info   VTestFileMetaInfo
	fn_test_info     VTestFnMetaInfo
	fn_assert_passes u64
	fn_passes        u64
	fn_fails         u64
	//
	total_assert_passes u64
	total_assert_fails  u64
}

fn new_normal_test_runner() &TestRunner {
	unsafe {
		mut tr := &NormalTestRunner{}
		tr.use_color = term.can_show_color_on_stderr()
		tr.use_relative_paths = match os.getenv('VERROR_PATHS') {
			'absolute' { false }
			else { true }
		}
		return tr
	}
}

fn (mut runner NormalTestRunner) free() {
	unsafe {
		runner.all_assertsions.free()
		runner.fname.free()
		runner.fn_test_info.free()
		runner.file_test_info.free()
	}
}

fn normalise_fname(name string) string {
	return 'fn ' + name.replace('__', '.').replace('main.', '')
}

fn (mut runner NormalTestRunner) start(ntests int) {
	unsafe {
		runner.all_assertsions = []&VAssertMetaInfo{cap: 1000}
	}
}

fn (mut runner NormalTestRunner) finish() {
}

fn (mut runner NormalTestRunner) exit_code() int {
	if runner.fn_fails > 0 {
		return 1
	}
	if runner.total_assert_fails > 0 {
		return 2
	}
	return 0
}

fn (mut runner NormalTestRunner) fn_start() bool {
	runner.fn_assert_passes = 0
	runner.fname = normalise_fname(runner.fn_test_info.name)
	return true
}

fn (mut runner NormalTestRunner) fn_pass() {
	runner.fn_passes++
}

fn (mut runner NormalTestRunner) fn_fail() {
	runner.fn_fails++
}

fn (mut runner NormalTestRunner) fn_error(line_nr int, file string, mod string, fn_name string, errmsg string) {
	filepath := if runner.use_relative_paths { file.clone() } else { os.real_path(file) }
	mut final_filepath := filepath + ':$line_nr:'
	if runner.use_color {
		final_filepath = term.gray(final_filepath)
	}
	mut final_funcname := 'fn ' + fn_name.replace('main.', '').replace('__', '.')
	if runner.use_color {
		final_funcname = term.red('✗ ' + final_funcname)
	}
	final_msg := if runner.use_color { term.dim(errmsg) } else { errmsg.clone() }
	eprintln('$final_filepath $final_funcname failed propagation with error: $final_msg')
	if os.is_file(file) {
		source_lines := os.read_lines(file) or { []string{len: line_nr + 1} }
		eprintln('${line_nr:5} | ${source_lines[line_nr - 1]}')
	}
}

fn (mut runner NormalTestRunner) assert_pass(i &VAssertMetaInfo) {
	runner.total_assert_passes++
	runner.fn_assert_passes++
	runner.all_assertsions << i
}

fn (mut runner NormalTestRunner) assert_fail(i &VAssertMetaInfo) {
	runner.total_assert_fails++
	filepath := if runner.use_relative_paths { i.fpath.clone() } else { os.real_path(i.fpath) }
	mut final_filepath := filepath + ':${i.line_nr + 1}:'
	if runner.use_color {
		final_filepath = term.gray(final_filepath)
	}
	mut final_funcname := 'fn ' + i.fn_name.replace('main.', '').replace('__', '.')
	if runner.use_color {
		final_funcname = term.red('✗ ' + final_funcname)
	}
	final_src := if runner.use_color {
		term.dim('assert ${term.bold(i.src)}')
	} else {
		'assert ' + i.src
	}
	eprintln('$final_filepath $final_funcname')
	if i.op.len > 0 && i.op != 'call' {
		mut lvtitle := '    Left value:'
		mut rvtitle := '    Right value:'
		mut slvalue := '$i.lvalue'
		mut srvalue := '$i.rvalue'
		if runner.use_color {
			slvalue = term.yellow(slvalue)
			srvalue = term.yellow(srvalue)
			lvtitle = term.gray(lvtitle)
			rvtitle = term.gray(rvtitle)
		}
		cutoff_limit := 30
		if slvalue.len > cutoff_limit || srvalue.len > cutoff_limit {
			eprintln('  > $final_src')
			eprintln(lvtitle)
			eprintln('      $slvalue')
			eprintln(rvtitle)
			eprintln('      $srvalue')
		} else {
			eprintln('   > $final_src')
			eprintln(' $lvtitle $slvalue')
			eprintln('$rvtitle $srvalue')
		}
	} else {
		eprintln('    $final_src')
	}
	if i.has_msg {
		mut mtitle := '        Message:'
		mut mvalue := '$i.message'
		if runner.use_color {
			mvalue = term.yellow(mvalue)
			mtitle = term.gray(mtitle)
		}
		eprintln('$mtitle $mvalue')
	}
	eprintln('')
	runner.all_assertsions << i
}
