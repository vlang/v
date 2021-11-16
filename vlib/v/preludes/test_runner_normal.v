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
	mut tr := &NormalTestRunner{}
	tr.use_color = term.can_show_color_on_stderr()
	tr.use_relative_paths = match os.getenv('VERROR_PATHS') {
		'absolute' { false }
		else { true }
	}
	return tr
}

fn (mut assertion NormalTestRunner) free() {
	unsafe {
		assertion.all_assertsions.free()
		assertion.fname.free()
		assertion.fn_test_info.free()
		assertion.file_test_info.free()
	}
}

fn normalise_fname(name string) string {
	return 'fn ' + name.replace('__', '.').replace('main.', '')
}

fn (mut assertion NormalTestRunner) start(ntests int) {
	assertion.all_assertsions = []&VAssertMetaInfo{cap: 1000}
}

fn (mut assertion NormalTestRunner) finish() {
}

fn (mut assertion NormalTestRunner) exit_code() int {
	if assertion.fn_fails > 0 {
		return 1
	}
	return 0
}

fn (mut assertion NormalTestRunner) fn_start() {
	assertion.fn_assert_passes = 0
	assertion.fname = normalise_fname(assertion.fn_test_info.name)
}

fn (mut assertion NormalTestRunner) fn_pass() {
	assertion.fn_passes++
}

fn (mut assertion NormalTestRunner) fn_fail() {
	assertion.fn_fails++
}

fn (mut assertion NormalTestRunner) fn_error(line_nr int, file string, mod string, fn_name string, errmsg string) {
	filepath := if assertion.use_relative_paths { file.clone() } else { os.real_path(file) }
	mut final_filepath := filepath + ':$line_nr:'
	if assertion.use_color {
		final_filepath = term.gray(final_filepath)
	}
	mut final_funcname := 'fn ' + fn_name.replace('main.', '').replace('__', '.')
	if assertion.use_color {
		final_funcname = term.red('✗ ' + final_funcname)
	}
	final_msg := if assertion.use_color { term.dim(errmsg) } else { errmsg.clone() }
	eprintln('$final_filepath $final_funcname failed propagation with error: $final_msg')
	if os.is_file(file) {
		source_lines := os.read_lines(file) or { []string{len: line_nr + 1} }
		eprintln('${line_nr:5} | ${source_lines[line_nr - 1]}')
	}
}

fn (mut assertion NormalTestRunner) assert_pass(i &VAssertMetaInfo) {
	assertion.total_assert_passes++
	assertion.fn_assert_passes++
	assertion.all_assertsions << i
}

fn (mut assertion NormalTestRunner) assert_fail(i &VAssertMetaInfo) {
	assertion.total_assert_fails++
	filepath := if assertion.use_relative_paths { i.fpath.clone() } else { os.real_path(i.fpath) }
	mut final_filepath := filepath + ':${i.line_nr + 1}:'
	if assertion.use_color {
		final_filepath = term.gray(final_filepath)
	}
	mut final_funcname := 'fn ' + i.fn_name.replace('main.', '').replace('__', '.')
	if assertion.use_color {
		final_funcname = term.red('✗ ' + final_funcname)
	}
	final_src := if assertion.use_color {
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
		if assertion.use_color {
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
	eprintln('')
	assertion.all_assertsions << i
}
