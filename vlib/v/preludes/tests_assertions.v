module main

import os
import term

const use_color = term.can_show_color_on_stderr()

const use_relative_paths = can_use_relative_paths()

fn can_use_relative_paths() bool {
	return match os.getenv('VERROR_PATHS') {
		'absolute' { false }
		else { true }
	}
}

// //////////////////////////////////////////////////////////////////
// / This file will get compiled as part of the main program,
// / for a _test.v file.
// / The methods defined here are called back by the test program's
// / assert statements, on each success/fail. The goal is to make
// / customizing the look & feel of the assertions results easier,
// / since it is done in normal V code, instead of in embedded C ...
// //////////////////////////////////////////////////////////////////
// TODO copy pasta builtin.v fn ___print_assert_failure
fn cb_assertion_failed(i &VAssertMetaInfo) {
	filepath := if use_relative_paths { i.fpath } else { os.real_path(i.fpath) }
	mut final_filepath := filepath + ':${i.line_nr + 1}:'
	if use_color {
		final_filepath = term.gray(final_filepath)
	}
	mut final_funcname := 'fn ' + i.fn_name.replace('main.', '').replace('__', '.')
	if use_color {
		final_funcname = term.red('✗ ' + final_funcname)
	}
	final_src := if use_color { term.dim('assert ${term.bold(i.src)}') } else { 'assert ' + i.src }
	eprintln('$final_filepath $final_funcname')
	if i.op.len > 0 && i.op != 'call' {
		mut lvtitle := '    Left value:'
		mut rvtitle := '    Right value:'
		mut slvalue := '$i.lvalue'
		mut srvalue := '$i.rvalue'
		if use_color {
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
}

fn cb_assertion_ok(i &VAssertMetaInfo) {
	// prints for every assertion instead of per test function
	// TODO: needs to be changed
	/*
	use_color := term.can_show_color_on_stderr()
	use_relative_paths := match os.getenv('VERROR_PATHS') {
		'absolute' { false }
		else { true }
	}
	filepath := if use_relative_paths { i.fpath } else { os.real_path(i.fpath) }
	final_filepath := if use_color {
		term.gray(filepath + ':${i.line_nr+1}')
	} else {
		filepath + ':${i.line_nr+1}'
	}
	mut final_funcname := i.fn_name.replace('main.', '').replace('__', '.')
	if use_color {
		final_funcname = term.green('✓ ' + final_funcname)
	}
	println('$final_funcname ($final_filepath)')
	*/
}

fn cb_propagate_test_error(line_nr int, file string, mod string, fn_name string, errmsg string) {
	filepath := if use_relative_paths { file } else { os.real_path(file) }
	mut final_filepath := filepath + ':$line_nr:'
	if use_color {
		final_filepath = term.gray(final_filepath)
	}
	mut final_funcname := 'fn ' + fn_name.replace('main.', '').replace('__', '.')
	if use_color {
		final_funcname = term.red('✗ ' + final_funcname)
	}
	final_msg := if use_color { term.dim(errmsg) } else { errmsg }
	eprintln('$final_filepath $final_funcname failed propagation with error: $final_msg')
	if os.is_file(file) {
		source_lines := os.read_lines(file) or { []string{len: line_nr + 1} }
		eprintln('${line_nr:5} | ${source_lines[line_nr - 1]}')
	}
}
