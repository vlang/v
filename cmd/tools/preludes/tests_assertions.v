module main

import os
import term

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
	use_color := term.can_show_color_on_stderr()
	use_relative_paths := match os.getenv('VERROR_PATHS') {
		'absolute' { false }
		else { true }
	}
	filepath := if use_relative_paths { i.fpath } else { os.real_path(i.fpath) }
	final_filepath := if use_color {
		term.gray(filepath + ':${i.line_nr+1}:')
	} else {
		filepath + ':${i.line_nr+1}:'
	}
	mut final_funcname := 'fn ' + i.fn_name.replace('main.', '').replace('__', '.')
	if use_color {
		final_funcname = term.red('✗ ' + final_funcname)
	}
	final_src := if use_color { term.dim('assert ${term.bold(i.src)}') } else { 'assert ' + i.src }
	eprintln('$final_filepath $final_funcname')
	eprintln('    $final_src')
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
		eprintln(lvtitle)
		eprintln('      $slvalue')
		eprintln(rvtitle)
		eprintln('      $srvalue')
	}
    eprintln('')
}

fn cb_assertion_ok(i &VAssertMetaInfo) {
	// prints for every assertion instead of per test function
	// TODO: needs to be changed
	/*use_color := term.can_show_color_on_stderr()
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
	println('$final_funcname ($final_filepath)')*/
}
