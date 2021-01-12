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
		filedir := os.dir(filepath)
		filename := os.base(filepath)
		term.gray(filedir + os.path_separator) + filename
	} else {
		filepath
	}
	final_funcname := i.fn_name.replace('main.', '').replace('__', '.')
	final_src := if use_color { term.gray('assert ${term.bold(i.src)}') } else { 'assert ' + i.src }
	eprintln('')
	eprintln('$final_filepath:${i.line_nr+1}: ${term.red(final_funcname)}')
	eprintln('')
	eprintln('    $final_src')
	eprintln('')
	if i.op.len > 0 && i.op != 'call' {
		mut slvalue := '$i.lvalue'
		mut srvalue := '$i.rvalue'
		// lpostfix := if slvalue == i.llabel { '.' } else { '<= `$i.llabel`' }
		// rpostfix := if srvalue == i.rlabel { '.' } else { '<= `$i.rlabel`' }
		if use_color {
			slvalue = term.yellow(slvalue)
			srvalue = term.yellow(srvalue)
		}
		eprintln(term.dim('    Left value:'))
		eprintln('      $slvalue')
		eprintln(term.dim('    Right value:'))
		eprintln('      $slvalue')
	}
}

fn cb_assertion_ok(i &VAssertMetaInfo) {
	// do nothing for now on an OK assertion
	// println('OK ${(i.line_nr+1):5d}|${i.src}')
}
