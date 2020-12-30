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
	final_filename := if use_relative_paths { i.fpath } else { os.real_path(i.fpath) }
	final_funcname := i.fn_name.replace('main.', '').replace('__', '.')
	final_src := if use_color { term.bold(i.src) } else { i.src }
	eprintln('')
	eprintln('$final_filename:${i.line_nr+1}: failed assert in function $final_funcname')
	eprintln('Source  : `$final_src`')
	if i.op.len > 0 && i.op != 'call' {
		mut slvalue := '$i.lvalue'
		mut srvalue := '$i.rvalue'
		// lpostfix := if slvalue == i.llabel { '.' } else { '<= `$i.llabel`' }
		// rpostfix := if srvalue == i.rlabel { '.' } else { '<= `$i.rlabel`' }
		if use_color {
			slvalue = term.bold(term.yellow(slvalue))
			srvalue = term.bold(term.yellow(srvalue))
		}
		eprintln('	 left value: $slvalue')
		eprintln('	right value: $srvalue')
	}
}

fn cb_assertion_ok(i &VAssertMetaInfo) {
	// do nothing for now on an OK assertion
	// println('OK ${(i.line_nr+1):5d}|${i.src}')
}
