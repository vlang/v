module main

import os
// //////////////////////////////////////////////////////////////////
// / This file will get compiled as part of the main program,
// / for a _test.v file.
// / The methods defined here are called back by the test program's
// / assert statements, on each success/fail. The goal is to make
// / customizing the look & feel of the assertions results easier,
// / since it is done in normal V code, instead of in embedded C ...
// //////////////////////////////////////////////////////////////////
fn cb_assertion_failed(filename string, line int, sourceline string, funcname string) {
	// color_on := term.can_show_color_on_stderr()
	use_relative_paths := match os.getenv('VERROR_PATHS') {
		'absolute'{
			false
		}
		else {
			true}
	}
	final_filename := if use_relative_paths { filename } else { os.real_path(filename) }
	final_funcname := funcname.replace('main__', '').replace('__', '.')
	eprintln('$final_filename:$line: failed assert in ${final_funcname}')
	eprintln('Source  : $sourceline')
}

fn cb_assertion_ok(filename string, line int, sourceline string, funcname string) {
	// do nothing for now on an OK assertion
	// println('OK ${line:5d}|$sourceline ')
	}
