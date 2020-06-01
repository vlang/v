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
fn cb_assertion_failed(i &VAssertMetaInfo) {
	// color_on := term.can_show_color_on_stderr()
	use_relative_paths := match os.getenv('VERROR_PATHS') {
		'absolute' {
			false
		} else {
			true
		}
	}
	final_filename := if use_relative_paths { i.fpath } else { os.real_path(i.fpath) }
	final_funcname := i.fn_name.replace('main__', '').replace('__', '.')
	eprintln('$final_filename:${i.line_nr+1}: failed assert in ${final_funcname}')
	eprintln('Source  : ${i.src}')
	if i.op != 'call' {
		eprintln('   left value: ${i.llabel} = ${i.lvalue}')
		eprintln('  right value: ${i.rlabel} = ${i.rvalue}')
	}
}

fn cb_assertion_ok(i &VAssertMetaInfo) {
	// do nothing for now on an OK assertion
	// println('OK ${(i.line_nr+1):5d}|${i.src}')
}
