module main

import os

////////////////////////////////////////////////////////////////////
/// This file will get compiled as part of the main program,
/// for a _test.v file.
/// The methods defined here are called back by the test program's
/// assert statements, on each success/fail. The goal is to make
/// customizing the look & feel of the assertions results easier,
/// since it is done in normal V code, instead of in embedded C ...
////////////////////////////////////////////////////////////////////

fn cb_assertion_failed(filename string, line int, sourceline string, funcname string){
	use_relative_paths := match os.getenv('VERROR_PATHS') {
		'absolute' { false }
		else { true }
	}
	final_filename := if use_relative_paths { filename } else { os.realpath( filename ) }
	final_funcname := funcname.replace('main__','')
	
	println('$final_filename:$line: assertion failure')
	println('Func: $final_funcname')
	println('Line: $sourceline')
}

fn cb_assertion_ok(filename string, line int, sourceline string, funcname string){
	//do nothing for now on an OK assertion
}
