module main

import os

// VAssertMetaInfo is used during assertions. An instance of it
// is filled in by compile time generated code, when an assertion fails.
pub struct VAssertMetaInfo {
pub:
	fpath   string // the source file path of the assertion
	line_nr int    // the line number of the assertion
	fn_name string // the function name in which the assertion is
	src     string // the actual source line of the assertion
	op      string // the operation of the assertion, i.e. '==', '<', 'call', etc ...
	llabel  string // the left side of the infix expressions as source
	rlabel  string // the right side of the infix expressions as source
	lvalue  string // the stringified *actual value* of the left side of a failed assertion
	rvalue  string // the stringified *actual value* of the right side of a failed assertion
}

const use_relative_paths = can_use_relative_paths()

fn can_use_relative_paths() bool {
	return match os.getenv('VERROR_PATHS') {
		'absolute' { false }
		else { true }
	}
}

fn myeprintln(s string) {
	println(s)
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
fn cb_assertion_failed(i VAssertMetaInfo) {
	filepath := if use_relative_paths { i.fpath } else { os.real_path(i.fpath) }
	mut final_filepath := filepath + ':${i.line_nr + 1}:'
	mut final_funcname := 'fn ' + i.fn_name
	final_src := 'assert ' + i.src

	myeprintln('${final_filepath} ${final_funcname}')

	if i.op.len > 0 && i.op != 'call' {
		mut lvtitle := '    Left value:'
		mut rvtitle := '    Right value:'
		mut slvalue := '${i.lvalue}'
		mut srvalue := '${i.rvalue}'
		cutoff_limit := 30
		if slvalue.len > cutoff_limit || srvalue.len > cutoff_limit {
			myeprintln('  > ${final_src}')
			myeprintln(lvtitle)
			myeprintln('      ${slvalue}')
			myeprintln(rvtitle)
			myeprintln('      ${srvalue}')
		} else {
			myeprintln('   > ${final_src}')
			myeprintln(' ${lvtitle} ${slvalue}')
			myeprintln('${rvtitle} ${srvalue}')
		}
	} else {
		myeprintln('    ${final_src}')
	}
	myeprintln('')
}

fn cb_assertion_ok(i &VAssertMetaInfo) {
}

fn cb_propagate_test_error(line_nr int, file string, mod string, fn_name string, errmsg string) {
	filepath := if use_relative_paths { file } else { os.real_path(file) }
	mut final_filepath := filepath + ':${line_nr}:'
	mut final_funcname := 'fn ' + fn_name.replace('main.', '').replace('__', '.')
	final_msg := errmsg
	myeprintln('${final_filepath} ${final_funcname} failed propagation with error: ${final_msg}')
	// TODO: implement os.is_file and os.read_lines:
	/*
	if os.is_file(file) {
		source_lines := os.read_lines(file) or { []string{len: line_nr + 1} }
		myeprintln('${line_nr:5} | ${source_lines[line_nr - 1]}')
	}
	*/
}
