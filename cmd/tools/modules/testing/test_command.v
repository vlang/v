module testing

import os
import time

struct TestCommand {
mut:
	vexe    string
	options string
	file    string
	run_js  bool
	//
	compile_cmd string
	executable  string
	//
	already_compiled bool
	compile_result   os.Result
	//
	exit_code int
	output    string
	//
	total_duration   time.Duration
	compile_duration time.Duration
	run_duration     time.Duration
	ts               &TestSession = unsafe { nil }
	mtc              &MessageThreadContext = unsafe { nil }
}

fn TestCommand.new(vexe string, options []string, file string, generated_binary_fpath string, run_js bool, ts &TestSession, mtc &MessageThreadContext) TestCommand {
	noptions := options.filter(it != '').join(' ')
	compile_cmd := '${os.quoted_path(vexe)} ${noptions} ${os.quoted_path(file)}'
	executable := if run_js {
		compile_cmd
	} else {
		generated_binary_fpath
	}
	// eprintln('>>>> vexe: ${vexe} | noptions: `${noptions}` | file: ${file} | generated_binary_fpath: ${generated_binary_fpath} | run_js: ${run_js} | compile_cmd: ${compile_cmd}')
	return TestCommand{
		vexe: vexe
		options: noptions
		file: file
		compile_cmd: compile_cmd
		executable: executable
		run_js: run_js
		ts: ts
		mtc: mtc
	}
}

fn (tc TestCommand) str() string {
	return tc.compile_cmd
}

fn (mut tc TestCommand) compile() {
	if tc.already_compiled {
		return
	}
	tc.ts.append_message(.compilation_begin, '', tc.mtc)
	sw := time.new_stopwatch()
	cres := os.execute(tc.compile_cmd)
	tc.compile_duration = sw.elapsed()
	tc.total_duration = tc.compile_duration
	tc.already_compiled = true
	tc.exit_code = cres.exit_code
	tc.output = cres.output
	tc.compile_result = cres
	tc.ts.append_message_with_duration(.compilation_end, '', tc.compile_duration, tc.mtc)
}

//

fn (mut tc TestCommand) execute() os.Result {
	if !tc.run_js {
		tc.compile()
		if tc.compile_result.exit_code != 0 {
			return tc.compile_result
		}
	}
	tc.ts.append_message(.run_begin, '', tc.mtc)
	sw := time.new_stopwatch()
	rres := os.execute(tc.executable)
	tc.run_duration = sw.elapsed()
	tc.total_duration += tc.run_duration
	tc.ts.append_message_with_duration(.run_end, '', tc.run_duration, tc.mtc)
	return os.Result{
		...rres
		output: '${tc.compile_result.output}\n${rres.output}'
	}
}

fn (mut tc TestCommand) system() int {
	if !tc.run_js {
		tc.compile()
		if tc.compile_result.exit_code != 0 {
			return tc.compile_result.exit_code
		}
	}
	sw := time.new_stopwatch()
	res := os.system(tc.executable)
	tc.run_duration = sw.elapsed()
	tc.total_duration += tc.run_duration
	return res
}
