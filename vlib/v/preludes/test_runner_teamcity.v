module main

import time

// Provide a Teamcity implementation of the TestRunner interface.
// Used in Teamcity and JetBrains IDEs for nice test reporting.

fn vtest_init() {
	change_test_runner(&TestRunner(TeamcityTestRunner{}))
}

struct TeamcityTestRunner {
mut:
	fname      string
	start_time time.Time

	file_test_info   VTestFileMetaInfo
	fn_test_info     VTestFnMetaInfo
	fn_assert_passes u64
	fn_passes        u64
	fn_fails         u64

	total_assert_passes u64
	total_assert_fails  u64
}

fn (mut runner TeamcityTestRunner) free() {
	unsafe {
		runner.fname.free()
		runner.fn_test_info.free()
		runner.file_test_info.free()
	}
}

fn normalise_fname(name string) string {
	return name.replace('__', '.').replace('main.', '')
}

fn (mut runner TeamcityTestRunner) start(ntests int) {
}

fn (mut runner TeamcityTestRunner) finish() {
}

fn (mut runner TeamcityTestRunner) exit_code() int {
	if runner.fn_fails > 0 {
		return 1
	}
	if runner.total_assert_fails > 0 {
		return 1
	}
	return 0
}

//

fn (mut runner TeamcityTestRunner) fn_start() bool {
	runner.fn_assert_passes = 0
	runner.start_time = time.now()
	runner.fname = normalise_fname(runner.fn_test_info.name)
	println("Start '${runner.fname}'")
	eprintln("##teamcity[testStarted name='${runner.fname}' locationHint='v_qn://${runner.file_test_info.file}:${runner.fname}']")
	return true
}

fn (mut runner TeamcityTestRunner) fn_pass() {
	runner.fn_passes++
	duration := runner.test_duration()
	eprintln("##teamcity[testFinished name='${runner.fname}' duration='${duration}']")
	println('\n')
}

fn (mut runner TeamcityTestRunner) fn_fail() {
	runner.fn_fails++
	duration := runner.test_duration()
	eprintln("##teamcity[testFailed name='${runner.fname}' duration='${duration}' message='assertion failed']")
	println('\n')
}

fn (mut runner TeamcityTestRunner) fn_error(line_nr int, file string, mod string, fn_name string, errmsg string) {
	eprintln('>>> TeamcityTestRunner fn_error ${runner.fname}, line_nr: ${line_nr}, file: ${file}, mod: ${mod}, fn_name: ${fn_name}, errmsg: ${errmsg}')
}

fn (mut runner TeamcityTestRunner) test_duration() i64 {
	return time.now().unix_time_milli() - runner.start_time.unix_time_milli()
}

//

fn (mut runner TeamcityTestRunner) assert_pass(i &VAssertMetaInfo) {
	runner.total_assert_passes++
	runner.fn_assert_passes++

	filepath := i.fpath.clone()
	println('>>> assert_pass ${filepath}:${i.line_nr + 1}')

	unsafe { i.free() }
}

fn (mut runner TeamcityTestRunner) assert_fail(i &VAssertMetaInfo) {
	runner.total_assert_fails++

	filepath := i.fpath.clone()
	mut final_filepath := filepath + ':${i.line_nr + 1}:'
	mut final_funcname := 'fn ' + i.fn_name.replace('main.', '').replace('__', '.')
	final_src := 'assert ' + i.src
	eprintln('${final_filepath} ${final_funcname}')
	if i.op.len > 0 && i.op != 'call' {
		mut lvtitle := '    Left value:'
		mut rvtitle := '    Right value:'
		mut slvalue := '${i.lvalue}'
		mut srvalue := '${i.rvalue}'
		cutoff_limit := 30
		if slvalue.len > cutoff_limit || srvalue.len > cutoff_limit {
			eprintln('  > ${final_src}')
			eprintln(lvtitle)
			eprintln('      ${slvalue}')
			eprintln(rvtitle)
			eprintln('      ${srvalue}')
		} else {
			eprintln('   > ${final_src}')
			eprintln(' ${lvtitle} ${slvalue}')
			eprintln('${rvtitle} ${srvalue}')
		}
	} else {
		eprintln('    ${final_src}')
	}
	if i.has_msg {
		mut mtitle := '        Message:'
		mut mvalue := '${i.message}'
		eprintln('${mtitle} ${mvalue}')
	}
	eprintln('')

	unsafe { i.free() }
}
