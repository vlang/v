module main

import time
import term

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

	assertion_info      VAssertMetaInfo
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

	msg := "Start '${runner.fname}' test"
	println(term.gray(msg))

	runner.print_service("
		|##teamcity[
		|testStarted name='${runner.fname}'
		|locationHint='v_qn://${runner.file_test_info.file}:${runner.fname}'
		|]".strip_margin())
	return true
}

fn (mut runner TeamcityTestRunner) fn_pass() {
	runner.fn_passes++
	duration := runner.test_duration()
	eprintln("##teamcity[testFinished name='${runner.fname}' duration='${duration}']")
	end_msg := "Finish '${runner.fname}' test"
	println(term.gray(end_msg))
	msg := "Test '${runner.fname}' passed"
	println(term.green(msg))
	println('\n')
}

fn (mut runner TeamcityTestRunner) fn_fail() {
	runner.fn_fails++
	duration := runner.test_duration()
	assertion := runner.assertion_info

	mut actual := runner.prepare_value(assertion.lvalue)
	mut expected := runner.prepare_value(assertion.rvalue)

	message := if assertion.has_msg {
		'Assertion "${assertion.message}" failed'
	} else {
		op := if assertion.op == '' { '' } else { assertion.op }

		if op == 'in' {
			parts := assertion.src.split('in')
			if parts.len == 2 {
				left := parts[0].trim(' ')
				right := parts[1].trim(' ')
				'Assertion that "${left}" in "${right}" failed'
			} else {
				'Assertion "${assertion.src}" failed'
			}
		} else if op == 'is' {
			'Assertion that left and right type are equal failed'
		} else if op == 'call' {
			actual = 'false'
			expected = 'true'
			'Assertion that function call "${assertion.src}" returns true failed'
		} else {
			lines := assertion.src.split_into_lines()
			if lines.len == 1 {
				'Assertion "${lines.first()}" failed'
			} else {
				'Assertion failed'
			}
		}
	}

	details := 'See failed assertion: ${assertion.fpath}:${assertion.line_nr + 1}'

	runner.print_service("
		 |##teamcity[
		 |testFailed name='${runner.fname}'
		 |duration='${duration}'
		 |details='${details}'
		 |type='comparisonFailure'
		 |actual='${actual}'
		 |expected='${expected}'
		 |message='${message}'
		 |]".strip_margin())
	println('\n')
}

// prepare_value escapes the value for Teamcity output.
// For example, it replaces `\n` with `|n`, otherwise Teamcity
// will not correctly parse the output.
fn (mut _ TeamcityTestRunner) prepare_value(raw string) string {
	return raw
		.replace('\n', '|n')
		.replace('\r', '|r')
		.replace('[', '|[')
		.replace(']', '|]')
		.replace("'", "|'")
}

fn (mut runner TeamcityTestRunner) fn_error(line_nr int, file string, mod string, fn_name string, errmsg string) {
	eprintln('>>> TeamcityTestRunner fn_error ${runner.fname}, line_nr: ${line_nr}, file: ${file}, mod: ${mod}, fn_name: ${fn_name}, errmsg: ${errmsg}')
}

fn (mut runner TeamcityTestRunner) test_duration() i64 {
	return time.now().unix_time_milli() - runner.start_time.unix_time_milli()
}

// print_service prepare and prints a Teamcity service message.
fn (mut runner TeamcityTestRunner) print_service(msg string) {
	without_new_lines := msg
		.trim('\n\r ')
		.replace('\r', '')
		.replace('\n', ' ')
	eprintln(without_new_lines)
}

//

fn (mut runner TeamcityTestRunner) assert_pass(i &VAssertMetaInfo) {
	runner.total_assert_passes++
	runner.fn_assert_passes++

	filepath := i.fpath.clone()
	msg := '>>> assertion passed ${filepath}:${i.line_nr + 1}'
	println(term.green(msg))

	unsafe { i.free() }
}

fn (mut runner TeamcityTestRunner) assert_fail(i &VAssertMetaInfo) {
	runner.total_assert_fails++
	runner.assertion_info = *i
}
