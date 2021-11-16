module main

// TAP, the Test Anything Protocol, is a simple text-based interface
// between testing modules in a test harness.
// TAP started life as part of the test harness for Perl but now has
// implementations in C, C++, Python, PHP, Perl, Java, JavaScript,
// Go, Rust, and others.
// Consumers and producers do not have to be written in the same
// language to interoperate. It decouples the reporting of errors
// from the presentation of the reports.
// For more details: https://testanything.org/

// This file implements a TAP producer for V tests.
// You can use it with:
// `VTEST_RUNNER=tap v run file_test.v`
// or
// `v -test-runner tap run file_test.v`

fn vtest_init() {
	change_test_runner(&TestRunner(TAPTestRunner{}))
}

struct TAPTestRunner {
mut:
	fname        string
	plan_tests   int
	test_counter int
	//
	file_test_info   VTestFileMetaInfo
	fn_test_info     VTestFnMetaInfo
	fn_assert_passes u64
	fn_passes        u64
	fn_fails         u64
	//
	total_assert_passes u64
	total_assert_fails  u64
}

fn (mut a TAPTestRunner) free() {
	unsafe {
		a.fname.free()
		a.fn_test_info.free()
		a.file_test_info.free()
	}
}

fn normalise_fname(name string) string {
	return 'fn ' + name.replace('__', '.').replace('main.', '')
}

fn (mut a TAPTestRunner) start(ntests int) {
	a.plan_tests = ntests
	println('1..$ntests')
	flush_stdout()
}

fn (mut a TAPTestRunner) finish() {
	println('# $a.plan_tests tests, ${a.total_assert_fails + a.total_assert_passes} assertions, $a.total_assert_fails failures')
}

fn (mut a TAPTestRunner) exit_code() int {
	if a.fn_fails > 0 {
		return 1
	}
	return 0
}

//

fn (mut a TAPTestRunner) fn_start() {
	a.fn_assert_passes = 0
	a.test_counter++
	a.fname = normalise_fname(a.fn_test_info.name)
	// eprintln('>>> TAPTestRunner fn_start $a.fname')
}

fn (mut a TAPTestRunner) fn_pass() {
	a.fn_passes++
	println('ok $a.test_counter - $a.fname')
	flush_stdout()
}

fn (mut a TAPTestRunner) fn_fail() {
	println('not ok $a.test_counter - $a.fname')
	flush_stdout()
	a.fn_fails++
}

fn (mut a TAPTestRunner) fn_error(line_nr int, file string, mod string, fn_name string, errmsg string) {
	println('# test function propagated error: $a.fname, line_nr: $line_nr, file: $file, mod: $mod, fn_name: $fn_name, errmsg: $errmsg')
	flush_stdout()
}

//

fn (mut a TAPTestRunner) assert_pass(i &VAssertMetaInfo) {
	a.total_assert_passes++
	a.fn_assert_passes++
	// eprintln('passed assert $a.fn_assert_passes in $a.fname, line: ${i.line_nr + 1}')
	unsafe { i.free() }
}

fn (mut a TAPTestRunner) assert_fail(i &VAssertMetaInfo) {
	a.total_assert_fails++
	println('# failed assert: ${a.fn_assert_passes + 1} in $a.fname, assert was in ${normalise_fname(i.fn_name)}, line: ${
		i.line_nr + 1}')
	flush_stdout()
	unsafe { i.free() }
}
