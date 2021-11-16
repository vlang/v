module main

// Provide a no-frills implementation of the TestRunner interface:

fn vtest_init() {
	change_test_runner(&TestRunner(SimpleTestRunner{}))
}

struct SimpleTestRunner {
mut:
	fname string
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

fn (mut a SimpleTestRunner) free() {
	unsafe {
		a.fname.free()
		a.fn_test_info.free()
		a.file_test_info.free()
	}
}

fn normalise_fname(name string) string {
	return 'fn ' + name.replace('__', '.').replace('main.', '')
}

fn (mut a SimpleTestRunner) start(ntests int) {
	eprintln('SimpleTestRunner testing start; expected: $ntests test functions')
}

fn (mut a SimpleTestRunner) finish() {
	eprintln('SimpleTestRunner testing finish; fn:[passes: $a.fn_passes, fails: $a.fn_fails], assert:[passes: $a.total_assert_passes, fails: $a.total_assert_fails]')
}

fn (mut a SimpleTestRunner) exit_code() int {
	if a.fn_fails > 0 {
		return 1
	}
	return 0
}

//

fn (mut a SimpleTestRunner) fn_start() {
	a.fn_assert_passes = 0
	a.fname = normalise_fname(a.fn_test_info.name)
	// eprintln('>>> SimpleTestRunner fn_start $a.fname')
}

fn (mut a SimpleTestRunner) fn_pass() {
	a.fn_passes++
	// eprintln('ok $a.fname')
}

fn (mut a SimpleTestRunner) fn_fail() {
	a.fn_fails++
	eprintln('>>> fail $a.fname')
}

fn (mut a SimpleTestRunner) fn_error(line_nr int, file string, mod string, fn_name string, errmsg string) {
	eprintln('>>> SimpleTestRunner fn_error $a.fname, line_nr: $line_nr, file: $file, mod: $mod, fn_name: $fn_name, errmsg: $errmsg')
}

//

fn (mut a SimpleTestRunner) assert_pass(i &VAssertMetaInfo) {
	a.total_assert_passes++
	a.fn_assert_passes++
	// eprintln('passed assert $a.fn_assert_passes in $a.fname, line: ${i.line_nr + 1}')
	unsafe { i.free() }
}

fn (mut a SimpleTestRunner) assert_fail(i &VAssertMetaInfo) {
	a.total_assert_fails++
	eprintln('> failed assert ${a.fn_assert_passes + 1} in $a.fname, assert was in ${normalise_fname(i.fn_name)}, line: ${
		i.line_nr + 1}')
	unsafe { i.free() }
}
