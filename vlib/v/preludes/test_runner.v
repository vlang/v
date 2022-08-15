[has_globals]
module main

__global test_runner TestRunner

///////////////////////////////////////////////////////////////////////////////
// This file will be compiled as part of the main program, for a _test.v file.
// The methods defined here are called back by the test program's assert
// statements, on each success/fail. The goal is to make customizing the look &
// feel of the assertions results easier, since it is done in normal V code.
///////////////////////////////////////////////////////////////////////////////

interface TestRunner {
mut:
	file_test_info VTestFileMetaInfo // filled in by generated code, before .start() is called.
	fn_test_info VTestFnMetaInfo // filled in by generated code, before .fn_start() is called.
	fn_assert_passes u64 // reset this to 0 in .fn_start(), increase it in .assert_pass()
	fn_passes u64 // increase this in .fn_pass()
	fn_fails u64 // increase this in .fn_fails()
	total_assert_passes u64 // increase this in .assert_pass()
	total_assert_fails u64 // increase this in .assert_fail()
	start(ntests int) // called before all tests, you can initialise private data here. ntests is the number of test functions in the _test.v file.
	finish() // called after all tests are finished, you can print some stats if you want here.
	exit_code() int // called right after finish(), it should return the exit code, that the test program will exit with.
	//
	fn_start() bool // called before the start of each test_ function. Return false, if the function should be skipped.
	fn_pass() // called after the end of each test_ function, with NO failed assertion.
	fn_fail() // called after the end of each test_ function, with a failed assertion, *or* returning an error.
	fn_error(line_nr int, file string, mod string, fn_name string, errmsg string) // called only for `fn test_xyz() ? { return error('message') }`, before .fn_fail() is called.
	//
	assert_pass(i &VAssertMetaInfo) // called after each `assert true`.
	assert_fail(i &VAssertMetaInfo) // called after each `assert false`.
	//
	free() // you should free all the private data of your runner here.
}

//

struct VTestFileMetaInfo {
	file  string
	tests int
}

// vtest_new_filemetainfo will be called right before .start(ntests),
// to fill in the .file_test_info field of the runner interface.
fn vtest_new_filemetainfo(file string, tests int) VTestFileMetaInfo {
	return VTestFileMetaInfo{
		file: file
		tests: tests
	}
}

[unsafe]
fn (i &VTestFileMetaInfo) free() {
	unsafe {
		i.file.free()
	}
}

//

struct VTestFnMetaInfo {
	name    string
	mod     string
	file    string
	line_nr int
}

// vtest_new_metainfo will be called once per each test function.
fn vtest_new_metainfo(name string, mod string, file string, line_nr int) VTestFnMetaInfo {
	return VTestFnMetaInfo{
		name: name
		mod: mod
		file: file
		line_nr: line_nr
	}
}

[unsafe]
fn (i &VTestFnMetaInfo) free() {
	unsafe {
		i.name.free()
		i.mod.free()
		i.file.free()
	}
}

//

[typedef]
struct C.main__TestRunner {
mut:
	_object voidptr
}

// change_test_runner should be called by preludes that implement the
// the TestRunner interface, in their vtest_init fn (see below), to
// customize the way that V shows test results
[manualfree]
pub fn change_test_runner(x &TestRunner) {
	pobj := unsafe { &C.main__TestRunner(&test_runner)._object }
	if pobj != 0 {
		test_runner.free()
		unsafe {
			(&C.main__TestRunner(&test_runner))._object = nil
		}
	}
	test_runner = *x
}

// vtest_init will be caled *before* the normal _vinit() function,
// to give a chance to the test runner implemenation to change the
// test_runner global variable. The reason vtest_init is called before
// _vinit, is because a _test.v file can define consts, and they in turn
// may use function calls in their declaration, which may do assertions.
// fn vtest_init() {
//	change_test_runner(&TestRunner(AnotherTestRunner{}))
// }

// TODO: remove vtest_option_cludge, it is only here so that
// `vlib/sync/channel_close_test.v` compiles with simpler runners,
// that do not `import os` (which has other `fn()?`). Without it,
// the C `Option_void` type is undefined -> C compilation error.
fn vtest_option_cludge() ? {
}
