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
	//
	total_assert_passes u64 // increase this in .assert_pass()
	total_assert_fails u64 // increase this in .assert_fail()
	//
	start(ntests int) // called before all tests, you can initialise private data here. ntests is the number of test functions in the _test.v file.
	finish() // called after all tests are finished, you can print some stats if you want here.
	exit_code() int // called right after finish(), it should return the exit code, that the test program will exit with.
	//
	fn_start() // called before the start of each test_ function.
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

// change_test_runner may be called by tests or preludes that implement
// the TestRunner interface to customize the way that V processes/shows
// test results.
[manualfree]
pub fn change_test_runner(x &TestRunner) {
	pobj := unsafe { &C.main__TestRunner(&test_runner)._object }
	if pobj != 0 {
		test_runner.free()
		unsafe {
			(&C.main__TestRunner(&test_runner))._object = voidptr(0)
		}
	}
	test_runner = *x
}
