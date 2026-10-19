// A failed assert in a test ends that test function, from inside its `lock`/`rlock` blocks.
// The locks must be released on that path, or the next test that locks the same value
// blocks forever (glibc) or panics with EDEADLK (macOS). The scenario fails tests on
// purpose, so it is built and run as a separate test program, with a deadline.
import os
import v.cmdexec

const child_source = '@[has_globals]
module main

struct Counter {
mut:
	n int
}

__global a shared Counter
__global b shared Counter

fn test_fail_in_lock() {
	lock a {
		a.n++
		assert a.n == 100
	}
}

fn test_fail_in_rlock() {
	rlock a {
		assert a.n == 100
	}
}

fn test_fail_in_two_locks_with_defer() {
	lock a; rlock b {
		defer {
			println("deferred in lock")
		}
		a.n++
		assert a.n == 100
	}
}

fn locked_check() {
	lock b {
		assert b.n == 100
	}
}

fn test_fail_in_lock_of_a_called_fn() {
	locked_check()
}

fn test_fail_in_lock_in_loop() {
	for i in 0 .. 3 {
		lock b {
			b.n += i + 1
			assert b.n == 100
		}
	}
}

fn return_in_lock() int {
	lock a {
		return a.n
	}
	return -1
}

fn test_leave_lock_with_return_break_and_continue() {
	for _ in 0 .. 2 {
		lock a {
			break
		}
	}
	for _ in 0 .. 2 {
		rlock b {
			continue
		}
	}
	assert return_in_lock() == 2
}

fn test_locks_again() {
	lock a, b {
		a.n++
		b.n++
	}
	rlock a, b {
		println("relocked \${a.n} \${b.n}")
	}
}
'

fn test_failed_assert_releases_the_locks_of_its_function() {
	root := os.join_path(os.vtmp_dir(), 'lock_assert_failure_unlock_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'locks_test.v')
	program := os.join_path(root, 'locks_test' + $if windows { '.exe' } $else { '' })
	os.write_file(source, child_source)!
	build := cmdexec.run_with_timeout(@VEXE, ['-no-retry-compilation', '-o', program, source],
		300_000)
	assert build.exit_code == 0, build.output
	run := cmdexec.run_with_timeout(program, []string{}, 30_000)
	output := run.output.replace('\r\n', '\n')
	assert output.contains('fn test_fail_in_lock\n'), output
	assert output.contains('fn test_fail_in_rlock\n'), output
	assert output.contains('fn test_fail_in_two_locks_with_defer\n'), output
	assert output.contains('deferred in lock\n'), output
	assert output.contains('fn locked_check\n'), output
	assert output.contains('fn test_fail_in_lock_in_loop\n'), output
	assert !output.contains('fn test_leave_lock_with_return_break_and_continue\n'), output
	assert output.contains('relocked 3 2\n'), output
	// The five failed tests, and nothing else.
	assert run.exit_code == 1, output
}
