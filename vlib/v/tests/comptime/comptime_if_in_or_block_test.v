// Regression test for https://github.com/vlang/v/issues/28022
// An `or {}` block whose body is a compile-time `$if / $else` used to emit an
// undeclared result temp: the live branch ended in `panic(err)` (void), while
// the value-producing branch was pruned as dead code, so no result temp was
// declared even though codegen still referenced it. This must now compile and
// run. The panicking branch is kept unreachable at runtime.

fn g(fail bool) !int {
	if fail {
		return error('boom')
	}
	return 42
}

fn test_comptime_if_in_or_block_compiles_and_runs() {
	// The call succeeds, so the `panic(err)` branch is never taken at runtime.
	x := g(false) or {
		$if !windows {
			panic(err)
		} $else {
			u32(0)
		}
	}
	assert x == 42
}
