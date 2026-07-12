// vtest build: !sanitize-address-gcc && !sanitize-address-clang
// vtest vflags: -autofree -gc none
// Regression test for the `-autofree` double-free of the `error_sentinel` singleton
// (review on https://github.com/vlang/v/pull/27544). Under `-autofree`, const cleanup
// frees every `IError` const's boxed object; the sentinel can also be copied/re-exported,
// so its shared `MessageError` object must be exempted from `free()` like `none__`.
// `-gc none` is required so `free()` actually returns memory (a double free would abort
// at program cleanup) instead of being a GC no-op.

const my_err = error_sentinel // re-export: shares the same boxed object

fn find(x int) !int {
	if x < 0 {
		return error_sentinel
	}
	return x
}

fn alias_find(x int) !int {
	if x < 0 {
		return my_err
	}
	return x
}

fn test_error_sentinel_autofree_no_double_free() {
	mut misses := 0
	for i := -50; i < 50; i++ {
		find(i) or { misses++ }
		alias_find(i) or { misses++ }
	}
	assert misses == 100
	assert my_err.msg() == 'error'
	assert error_sentinel.msg() == 'error'
}
