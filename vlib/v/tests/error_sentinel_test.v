// Test for https://github.com/vlang/v/issues/27508
// `error_sentinel` is a reusable, allocation-free `IError` (a cached const, like
// `none`), for hot stateless/"not found" error paths in `!T` functions.

fn find(x int) !int {
	if x < 0 {
		return error_sentinel
	}
	return x
}

fn test_error_sentinel_is_returned_as_error() {
	mut misses := 0
	for i := -3; i < 3; i++ {
		v := find(i) or {
			misses++
			continue
		}
		assert v == i
	}
	assert misses == 3
}

fn test_error_sentinel_message() {
	if _ := find(-1) {
		assert false
	} else {
		assert err.msg() == 'error'
		assert err.code() == 0
	}
}

fn test_error_sentinel_is_stable_identity() {
	// Every call returns the same cached const instance (no per-call allocation).
	a := error_sentinel
	b := error_sentinel
	assert a.msg() == b.msg()
	assert a is MessageError
}
