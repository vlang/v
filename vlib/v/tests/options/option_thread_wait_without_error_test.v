fn optional_worker(value int) ?int {
	if value < 0 {
		return none
	}
	return value
}

fn optional_void_worker(present bool) ? {
	if !present {
		return none
	}
}

fn result_worker(value int) !int {
	if value < 0 {
		return error_with_code('worker failure', -value)
	}
	return value
}

fn test_wait_collects_optional_values() {
	mut workers := []thread ?int{}
	workers << spawn optional_worker(1)
	workers << spawn optional_worker(2)
	assert workers.wait()? == [1, 2]
}

fn test_wait_returns_none_without_error_storage() {
	mut workers := []thread ?int{}
	workers << spawn optional_worker(1)
	workers << spawn optional_worker(-1)
	assert workers.wait() == none
	mut empty := []thread ?{}
	empty << spawn optional_void_worker(false)
	empty.wait() or { return }
	assert false
}

fn test_wait_preserves_first_result_error() {
	mut workers := []thread !int{}
	workers << spawn result_worker(-3)
	workers << spawn result_worker(-5)
	workers << spawn result_worker(1)
	workers.wait() or {
		assert err.msg() == 'worker failure'
		assert err.code() == 3
		return
	}
	assert false
}

fn result_void_worker(value int) ! {
	if value < 0 {
		return error_with_code('worker failure', -value)
	}
}

fn test_wait_preserves_void_result_error() {
	mut workers := []thread !{}
	workers << spawn result_void_worker(1)
	workers << spawn result_void_worker(-7)
	workers.wait() or {
		assert err.code() == 7
		return
	}
	assert false
}
