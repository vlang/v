// In a `!T` function, `return a() or { b() }` with a `!T` call `b()` returns the
// result of `b()` as it is: its value, or its error.
struct Host {
mut:
	fail_reject bool
	value       int = 5
	rejected    int
}

fn (mut host Host) reject(e IError) !voidptr {
	host.rejected++
	if host.fail_reject {
		return error('reject failed: ${e}')
	}
	return voidptr(&host.value)
}

fn (mut host Host) decode(ok bool) !voidptr {
	if !ok {
		return error('decode failed')
	}
	return unsafe { nil }
}

fn run(mut host Host, ok bool) !voidptr {
	return host.decode(ok) or { host.reject(err) }
}

fn first(ok bool) !int {
	if ok {
		return 1
	}
	return error('first')
}

fn second(ok bool) !int {
	if ok {
		return 2
	}
	return error('second')
}

fn either(a bool, b bool) !int {
	return first(a) or {
		message := err.msg()
		assert message == 'first'
		second(b)
	}
}

fn test_returned_or_block_result_tail() {
	mut host := Host{}
	assert run(mut host, true)! == unsafe { nil }
	assert host.rejected == 0
	assert run(mut host, false)! == voidptr(&host.value)
	assert host.rejected == 1
	host.fail_reject = true
	if _ := run(mut host, false) {
		assert false
	} else {
		assert err.msg() == 'reject failed: decode failed'
	}
	assert host.rejected == 2
}

fn test_returned_or_block_result_tail_in_closure() {
	mut host := &Host{}
	handle := voidptr(host)
	decode := fn [handle] (ok bool) !voidptr {
		mut h := unsafe { &Host(handle) }
		return h.decode(ok) or { h.reject(err) }
	}
	assert decode(false)! == voidptr(&host.value)
	assert host.rejected == 1
}

fn test_returned_or_block_result_tail_after_statements() {
	assert either(true, false)! == 1
	assert either(false, true)! == 2
	either(false, false) or {
		assert err.msg() == 'second'
		return
	}
	assert false
}
