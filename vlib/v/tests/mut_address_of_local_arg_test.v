// `f(mut &local)` passes the local's address to a `mut` parameter, wherever the local
// lives. Scalar results cannot carry that address out, so reading them back and returning
// them leaves the local on the stack, be they several, or in an Option or a Result; a
// local that is on the heap for another reason is passed as the address it is stored as,
// not as the address of that.

struct MutAddrEntry {
mut:
	ino  u64
	name [16]u8
}

fn mut_addr_fill(mut e MutAddrEntry) (u64, u64) {
	e.ino = 7
	e.name[0] = `x`
	return 0, 0
}

fn mut_addr_bump(mut e MutAddrEntry) u64 {
	e.ino += 3
	return 1
}

fn mut_addr_fill_result(mut e MutAddrEntry) !u64 {
	e.ino = 11
	return 1
}

fn mut_addr_fill_option(mut e MutAddrEntry) ?u64 {
	if e.ino == 99 {
		return none
	}
	e.ino = 13
	return 2
}

fn mut_addr_fill_result_pair(mut e MutAddrEntry) !(u64, u64) {
	e.ino = 17
	return 3, 4
}

fn mut_addr_two_results() (u64, u64) {
	mut e := MutAddrEntry{}
	ret, err := mut_addr_fill(mut &e)
	if err != 0 {
		return ret, err
	}
	return e.ino, u64(e.name[0])
}

fn mut_addr_two_results_in_a_loop(count int) (u64, u64) {
	mut total := u64(0)
	for _ in 0 .. count {
		mut e := MutAddrEntry{}
		ret, err := mut_addr_fill(mut &e)
		if err != 0 {
			return if total != 0 { total, u64(0) } else { ret, err }
		}
		total += e.ino
	}
	return total, 0
}

fn mut_addr_forwarded_results() (u64, u64) {
	mut e := MutAddrEntry{
		ino: 100
	}
	return mut_addr_fill(mut &e)
}

fn mut_addr_result() !u64 {
	mut e := MutAddrEntry{}
	n := mut_addr_fill_result(mut &e)!
	if e.ino != 11 {
		return error('the callee did not write the local')
	}
	return n
}

fn mut_addr_forwarded_result() !u64 {
	mut e := MutAddrEntry{}
	return mut_addr_fill_result(mut &e)
}

fn mut_addr_option(start u64) ?u64 {
	mut e := MutAddrEntry{
		ino: start
	}
	n := mut_addr_fill_option(mut &e) or { return none }
	if e.ino != 13 {
		return none
	}
	return n
}

fn mut_addr_result_pair() !(u64, u64) {
	mut e := MutAddrEntry{}
	a, b := mut_addr_fill_result_pair(mut &e)!
	if e.ino != 17 {
		return error('the callee did not write the local')
	}
	return a, b
}

fn mut_addr_returned_local() &MutAddrEntry {
	mut e := MutAddrEntry{
		ino: 4
	}
	n := mut_addr_bump(mut &e)
	e.ino += n
	return &e
}

fn test_scalar_results_of_a_call_taking_mut_address() {
	ino, first := mut_addr_two_results()
	assert ino == 7
	assert first == u64(`x`)
}

fn test_scalar_results_of_a_call_taking_mut_address_in_a_loop() {
	total, err := mut_addr_two_results_in_a_loop(4)
	assert total == 28
	assert err == 0
}

fn test_forwarded_scalar_results_of_a_call_taking_mut_address() {
	a, b := mut_addr_forwarded_results()
	assert a == 0
	assert b == 0
}

fn test_result_and_option_of_scalars_from_a_call_taking_mut_address() {
	assert mut_addr_result()! == 1
	assert mut_addr_forwarded_result()! == 1
	assert mut_addr_option(0)? == 2
	assert mut_addr_option(99) == none
	a, b := mut_addr_result_pair()!
	assert a == 3
	assert b == 4
}

fn test_mut_address_of_a_local_moved_to_the_heap() {
	e := mut_addr_returned_local()
	assert e.ino == 8
}
