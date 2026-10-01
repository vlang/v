// `f(mut &local)` passes the local's address to a `mut` parameter, wherever the local
// lives. Scalar results cannot carry that address out, so reading them back and returning
// them leaves the local on the stack, be they several or in an Option. A Result can: its
// error may be a custom one that keeps the address, so the local is moved to the heap, as
// one whose address is returned is. Such a local is passed as the address it is stored
// as, not as the address of that.

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

fn mut_addr_fill_option_pair(mut e MutAddrEntry) ?(u64, u64) {
	e.ino = 19
	return 5, 6
}

fn mut_addr_fill_result_pair(mut e MutAddrEntry) !(u64, u64) {
	e.ino = 17
	return 3, 4
}

const mut_addr_kept_words = 512

// Large enough for the calls made after its function returned to run over all of it, were
// it left on the stack.
struct MutAddrKept {
mut:
	words [mut_addr_kept_words]u64
}

struct MutAddrKeptError {
	Error
	kept &MutAddrKept = unsafe { nil }
}

fn mut_addr_fail_and_keep(mut k MutAddrKept) !int {
	for i in 0 .. mut_addr_kept_words {
		k.words[i] = u64(41 + i)
	}
	return MutAddrKeptError{
		kept: unsafe { k }
	}
}

fn mut_addr_forwarded_error() !int {
	mut k := MutAddrKept{}
	return mut_addr_fail_and_keep(mut &k)
}

fn mut_addr_propagated_error() !int {
	mut k := MutAddrKept{}
	n := mut_addr_fail_and_keep(mut &k)!
	return n
}

fn mut_addr_returned_error() !int {
	mut k := MutAddrKept{}
	n := mut_addr_fail_and_keep(mut &k) or { return err }
	return n
}

@[noinline]
fn mut_addr_use_the_stack(n int) u64 {
	mut buf := [mut_addr_kept_words]u64{}
	for i in 0 .. mut_addr_kept_words {
		buf[i] = u64(0xdead0000 + i + n)
	}
	return if n > 0 { mut_addr_use_the_stack(n - 1) + buf[n % mut_addr_kept_words] } else { 0 }
}

// What the error kept is intact after the frames of other calls took the place of the
// one it was made in.
fn mut_addr_kept_is_intact(err IError) bool {
	assert mut_addr_use_the_stack(16) != 0
	if err is MutAddrKeptError {
		for i in 0 .. mut_addr_kept_words {
			if err.kept.words[i] != u64(41 + i) {
				return false
			}
		}
		return true
	}
	return false
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

fn mut_addr_forwarded_option() ?u64 {
	mut e := MutAddrEntry{}
	return mut_addr_fill_option(mut &e)
}

fn mut_addr_option_pair() ?(u64, u64) {
	mut e := MutAddrEntry{}
	a, b := mut_addr_fill_option_pair(mut &e)?
	if e.ino != 19 {
		return none
	}
	return a, b
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
	assert mut_addr_forwarded_option()? == 2
	c, d := mut_addr_option_pair() or { u64(0), u64(0) }
	assert c == 5
	assert d == 6
	a, b := mut_addr_result_pair()!
	assert a == 3
	assert b == 4
}

fn test_result_error_keeping_the_mut_address_it_was_given() {
	mut failures := 0
	mut_addr_forwarded_error() or {
		failures++
		assert mut_addr_kept_is_intact(err)
	}
	mut_addr_propagated_error() or {
		failures++
		assert mut_addr_kept_is_intact(err)
	}
	mut_addr_returned_error() or {
		failures++
		assert mut_addr_kept_is_intact(err)
	}
	assert failures == 3
}

fn test_mut_address_of_a_local_moved_to_the_heap() {
	e := mut_addr_returned_local()
	assert e.ino == 8
}
