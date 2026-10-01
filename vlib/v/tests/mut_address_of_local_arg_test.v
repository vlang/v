// `f(mut &local)` passes the local's address to a `mut` parameter, wherever the local
// lives: a local that is on the heap is passed as the address it is stored as, not as
// the address of that.

struct MutAddrEntry {
mut:
	ino  u64
	name [16]u8
}

fn mut_addr_bump(mut e MutAddrEntry) u64 {
	e.ino += 3
	return 1
}

fn mut_addr_returned_local() &MutAddrEntry {
	mut e := MutAddrEntry{
		ino: 4
	}
	n := mut_addr_bump(mut &e)
	e.ino += n
	return &e
}

fn test_mut_address_of_a_local_moved_to_the_heap() {
	e := mut_addr_returned_local()
	assert e.ino == 8
}
