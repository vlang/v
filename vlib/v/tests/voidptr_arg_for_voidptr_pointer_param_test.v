@[translated]
module main

fn set_tail(pz &voidptr) {
	unsafe {
		*pz = voidptr(c'tail')
	}
}

// A `voidptr` passed to a `&voidptr` (C's `void **`) parameter is the pointer to
// write through, as in V1, not a value to take the address of (C translated by
// c2v: `sqlite3_prepare16(db, sql, n, &stmt, voidptr(&z_tail))`).
fn test_voidptr_argument_for_a_pointer_to_voidptr_parameter() {
	z_tail := unsafe { nil }
	set_tail(voidptr(&z_tail))
	assert z_tail != unsafe { nil }
	assert unsafe { cstring_to_vstring(&char(z_tail)) } == 'tail'
}
