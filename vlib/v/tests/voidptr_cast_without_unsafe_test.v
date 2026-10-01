// vtest vflags: -W
// Like V1, only `&T(variable)` and `&T(cast)` warn when cast from voidptr outside
// `unsafe`; with -W, any such warning would fail this test.
struct VoidptrCastHolder {
	data voidptr
}

struct VoidptrCastNode {
	id int
}

fn voidptr_cast_alloc() voidptr {
	return unsafe { malloc(16) }
}

fn voidptr_cast_count(p charptr, n int) int {
	return if p == unsafe { nil } { 0 } else { n }
}

fn test_voidptr_casts_that_v1_accepts_outside_unsafe() {
	holder := VoidptrCastHolder{
		data: voidptr_cast_alloc()
	}
	from_field := &u8(holder.data)
	from_call := &u64(voidptr_cast_alloc())
	buf := voidptr_cast_alloc()
	assert voidptr_cast_count(charptr(buf), 3) == 3
	node := &VoidptrCastNode(buf)
	assert from_field != unsafe { nil }
	assert from_call != unsafe { nil }
	assert node != unsafe { nil }
}
