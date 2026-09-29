// The array allocator places a header in front of the element storage, so the
// header has to be wide enough for the alignment a 128-bit integer needs. With a
// pointer-sized header the data of `[]u128` sat on an 8-byte boundary, and every
// element address handed to a typed C load or store was under-aligned.
struct WideHolder {
	flag int
	wide u128
}

fn alignment16(p voidptr) u64 {
	return u64(p) % 16
}

fn test_heap_array_data_is_aligned_for_wide_elements() {
	// The literal, the appended and the capacity-only forms all take their storage
	// from the same allocator, so all of them have to come out aligned.
	u := []u128{len: 2}
	i := []i128{len: 2}
	h := []WideHolder{len: 2}
	mut pushed := []u128{}
	pushed << u128(1)
	capacity := []u128{len: 0, cap: 4}
	assert alignment16(u.data) == 0
	assert alignment16(i.data) == 0
	assert alignment16(h.data) == 0
	assert alignment16(pushed.data) == 0
	assert alignment16(capacity.data) == 0
	assert unsafe { alignment16(&u[1]) } == 0
	assert unsafe { alignment16(&h[0].wide) } == 0
}

fn test_growing_an_array_keeps_the_alignment() {
	// Reallocation goes through the same size helper, so the block that follows a
	// grow has to keep the alignment the first one had.
	mut a := []u128{}
	for n in 0 .. 200 {
		a << u128(n)
	}
	assert alignment16(a.data) == 0
	assert a.len == 200
	assert a[199] == u128(199)
	assert a[0] + a[199] == u128(199)
}

fn test_wide_elements_survive_their_storage() {
	// A misaligned store can lose the high half, so read the values back.
	mut u := []u128{}
	mut i := []i128{}
	for n in 0 .. 8 {
		u << (u128(1) << (n * 16)) + u128(n)
		i << i128(-1) - i128(n)
	}
	assert u[7] == (u128(1) << 112) + u128(7)
	assert u[7] - u[6] == (u128(1) << 112) - (u128(1) << 96) + u128(1)
	assert i[7] == i128(-8)
	assert i[0] == i128(-1)
}
