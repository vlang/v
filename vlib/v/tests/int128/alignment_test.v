// The managed array allocator must align storage independently of pointer width
// and the C allocator's alignment, including the portable 32-bit representation.
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

fn test_wide_array_slices_clones_growth_and_free_keep_alignment_and_values() {
	mut owner := []WideHolder{len: 4, cap: 8, init: WideHolder{
		flag: index
		wide: (u128(1) << 100) + u128(index)
	}}
	mut slice := unsafe { owner[1..3] }
	assert alignment16(slice.data) == 0
	assert slice[0].wide == (u128(1) << 100) + u128(1)
	mut copied := slice.clone()
	assert alignment16(copied.data) == 0
	assert copied.data != slice.data
	unsafe { slice.free() }
	assert owner[1].wide == copied[0].wide
	slice << WideHolder{ flag: 9, wide: u128(9) }
	assert slice.data != unsafe { &owner[1] }
	assert alignment16(slice.data) == 0
	assert owner[3].flag == 3
	assert slice[2].wide == u128(9)
	unsafe { copied.flags.set(.noslices) }
	for n in 0 .. 100 {
		copied << WideHolder{ flag: n, wide: u128(n) }
		assert alignment16(copied.data) == 0
	}
	assert copied[0].wide == owner[1].wide
	assert copied.last().wide == u128(99)
	owner.drop(1)
	assert alignment16(owner.data) == 0
	assert owner[0].wide == copied[0].wide
	unsafe {
		slice.free()
		copied.free()
		owner.free()
	}
}
