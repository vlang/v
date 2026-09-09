// Regression test for https://github.com/vlang/v/issues/27507
// `array.slice()` marks the source buffer's data header (`has_slices`) so a later
// grow/delete of the parent does copy-on-grow instead of mutating a buffer that a
// live slice still references. The marking was made cheaper (inlined header lookup,
// store only when not already set); this test guards that the behavior is unchanged.

fn test_live_slice_survives_parent_grow() {
	mut a := [1, 2, 3, 4, 5]
	s := a[1..4]
	assert s == [2, 3, 4]
	// growing the parent must copy, leaving the slice intact
	for i in 0 .. 200 {
		a << i
	}
	a[2] = 999
	assert s == [2, 3, 4]
}

fn test_repeated_transient_slices_of_same_buffer() {
	mut buf := []u8{len: 0, cap: 256}
	for i in 0 .. 256 {
		buf << u8(i)
	}
	mut acc := u64(0)
	for _ in 0 .. 10000 {
		w := buf[8..buf.len] // read-only window, dropped immediately
		for b in w {
			acc += b
		}
	}
	// 10000 iterations * sum(8..256)
	mut one := u64(0)
	for j := 8; j < 256; j++ {
		one += u64(j)
	}
	assert acc == one * 10000
}

fn test_slice_of_slice() {
	a := [10, 20, 30, 40, 50]
	s := a[1..5] // [20,30,40,50]
	b := s[1..3] // [30,40]
	assert b == [30, 40]
}
