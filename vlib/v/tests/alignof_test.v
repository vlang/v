struct S1 {
	p voidptr
}

struct S2 {
	i int
}

fn test_alignof() {
	assert alignof[rune]() == 4
	assert alignof[[44]u8]() == 1
	assert alignof(`€`) == 4
	// depends on -m32/64
	assert alignof[S1]() in [u32(4), 8]
	s := S2{}
	assert alignof(s.i) == 4

	assert alignof('hello') == $if x64 {
		8
	} $else {
		4
	}
}
