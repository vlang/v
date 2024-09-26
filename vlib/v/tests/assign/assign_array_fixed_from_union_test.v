union Convertor {
	su8_array_p  [20]u8
	sint_array_p [5]int
}

fn test_main() {
	a := [1, 2, 3, 4, 5]!
	p := voidptr(unsafe { &Convertor(&a[0]) })
	c := unsafe { &Convertor(p).su8_array_p }
	dump(a)
	dump(p)
	dump(c)
	assert a == [1, 2, 3, 4, 5]!
	assert p != 0
	$if little_endian {
		assert c == [u8(1), 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0]!
	}
	$if big_endian {
		assert c == [u8(0), 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5]!
	}
}
