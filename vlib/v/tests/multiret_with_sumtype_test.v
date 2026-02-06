type Abc = int | rune | string | u32

fn cyz() (Abc, string) {
	return 'a', 'b'
}

fn test_multiret_with_sumtype() {
	x, y := cyz()
	println(x)
	println(y)
	assert x == Abc('a')
	assert y == 'b'
}
