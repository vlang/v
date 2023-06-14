enum Nums as u8 {
	one
	two
	three = 0xff
}

fn test_main() {
	mut a := Nums.one
	assert a == Nums.one
	assert int(Nums.three) == 0xff
	assert Nums.three == unsafe { Nums(255) }
}
