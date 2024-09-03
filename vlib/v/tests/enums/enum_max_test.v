enum Nums as u8 {
	one
	two
	three = 0xff
}

enum Nums2 as u32 {
	one
	two
	three = 0xFFFFFFFF
}

enum Nums3 as u64 {
	one
	two
	three = 0xFFFFFFFFFFFFFFFF
}

fn test_main() {
	mut a := Nums.one
	assert a == Nums.one
	assert int(Nums.three) == 0xff
	assert Nums.three == unsafe { Nums(255) }

	assert u64(Nums2.three) == 0xFFFFFFFF
	assert u64(Nums3.three) == 0xFFFFFFFFFFFFFFFF
}
