import math.big

fn test_get_bit_negative_small_integers() {
	for value in i64(-256) .. 0 {
		x := big.integer_from_i64(value)
		for index in u32(0) .. 64 {
			assert x.get_bit(index) == (((u64(value) >> index) & 1) != 0)
		}
		assert x.get_bit(64)
		assert x.get_bit(127)
		assert x.get_bit(u32(0xffffffff))
	}
}

fn test_get_bit_negative_borrow_across_digits() {
	for shift in u32(60) .. 193 {
		power := big.one_int.left_shift(shift)
		negative := power.neg()
		for index in u32(0) .. shift + 65 {
			assert negative.get_bit(index) == (index >= shift)
		}
		below := negative - big.one_int
		assert below.get_bit(0)
		assert below.get_bit(shift - 1)
		assert !below.get_bit(shift)
		assert below.get_bit(shift + 1)
		above := negative + big.one_int
		assert above.get_bit(0)
		assert !above.get_bit(shift - 1)
		assert above.get_bit(shift)
	}
}
