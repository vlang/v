import bits

fn test_next_pow2() {
	assert bits.next_pow2(3) == 4	
}

fn test_closest_pow2() {
	assert bits.closest_pow2(10) == 8	
}

fn test_bits_log2() {
	assert bits.bits_log2(8) == 3	
}

fn test_is_pow2() {
	assert bits.is_pow2(4) == true	
}
