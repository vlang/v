module types

fn test_valid_string_data_accepts_addresses_above_2_gib() {
	// Only the address is inspected. C sign-extends a 32-bit pointer converted to u64,
	// so 32-bit hosts rejected every address above 2 GiB and dropped the smartcasts
	// recorded under such names.
	high := unsafe { tos(&u8(voidptr(usize(0x9000_0000))), 1) }
	assert valid_string_data(high)
	low := unsafe { tos(&u8(voidptr(usize(16))), 1) }
	assert !valid_string_data(low)
}
