fn test_for_c_u8_inclusive_upper_bound_does_not_wrap() {
	to := u8(255)
	mut seen := []u8{}
	for i := u8(253); i <= to; i++ {
		seen << i
	}
	assert seen == [u8(253), 254, 255]
}

fn test_for_c_u8_inclusive_upper_bound_continue_does_not_wrap() {
	to := u8(255)
	mut seen := []u8{}
	for i := u8(254); i <= to; i++ {
		if i == to {
			continue
		}
		seen << i
	}
	assert seen == [u8(254)]
}

fn test_for_c_u8_inclusive_lower_bound_does_not_wrap() {
	mut seen := []u8{}
	for i := u8(2); i >= u8(0); i-- {
		seen << i
	}
	assert seen == [u8(2), 1, 0]
}
