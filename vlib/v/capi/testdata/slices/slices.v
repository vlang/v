module slices

pub fn bytes() []u8 {
	return [u8(1), 2, 3]
}

pub fn maybe_bytes() ![]u8 {
	return [u8(4), 5]
}
