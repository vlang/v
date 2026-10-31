module constants

pub const table = [u8(1), 2, 3]!
pub const fixed_elements = [i64(4), 5]

// first returns the first element of the module's fixed array constant.
pub fn first() u8 {
	return table[0]
}

// dynamic_first returns the first element of the module's dynamic array constant.
pub fn dynamic_first() i64 {
	return fixed_elements[0]
}
