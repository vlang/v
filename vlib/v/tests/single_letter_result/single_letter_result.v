module single_letter_result

pub struct M {
pub mut:
	x int
}

// get returns a one-letter concrete struct, or an error for nonempty input.
pub fn get(data []u8) !M {
	if data.len > 0 { return error('unexpected data') }
	return M{ x: 42 }
}

// optional returns a one-letter concrete struct when available.
pub fn optional(available bool) ?M {
	if !available { return none }
	return M{ x: 24 }
}
