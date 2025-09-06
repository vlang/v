module big

// from_json_number implements a custom decoder for json2
pub fn (mut result Integer) from_json_number(raw_number string) ! {
	mut index := 0
	mut is_negative := false

	if raw_number[0] == `-` {
		is_negative = true
		index++
	}

	ten := integer_from_int(10)

	for index < raw_number.len {
		digit := raw_number[index] - `0`

		if digit > 9 { // comma, e and E are all smaller 0 in ASCII so they underflow
			return error('expected integer but got real number')
		}

		result = (result * ten) + integer_from_int(int(digit))

		index++
	}

	if is_negative {
		result = result * integer_from_int(-1)
	}
}

// to_json implements a custom encoder for json2
pub fn (result Integer) to_json() string {
	return result.str()
}
