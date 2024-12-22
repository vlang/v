module utils

pub fn only_number(value string) f64 {
	mut result := ''

	for chr in value.trim_space() {
		if chr.is_digit() || chr == `.` {
			result += chr.ascii_str()
		} else if chr == `,` {
			result += '.'
		}
	}

	return result.f64()
}
