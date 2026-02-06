import strconv

pub fn valid_triangle(a u8, b u8, c u8) ![3]u8 {
	if int(a) + int(b) <= int(c) {
		return error('Invalid: a + b <= c')
	}
	if int(b) + int(c) <= int(a) {
		return error('Invalid: b + c <= a')
	}
	if int(c) + int(a) <= int(b) {
		return error('Invalid: c + a <= b')
	}
	return [a, b, c]!
}

pub fn triangle_from_string(sides string) ![3]u8 {
	s := sides.split(',')
	if s.len != 3 {
		return error('Invalid: number of sides')
	}
	a := strconv.atou8(s[0])!
	b := strconv.atou8(s[1])!
	c := strconv.atou8(s[2])!
	// warning: don't append `!` to next call
	// will confuse with fixed and return `garbage`.
	return valid_triangle(a, b, c)!
}

fn test_main() {
	assert triangle_from_string('3,4,5')![0] == 3
	assert triangle_from_string('3,4,5')![1] == 4
	assert triangle_from_string('3,4,5')![2] == 5
}
