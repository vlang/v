fn main() {
	out := if temp := f('') {
		temp
	} else if temp := f('something') {
		temp
	} else {
		return
	}

	assert out[0] == 104
	assert out[1] == 105
}

fn f(s string) ?[]u8 {
	if s == '' {
		return none
	}
	return [u8(104), 105]
}
