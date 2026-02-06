interface IValue {
	value() [2]int
}

struct Speed {
	data [2]int
}

fn (s Speed) value() [2]int {
	return s.data
}

fn get_value(v IValue) [2]int {
	return v.value()
}

fn test_main() {
	s := Speed{[35, 36]!}
	assert get_value(s) == [35, 36]!
}
