struct AA {
mut:
	a i64 = 22
	b i64
	c i64
}

pub fn reassign(val int) int {

	mut a := AA{}

	a.b = val
	
	return take(a)
}

pub fn take(input AA) int {
	return int(input.b)
}