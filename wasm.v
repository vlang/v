struct AA {
mut:
	a int = 22
	b i64
}

pub fn reassign(input int) int {
	mut a := AA{a: 20}
	a.b = 2500000

	return int(a.b)
}