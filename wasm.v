struct BB {
mut:
	a i64 = 22
	b AA
}

struct AA {
mut:
	a i64 = 91
	b i64 = 92
	c i64 = 93
}

pub fn make(nval AA) i64 {
	val := BB{b: nval}

	return val.b.b
}

pub fn reassign(nval int) int {
	val := make(AA{b: nval})
	return int(val)
}