struct AA {
mut:
	a i64 = 91
	b i64 = 92
	c i64 = 93
}

fn my_func(val int) (AA, AA) {
	return AA{b: val}, AA{a: val}
}

pub fn accept(val int) int {
	a, b := my_func(val)
	return int(a.b + b.a)
}