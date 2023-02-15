struct AA {
mut:
	a i64 = 91
	b i64 = 92
	c i64 = 93
}

fn my_func(val int) (AA, AA) {
	return AA{b: val}, AA{a: val}
}

fn accept() {
	my_func(20)
}