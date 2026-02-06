pub type IntSlice = []int

pub fn (mut x IntSlice) swap(i int, j int) {
	x[i], x[j] = x[j], x[i]
}

fn test_cross_assign_aliased_array() {
	mut x := IntSlice([11, 22])
	println(x)
	x.swap(0, 1)
	println(x)
	assert x == IntSlice([22, 11])
}
