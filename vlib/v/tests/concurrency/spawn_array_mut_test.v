fn mutable_array_arg(mut a []int) {
	println(a)
	a << 4
	assert a.len == 4
}

fn test_main() {
	mut a := []int{}
	a << 1
	a << 2
	a << 3
	w := spawn mutable_array_arg(mut a)
	w.wait()
}
