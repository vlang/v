fn foo(mut arr []&int) {
	arr = arr.filter(it != unsafe { nil })
}

fn test_array_filter_of_fn_mut_arg() {
	mut arr := []&int{}
	foo(mut arr)
	println(arr.len)
	assert arr.len == 0
}
