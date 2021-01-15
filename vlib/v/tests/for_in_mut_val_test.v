fn foo(mut arr []int) {
	for _, mut j in arr {
		j *= 2
	}
}

fn test_for_in_mut_val() {
	mut arr := [1, 2, 3]
	foo(mut arr)
	println(arr)
	assert arr == [2, 4, 6]
}
