fn foo1(mut arr []int) {
	for _, mut j in arr {
		j *= 2
	}
}

fn test_for_in_mut_val_of_array() {
	mut arr := [1, 2, 3]
	foo1(mut arr)
	println(arr)
	assert arr == [2, 4, 6]
}

fn foo2(mut arr [3]int) {
	for _, mut j in arr {
		j *= 2
	}
}

fn test_for_in_mut_val_of_fixed_array() {
	mut arr := [1,2,3]!
	foo2(mut arr)
	println(arr)
	assert arr == [2, 4, 6]!
}
