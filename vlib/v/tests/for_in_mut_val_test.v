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

fn foo3(mut m map[string][3]int){
	for i in 0..m['hello'].len {
		m['hello'][i] *= 2
	}
}

fn test_fn_mut_val_of_map() {
	mut m := {'hello': [1,2,3]!}
	foo3(mut m)
	println(m)
	assert '$m' == "{'hello': [2, 4, 6]}"
}

fn foo4(mut m map[string][3]int){
	for _, mut j in m['hello'] {
		j *= 2
	}
}

fn test_for_in_mut_val_of_map() {
	mut m := {'hello':[1,2,3]!}
	foo4(mut m)
	println(m)
	assert '$m' == "{'hello': [2, 4, 6]}"
}

fn test_for_in_mut_val_of_map_direct() {
	mut m := {'foo': 1, 'bar': 2}
	for _, mut j in m {
		j = 3
	}
	println(m)
	assert '$m' == "{'foo': 3, 'bar': 3}"
}
