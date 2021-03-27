fn array_mut_slice(mut a []int) {
	assert a[1..3].map(it) == [3, 5]
}

fn test_array_mut_slice() {
	mut a := [1, 3, 5, 7, 9]
	array_mut_slice(mut a)
}

fn test_array_slice_clone() {
	arr := [1, 2, 3, 4, 5]
	cl := arr[1..].clone()
	assert cl == [2, 3, 4, 5]
}

fn test_array_slice_clone2() {
	arr := [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
	cl := arr[1..].clone()[2..].clone()
	assert cl == [4, 5, 6, 7, 8, 9, 10]
}

fn access_slice_attribute(mut arr []int) int {
	slice := arr[..arr.len - 1]
	return slice.len
}

fn test_access_slice_attribute() {
	mut arr := [1, 2, 3, 4, 5]
	assert access_slice_attribute(mut arr) == 4
}

fn fixed_array_slice(a [3]int) {
	assert a[0..] == [1, 2, 3]
	assert a[..a.len] == [1, 2, 3]
}

fn mut_fixed_array_slice(mut a [3]int) {
	assert a[0..] == [1, 2, 3]
	assert a[..a.len] == [1, 2, 3]
}

fn test_fixed_array_slice() {
	fixed_array1 := [1, 2, 3]!
	arr1 := fixed_array1[0..]
	assert arr1 == [1, 2, 3]
	fixed_array2 := [[1, 2], [2, 3], [3, 4], [4, 5]]!
	arr2 := fixed_array2[0..]
	assert arr2 == [[1, 2], [2, 3], [3, 4], [4, 5]]
	mut arr := [1, 2, 3]!
	fixed_array_slice(arr)
	mut_fixed_array_slice(mut arr)
}

fn pointer_array_slice(mut a []int) {
	assert a[0..] == [1, 2, 3]
	assert a[..a.len] == [1, 2, 3]
}

fn test_pointer_array_slice() {
	mut arr := [1, 2, 3]
	pointer_array_slice(mut arr)
}
