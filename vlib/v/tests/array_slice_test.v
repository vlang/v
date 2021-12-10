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

fn test_push_to_orig() {
	mut orig := [1, 2, 3, 4]
	slice := orig[1..3]
	for _ in 0 .. 1000 {
		orig << 9
	}
	orig[2] = 7
	slice2 := orig[1..3]
	assert slice == [2, 3] || slice == [2, 7]
	assert slice2 == [2, 7]
}

fn test_self_slice_push() {
	mut a := [1, 2, 3]
	a = a[1..]
	a << 4
	assert a == [2, 3, 4]
}

fn test_slice_push_child() {
	mut a := [1.0, 2.0625, 3.5, -7.75, 7.125, 8.4375, 0.5]
	mut b := a[2..6] // `b` is initially created as reference
	mut c := b[1..3] // `c` is initiall reference to `a` and `b`
	b << -2.25 // `b` should be reallocated, so `a` doesn't change
	c[1] = -13.5 // this should change `c` and `a` but not `b`
	assert c == [-7.75, -13.5]
	assert a == [1.0, 2.0625, 3.5, -7.75, -13.5, 8.4375, 0.5]
	assert b == [3.5, -7.75, 7.125, 8.4375, -2.25]
}

fn test_predictable_reallocation_parent() {
	mut a := []i64{len: 4, cap: 6, init: -25}
	mut b := a[1..3]
	b[1] = -5238543910438573201
	assert a == [i64(-25), -25, -5238543910438573201, -25]
	a << 5
	b[1] = 13
	assert a == [i64(-25), -25, 13, -25, 5]
	a << -7
	b[0] = 8
	assert a == [i64(-25), 8, 13, -25, 5, -7]
	a << 9 // here `a` will be reallocated as `cap` is exceeded
	b[1] = -19 // `a` will not change any more
	assert a == [i64(-25), 8, 13, -25, 5, -7, 9]
	assert b == [i64(8), -19]
}
