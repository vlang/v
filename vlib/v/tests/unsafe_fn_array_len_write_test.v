// Like V1, the body of an `@[unsafe]` function may set an array's len, e.g. to
// build a list over storage the caller keeps.
@[unsafe]
fn unsafe_fn_list_over(storage &int, count int) []int {
	mut list := []int{}
	list.data = voidptr(storage)
	list.len = count
	list.cap = count
	list.flags = .nogrow | .nofree
	return list
}

fn test_unsafe_fn_can_set_array_len() {
	storage := [4, 5, 6]!
	list := unsafe { unsafe_fn_list_over(&storage[0], 3) }
	assert list.len == 3
	assert list[1] == 5
}
