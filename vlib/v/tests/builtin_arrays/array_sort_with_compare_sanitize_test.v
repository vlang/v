fn test_sort_with_compare_works_even_in_sanitized_modes() {
	mut a := []int{}
	dump(a.data)
	dump(a.len)
	dump(a.cap)
	a.sort()
	dump(a)
	a.sort_with_compare(fn (sa &string, sb &string) int {
		assert false, 'this should not be called, the array len is 0'
		return (*sa).len - (*sb).len
	})
	dump(a)
	assert a.len == 0

	mut cloned_a := a.clone()
	dump(cloned_a.data)
	dump(cloned_a.len)
	dump(cloned_a.cap)
	cloned_a.sort()
	cloned_a.sort_with_compare(fn (sa &string, sb &string) int {
		assert false, 'this should not be called, the array len is 0, even after .clone()'
		return (*sa).len - (*sb).len
	})
	dump(cloned_a)
	assert cloned_a.len == 0
}
