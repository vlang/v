// The binding of an `if i := opt {` guard inside a generic function has the
// unwrapped type, also where it indexes the left side of an assignment.
fn set_at[T](index ?int, xs []int) []int {
	mut a := xs.clone()
	if i := index {
		a[i] = 9
	}
	return a
}

fn add_at[T](index ?int, xs []int) []int {
	mut a := xs.clone()
	if i := index {
		j := i
		a[j] += 10
	}
	return a
}

fn test_if_guard_binding_indexes_assignment_in_generic_fn() {
	assert set_at[int](1, [1, 2, 3]) == [1, 9, 3]
	assert set_at[int](none, [1, 2, 3]) == [1, 2, 3]
	assert add_at[string](2, [1, 2, 3]) == [1, 2, 13]
}
