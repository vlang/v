struct Thing[T] {
}

fn (t Thing[T]) funk() {
	_ = 'hello'[0]
}

fn test_string_index_in_generic_struct_method() {
	_ = Thing[int]{}
	assert true
}
