struct Test[T] {
mut:
	val T
}

fn (mut t Test[T]) set_val(val T) {
	t.val = val
}

fn process[T](mut t Test[T]) int {
	t.set_val('qqq')
	anon := fn [t] [T]() int {
		println(t)
		return 1
	}
	return anon()
}

fn test_generic_closure_with_generic_capture_can_be_assigned_to_local_variable() {
	mut t := Test[string]{}
	assert process(mut t) == 1
	assert t.val == 'qqq'
}
