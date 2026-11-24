// The program has a function `callback` that returns `?bool`. A parameter of that
// name assigned to a local keeps the parameter's type.
fn callback(x int) ?bool {
	return x > 0
}

struct Box[T] {
	value T
}

fn (b Box[T]) local(callback fn (T) string) string {
	show := callback
	return show(b.value)
}

fn local_int(value int, callback fn (int) string) string {
	show := callback
	return show(value)
}

fn test_parameter_value_named_like_a_function_keeps_its_type() {
	assert callback(1) or { false }
	assert local_int(3, fn (v int) string {
		return 'n${v}'
	}) == 'n3'
	assert Box[int]{
		value: 7
	}.local(fn (v int) string {
		return 'v${v}'
	}) == 'v7'
}
