// Regression test for https://github.com/vlang/v/issues/28889:
// a local initialized from an inferred generic call inside a generic body
// must keep the specialized type when it is used in a later generic call.
fn load[T](value &T) T {
	return unsafe { *value }
}

fn store[T](mut destination T, value T) {
	destination = value
}

struct Counter[T] {
mut:
	value usize
}

fn (mut c Counter[T]) step() {
	value := load(&c.value)
	store(mut &c.value, (value + 1) & 7)
}

fn bitwise_ops[T](start usize) usize {
	mut v := start
	value := load(&v)
	store(mut &v, value | 8)
	next := load(&v)
	store(mut &v, next ^ 1)
	return v
}

fn test_generic_receiver_method_uses_inferred_generic_call_result() {
	mut c := Counter[int]{}
	for _ in 0 .. 9 {
		c.step()
	}
	assert c.value == 1
}

fn test_generic_fn_uses_inferred_generic_call_result() {
	assert bitwise_ops[int](3) == 10
	assert bitwise_ops[string](8) == 9
}
