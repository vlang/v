struct GenericCallback[T] {
	callback fn (T) T @[required]
}

fn (mut callback GenericCallback[T]) call_mut(value T) T {
	return callback.callback(value)
}

fn (callback &GenericCallback[T]) call_pointer(value T) T {
	return callback.callback(value)
}

fn test_generic_fn_field_calls_on_pointer_backed_receivers() {
	mut callback := GenericCallback[int]{
		callback: fn (value int) int {
			return value + 1
		}
	}
	assert callback.call_mut(1) == 2
	assert callback.call_pointer(2) == 3
}
