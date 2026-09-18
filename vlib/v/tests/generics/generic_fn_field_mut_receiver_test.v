struct FnFieldReceiver[T] {
	callback fn (T) int
mut:
	total int
}

fn (mut receiver FnFieldReceiver[T]) call_mut(value T) int {
	result := receiver.callback(value)
	receiver.total += result
	return result
}

fn (receiver FnFieldReceiver[T]) call_value(value T) int {
	return receiver.callback(value)
}

fn (receiver &FnFieldReceiver[T]) call_pointer(value T) int {
	return receiver.callback(value)
}

fn call_fn_field_mut[T](mut receiver FnFieldReceiver[T], value T) int {
	result := receiver.callback(value)
	receiver.total += result
	return result
}

fn fn_field_double(value int) int {
	return value * 2
}

fn fn_field_length(value string) int {
	return value.len
}

fn test_generic_fn_field_on_mutable_receivers_and_parameters() {
	mut ints := FnFieldReceiver[int]{
		callback: fn_field_double
	}
	assert ints.call_mut(7) == 14
	assert call_fn_field_mut[int](mut ints, 3) == 6
	assert ints.total == 20

	mut strings := FnFieldReceiver[string]{
		callback: fn_field_length
	}
	assert strings.call_mut('hello') == 5
	assert call_fn_field_mut[string](mut strings, 'vlang') == 5
	assert strings.total == 10
}

fn test_generic_fn_field_on_value_and_pointer_receivers() {
	value := FnFieldReceiver[int]{
		callback: fn_field_double
	}
	assert value.callback(2) == 4
	assert value.call_value(3) == 6
	assert value.call_pointer(4) == 8

	mut pointer := &FnFieldReceiver[int]{
		callback: fn_field_double
	}
	assert pointer.callback(5) == 10
	assert pointer.call_value(6) == 12
	assert pointer.call_pointer(7) == 14
	assert pointer.call_mut(8) == 16
	assert call_fn_field_mut[int](mut pointer, 9) == 18
	assert pointer.total == 34
}

struct FnFieldContainer[T] {
mut:
	receiver FnFieldReceiver[T]
}

fn (mut container FnFieldContainer[T]) call_nested(value T) int {
	result := container.receiver.callback(value)
	container.receiver.total += result
	return result
}

fn test_generic_fn_field_on_nested_mutable_receiver() {
	mut container := FnFieldContainer[int]{
		receiver: FnFieldReceiver[int]{
			callback: fn_field_double
		}
	}
	assert container.call_nested(11) == 22
	assert container.receiver.total == 22
}
