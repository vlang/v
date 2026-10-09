struct CapturedValue[T] {
	item T
}

fn (value CapturedValue[T]) get() !T {
	return value.item
}

fn (value CapturedValue[T]) plain() T {
	return value.item
}

fn (value &CapturedValue[T]) pointer() T {
	return value.item
}

fn read_direct_result[M](model &M) string {
	item := model.get() or { panic(err) }
	return item.str()
}

fn read_captured_result[M](model &M) string {
	read := fn [model] [M]() string {
		item := model.get() or { panic(err) }
		return item.str()
	}
	return read()
}

fn read_captured_plain[M](model &M) string {
	read := fn [model] [M]() string {
		return model.plain().str()
	}
	return read()
}

fn read_captured_pointer[M](model &M) string {
	read := fn [model] [M]() string {
		return model.pointer().str()
	}
	return read()
}

fn test_generic_receiver_arguments_come_from_the_captured_receiver() {
	integer := CapturedValue[int]{ item: 12 }
	decimal := CapturedValue[f64]{ item: 12.5 }
	text := CapturedValue[string]{ item: 'hello' }
	flag := CapturedValue[bool]{ item: true }
	assert read_captured_result(&integer) == '12'
	assert read_captured_result(&decimal) == '12.5'
	assert read_captured_result(&text) == 'hello'
	assert read_captured_result(&flag) == 'true'
	assert read_captured_plain(&integer) == '12'
	assert read_captured_plain(&decimal) == '12.5'
	assert read_captured_plain(&text) == 'hello'
	assert read_captured_plain(&flag) == 'true'
	assert read_direct_result(&integer) == '12'
	assert read_direct_result(&decimal) == '12.5'
	assert read_captured_pointer(&integer) == '12'
	assert read_captured_pointer(&text) == 'hello'
}
