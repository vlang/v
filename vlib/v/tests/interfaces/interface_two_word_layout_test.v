interface Speaker {
	speak() string
}

interface EmptyInterface {}

struct Cat {}

fn (_ Cat) speak() string {
	return 'meow'
}

fn interface_call(value Speaker) string {
	return value.speak()
}

fn test_interface_header_is_two_words() {
	assert sizeof(Speaker) == 2 * sizeof(voidptr)
	assert sizeof(EmptyInterface) == 2 * sizeof(voidptr)
}

fn test_two_word_interface_dispatch_and_type_tag() {
	value := Speaker(Cat{})
	assert interface_call(value) == 'meow'
	assert value.type_name() == 'Cat'
	assert value.type_idx() > 0
	assert value is Cat
}

fn test_two_word_empty_interface_type_tag() {
	value := EmptyInterface(42)
	assert value.type_name() == 'int'
	assert value.type_idx() > 0
	assert value is int
}
