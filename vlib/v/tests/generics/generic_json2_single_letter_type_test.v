import json2

struct T {
	id    int
	flags u32
	value json2.Any
}

fn same_letter_identity[T](value T) T {
	return value
}

fn same_letter_forward[T](value T) T {
	return same_letter_identity[T](value)
}

struct SingleLetterBox[T] {
	value T
}

fn (box SingleLetterBox[T]) get() T {
	return same_letter_identity[T](box.value)
}

fn test_json2_decode_array_of_type_named_like_generic_parameter() {
	ts := json2.decode[[]T]('[{"id":1,"flags":2,"value":"ab"}]') or { panic(err) }
	assert ts.len == 1
	assert ts[0].id == 1
	assert ts[0].flags == 2
	assert ts[0].value as string == 'ab'
	item := json2.decode[T]('{"id":3,"flags":4,"value":"cd"}') or { panic(err) }
	assert item.id == 3
	assert item.flags == 4
	assert item.value as string == 'cd'
}

fn test_generic_parameter_shadows_concrete_single_letter_type_in_forwarded_calls() {
	assert same_letter_forward[int](7) == 7
	item := same_letter_forward[T](T{ id: 9 })
	assert item.id == 9
	int_box := SingleLetterBox[int]{ value: 5 }
	assert int_box.get() == 5
	struct_box := SingleLetterBox[T]{ value: T{ id: 11 } }
	assert struct_box.get().id == 11
}
