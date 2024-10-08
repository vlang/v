enum Foo {
	zero
	first = 1
	third = 3
	fourth
}

const enum_size = i32(Foo.third)

fn test_enum_val_as_fixed_array_size() {
	arr1 := [int(Foo.first)]int{}
	assert arr1 == [0]!

	// TODO: check why it fails on MSVC
	$if !msvc {
		arr2 := [enum_size]int{}
		assert arr2 == [0, 0, 0]!
	}

	arr3 := [int(Foo.fourth)]int{}
	assert arr3 == [0, 0, 0, 0]!
}

fn test_for_in_array_named_array() {
	mut array := [1]
	for elem in array {
		assert elem == 1
	}
	for mut elem in array {
		assert *elem == 1
		elem = 2
		assert *elem == 2
	}
}

fn test_for_in_shared_array_named_array() {
	shared array := &[1]
	rlock array {
		for elem in array {
			assert elem == 1
		}
	}
}

fn test_fixed_array_to_dynamic_array() {
	y := [1, 2, 3]!
	mut x := unsafe { y[..] }
	x << 4
	assert x.len == 4
}

fn test_append_array_used_as_fn_param() {
	mut arr1 := [][]string{}
	mut arr2 := [][]string{}
	mut arr3 := []string{}
	arr4 := []string{}
	arr1 << arr2
	arr1 << arr3
	arr3 << arr4
	append_2d_2d(mut arr1, arr2)
	append_2d_1d(mut arr1, arr3)
	append_1d_1d(mut arr3, arr4)
	assert true
}

fn append_2d_2d(mut arr1 [][]string, arr2 [][]string) {
	arr1 << arr2
}

fn append_2d_1d(mut arr1 [][]string, arr2 []string) {
	arr1 << arr2
}

fn append_1d_1d(mut arr1 []string, arr2 []string) {
	arr1 << arr2
}

fn test_string_buffer_to_generic_unsigned() {
	// unsigned
	string_value := '123'
	bytes := unsafe { string_value.str.vbytes(3) }
	value := u32(0)
	unsafe { bytes.string_buffer_to_generic[u32](&value) }
	assert value == 123
}

fn test_string_buffer_to_generic_signed() {
	// signed
	string_value := '-123'
	bytes := unsafe { string_value.str.vbytes(4) }
	value := int(0)
	unsafe { bytes.string_buffer_to_generic[int](&value) }
	assert value == -123
}

fn test_string_buffer_to_generic_float() {
	// float
	string_value := '123.456'
	bytes := unsafe { string_value.str.vbytes(7) }
	value := f32(0)
	unsafe { bytes.string_buffer_to_generic[f32](&value) }
	assert value == 123.456
}
