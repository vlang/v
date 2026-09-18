fn empty_literal_cstr() &char {
	return ''.str
}

fn nonempty_literal_cstr() &char {
	return 'hello'.str
}

fn empty_variable_cstr() &char {
	s := ''
	return s.str
}

fn empty_parenthesized_cstr() &char {
	return ('').str
}

fn literal_string_buffer() &u8 {
	return 'hello'.str
}

fn first_string_buffer_byte(buffer &u8) u8 {
	return unsafe { buffer[0] }
}

fn test_string_literal_str_field_can_be_returned_as_char_pointer() {
	assert empty_literal_cstr() != unsafe { nil }
	assert nonempty_literal_cstr() != unsafe { nil }
	assert empty_variable_cstr() != unsafe { nil }
	assert empty_parenthesized_cstr() != unsafe { nil }
}

fn test_string_literal_str_field_points_to_the_literal_bytes() {
	empty := ''.str
	assert empty != unsafe { nil }
	assert unsafe { empty[0] } == 0
	buffer := literal_string_buffer()
	assert unsafe { buffer[0] } == `h`
	assert unsafe { buffer[4] } == `o`
	assert unsafe { buffer[5] } == 0
	assert first_string_buffer_byte('hello'.str) == `h`
	assert first_string_buffer_byte(r'raw'.str) == `r`
}

fn test_string_literal_fields_match_variable_and_parenthesized_fields() {
	s := 'hello'
	assert first_string_buffer_byte(s.str) == first_string_buffer_byte('hello'.str)
	assert first_string_buffer_byte(('hello').str) == first_string_buffer_byte('hello'.str)
	assert ''.len == 0
	assert 'hello'.len == 5
}

fn test_string_literal_method_calls_still_work() {
	assert ''.str() == ''
	assert 'hello'.str() == 'hello'
	assert 'hello'.to_upper() == 'HELLO'
}
