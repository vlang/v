module cleanc

fn test_c_string_literal_content_to_c_single_line() {
	out := c_string_literal_content_to_c('hello')
	assert out == '"hello"'
}

fn test_c_string_literal_content_to_c_multiline() {
	out := c_string_literal_content_to_c('hello\nworld')
	assert out == '"hello\\n"\n"world"'
}

fn test_c_string_literal_content_to_c_trailing_newline() {
	out := c_string_literal_content_to_c('hello\n')
	assert out == '"hello\\n"\n""'
}

fn test_c_string_literal_content_to_c_escapes_quote() {
	out := c_string_literal_content_to_c('say "hello"')
	assert out == '"say \\"hello\\""'
}

fn test_c_string_literal_content_to_c_preserves_percent_placeholders() {
	out := c_string_literal_content_to_c('"%s"')
	assert out == '"\\"%s\\""'
}

fn test_c_string_literal_content_to_c_splits_hex_escape_before_hex_digit() {
	out := c_string_literal_content_to_c(r'\x0c8')
	assert out == '"\\x0c""8"'
}

fn test_fixed_array_elem_type_ready_accepts_primitive_alias() {
	mut g := Gen.new([])
	g.primitive_type_aliases['sha3__Lane'] = true
	assert g.fixed_array_elem_type_ready('sha3__Lane')
}

fn test_fixed_array_elem_type_ready_waits_for_alias_base() {
	mut g := Gen.new([])
	g.alias_base_types['foo__Alias'] = 'foo__Thing'
	assert !g.fixed_array_elem_type_ready('foo__Alias')
	g.emitted_types['body_foo__Thing'] = true
	assert g.fixed_array_elem_type_ready('foo__Alias')
}
