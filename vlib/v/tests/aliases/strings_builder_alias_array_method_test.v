module main

import strings

fn builder_string(val int) string {
	mut builder := strings.Builder{}
	builder.write_decimal(val)
	return builder.str()
}

fn test_strings_builder_alias_array_method() {
	assert builder_string(42) == '42'
}
