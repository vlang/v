module types

import v.flat

fn test_uint_is_platform_width_unsigned_builtin() {
	assert is_builtin_type_name('uint')
	assert builtin_type_value('uint') == Type(uint_)
	assert builtin_type_value('uint') == builtin_type_value('usize')
	assert builtin_type_value('uint').is_integer()

	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	assert tc.parse_type('uint') == Type(uint_)
	assert tc.c_type(tc.parse_type('uint')) == 'size_t'
}
