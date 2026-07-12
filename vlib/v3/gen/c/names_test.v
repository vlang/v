module c

// test_c_name_sanitize_operator_overloads validates this v3 regression case.
fn test_c_name_sanitize_operator_overloads() {
	assert c_name('Point.<') == 'Point__lt'
	assert c_name('Point.<=') == 'Point__le'
	assert c_name('Point.>') == 'Point__gt'
	assert c_name('Point.>=') == 'Point__ge'
}

fn test_c_name_sanitize_escaped_keywords() {
	assert c_name('@true') == '_v_true'
	assert c_name('@false') == '_v_false'
	assert c_name('Kind.@asm') == 'Kind___v_asm'
}

fn test_c_name_libc_collision_abs() {
	assert c_name('abs') == 'v_abs'
	assert c_name('C.abs') == 'abs'
}

fn test_cgen_flattened_generic_receiver_short_variants() {
	assert cgen_flattened_generic_receiver_short_variants('foo__Bar_baz__Qux') == [
		'Bar_Qux',
	]
	assert cgen_flattened_generic_receiver_short_variants('mod.foo__Bar_baz__Qux') == [
		'Bar_Qux',
		'mod.Bar_Qux',
	]
}

fn test_cgen_typeof_display_canonicalizes_fixed_array_generic_args() {
	assert typeof_display_type_name('Box[int[3]]') == 'Box[[3]int]'
	assert typeof_display_type_name('Pair[int[3], Box[string[2]]]') == 'Pair[[3]int, Box[[2]string]]'
	assert typeof_display_type_name('Box[int][3]') == '[3]Box[int]'
}
