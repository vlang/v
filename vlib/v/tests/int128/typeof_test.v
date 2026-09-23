// `typeof` and a method call on a mixed-width expression. The checker records such
// an expression under the narrower operand's type, so both used to name 64 bits:
// `typeof(x + u64(1))` said `u64`, and `(x + u64(1)).str()` printed 6 on the
// native representation, or failed to compile where the value is a struct.
fn test_typeof_names_the_wider_operand() {
	x := (u128(1) << 100) + u128(5)
	assert typeof(x + u64(1)) == 'u128'
	assert typeof(u64(1) + x) == 'u128'
	assert typeof(x + 1) == 'u128'
	assert typeof(x) == 'u128'
	assert typeof(x + u128(1)) == 'u128'
}

fn test_typeof_leaves_narrow_expressions_alone() {
	assert typeof(u64(1) + u64(1)) == 'u64'
	assert typeof(int(1) + 2) == 'int'
	a := u64(3)
	assert typeof(a * 2) == 'u64'
}

fn test_typeof_on_a_signed_wide_expression() {
	y := i128(-5)
	assert typeof(y + i64(1)) == 'i128'
	assert typeof(y + 1) == 'i128'
}

fn test_str_on_a_mixed_width_expression_keeps_the_value() {
	x := (u128(1) << 100) + u128(5)
	assert (x + u64(1)).str() == '1267650600228229401496703205382'
	assert (x + 1).str() == '1267650600228229401496703205382'
	assert x.str() == '1267650600228229401496703205381'
	assert (u64(1) + u64(1)).str() == '2'
	assert (85 + 2).str() == '87'
}

fn test_str_on_a_narrowing_cast_stays_narrow() {
	// The cast below is a narrow value with a 128-bit operand under it, and it has
	// to keep printing as a character rather than as the wide number.
	wide := u128(65)
	assert wide.char_str() == 'A'
	assert wide.str() == '65'
	assert u128(255).char_str() == '\u00ff'
}
