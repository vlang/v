module token

fn test_operator_properties_are_owned_by_tokens() {
	assert Token.plus.is_infix()
	assert Token.plus.left_binding_power() == .sum
	assert Token.pipe.left_binding_power() == .sum
	assert Token.xor.left_binding_power() == .sum
	assert Token.mul.left_binding_power() == .product
	assert Token.amp.left_binding_power() == .product
	assert Token.power.left_binding_power() == .power
	assert Token.power.right_binding_power() == .power
	assert int(Token.power.left_binding_power()) > int(Token.mul.left_binding_power())
	// `<<` `>>` `>>>` share the `product` level with `* / % &`, so they bind
	// tighter than `+ - | ^` at `sum` (V precedence, docs Appendix II).
	assert Token.left_shift.left_binding_power() == .product
	assert int(Token.left_shift.left_binding_power()) > int(Token.plus.left_binding_power())
	assert Token.logical_or.left_binding_power() == .logical_or
	assert Token.logical_or.right_binding_power() == .logical_and
	assert Token.eq.is_comparison()
	assert Token.plus.is_overloadable()
	assert Token.minus.is_prefix()
	assert Token.inc.is_postfix()
	assert Token.right_shift_unsigned_assign.is_assignment()
	assert Token.power_assign.is_assignment()
	assert Token.power.is_overloadable()
	assert !Token.name.is_infix()
	assert !Token.number.is_assignment()
}

fn test_file_position_resolves_file_local_offsets() {
	src := 'line one\nsecond line\nthird\n'
	mut fs := FileSet.new()
	mut f := fs.add_file('x.v', src.len)
	f.index_lines(src)
	// Pos.offset is file-local: offset 0 is the file start, not fs.base.
	start := f.position(new_pos(1, 0))
	assert start.line == 1
	assert start.column == 1
	assert f.line(new_pos(1, 0)) == 1
	// Offset 9 is the start of the second line (`line one\n` is 9 bytes).
	second := f.position(new_pos(1, 9))
	assert second.line == 2
	assert second.column == 1
	assert f.line(new_pos(1, 9)) == 2
}

fn test_keyword_property_does_not_depend_on_enum_ordinals() {
	assert Token.key_as.is_keyword()
	assert Token.key_unsafe.is_keyword()
	assert Token.key_volatile.is_keyword()
	assert !Token.name.is_keyword()
	assert !Token.lcbr.is_keyword()
}
