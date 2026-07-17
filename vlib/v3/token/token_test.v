module token

fn test_operator_properties_are_owned_by_tokens() {
	assert Token.plus.is_infix()
	assert Token.plus.left_binding_power() == .sum
	assert Token.pipe.left_binding_power() == .sum
	assert Token.xor.left_binding_power() == .sum
	assert Token.mul.left_binding_power() == .product
	assert Token.amp.left_binding_power() == .product
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
	assert !Token.name.is_infix()
	assert !Token.number.is_assignment()
}

fn test_keyword_property_does_not_depend_on_enum_ordinals() {
	assert Token.key_as.is_keyword()
	assert Token.key_unsafe.is_keyword()
	assert Token.key_volatile.is_keyword()
	assert !Token.name.is_keyword()
	assert !Token.lcbr.is_keyword()
}
