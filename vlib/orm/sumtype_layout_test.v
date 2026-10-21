module orm

fn infix_value() Primitive {
	leaf := Primitive(7)
	return InfixType{
		name:     'count'
		operator: .add
		right:    &leaf
	}
}

fn test_primitive_has_finite_inline_recursive_layout() {
	assert sizeof(Primitive) > sizeof(InfixType)
	value := infix_value()
	copy := value
	assert value is InfixType
	assert copy is InfixType
	assert *(copy as InfixType).right == Primitive(7)
	assert (copy as InfixType).name == 'count'
}
