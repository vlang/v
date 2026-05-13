@[flag]
enum FlagEnumCompoundAssign {
	a
	b
	c
}

type FlagEnumCompoundAssignAlias = FlagEnumCompoundAssign

fn test_flag_enum_compound_assign_with_short_enum_values() {
	mut flag := FlagEnumCompoundAssign.a
	flag |= .b
	assert flag == .a | .b
	flag ^= .a
	assert flag == .b
	flag &= .b
	assert flag == .b
}

fn test_flag_enum_alias_compound_assign_with_short_enum_values() {
	mut flag := FlagEnumCompoundAssignAlias(FlagEnumCompoundAssign.a)
	flag |= .c
	assert flag == .a | .c
	flag ^= .a
	assert flag == .c
	flag &= .c
	assert flag == .c
}
