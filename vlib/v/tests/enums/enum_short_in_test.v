enum EnumInKind {
	a
	b
}

fn short_in_left(kinds []EnumInKind) bool {
	return .a in kinds
}

fn short_not_in_left(kinds []EnumInKind) bool {
	return .b !in kinds
}

fn short_in_fixed(kinds [2]EnumInKind) bool {
	return .a in kinds
}

fn test_short_enum_on_left_of_in() {
	kinds := [EnumInKind.a]
	assert short_in_left(kinds)
	assert .b in [EnumInKind.b]
	assert .b !in kinds
	assert short_not_in_left(kinds)
	assert short_in_fixed([2]EnumInKind{init: EnumInKind.a})
	assert !short_in_left([EnumInKind.b])
	// shorthand on the right keeps working
	k := EnumInKind.a
	assert k in [.a, .b]
	assert k !in [.b]
}
