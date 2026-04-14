module main

struct DefaultAlpha {
	x int
}

struct DefaultBeta {
	x int
}

type DefaultSum = DefaultAlpha | DefaultBeta

struct DefaultHolder {
	s DefaultSum
}

struct NestedAlpha {
	x int
}

struct NestedBeta {
	x int
}

struct NestedGamma {
	x int
}

struct NestedDelta {
	x int
}

type NestedLeft = NestedAlpha | NestedBeta
type NestedRight = NestedDelta | NestedGamma
type NestedOuter = NestedLeft | NestedRight

struct NestedHolder {
	o NestedOuter
}

fn test_default_sumtype_common_field_is_accessible() {
	holder := DefaultHolder{}
	assert holder.s.x == 0
}

fn test_default_nested_sumtype_common_field_is_accessible() {
	holder := NestedHolder{}
	assert holder.o.x == 0
}
