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

type RecursiveDefaultSum = RecursiveDefaultBranch | RecursiveDefaultLeaf

struct RecursiveDefaultBranch {
	x     int
	child RecursiveDefaultSum
}

struct RecursiveDefaultLeaf {
	x int
}

struct RecursiveDefaultHolder {
	node RecursiveDefaultSum
}

fn test_default_sumtype_common_field_is_accessible() {
	holder := DefaultHolder{}
	assert holder.s.x == 0
}

fn test_default_nested_sumtype_common_field_is_accessible() {
	holder := NestedHolder{}
	assert holder.o.x == 0
}

fn test_recursive_default_sumtype_common_field_is_accessible() {
	holder := RecursiveDefaultHolder{}
	node := holder.node
	assert node is RecursiveDefaultBranch
	assert node.x == 0
	if node is RecursiveDefaultBranch {
		assert node.child is RecursiveDefaultBranch
		assert node.child.x == 0
	}
}
