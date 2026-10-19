// Like V1, a struct literal may store `&local` of an immutable local in a `mut:`
// field, unless the literal itself is the value of a `mut` binding.
struct RefNode {
	value int
}

struct RefKind {
mut:
	lhs ?&RefNode
	rhs &RefNode = unsafe { nil }
}

fn make_ref_node(value int) RefNode {
	return RefNode{
		value: value
	}
}

fn ref_kind_sum(kind RefKind) int {
	mut sum := kind.rhs.value
	if lhs := kind.lhs {
		sum += lhs.value
	}
	return sum
}

fn copy_ref_kind(kind RefKind) RefKind {
	return kind
}

fn test_immutable_local_reference_in_mut_field_of_call_argument() {
	lhs := make_ref_node(1)
	rhs := make_ref_node(2)
	assert ref_kind_sum(RefKind{
		lhs: &lhs
		rhs: &rhs
	}) == 3
	copied := copy_ref_kind(RefKind{
		lhs: &rhs
		rhs: &lhs
	})
	assert ref_kind_sum(copied) == 3
}

fn test_immutable_local_reference_in_mut_field_of_immutable_binding() {
	lhs := make_ref_node(3)
	rhs := make_ref_node(4)
	kind := RefKind{
		lhs: &lhs
		rhs: &rhs
	}
	assert ref_kind_sum(kind) == 7
}
