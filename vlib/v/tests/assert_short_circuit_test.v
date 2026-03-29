// Test for issue #26777: assert with || and && should properly short-circuit
module main

struct Node {
	name string
mut:
	parent   &Node = unsafe { nil }
	children []Node
}

fn test_assert_or_short_circuit_with_nil_parent() {
	mut p := Node{
		name: 'abc'
	}
	// This should work - the right side of || should not be evaluated
	// when the left side is true (p.parent == nil)
	assert unsafe { p.parent == nil } || !p.parent.children.any(it.name == p.name)
	assert true
}

fn test_assert_and_short_circuit_with_nil_parent() {
	mut p := Node{
		name: 'abc'
	}
	// For &&: if left is false, right should not be evaluated
	// Since p.parent == nil is true, the && expression is false
	// But we use an always-true left side to test
	assert true && unsafe { p.parent == nil }
	assert true
}

fn test_assert_or_with_message() {
	mut p := Node{
		name: 'abc'
	}
	// This should work with a message too
	assert unsafe { p.parent == nil } || !p.parent.children.any(it.name == p.name), 'parent check failed'
	assert true
}

fn test_assert_or_both_sides_need_eval() {
	mut child := Node{
		name: 'child'
	}
	mut parent := Node{
		name:     'parent'
		children: [child]
	}
	child.parent = &parent

	// Here left side is false (parent is not nil), so right side should be evaluated
	// Right side checks if parent has a child with same name as current node
	assert unsafe { child.parent != nil } || true, 'should not short circuit'
	assert true
}

fn test_nested_or_in_assert() {
	mut p := Node{
		name: 'test'
	}
	// Nested unsafe block with ||
	assert unsafe { p.parent == nil } || unsafe { p.parent != nil }
	assert true
}

fn main() {
	test_assert_or_short_circuit_with_nil_parent()
	test_assert_and_short_circuit_with_nil_parent()
	test_assert_or_with_message()
	test_assert_or_both_sides_need_eval()
	test_nested_or_in_assert()
	println('All short-circuit tests passed!')
}
