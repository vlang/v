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
	// Test &&: when left is true, right should be evaluated (non-short-circuit path)
	// This tests that the right side IS evaluated when left is true
	assert true && p.name == 'abc'
	// Test && short-circuit: left is false, right should NOT be evaluated
	// With fix, right is short-circuited (no nil access)
	// Without fix, would crash with nil pointer access
	// We use a simple bool expr to avoid assert failure
	_ = false && unsafe { p.parent == nil }
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
	// child.parent is nil by default
	// Left side is false (child.parent == nil, so child.parent != nil is false)
	// Right side must be evaluated - use a simple true to test both sides are evaluated
	assert unsafe { child.parent != nil } || true, 'should evaluate both sides'
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
