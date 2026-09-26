// vtest vflags: -d ownership
// Recursive calls prepend the argument projection to the callee's returned parameter path
// (`.left.name`, `.left.right.name`, ...). Return alias inference used to grow those paths
// forever, so compiling this file with the ownership checker never finished.

struct RecursiveAliasNode {
	name  string
	left  &RecursiveAliasNode = unsafe { nil }
	right &RecursiveAliasNode = unsafe { nil }
}

fn recursive_alias_outermost(n &RecursiveAliasNode, go_left bool) string {
	if go_left && !isnil(n.left) {
		return recursive_alias_outermost(n.left, go_left)
	}
	if !go_left && !isnil(n.right) {
		return recursive_alias_outermost(n.right, go_left)
	}
	return n.name
}

fn recursive_alias_zigzag_left(n &RecursiveAliasNode) string {
	if isnil(n.left) {
		return n.name
	}
	return recursive_alias_zigzag_right(n.left)
}

fn recursive_alias_zigzag_right(n &RecursiveAliasNode) string {
	if isnil(n.right) {
		return n.name
	}
	return recursive_alias_zigzag_left(n.right)
}

fn test_recursive_return_param_aliases_reach_fixed_point() {
	leaf_a := &RecursiveAliasNode{
		name: 'a'
	}
	leaf_b := &RecursiveAliasNode{
		name: 'b'
	}
	mid := &RecursiveAliasNode{
		name:  'mid'
		left:  leaf_a
		right: leaf_b
	}
	root := &RecursiveAliasNode{
		name:  'root'
		left:  mid
		right: leaf_a
	}
	assert recursive_alias_outermost(root, true) == 'a'
	assert recursive_alias_outermost(root, false) == 'a'
	assert recursive_alias_outermost(mid, false) == 'b'
	assert recursive_alias_zigzag_left(root) == 'b'
	assert recursive_alias_zigzag_right(root) == 'a'
}
