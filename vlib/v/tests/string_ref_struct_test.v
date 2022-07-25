struct Node {
	val   int
	left  &Node
	right &Node
}

fn test_string_ref_struct() {
	n := Node{123, 0, 0}
	println(n.left)
	assert '$n.left' == '&nil'
}

fn test_string_ref_struct_with_nil_instead_of_0() {
	n := Node{123, unsafe { nil }, unsafe { nil }}
	println(n.left)
	assert '$n.left' == '&nil'
}
