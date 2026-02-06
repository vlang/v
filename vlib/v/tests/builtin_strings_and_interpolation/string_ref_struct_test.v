struct Node {
	val   int
	left  &Node = unsafe { nil }
	right &Node = unsafe { nil }
}

fn test_string_ref_struct_with_nil_instead_of_0() {
	n := Node{123, unsafe { nil }, unsafe { nil }}
	println(n.left)
	assert '${n.left}' == '&nil'
}
