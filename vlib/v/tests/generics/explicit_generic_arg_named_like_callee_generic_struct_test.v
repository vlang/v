import json2

// json2 has a private generic `Node[T]`. A plain `Node` of this module, passed as
// an explicit type argument, must not be mistaken for it.
struct Node {
	name string
}

fn test_explicit_type_arg_named_like_a_generic_struct_of_the_callee_module() {
	node := json2.decode[Node]('{"name":"leaf"}')!
	assert node.name == 'leaf'
}
