interface NodeInterface {
mut:
	node_type NodeType
	node_name string
}

enum NodeType {
	none
}

struct Node {
mut:
	// removing this field works makes `InterfaceNode(x).node_name.len` below work as expected
	node_type NodeType
	node_name string
}

struct Element {
	Node
}

struct HTMLElement {
	Element
}

fn test_interface_with_multi_nested_embed() {
	x := &HTMLElement{}
	struct_name_len := x.node_name.len
	interface_name_len := NodeInterface(x).node_name.len
	println('struct: ${struct_name_len}')
	assert struct_name_len == 0
	println('interface: ${interface_name_len}')
	assert interface_name_len == 0
}
