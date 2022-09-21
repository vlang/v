module main

interface TypeFactory<T> {
	get_type(type_name string) T
}

enum NodeType {
	unknown
	expression
	statement
	literal
}

struct EnumTypeFactory {}

fn (f &EnumTypeFactory) get_type(type_name string) NodeType {
	return match type_name {
		'expression' { NodeType.expression }
		'statement' { NodeType.statement }
		'literal' { NodeType.literal }
		else { NodeType.unknown }
	}
}

struct RawNode {
	type_name string
}

struct Node<T> {
	factory   TypeFactory<T>
	type_name NodeType
	raw_node  RawNode
}

fn new_node<T>(factory TypeFactory<T>, raw_node RawNode) ?Node<T> {
	return Node<T>{
		factory: factory
		type_name: factory.get_type(raw_node.type_name)
		raw_node: raw_node
	}
}

fn program<T>(factory TypeFactory<T>) ? {
	root1 := new_node<T>(factory, RawNode{'literal'})?
	println(root1)
	assert root1.type_name == .literal

	root2 := new_node<T>(root1.factory, RawNode{'expression'})?
	println(root2)
	assert root2.type_name == .expression
}

fn test_generic_interface_with_non_generic_method() {
	program<NodeType>(&EnumTypeFactory{})?
}
