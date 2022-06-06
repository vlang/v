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

fn (f EnumTypeFactory) get_type(type_name string) NodeType {
	return match type_name {
		'expression' { NodeType.expression }
		'statement' { NodeType.statement }
		'literal' { NodeType.literal }
		else { NodeType.unknown }
	}
}

struct Node<T> {
	factory   TypeFactory<T>
	type_name NodeType
}

fn new_node<T>(type_name string, factory TypeFactory<T>) Node<T> {
	return Node<T>{
		factory: factory
		type_name: factory.get_type(type_name)
	}
}

fn test_generic_interface_with_non_generic_method() {
	root1 := new_node<NodeType>('literal', EnumTypeFactory{})
	println(root1)
	assert root1.type_name == .literal

	root2 := new_node<NodeType>('expression', root1.factory)
	println(root2)
	assert root2.type_name == .expression
}
