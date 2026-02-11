module main

// Define a simple interface
pub interface Node {
	kind() int
	name() string
}

// Implement the interface
pub struct MyNode {
pub:
	value int
	text  string
}

pub fn (n MyNode) kind() int {
	return 1
}

pub fn (n MyNode) name() string {
	return n.text
}

// Define a type alias to the interface
pub type Expr = Node

// Function using the type alias
pub fn process_node(expr Expr) string {
	return expr.name()
}

fn test_alias_interface() {
	node := MyNode{
		value: 42
		text:  'test'
	}
	result := process_node(node)
	println('Result: ${result}')
	assert result == 'test'
}
