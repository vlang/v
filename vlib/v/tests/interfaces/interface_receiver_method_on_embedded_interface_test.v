interface Node {
	id   string
	name string
mut:
	children []&Node
}

fn (mut node Node) append_child(child &Node) {
	node.children << child
}

interface Element {
	Node
	attributes map[string]string
}

@[heap]
struct NodeBase {
	id   string
	name string
mut:
	children []&Node
}

@[heap]
struct Text {
	NodeBase
	text string
}

@[heap]
struct HTMLBodyElement {
	NodeBase
	attributes map[string]string
}

fn new_element() Element {
	return &HTMLBodyElement{
		name: 'body'
	}
}

fn new_child(name string) &Node {
	return &Node(&Text{
		name: name
		text: 'Hello, World!'
	})
}

fn test_receiver_method_on_embedded_interface() {
	mut element := &Element(&HTMLBodyElement{
		name: 'body'
	})
	element.append_child(new_child('text'))

	assert element.name == 'body'
	assert element.children.len == 1
	child := element.children[0]
	if child is Text {
		assert child.name == 'text'
		assert child.text == 'Hello, World!'
	} else {
		assert false, 'child should be Text'
	}
}

fn test_non_addressable_receiver_method_on_embedded_interface() {
	new_element().append_child(new_child('temporary'))
}

fn test_smartcast_receiver_method_on_embedded_interface() {
	mut node := Node(&HTMLBodyElement{
		name: 'body'
	})
	if mut node is Element {
		node.append_child(new_child('smartcast'))
		assert node.children.len == 1
		assert node.children[0].name == 'smartcast'
	} else {
		assert false, 'node should be Element'
	}
}
