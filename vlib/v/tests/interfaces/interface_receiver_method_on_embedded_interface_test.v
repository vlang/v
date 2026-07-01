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

fn test_receiver_method_on_embedded_interface() {
	mut element := &Element(&HTMLBodyElement{
		name: 'body'
	})
	element.append_child(&Node(&Text{
		name: 'text'
		text: 'Hello, World!'
	}))

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
