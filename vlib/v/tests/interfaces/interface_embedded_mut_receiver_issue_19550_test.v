interface Node {
	id   string
	name string
mut:
	children []&Node
}

fn (mut node Node) append_child(child &Node) {
	node.children << child
}

fn (node Node) child_count() int {
	return node.children.len
}

interface Element {
	Node
	attributes map[string]string
}

interface DocumentElement {
	Element
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

fn test_embedded_interface_mut_receiver_on_pointer() {
	mut body := &HTMLBodyElement{ name: 'body' }
	mut element := &Element(body)
	child := &Node(&Text{ name: 'text', text: 'Hello, World!' })
	element.append_child(child)
	assert body.children.len == 1
	assert element.children.len == 1
	assert element.child_count() == 1
	assert element.children[0].name == 'text'
	assert element.children[0] == child
}

fn test_embedded_interface_mut_receiver_on_value() {
	mut body := HTMLBodyElement{ name: 'body' }
	mut element := Element(body)
	element.append_child(&Node(&Text{ name: 'first' }))
	element.append_child(&Node(&Text{ name: 'second' }))
	assert body.children.len == 2
	assert element.children.len == 2
	assert element.child_count() == 2
	assert element.children[0].name == 'first'
	assert element.children[1].name == 'second'
}

fn test_embedded_interface_mut_receiver_through_multiple_levels() {
	mut body := &HTMLBodyElement{ name: 'body' }
	mut element := &DocumentElement(body)
	element.append_child(&Node(&Text{ name: 'text' }))
	assert body.children.len == 1
	assert element.children.len == 1
	assert element.child_count() == 1
}
