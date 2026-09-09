interface Node {
	id   string
	name string
mut:
	children []&Node
}

fn (mut node Node) append_child(child &Node) {
	node.children << child
}

fn (mut node Node) append_child_through_method(child &Node) {
	node.append_child(child)
}

fn (mut node Node) append_child_through_comptime_method(child &Node) {
	$for method in Node.methods {
		if method.name == 'append_child' {
			node.$method(child)
		}
	}
}

fn (node &Node) check() bool {
	return true
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

struct CallCounter {
mut:
	calls int
}

fn new_element() &Element {
	return &Element(&HTMLBodyElement{
		name: 'body'
	})
}

fn new_child(name string) &Node {
	return &Node(&Text{
		name: name
		text: 'Hello, World!'
	})
}

fn new_counted_element(mut counter CallCounter) &Element {
	counter.calls++
	return new_element()
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

fn test_field_mutation_through_delegated_receiver_method_on_embedded_interface() {
	mut element := &Element(&HTMLBodyElement{
		name: 'body'
	})
	element.append_child_through_method(new_child('delegated'))

	assert element.children.len == 1
	assert element.children[0].name == 'delegated'
}

fn test_field_mutation_through_comptime_receiver_method_on_embedded_interface() {
	mut element := &Element(&HTMLBodyElement{
		name: 'body'
	})
	element.append_child_through_comptime_method(new_child('comptime delegated'))

	assert element.children.len == 1
	assert element.children[0].name == 'comptime delegated'
}

fn test_non_addressable_receiver_method_on_embedded_interface() {
	assert new_element().check()
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

fn test_lazy_receiver_method_on_embedded_interface() {
	mut counter := CallCounter{}
	assert !(false && new_counted_element(mut counter).check())
	assert counter.calls == 0
}

fn test_loop_condition_receiver_method_on_embedded_interface() {
	mut counter := CallCounter{}
	mut iterations := 0
	for new_counted_element(mut counter).check() && iterations < 3 {
		iterations++
	}
	assert counter.calls == 4
}

fn test_shared_receiver_method_on_embedded_interface() {
	shared element := Element(&HTMLBodyElement{
		name: 'shared body'
	})
	lock element {
		assert element.check()
	}
}
