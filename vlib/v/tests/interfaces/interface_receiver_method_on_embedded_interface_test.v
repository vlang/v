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

fn (mut node Node) replace_only_on_windows(next &Node) {
	$if windows {
		node = unsafe { *next }
	}
}

fn (node &Node) replace_through_helper_only_on_windows(next &Node) {
	replace_pointer_only_on_windows(node, next)
}

fn (node &Node) check() bool {
	return true
}

fn (node &Node) copy_value() Node {
	return unsafe { *node }
}

fn (node &Node) read_field_through_local_alias() string {
	alias := unsafe { node }
	return alias.name
}

fn (node &Node) read_field_through_pointer_helper() string {
	return read_node_name(node)
}

fn (node &Node) read_field_through_pointer_alias_helper() string {
	return read_node_name_through_alias(node)
}

fn (node &Node) read_field_through_forward_helper() string {
	return read_node_name_declared_later(node)
}

fn (node &Node) local_pointer_helper_array_len() int {
	return local_pointer_array_len(node)
}

fn (node &Node) inspect_through_generic_helper() int {
	return inspect_value(node)
}

fn (node &Node) append_child_through_pointer_helper(child &Node) {
	append_node_child(node, child)
}

fn (node &Node) unrelated_pointer_array_len() int {
	return unrelated_pointer_array(node).len
}

fn (node &Node) read_field_through_rebound_alias(other &Node) string {
	mut alias := unsafe { node }
	alias = unsafe { other }
	return read_node_name(alias)
}

fn read_node_name(node &Node) string {
	return node.name
}

fn read_node_name_through_alias(node &Node) string {
	alias := unsafe { node }
	return alias.name
}

fn inspect_value[T](value T) int {
	return 1
}

fn append_node_child(node &Node, child &Node) {
	unsafe {
		node.children << child
	}
}

fn local_pointer_array_len(node &Node) int {
	mut saved := []&Node{}
	saved << node
	return saved.len
}

fn replace_pointer_only_on_windows(node &Node, next &Node) {
	$if windows {
		unsafe {
			*node = *next
		}
	}
}

fn unrelated_pointer_array(node &Node) []&Node {
	return []&Node{}
}

struct LocalNodeHolder {
	node &Node
}

fn (node &Node) read_field_through_local_holder() string {
	holder := LocalNodeHolder{
		node: unsafe { node }
	}
	return holder.node.name
}

fn (node &Node) read_field_through_local_array() string {
	mut saved := []&Node{}
	saved << node
	return saved[0].name
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

fn test_inactive_comptime_receiver_replacement_is_ignored() {
	$if !windows {
		mut element := Element(HTMLBodyElement{
			name: 'body'
		})
		element.replace_only_on_windows(new_child('replacement'))
		assert element.name == 'body'
	}
}

fn test_inactive_comptime_helper_replacement_is_ignored() {
	$if !windows {
		element := new_element()
		element.replace_through_helper_only_on_windows(new_child('replacement'))
		assert element.name == 'body'
	}
}

fn test_non_addressable_receiver_method_on_embedded_interface() {
	assert new_element().check()
}

fn test_dereferenced_pointer_receiver_value_does_not_escape() {
	element := new_element()
	copy := element.copy_value()
	assert copy.name == 'body'
}

fn test_local_pointer_receiver_alias_does_not_escape() {
	element := new_element()
	assert element.read_field_through_local_alias() == 'body'
}

fn test_read_only_pointer_receiver_helper_does_not_escape() {
	element := new_element()
	assert element.read_field_through_pointer_helper() == 'body'
}

fn test_read_only_pointer_receiver_alias_helper_does_not_escape() {
	element := new_element()
	assert element.read_field_through_pointer_alias_helper() == 'body'
}

fn test_forward_pointer_receiver_helper_does_not_escape() {
	element := new_element()
	assert element.read_field_through_forward_helper() == 'body'
}

fn test_local_pointer_helper_array_does_not_escape() {
	element := new_element()
	assert element.local_pointer_helper_array_len() == 1
}

fn test_generic_pointer_receiver_helper_does_not_escape() {
	element := new_element()
	assert element.inspect_through_generic_helper() == 1
}

fn test_pointer_receiver_helper_can_mutate_fields() {
	element := new_element()
	element.append_child_through_pointer_helper(new_child('helper'))
	assert element.children.len == 1
	assert element.children[0].name == 'helper'
}

fn test_unrelated_pointer_helper_return_does_not_escape() {
	element := new_element()
	assert element.unrelated_pointer_array_len() == 0
}

fn test_rebound_pointer_receiver_alias_does_not_escape() {
	element := new_element()
	assert element.read_field_through_rebound_alias(new_child('other')) == 'other'
}

fn test_local_pointer_receiver_holder_does_not_escape() {
	element := new_element()
	assert element.read_field_through_local_holder() == 'body'
}

fn test_local_pointer_receiver_array_does_not_escape() {
	element := new_element()
	assert element.read_field_through_local_array() == 'body'
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

fn read_node_name_declared_later(node &Node) string {
	return node.name
}

fn test_shared_receiver_method_on_embedded_interface() {
	shared element := Element(&HTMLBodyElement{
		name: 'shared body'
	})
	lock element {
		assert element.check()
	}
}
