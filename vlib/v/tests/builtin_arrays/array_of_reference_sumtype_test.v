struct Element {
	AbstractNode
mut:
	name       string
	value      string
	attributes []&Attribute
}

struct Attribute {
	AbstractNode
mut:
	name  string
	value string
}

pub type Node = Attribute | Element

struct AbstractNode {
pub mut:
	child_nodes []&Node
}

pub fn (mut n AbstractNode) append_child(child &Node) {
	n.child_nodes << child
}

pub fn (n &AbstractNode) has_child(child &Node) int {
	return n.child_nodes.index(child)
}

fn test_array_of_reference_sumtype() {
	mut parent := &Element{
		name: 'parent'
	}
	mut child := &Element{
		name: 'child'
	}
	parent.append_child(child)

	ret := parent.has_child(child)
	println(ret)
	dump(parent)
	assert parent.child_nodes[0].name == 'child'
	assert ret == 0
}
