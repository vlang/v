struct Element {
	AbstractNode
	name       string
	attributes []&Attribute
}

struct Attribute {
	AbstractNode
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

fn test_array_of_reference_sumtype_append() {
	mut parent := &Element{
		name: 'parent'
	}
	mut child := &Element{
		name: 'child'
	}
	parent.append_child(child)

	dump(parent)
	assert parent.child_nodes[0].name == 'child'
}
