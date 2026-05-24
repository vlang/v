module main

pub type SplitNode = EditorLeaf | SplitContainer

pub struct EditorLeaf {
	id int
}

pub struct SplitContainer {
	children []SplitNode
}

fn sum_type_count() int {
	node := SplitContainer{
		children: [SplitNode(EditorLeaf{
			id: 1
		}), SplitNode(EditorLeaf{
			id: 2
		})]
	}
	return node.children.len
}
