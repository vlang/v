import datatypes { DoublyLinkedList }

pub type LayoutBoxId = usize

pub struct LayoutBox {
}

pub struct LayoutTree {
mut:
	root  ?LayoutBoxId
	boxes []LayoutBox
}

pub fn LayoutTree.new() LayoutTree {
	return LayoutTree{
		root:  ?LayoutBoxId(none)
		boxes: []LayoutBox{}
	}
}

fn test_main() {
	mut tree := LayoutTree.new()
	tree.root = 1
	if tree.root != none {
		mut parents := DoublyLinkedList[LayoutBoxId]{}
		parents.push_back(tree.root)
		assert parents.len == 1
	} else {
		assert false
	}
}
