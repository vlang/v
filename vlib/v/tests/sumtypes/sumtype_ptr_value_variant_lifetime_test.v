struct Leaf {
	id    string
	value int
}

struct Branch {
	id    string
	left  &Node = unsafe { nil }
	right &Node = unsafe { nil }
}

type Node = Branch | Leaf

fn make_tree() &Node {
	left := &Node(Leaf{
		id:    'left_leaf'
		value: 42
	})
	right := &Node(Leaf{
		id:    'right_leaf'
		value: 99
	})
	return &Node(Branch{
		id:    'root'
		left:  left
		right: right
	})
}

fn churn_stack() u64 {
	mut data := [512]u64{}
	for i in 0 .. data.len {
		data[i] = u64(i + 1) * 97
	}
	mut total := u64(0)
	for value in data {
		total ^= value
	}
	return total
}

fn test_pointer_sumtype_from_value_variant_keeps_variant_data_alive() {
	tree := make_tree()
	assert churn_stack() != 0
	if tree is Branch {
		assert tree.id == 'root'
		assert tree.left != unsafe { nil }
		assert tree.right != unsafe { nil }
		left := tree.left
		right := tree.right
		if left is Leaf {
			assert left.id == 'left_leaf'
			assert left.value == 42
		} else {
			assert false
		}
		if right is Leaf {
			assert right.id == 'right_leaf'
			assert right.value == 99
		} else {
			assert false
		}
	} else {
		assert false
	}
}
