import datatypes

struct KeyVal {
mut:
	key int
	val int
}

fn (a KeyVal) == (b KeyVal) bool {
	return a.key == b.key
}

fn (a KeyVal) < (b KeyVal) bool {
	return a.key < b.key
}

fn main() {
	mut bst := datatypes.BSTree<KeyVal>{}
	bst.insert(KeyVal{ key: 1, val: 12 })
	println(bst.in_order_traversal())

	bst.insert(KeyVal{ key: 2, val: 34 })
	bst.insert(KeyVal{ key: -2, val: 203 })

	for elem in bst.in_order_traversal() {
		println(elem.val)
	}
}
