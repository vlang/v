interface Ennemi {
}

struct Map {
	ennemis []Ennemi
}

fn test_interface_arr_for_mut_iter_index() {
	mut maap := Map{}

	for mut ennemi in maap.ennemis {
		assert maap.ennemis.index(ennemi) == 0
	}
}

@[heap]
interface WalkerMut {
mut:
	children []&WalkerMut
	value    int

	get_children() []&WalkerMut
	add_child(mut child WalkerMut)
	bump()
}

@[heap]
struct WalkNodeMut {
mut:
	children []&WalkerMut
	value    int
}

fn (n &WalkNodeMut) get_children() []&WalkerMut {
	return n.children
}

fn (mut n WalkNodeMut) add_child(mut child WalkerMut) {
	n.children << child
}

fn (mut n WalkNodeMut) bump() {
	n.value++
}

fn walk_mut(mut node WalkerMut) {
	node.bump()
	for mut child in node.get_children() {
		walk_mut(mut child)
	}
}

fn add_child_from_concrete(mut root WalkNodeMut, mut child WalkNodeMut) {
	root.add_child(mut child)
}

fn test_for_in_array_of_interface_ptr_regression() {
	mut root := &WalkNodeMut{}
	mut child := &WalkNodeMut{}
	add_child_from_concrete(mut root, mut child)
	walk_mut(mut root)
	assert child.value == 1
	assert root.value == 1
}
