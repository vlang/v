struct Tree {
mut:
	nr_elems int
	parent   ?&Tree
}

fn (mut t Tree) set_nr_elems(name string, value int) {
	t.parent or { return }.nr_elems = value
}

fn test_main() {
	parent := Tree{
		nr_elems: 11
	}
	mut child := Tree{
		parent: unsafe { &parent }
	}
	child.set_nr_elems('Buzz', 123)
	assert child.parent or { return }.nr_elems == 123
}
