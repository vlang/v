module main

pub struct Tree {}

pub type TreeBelt = map[string]fn (input &Tree, belt TreeBelt) []&Tree

pub fn (tree &Tree) processed() TreeBelt {
	mut blet := TreeBelt(map[string]fn (&Tree, TreeBelt) []&Tree{})
	blet['foo'] = fn (input &Tree, belt TreeBelt) []&Tree {
		return [input]
	}
	return blet
}

fn test_main() {
	tree := Tree{}
	ret := tree.processed()
	assert ret.len == 1
	ret2 := ret['foo'](tree, ret)
	assert ret2[0] == tree
}
