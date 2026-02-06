module main

import arrays

pub struct Tree {
pub:
	//	Type of structural node, `value` should be empty
	type string
	// Content of data node, `type` should be empty
	value string
pub mut:
	//	Child nodes
	kids []&Tree
}

@[params]
struct TreeCloneParams {
	kids []&Tree
}

// Makes new derived node with different kids id defined.
pub fn (tree Tree) clone(p TreeCloneParams) &Tree {
	return &Tree{
		type:  tree.type
		value: tree.value
		kids:  p.kids
	}
}

// Collection of hask tools for processing tree.
pub type TreeBelt = map[string]fn (input &Tree, belt TreeBelt) []&Tree

// Hask tool for processing node.
pub type TreeHack = fn (input &Tree, belt TreeBelt) []&Tree

pub type TreeContext = map[string]string

@[params]
struct TreeHackParams {
	belt    TreeBelt
	context ?TreeContext
}

// Transform tree through context with transformers
pub fn (tree Tree) hack(p TreeHackParams) []&Tree {
	return arrays.concat(arrays.flatten(tree.kids.map(it.hack_self(p))))
}

pub fn (tree Tree) hack_self(p TreeHackParams) []&Tree {
	mut handle := fn [tree] (input &Tree, belt TreeBelt) []&Tree {
		return [
			input.clone(kids: input.hack(belt: belt)),
		]
	}

	if action := p.belt[tree.type] {
		handle = action
	}

	return handle(tree, p.belt)
}

fn test_main() {
	t := Tree{}
	r := t.hack_self(belt: TreeBelt(map[string]fn (&Tree, TreeBelt) []&Tree{}))
	assert r[0].type == ''
	assert r[0].value == ''
	assert r[0].kids.len == 0
}
