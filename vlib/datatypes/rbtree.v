module datatypes

/// Internal rapresentation of the tree node
[heap]
struct RBTreeNode<T> {
mut:
	is_init bool // mark node as initialized
	value   T    // value of the node
	parent  &RBTreeNode<T> = 0
	left    &RBTreeNode<T> = 0
	right   &RBTreeNode<T> = 0
	color   bool // mark the node as red or black
}

// new_root_node create new root rbt node
fn new_rbt_root_node<T>(value &T) &RBTreeNode<T> {
	return &RBTreeNode<T>{
		is_init: true
		value: value
		color: false
		parent: new_rbt_none_node<T>(true)
		left: new_rbt_none_node<T>(true)
		right: new_rbt_none_node<T>(true)
	}
}

// new_node create a new rb node with a parante
fn new_rbt_node<T>(parent &RBTreeNode<T>, value &T) &RBTreeNode<T> {
	return &RBTreeNode<T>{
		is_init: true
		value: value
		parent: parant
	}
}

// new_none_node create a dummy node.
fn new_rbt_none_node<T>(init bool) &RBTreeNode<T> {
	return &RBTreeNode<T>{
		is_init: init
	}
}

// set_to_red set the color of the node to red
fn (mut node RBTreeNode<T>) set_to_red() {
	node.color = true
}

// set_to_black set the color of the node to black
fn (mut node RBTreeNode<T>) set_to_black() {
	node.color = false
}

// Pure Red-Black Tree implementation
//
// Pure V implementation of the RB-Tree
// Time complexity on main operation O(N log N)
// in the wrost case still O(N log N)
// Space complexity O(N)
pub struct RBTree<T> {
mut:
	root &RBTreeNode<T> = 0
}

pub fn (mut rbt RBTree<T>) insert(value &T) bool {
	return false
}

pub fn (mut rbt RBTree<T>) contains(value &T) bool {
	return false
}

pub fn (mut rbt RBTree<T>) remove(value &T) bool {
	return false
}

pub fn (rbt &RBTree<T>) is_empty() bool {
	return rbt.root == 0
}

pub fn (rbt &RBTree<T>) max() ?&T {
	return error('not implemented')
}

pub fn (rbt &RBTree<T>) min() ?&T {
	return error('not implemented')
}
