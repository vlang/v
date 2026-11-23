// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module scene

import gg

// Node is one transform in a parent-relative scene tree. Children move with
// their parent, mirroring the scene graphs of three.js and Godot, but without
// any rendering or engine policy.
@[heap]
pub struct Node {
pub mut:
	name     string
	parent   &Node = unsafe { nil }
	children []&Node
	position gg.Vec3
	rotation gg.Vec3 // Euler angles in degrees
	scale    gg.Vec3 = gg.Vec3{1, 1, 1}
	visible  bool    = true
}

// new_node returns an empty node with the given name.
pub fn new_node(name string) &Node {
	return &Node{ name: name }
}

// add attaches child to this node. Re-adding moves the child.
pub fn (mut n Node) add(mut child &Node) {
	if child == n {
		return
	}
	// Reject cycles formed through n's ancestors: attaching child here would
	// make n (and everything above it) a descendant of child.
	mut ancestor := n.parent
	for !isnil(ancestor) {
		if ancestor == child {
			return
		}
		ancestor = ancestor.parent
	}
	if !isnil(child.parent) {
		child.parent.remove(mut child)
	}
	child.parent = n
	n.children << child
}

// remove detaches child from this node.
pub fn (mut n Node) remove(mut child &Node) {
	for i, c in n.children {
		if c == child {
			mut remaining := []&Node{len: n.children.len - 1}
			for j in 0 .. n.children.len {
				if j < i {
					remaining[j] = n.children[j]
				} else if j > i {
					remaining[j - 1] = n.children[j]
				}
			}
			n.children = remaining
			child.parent = unsafe { nil }
			return
		}
	}
}

// find returns the first descendant with the given name, including self.
pub fn (n &Node) find(name string) ?&Node {
	if n.name == name {
		return n
	}
	for child in n.children {
		if found := child.find(name) {
			return found
		}
	}
	return none
}

// traverse calls visit for this node and every descendant in depth-first
// order.
pub fn (n &Node) traverse(visit fn (&Node)) {
	visit(n)
	for child in n.children {
		child.traverse(visit)
	}
}

// world_position returns the accumulated position of this node through its
// parent chain. Rotations are not applied; this is a lightweight transform
// helper, not a full matrix hierarchy.
pub fn (n &Node) world_position() gg.Vec3 {
	mut result := n.position
	mut parent := n.parent
	for !isnil(parent) {
		result = result + parent.position
		parent = parent.parent
	}
	return result
}

// is_visible reports whether this node and all ancestors are visible.
pub fn (n &Node) is_visible() bool {
	if !n.visible {
		return false
	}
	mut parent := n.parent
	for !isnil(parent) {
		if !parent.visible {
			return false
		}
		parent = parent.parent
	}
	return true
}
