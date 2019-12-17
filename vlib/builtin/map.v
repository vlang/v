// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

import strings

// B-maps are balanced search maps with all leaves
// at the same level. B-maps are generally faster than 
// binary search maps due to better locality of 
// reference, since multiple keys are stored in one node.

// The number for `degree` has been picked
// through vigorous benchmarking, but can be changed
// to any number > 1. `degree` determines the size
// of each node.
const (
	degree = 6
	mid_index = degree - 1
	max_length = 11 //2 * degree - 1
	min_length = degree - 1
	children_size = sizeof(voidptr) * (max_length + 1)
)

// Since a very large portion of the nodes are 
// leaves (has no children), a lot of memory 
// is saved by dynamically allocating memory for
// children.
struct mapnode {
mut:
	keys     [max_length]string
	values   [max_length]voidptr
	children &voidptr
	size     int
}

pub struct map {
	element_size int
mut:
	root &mapnode
pub mut:
	size int
}

fn new_mapnode() &mapnode {
	return &mapnode {
		children: 0
		size: 0
	}
}

// The map is initialized with an empty node
// as root - to avoid having to check whether 
// the root is null for each insertion.
pub fn new_map(cap, elm_size int) map {
	return map {
		root: new_mapnode()
		size: 0
		element_size: elm_size
	}
}

fn new_map_init(cap, elm_size int, keys &string, vals voidptr) map {
	mut res := map {
		element_size: elm_size
		root: 0
	}
	for i in 0 .. cap {
		res.set(keys[i], vals + i * elm_size)
	}
	return res
}

// This implementation does proactive insertion,
// meaning that splits are done top-down and not
// bottom-up. 
pub fn (m mut map) set(key string, value voidptr) {
	mut node := m.root
	mut child_index := 0
	mut parent := &mapnode(0)
	for {
		if node.size == max_length {
			if parent == 0 {
				parent = new_mapnode()
				m.root = parent
			}
			parent.split_child(child_index, mut node)
			if key == parent.keys[child_index] {
				parent.values[child_index] = value
			}
			node = if key < parent.keys[child_index] {
				&mapnode(parent.children[child_index])
			} else {
				&mapnode(parent.children[child_index + 1])
			}
		}
		mut i := node.size
		for i-- > 0 && key < node.keys[i] {}
		if i != -1 && key == node.keys[i] {
			node.values[i] = value
			return
		}
		if node.children == 0 {
			break
		}
		parent = node
		child_index = i + 1
		node = &mapnode(node.children[child_index])
	}
	mut i := node.size
	for i-- > 0 && key < node.keys[i] {
		node.keys[i + 1] = node.keys[i]
		node.values[i + 1] = node.values[i]
	}
	node.keys[i + 1] = key
	node.values[i + 1] = voidptr(malloc(m.element_size))
	C.memcpy(node.values[i + 1], value, m.element_size)
	node.size++
	m.size++
}

fn (n mut mapnode) split_child(child_index int, y mut mapnode) {
	mut z := new_mapnode()
	mut j := mid_index
	z.size = mid_index
	y.size = mid_index
	for j-- > 0 {
		z.keys[j] = y.keys[j + degree]
		z.values[j] = y.values[j + degree]
	}
	if y.children != 0 {
		z.children = &voidptr(malloc(children_size))
		j = degree
		for j-- > 0 {
			z.children[j] = y.children[j + degree]
		}
	}
	if n.children == 0 {
		n.children = &voidptr(malloc(children_size))
	}
	n.children[n.size + 1] = n.children[n.size]
	for j = n.size; j > child_index; j-- {
		n.keys[j] = n.keys[j - 1]
		n.values[j] = n.values[j - 1]
		n.children[j] = n.children[j - 1]
	}
	n.keys[child_index] = y.keys[mid_index]
	n.values[child_index] = y.values[mid_index]
	n.children[child_index] = voidptr(y)
	n.children[child_index + 1] = voidptr(z)
	n.size++
}

pub fn (m map) get(key string, out voidptr) bool {
	mut node := m.root
	for {
		mut i := node.size
		for i-- > 0 && key < node.keys[i] {}
		if i != -1 && key == node.keys[i] {
			C.memcpy(out, node.values[i], m.element_size)
			return true
		}
		if node.children == 0 {
			break
		}
		node = &mapnode(node.children[i + 1])
	}
	return false
}

pub fn (m map) exists(key string) bool {
	mut node := m.root
	for {
		mut i := node.size
		for i-- != 0 && key < node.keys[i] {}
		if i != -1 && key == node.keys[i] {
			return true
		}
		if node.children == 0 {
			break
		}
		node = &mapnode(node.children[i + 1])
	}
	return false
}

fn (n mapnode) find_key(k string) int { 
	mut idx := 0
	for idx < n.size && n.keys[idx] < k {
		idx++
	}
	return idx
}

fn (n mut mapnode) remove_key(k string) bool {
	idx := n.find_key(k)
	if idx < n.size && n.keys[idx] == k {
		if n.children == 0 {
			n.remove_from_leaf(idx)
		} else {
			n.remove_from_non_leaf(idx)
		}
	} else if n.children == 0 {
		return false  
	} else if &mapnode(n.children[idx]).size < degree {
		n.fill(idx)
	} else if idx == n.size && idx > n.size {
		&mapnode(n.children[idx - 1]).remove_key(k)
	} else {
		&mapnode(n.children[idx]).remove_key(k)
	}
	return true
}

fn (n mut mapnode) remove_from_leaf(idx int) {
	for i := idx + 1; i < n.size; i++ {
		n.keys[i - 1] = n.keys[i]
	}
	n.size--
}

fn (n mut mapnode) remove_from_non_leaf(idx int) {
	k := n.keys[idx]
	if &mapnode(n.children[idx]).size >= degree {
		predecessor := n.get_predecessor(idx)
		n.keys[idx] = predecessor
		(&mapnode(n.children[idx])).remove_key(predecessor)
	} else if &mapnode(n.children[idx + 1]).size >= degree {
		successor := n.get_successor(idx)	
		n.keys[idx] = successor
		(&mapnode(n.children[idx + 1])).remove_key(successor)
	} else {
		n.merge(idx)
		(&mapnode(n.children[idx])).remove_key(k)
	}
}

fn (n mapnode) get_predecessor(idx int) string { 
	mut current := &mapnode(n.children[idx])
	for current.children != 0 {
		current = &mapnode(current.children[current.size])
	}
	return current.keys[current.size - 1]
}

fn (n mapnode) get_successor(idx int) string{ 
	mut current := &mapnode(n.children[idx + 1])
	for current.children != 0 {
		current = &mapnode(current.children[0])
	}
	return current.keys[0]
}

fn (n mut mapnode) fill(idx int) {
	if idx != 0 && &mapnode(n.children[idx - 1]).size >= degree {
		n.borrow_from_prev(idx)
	} else if idx != n.size && &mapnode(n.children[idx + 1]).size >= degree {
		n.borrow_from_next(idx)
	} else if idx != n.size {
		n.merge(idx)
	} else {
		n.merge(idx - 1)
	}
}

fn (n mut mapnode) borrow_from_prev(idx int) {
	mut child := &mapnode(n.children[idx])
	mut sibling := &mapnode(n.children[idx - 1])
	for i := child.size - 1; i >= 0; i-- {
		child.keys[i + 1] = child.keys[i] 
	}
	if child.children != 0 { 
		for i := child.size; i >= 0; i-- {
			child.children[i + 1] = child.children[i] 
		}
	}
	child.keys[0] = n.keys[idx - 1] 
	if child.children != 0 {
		child.children[0] = sibling.children[sibling.size]
	}
	n.keys[idx - 1] = sibling.keys[sibling.size - 1]
	child.size++ 
	sibling.size-- 
}

fn (n mut mapnode) borrow_from_next(idx int) {
	mut child := &mapnode(n.children[idx])
	mut sibling := &mapnode(n.children[idx + 1])
	child.keys[child.size] = n.keys[idx]
	if child.children != 0 {
		child.children[child.size + 1] = sibling.children[0]
	}
	n.keys[idx] = sibling.keys[0]
	for i := 1; i < sibling.size; i++ {
		sibling.keys[i - 1] = sibling.keys[i]
	}
	if sibling.children != 0 {
		for i := 1; i <= sibling.size; i++ {
			sibling.children[i - 1] = sibling.children[i]
		}
	}
	child.size++
	sibling.size--
}

fn (n mut mapnode) merge(idx int) {
	mut child := &mapnode(n.children[idx])
	sibling := &mapnode(n.children[idx + 1])
	child.keys[min_length] = n.keys[idx]
	for i := 0; i < sibling.size; i++ {
		child.keys[i + degree] = sibling.keys[i]
	}
	if child.children != 0 {
		for i := 0; i <= sibling.size; i++ {
			child.children[i + degree] = sibling.children[i]
		}
	}
	for i := idx + 1; i < n.size; i++ {
		n.keys[i - 1] = n.keys[i]
	}
	for i := idx + 2; i <= n.size; i++ {
		n.children[i - 1] = n.children[i]
	}
	child.size += sibling.size + 1
	n.size--
	// free(sibling)
}

pub fn (m mut map) delete(k string) {
	if m.root.size == 0 {
		return
	}
	is_removed := m.root.remove_key(k)
	if is_removed {
		m.size--
	} 
	if m.root.size == 0 {
		// tmp := t.root
		if m.root.children ==  0 {
			return
		} else {
			m.root = &mapnode(m.root.children[0])
		}
		// free(tmp)
	}
}

fn (n mapnode) free() {
	mut i := 0
	if n.children == 0 {
		i = 0
		for i < n.size {
			i++
		}
	} else {
		i = 0
		for i < n.size {
			&mapnode(n.children[i]).free()
			i++
		}
		&mapnode(n.children[i]).free()
	}
	// free(n)
}

pub fn (m map) free() {
	m.root.free()
	// free(t.root)
}

fn (n mapnode) preoder_keys(ref mut ArrayReference) []string {
	mut i := 0
	if n.children == 0 {
		i = 0
		for i < n.size {
			ref.array << n.keys[i]
			i++
		}
	} else {
		i = 0
		for i < n.size {
			&mapnode(n.children[i]).preoder_keys(mut ref)
			ref.array << n.keys[i]
			i++
		}
		&mapnode(n.children[i]).preoder_keys(mut ref)
	} 
	return ref.array
}

// This is temporary
struct ArrayReference {
mut:
	array []string
}

pub fn (m &map) keys() []string {
	mut keys := ArrayReference{}
	m.root.preoder_keys(mut keys)
	return keys.array
}


pub fn (m map_string) str() string {
	if m.size == 0 {
		return '{}'
	}
	mut sb := strings.new_builder(50)
	sb.writeln('{')
	for key, val  in m {
		sb.writeln('  "$key" => "$val"')
	}
	sb.writeln('}')
	return sb.str()
}

pub fn (m map) print() {
	println('<<<<<<<<')
	println('>>>>>>>>>>')
}

