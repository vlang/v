// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

// import strings

// B-trees are balanced search trees with all leaves at
// the same level. B-trees are generally faster than
// binary search trees due to the better locality of
// reference, since multiple keys are stored in one node.

// The number for `degree` has been picked through vigor-
// ous benchmarking but can be changed to any number > 1.
// `degree` determines the size of each node.
const (
	degree = 6
	mid_index = degree - 1
	max_size = 2 * degree - 1
	children_bytes = sizeof(voidptr) * (max_size + 1)
)

pub struct SortedMap {
	value_bytes int
mut:
	root &mapnode
pub mut:
	size int
}

struct mapnode {
mut:
	children &voidptr
	size     int
	keys     [11]string  // TODO: Should use `max_size`
	values   [11]voidptr // TODO: Should use `max_size`
}

fn new_sorted_map(n, value_bytes int) SortedMap { // TODO: Remove `n`
	return SortedMap {
		value_bytes: value_bytes
		root: new_node()
		size: 0
	}
}

fn new_sorted_map_init(n, value_bytes int, keys &string, values voidptr) SortedMap {
	mut out := new_sorted_map(n, value_bytes)
	for i in 0 .. n {
		out.set(keys[i], byteptr(values) + i * value_bytes)
	}
	return out
}

// The tree is initialized with an empty node as root to
// avoid having to check whether the root is null for
// each insertion.
fn new_node() &mapnode {
	return &mapnode {
		children: 0
		size: 0
	}
}

// This implementation does proactive insertion, meaning
// that splits are done top-down and not bottom-up.
fn (mut m SortedMap) set(key string, value voidptr) {
	mut node := m.root
	mut child_index := 0
	mut parent := &mapnode(0)
	for {
		if node.size == max_size {
			if isnil(parent) {
				parent = new_node()
				m.root = parent
			}
			parent.split_child(child_index, mut node)
			if key == parent.keys[child_index] {
				C.memcpy(parent.values[child_index], value, m.value_bytes)
				return
			}
			node = if key < parent.keys[child_index] {
				&mapnode(parent.children[child_index])
			} else {
				&mapnode(parent.children[child_index + 1])
			}
		}
		mut i := 0
		for i < node.size && key > node.keys[i] { i++ }
		if i != node.size && key == node.keys[i] {
			C.memcpy(node.values[i], value, m.value_bytes)
			return
		}
		if isnil(node.children) {
			mut j := node.size - 1
			for j >= 0 && key < node.keys[j] {
				node.keys[j + 1] = node.keys[j]
				node.values[j + 1] = node.values[j]
				j--
			}
			node.keys[j + 1] = key
			node.values[j + 1] = malloc(m.value_bytes)
			C.memcpy(node.values[j + 1], value, m.value_bytes)
			node.size++
			m.size++
			return
		}
		parent = node
		child_index = i
		node = &mapnode(node.children[child_index])
	}
}

fn (mut n mapnode) split_child(child_index int, y mut mapnode) {
	mut z := new_node()
	z.size = mid_index
	y.size = mid_index
	for j := mid_index - 1; j >= 0; j-- {
		z.keys[j] = y.keys[j + degree]
		z.values[j] = y.values[j + degree]
	}
	if !isnil(y.children) {
		z.children = &voidptr(malloc(children_bytes))
		for jj := degree - 1; jj >= 0; jj-- {
			z.children[jj] = y.children[jj + degree]
		}
	}
	if isnil(n.children) {
		n.children = &voidptr(malloc(children_bytes))
	}
	n.children[n.size + 1] = n.children[n.size]
	for j := n.size; j > child_index; j-- {
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

fn (m SortedMap) get(key string, out voidptr) bool {
	mut node := m.root
	for {
		mut i := node.size - 1
		for i >= 0 && key < node.keys[i] { i-- }
		if i != -1 && key == node.keys[i] {
			C.memcpy(out, node.values[i], m.value_bytes)
			return true
		}
		if isnil(node.children) {
			break
		}
		node = &mapnode(node.children[i + 1])
	}
	return false
}

fn (m SortedMap) exists(key string) bool {
	if isnil(m.root) { // TODO: find out why root can be nil
		return false
	}
	mut node := m.root
	for {
		mut i := node.size - 1
		for i >= 0 && key < node.keys[i] { i-- }
		if i != -1 && key == node.keys[i] {
			return true
		}
		if isnil(node.children) {
			break
		}
		node = &mapnode(node.children[i + 1])
	}
	return false
}

fn (n &mapnode) find_key(k string) int {
	mut idx := 0
	for idx < n.size && n.keys[idx] < k {
		idx++
	}
	return idx
}

fn (mut n mapnode) remove_key(k string) bool {
	idx := n.find_key(k)
	if idx < n.size && n.keys[idx] == k {
		if isnil(n.children) {
			n.remove_from_leaf(idx)
		} else {
			n.remove_from_non_leaf(idx)
		}
		return true
	} else {
		if isnil(n.children) {
			return false
		}
		flag := if idx == n.size {true} else {false}
		if (&mapnode(n.children[idx])).size < degree {
			n.fill(idx)
		}

		if flag && idx > n.size {
			return (&mapnode(n.children[idx - 1])).remove_key(k)
		} else {
			return (&mapnode(n.children[idx])).remove_key(k)
		}
	}
}

fn (mut n mapnode) remove_from_leaf(idx int) {
	for i := idx + 1; i < n.size; i++ {
		n.keys[i - 1] = n.keys[i]
		n.values[i - 1] = n.values[i]
	}
	n.size--
}

fn (mut n mapnode) remove_from_non_leaf(idx int) {
	k := n.keys[idx]
	if &mapnode(n.children[idx]).size >= degree {
		mut current := &mapnode(n.children[idx])
		for !isnil(current.children) {
			current = &mapnode(current.children[current.size])
		}
		predecessor := current.keys[current.size - 1]
		n.keys[idx] = predecessor
		n.values[idx] = current.values[current.size - 1]
		(&mapnode(n.children[idx])).remove_key(predecessor)
	} else if &mapnode(n.children[idx + 1]).size >= degree {
		mut current := &mapnode(n.children[idx + 1])
		for !isnil(current.children) {
			current = &mapnode(current.children[0])
		}
		successor := current.keys[0]
		n.keys[idx] = successor
		n.values[idx] = current.values[0]
		(&mapnode(n.children[idx + 1])).remove_key(successor)
	} else {
		n.merge(idx)
		(&mapnode(n.children[idx])).remove_key(k)
	}
}

fn (mut n mapnode) fill(idx int) {
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

fn (mut n mapnode) borrow_from_prev(idx int) {
	mut child := &mapnode(n.children[idx])
	mut sibling := &mapnode(n.children[idx - 1])
	for i := child.size - 1; i >= 0; i-- {
		child.keys[i + 1] = child.keys[i]
		child.values[i + 1] = child.values[i]
	}
	if !isnil(child.children) {
		for i := child.size; i >= 0; i-- {
			child.children[i + 1] = child.children[i]
		}
	}
	child.keys[0] = n.keys[idx - 1]
	child.values[0] = n.values[idx - 1]
	if !isnil(child.children) {
		child.children[0] = sibling.children[sibling.size]
	}
	n.keys[idx - 1] = sibling.keys[sibling.size - 1]
	n.values[idx - 1] = sibling.values[sibling.size - 1]
	child.size++
	sibling.size--
}

fn (mut n mapnode) borrow_from_next(idx int) {
	mut child := &mapnode(n.children[idx])
	mut sibling := &mapnode(n.children[idx + 1])
	child.keys[child.size] = n.keys[idx]
	child.values[child.size] = n.values[idx]
	if !isnil(child.children) {
		child.children[child.size + 1] = sibling.children[0]
	}
	n.keys[idx] = sibling.keys[0]
	n.values[idx] = sibling.values[0]
	for i := 1; i < sibling.size; i++ {
		sibling.keys[i - 1] = sibling.keys[i]
		sibling.values[i - 1] = sibling.values[i]
	}
	if !isnil(sibling.children) {
		for i := 1; i <= sibling.size; i++ {
			sibling.children[i - 1] = sibling.children[i]
		}
	}
	child.size++
	sibling.size--
}

fn (mut n mapnode) merge(idx int) {
	mut child := &mapnode(n.children[idx])
	sibling := &mapnode(n.children[idx + 1])
	child.keys[mid_index] = n.keys[idx]
	child.values[mid_index] = n.values[idx]
	for i in 0..sibling.size {
		child.keys[i + degree] = sibling.keys[i]
		child.values[i + degree] = sibling.values[i]
	}
	if !isnil(child.children) {
		for i := 0; i <= sibling.size; i++ {
			child.children[i + degree] = sibling.children[i]
		}
	}
	for i := idx + 1; i < n.size; i++ {
		n.keys[i - 1] = n.keys[i]
		n.values[i - 1] = n.values[i]
	}
	for i := idx + 2; i <= n.size; i++ {
		n.children[i - 1] = n.children[i]
	}
	child.size += sibling.size + 1
	n.size--
	// free(sibling)
}

pub fn (mut m SortedMap) delete(key string) {
	if m.root.size == 0 {
		return
	}

	removed := m.root.remove_key(key)
	if removed {
		m.size--
	}

	if m.root.size == 0 {
		// tmp := t.root
		if isnil(m.root.children) {
			return
		} else {
			m.root = &mapnode(m.root.children[0])
		}
		// free(tmp)
	}
}

// Insert all keys of the subtree into array `keys`
// starting at `at`. Keys are inserted in order.
fn (n &mapnode) subkeys(keys mut []string, at int) int {
	mut position := at
	if !isnil(n.children) {
		// Traverse children and insert
		// keys inbetween children
		for i in 0..n.size {
			child := &mapnode(n.children[i])
			position += child.subkeys(mut keys, position)
			keys[position] = n.keys[i]
			position++
		}
		// Insert the keys of the last child
		child := &mapnode(n.children[n.size])
		position += child.subkeys(mut keys, position)
	} else {
		// If leaf, insert keys
		for i in 0..n.size {
			keys[position + i] = n.keys[i]
		}
		position += n.size
	}
	// Return # of added keys
	return position - at
}

pub fn (m &SortedMap) keys() []string {
	mut keys := [''].repeat(m.size)
	if isnil(m.root) || m.root.size == 0 {
		return keys
	}
	m.root.subkeys(mut keys, 0)
	return keys
}

fn (mut n mapnode) free() {
	println('TODO')
}

pub fn (mut m SortedMap) free() {
	if isnil(m.root) {
		return
	}
	m.root.free()
}

pub fn (m SortedMap) print() {
	println('TODO')
}

// pub fn (m map_string) str() string {
// 	if m.size == 0 {
// 		return '{}'
// 	}
// 	mut sb := strings.new_builder(50)
// 	sb.writeln('{')
// 	for key, val  in m {
// 		sb.writeln('  "$key" => "$val"')
// 	}
// 	sb.writeln('}')
// 	return sb.str()
// }
