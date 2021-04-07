// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
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
// `degree` determines the maximum length of each node.
const (
	degree         = 6
	mid_index      = degree - 1
	max_len        = 2 * degree - 1
	children_bytes = sizeof(voidptr) * (max_len + 1)
)

pub struct SortedMap {
	value_bytes int
mut:
	root &mapnode
pub mut:
	len int
}

struct mapnode {
mut:
	children &voidptr
	len      int
	keys     [11]string  // TODO: Should use `max_len`
	values   [11]voidptr // TODO: Should use `max_len`
}

fn new_sorted_map(n int, value_bytes int) SortedMap { // TODO: Remove `n`
	return SortedMap{
		value_bytes: value_bytes
		root: new_node()
		len: 0
	}
}

fn new_sorted_map_init(n int, value_bytes int, keys &string, values voidptr) SortedMap {
	mut out := new_sorted_map(n, value_bytes)
	for i in 0 .. n {
		unsafe {
			out.set(keys[i], &byte(values) + i * value_bytes)
		}
	}
	return out
}

// The tree is initialized with an empty node as root to
// avoid having to check whether the root is null for
// each insertion.
fn new_node() &mapnode {
	return &mapnode{
		children: 0
		len: 0
	}
}

// This implementation does proactive insertion, meaning
// that splits are done top-down and not bottom-up.
fn (mut m SortedMap) set(key string, value voidptr) {
	mut node := m.root
	mut child_index := 0
	mut parent := &mapnode(0)
	for {
		if node.len == max_len {
			if isnil(parent) {
				parent = new_node()
				m.root = parent
			}
			parent.split_child(child_index, mut node)
			if key == parent.keys[child_index] {
				unsafe {
					C.memcpy(parent.values[child_index], value, m.value_bytes)
				}
				return
			}
			if key < parent.keys[child_index] {
				node = unsafe { &mapnode(parent.children[child_index]) }
			} else {
				node = unsafe { &mapnode(parent.children[child_index + 1]) }
			}
		}
		mut i := 0
		for i < node.len && key > node.keys[i] {
			i++
		}
		if i != node.len && key == node.keys[i] {
			unsafe {
				C.memcpy(node.values[i], value, m.value_bytes)
			}
			return
		}
		if isnil(node.children) {
			mut j := node.len - 1
			for j >= 0 && key < node.keys[j] {
				node.keys[j + 1] = node.keys[j]
				node.values[j + 1] = node.values[j]
				j--
			}
			node.keys[j + 1] = key
			unsafe {
				node.values[j + 1] = malloc(m.value_bytes)
				C.memcpy(node.values[j + 1], value, m.value_bytes)
			}
			node.len++
			m.len++
			return
		}
		parent = node
		child_index = i
		node = unsafe { &mapnode(node.children[child_index]) }
	}
}

fn (mut n mapnode) split_child(child_index int, mut y mapnode) {
	mut z := new_node()
	z.len = mid_index
	y.len = mid_index
	for j := mid_index - 1; j >= 0; j-- {
		z.keys[j] = y.keys[j + degree]
		z.values[j] = y.values[j + degree]
	}
	if !isnil(y.children) {
		z.children = unsafe { &voidptr(malloc(int(children_bytes))) }
		for jj := degree - 1; jj >= 0; jj-- {
			unsafe {
				z.children[jj] = y.children[jj + degree]
			}
		}
	}
	unsafe {
		if isnil(n.children) {
			n.children = &voidptr(malloc(int(children_bytes)))
		}
		n.children[n.len + 1] = n.children[n.len]
	}
	for j := n.len; j > child_index; j-- {
		n.keys[j] = n.keys[j - 1]
		n.values[j] = n.values[j - 1]
		unsafe {
			n.children[j] = n.children[j - 1]
		}
	}
	n.keys[child_index] = y.keys[mid_index]
	n.values[child_index] = y.values[mid_index]
	unsafe {
		n.children[child_index] = voidptr(y)
		n.children[child_index + 1] = voidptr(z)
	}
	n.len++
}

fn (m SortedMap) get(key string, out voidptr) bool {
	mut node := m.root
	for {
		mut i := node.len - 1
		for i >= 0 && key < node.keys[i] {
			i--
		}
		if i != -1 && key == node.keys[i] {
			unsafe {
				C.memcpy(out, node.values[i], m.value_bytes)
			}
			return true
		}
		if isnil(node.children) {
			break
		}
		node = unsafe { &mapnode(node.children[i + 1]) }
	}
	return false
}

fn (m SortedMap) exists(key string) bool {
	if isnil(m.root) { // TODO: find out why root can be nil
		return false
	}
	mut node := m.root
	for {
		mut i := node.len - 1
		for i >= 0 && key < node.keys[i] {
			i--
		}
		if i != -1 && key == node.keys[i] {
			return true
		}
		if isnil(node.children) {
			break
		}
		node = unsafe { &mapnode(node.children[i + 1]) }
	}
	return false
}

fn (n &mapnode) find_key(k string) int {
	mut idx := 0
	for idx < n.len && n.keys[idx] < k {
		idx++
	}
	return idx
}

fn (mut n mapnode) remove_key(k string) bool {
	idx := n.find_key(k)
	if idx < n.len && n.keys[idx] == k {
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
		flag := if idx == n.len { true } else { false }
		if unsafe { &mapnode(n.children[idx]) }.len < degree {
			n.fill(idx)
		}

		mut node := &mapnode(0)
		if flag && idx > n.len {
			node = unsafe { &mapnode(n.children[idx - 1]) }
		} else {
			node = unsafe { &mapnode(n.children[idx]) }
		}
		return node.remove_key(k)
	}
}

fn (mut n mapnode) remove_from_leaf(idx int) {
	for i := idx + 1; i < n.len; i++ {
		n.keys[i - 1] = n.keys[i]
		n.values[i - 1] = n.values[i]
	}
	n.len--
}

fn (mut n mapnode) remove_from_non_leaf(idx int) {
	k := n.keys[idx]
	if unsafe { &mapnode(n.children[idx]) }.len >= degree {
		mut current := unsafe { &mapnode(n.children[idx]) }
		for !isnil(current.children) {
			current = unsafe { &mapnode(current.children[current.len]) }
		}
		predecessor := current.keys[current.len - 1]
		n.keys[idx] = predecessor
		n.values[idx] = current.values[current.len - 1]
		mut node := unsafe { &mapnode(n.children[idx]) }
		node.remove_key(predecessor)
	} else if unsafe { &mapnode(n.children[idx + 1]) }.len >= degree {
		mut current := unsafe { &mapnode(n.children[idx + 1]) }
		for !isnil(current.children) {
			current = unsafe { &mapnode(current.children[0]) }
		}
		successor := current.keys[0]
		n.keys[idx] = successor
		n.values[idx] = current.values[0]
		mut node := unsafe { &mapnode(n.children[idx + 1]) }
		node.remove_key(successor)
	} else {
		n.merge(idx)
		mut node := unsafe { &mapnode(n.children[idx]) }
		node.remove_key(k)
	}
}

fn (mut n mapnode) fill(idx int) {
	if idx != 0 && unsafe { &mapnode(n.children[idx - 1]) }.len >= degree {
		n.borrow_from_prev(idx)
	} else if idx != n.len && unsafe { &mapnode(n.children[idx + 1]) }.len >= degree {
		n.borrow_from_next(idx)
	} else if idx != n.len {
		n.merge(idx)
	} else {
		n.merge(idx - 1)
	}
}

fn (mut n mapnode) borrow_from_prev(idx int) {
	mut child := unsafe { &mapnode(n.children[idx]) }
	mut sibling := unsafe { &mapnode(n.children[idx - 1]) }
	for i := child.len - 1; i >= 0; i-- {
		child.keys[i + 1] = child.keys[i]
		child.values[i + 1] = child.values[i]
	}
	if !isnil(child.children) {
		for i := child.len; i >= 0; i-- {
			unsafe {
				child.children[i + 1] = child.children[i]
			}
		}
	}
	child.keys[0] = n.keys[idx - 1]
	child.values[0] = n.values[idx - 1]
	if !isnil(child.children) {
		unsafe {
			child.children[0] = sibling.children[sibling.len]
		}
	}
	n.keys[idx - 1] = sibling.keys[sibling.len - 1]
	n.values[idx - 1] = sibling.values[sibling.len - 1]
	child.len++
	sibling.len--
}

fn (mut n mapnode) borrow_from_next(idx int) {
	mut child := unsafe { &mapnode(n.children[idx]) }
	mut sibling := unsafe { &mapnode(n.children[idx + 1]) }
	child.keys[child.len] = n.keys[idx]
	child.values[child.len] = n.values[idx]
	if !isnil(child.children) {
		unsafe {
			child.children[child.len + 1] = sibling.children[0]
		}
	}
	n.keys[idx] = sibling.keys[0]
	n.values[idx] = sibling.values[0]
	for i := 1; i < sibling.len; i++ {
		sibling.keys[i - 1] = sibling.keys[i]
		sibling.values[i - 1] = sibling.values[i]
	}
	if !isnil(sibling.children) {
		for i := 1; i <= sibling.len; i++ {
			unsafe {
				sibling.children[i - 1] = sibling.children[i]
			}
		}
	}
	child.len++
	sibling.len--
}

fn (mut n mapnode) merge(idx int) {
	mut child := unsafe { &mapnode(n.children[idx]) }
	sibling := unsafe { &mapnode(n.children[idx + 1]) }
	child.keys[mid_index] = n.keys[idx]
	child.values[mid_index] = n.values[idx]
	for i in 0 .. sibling.len {
		child.keys[i + degree] = sibling.keys[i]
		child.values[i + degree] = sibling.values[i]
	}
	if !isnil(child.children) {
		for i := 0; i <= sibling.len; i++ {
			unsafe {
				child.children[i + degree] = sibling.children[i]
			}
		}
	}
	for i := idx + 1; i < n.len; i++ {
		n.keys[i - 1] = n.keys[i]
		n.values[i - 1] = n.values[i]
	}
	for i := idx + 2; i <= n.len; i++ {
		unsafe {
			n.children[i - 1] = n.children[i]
		}
	}
	child.len += sibling.len + 1
	n.len--
	// free(sibling)
}

pub fn (mut m SortedMap) delete(key string) {
	if m.root.len == 0 {
		return
	}

	removed := m.root.remove_key(key)
	if removed {
		m.len--
	}

	if m.root.len == 0 {
		// tmp := t.root
		if isnil(m.root.children) {
			return
		} else {
			m.root = unsafe { &mapnode(m.root.children[0]) }
		}
		// free(tmp)
	}
}

// Insert all keys of the subtree into array `keys`
// starting at `at`. Keys are inserted in order.
fn (n &mapnode) subkeys(mut keys []string, at int) int {
	mut position := at
	if !isnil(n.children) {
		// Traverse children and insert
		// keys inbetween children
		for i in 0 .. n.len {
			child := unsafe { &mapnode(n.children[i]) }
			position += child.subkeys(mut keys, position)
			keys[position] = n.keys[i]
			position++
		}
		// Insert the keys of the last child
		child := unsafe { &mapnode(n.children[n.len]) }
		position += child.subkeys(mut keys, position)
	} else {
		// If leaf, insert keys
		for i in 0 .. n.len {
			keys[position + i] = n.keys[i]
		}
		position += n.len
	}
	// Return # of added keys
	return position - at
}

pub fn (m &SortedMap) keys() []string {
	mut keys := []string{len: m.len}
	if isnil(m.root) || m.root.len == 0 {
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
// 	if m.len == 0 {
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
