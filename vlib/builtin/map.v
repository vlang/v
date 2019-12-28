// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

import strings

// B-trees are balanced search trees with all leaves
// at the same level. B-trees are generally faster than 
// binary search trees due to the better locality of 
// reference, since multiple keys are stored in one node.

// The number for `degree` has been picked
// through vigorous benchmarking, but can be changed
// to any number > 1. `degree` determines the size
// of each node.

const (
	degree = 6
	mid_index = degree - 1
	max_length = 11// should be 2 * degree - 1
	min_length = degree - 1
	children_size = sizeof(voidptr) * (max_length + 1)
)

pub struct map {
	element_size int
mut:
	root &mapnode
pub mut:
	size int
}

struct mapnode {
mut:
  	keys     [11]string
	values   [11]voidptr
	children &voidptr
	size     int
}

fn new_map(cap, elm_size int) map {
	return map {
		element_size: elm_size
		root: new_node()
		size: 0
	}
}

// `m := { 'one': 1, 'two': 2 }`
fn new_map_init(cap, elm_size int, keys &string, vals voidptr) map {
	mut res := map {
		element_size: elm_size
		root: new_node()
		size: 0
	}
	for i in 0 .. cap {
		res.set(keys[i], vals + i * elm_size)
	}
	return res
}

fn new_node() &mapnode {
	return &mapnode {
		children: 0
		size: 0
	}
}

fn (m mut map) set(key string, value voidptr) {
	mut node := m.root
	mut child_index := 0
	mut parent := &mapnode(0)
	for {
		if node.size == max_length {
			if parent == 0 {
				parent = new_node()
				m.root = parent
			}
			parent.split_child(child_index, mut node)
			if key == parent.keys[child_index] {
				C.memcpy(parent.values[child_index], value, m.element_size)
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
			C.memcpy(node.values[i], value, m.element_size)
			return
		}
		if node.children == 0 {
			mut j := node.size - 1
			for j >= 0 && key < node.keys[j] {
				node.keys[j + 1] = node.keys[j]
				node.values[j + 1] = node.values[j]
				j--
			}
			node.keys[j + 1] = key
			node.values[j + 1] = malloc(m.element_size)
			C.memcpy(node.values[j + 1], value, m.element_size)
			node.size++
			m.size++
			return
		}
		parent = node
		child_index = i
		node = &mapnode(node.children[child_index])
	}
}

fn (n mut mapnode) split_child(child_index int, y mut mapnode) {
	mut z := new_node()
	z.size = mid_index
	y.size = mid_index
	for j := mid_index - 1; j >= 0; j-- {
		z.keys[j] = y.keys[j + degree]
		z.values[j] = y.values[j + degree]
	}
	if y.children != 0 {
		z.children = &voidptr(malloc(children_size))
		for j := degree - 1; j >= 0; j-- {
			z.children[j] = y.children[j + degree]
		}
	}
	if n.children == 0 {
		n.children = &voidptr(malloc(children_size))
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

fn (m map) get(key string, out voidptr) bool {
	mut node := m.root
	for {
		mut i := node.size - 1
		for i >= 0 && key < node.keys[i] { i-- }
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

fn (m map) exists(key string) bool {
	mut node := m.root
	for {
		mut i := node.size - 1
		for i >= 0 && key < node.keys[i] { i-- }
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

fn (n mut mapnode) remove_key(k string) {
	idx := n.find_key(k)
	if idx < n.size && n.keys[idx] == k {
		if n.children == 0 {
			n.remove_from_leaf(idx)
		} else {
			n.remove_from_non_leaf(idx)
		}
	} else {
		if n.children == 0 {
			return
		}
		flag := if idx == n.size {true} else {false}
		if (&mapnode(n.children[idx])).size < degree {
			n.fill(idx)
		}

		if flag && idx > n.size {
			(&mapnode(n.children[idx - 1])).remove_key(k)
		} else {
			(&mapnode(n.children[idx])).remove_key(k)
		}
	}
}

fn (n mut mapnode) remove_from_leaf(idx int) {
	for i := idx + 1; i < n.size; i++ {
		n.keys[i - 1] = n.keys[i]
		n.values[i - 1] = n.values[i]
	}
	n.size--
}

fn (n mut mapnode) remove_from_non_leaf(idx int) {
	k := n.keys[idx]
	if &mapnode(n.children[idx]).size >= degree {
		mut current := &mapnode(n.children[idx])
		for current.children != 0 {
			current = &mapnode(current.children[current.size])
		}
		predecessor := current.keys[current.size - 1]
		n.keys[idx] = predecessor
		n.values[idx] = current.values[current.size - 1]
		(&mapnode(n.children[idx])).remove_key(predecessor)
	} else if &mapnode(n.children[idx + 1]).size >= degree {
		mut current := &mapnode(n.children[idx + 1])
		for current.children != 0 {
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
		child.values[i + 1] = child.values[i] 
	}
	if child.children != 0 { 
		for i := child.size; i >= 0; i-- {
			child.children[i + 1] = child.children[i] 
		}
	}
	child.keys[0] = n.keys[idx - 1] 
	child.values[0] = n.values[idx - 1] 
	if child.children != 0 {
		child.children[0] = sibling.children[sibling.size]
	}
	n.keys[idx - 1] = sibling.keys[sibling.size - 1]
	n.values[idx - 1] = sibling.values[sibling.size - 1]
	child.size++ 
	sibling.size-- 
}

fn (n mut mapnode) borrow_from_next(idx int) {
	mut child := &mapnode(n.children[idx])
	mut sibling := &mapnode(n.children[idx + 1])
	child.keys[child.size] = n.keys[idx]
	child.values[child.size] = n.values[idx]
	if child.children != 0 {
		child.children[child.size + 1] = sibling.children[0]
	}
	n.keys[idx] = sibling.keys[0]
	n.values[idx] = sibling.values[0]
	for i := 1; i < sibling.size; i++ {
		sibling.keys[i - 1] = sibling.keys[i]
		sibling.values[i - 1] = sibling.values[i]
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
	child.values[min_length] = n.values[idx]
	for i := 0; i < sibling.size; i++ {
		child.keys[i + degree] = sibling.keys[i]
		child.values[i + degree] = sibling.values[i]
	}
	if child.children != 0 {
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

pub fn (m mut map) delete(key string) {
	if m.root.size == 0 {
		return
	}
	
	// This is slow
	if m.exists(key) {
		m.size--
	} else {
		return
	}

	m.root.remove_key(key)
	
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

struct ArrayReference {
mut:
	array []string
}

pub fn (m &map) keys() []string {
	mut keys := ArrayReference{}
	return m.root.preoder_keys(mut keys)
}

fn (n mut mapnode) free() {
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

pub fn (m mut map) free() {
	if m.root == 0 {
		return
	}
	m.root.free()
}

pub fn (m map) print() {
	println('<<<<<<<<')
	//for i := 0; i < m.entries.len; i++ {
		// entry := m.entries[i]
		// println('$entry.key => $entry.val')
	//}
	/*
	for i := 0; i < m.cap * m.element_size; i++ {
		b := m.table[i]
		print('$i: ')
		C.printf('%02x', b)
		println('')
	}
*/
	println('>>>>>>>>>>')
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