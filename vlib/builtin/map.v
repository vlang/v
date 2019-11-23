// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

import strings

pub struct map {
	element_size int
	root      &mapnode
pub:
	size int
}

struct mapnode {
	left &mapnode
	right &mapnode
	is_empty bool // set by delete()
	key string
	val voidptr
}

fn new_map(cap, elm_size int) map {
	res := map {
		element_size: elm_size
		root: 0
	}
	return res
}

// `m := { 'one': 1, 'two': 2 }`
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

fn new_node(key string, val voidptr, element_size int) &mapnode {
	new_e := &mapnode {
		key: key
		val: malloc(element_size)
		left: 0
		right: 0
	}
	C.memcpy(new_e.val, val, element_size)
	return new_e
}

fn (m mut map) insert(n mut mapnode, key string, val voidptr) {
	if n.key == key {
		C.memcpy(n.val, val, m.element_size)
		return
	}
	if n.key > key {
		if n.left == 0 {
			n.left = new_node(key, val, m.element_size)
			m.size++
		}  else {
			m.insert(mut n.left, key, val)
		}
		return
	}
	if n.right == 0 {
		n.right = new_node(key, val, m.element_size)
		m.size++
	}  else {
		m.insert(mut n.right, key, val)
	}
}

fn (n & mapnode) find(key string, out voidptr, element_size int) bool{
	if n.key == key {
		C.memcpy(out, n.val, element_size)
		return true
	}
	else if n.key > key {
		if n.left == 0 {
			return false
		}  else {
			return n.left.find(key, out, element_size)
		}
	}
	else {
		if n.right == 0 {
			return false
		}  else {
			return n.right.find(key, out, element_size)
		}
	}
}

// same as `find`, but doesn't return a value. Used by `exists`
fn (n & mapnode) find2(key string, element_size int) bool{
	if n.key == key && !n.is_empty {
		return true
	}
	else if n.key > key {
		if isnil(n.left) {
			return false
		}  else {
			return n.left.find2(key, element_size)
		}
	}
	else {
		if isnil(n.right) {
			return false
		}  else {
			return n.right.find2(key, element_size)
		}
	}
}

fn (m mut map) set(key string, val voidptr) {
	if isnil(m.root) {
		m.root = new_node(key, val, m.element_size)
		m.size++
		return
	}
	m.insert(mut m.root, key, val)
}

/*
fn (m map) bs(query string, start, end int, out voidptr) {
	// println('bs "$query" $start -> $end')
	mid := start + ((end - start) / 2)
	if end - start == 0 {
		last := m.entries[end]
		C.memcpy(out, last.val, m.element_size)
		return
	}
	if end - start == 1 {
		first := m.entries[start]
		C.memcpy(out, first.val, m.element_size)
		return
	}
	if mid >= m.entries.len {
		return
	}
	mid_msg := m.entries[mid]
	// println('mid.key=$mid_msg.key')
	if query < mid_msg.key {
		m.bs(query, start, mid, out)
		return
	}
	m.bs(query, mid, end, out)
}
*/

fn preorder_keys(node &mapnode, keys mut []string, key_i int) int {
	mut i := key_i
	if !node.is_empty {
		keys[i] = node.key
		i++
	}
	if !isnil(node.left) {
		i = preorder_keys(node.left, mut keys, i)
	}
	if !isnil(node.right) {
		i = preorder_keys(node.right, mut keys, i)
	}
	return i
}

pub fn (m &map) keys() []string {
	mut keys := [''].repeat(m.size)
	if isnil(m.root) {
		return keys
	}
	preorder_keys(m.root, mut keys, 0)
	return keys
}

fn (m map) get(key string, out voidptr) bool {
	//println('g')
	if m.root == 0 {
		return false
	}
	return m.root.find(key, out, m.element_size)
}

pub fn (n mut mapnode) delete(key string, element_size int) {
	if n.key == key {
		C.memset(n.val, 0, element_size)
		n.is_empty = true
		return
	}
	else if n.key > key {
		if isnil(n.left) {
			return
		}  else {
			n.left.delete(key, element_size)
		}
	}
	else {
		if isnil(n.right) {
			return
		}  else {
			n.right.delete(key, element_size)
		}
	}
}

pub fn (m mut map) delete(key string) {
	if m.exists(key) {
		m.root.delete(key, m.element_size)
		m.size--
	}
}

fn (m map) exists(key string) bool {
	return !isnil(m.root) && m.root.find2(key, m.element_size)
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

fn (n mut mapnode) free() {
	if n.val != 0 {
		free(n.val)
	}	
	if n.left != 0 {
		n.left.free()
	}	
	if n.right != 0 {
		n.right.free()
	}	
	free(n)
}

pub fn (m mut map) free() {
	if m.root == 0 {
		return
	}	
	m.root.free()
	// C.free(m.table)
	// C.free(m.keys_table)
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
