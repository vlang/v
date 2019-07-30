// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

struct map {
	element_size int
	root      *Node 
	_keys []string // used by `keys()` TODO remove this from map struct, 
	key_i int      // store in a separate var 
pub: 
	size int 
}

struct Node {
	left *Node
	right *Node 
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

fn new_node(key string, val voidptr, element_size int) *Node {
	new_e := &Node {
		key: key
		val: malloc(element_size)
		left: 0
		right: 0 
	}
	C.memcpy(new_e.val, val, element_size)
	return new_e
}

fn (m mut map) insert(n mut Node, key string, val voidptr) {
	if n.key == key {
		C.memcpy(n.val, val, m.element_size)
		return 
	} 
	if n.key > key {
		if isnil(n.left) {
			n.left = new_node(key, val, m.element_size) 
			m.size++ 
		}  else { 
			m.insert(mut n.left, key, val) 
		} 
		return 
	} 
	if isnil(n.right) {
		n.right = new_node(key, val, m.element_size)  
		m.size++ 
	}  else { 
		m.insert(mut n.right, key, val) 
	} 
} 

fn (n & Node) find(key string, out voidptr, element_size int) bool{ 
	if n.key == key {
		C.memcpy(out, n.val, element_size)
		return true 
	} 
	else if n.key > key {
		if isnil(n.left) {
			return false 
		}  else { 
			return n.left.find(key, out, element_size) 
		} 
	} 
	else {
		if isnil(n.right) {
			return false 
		}  else { 
			return n.right.find(key, out, element_size) 
		} 
	} 
	return false 
} 

// same as `find`, but doesn't return a value. Used by `exists` 
fn (n & Node) find2(key string, element_size int) bool{ 
	if n.key == key {
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
	return false 
} 

fn (m mut map) _set(key string, val voidptr) {
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

fn (m mut map) preorder_keys(node &Node) { 
	m._keys[m.key_i] = node.key 
	m.key_i++ 
	if !isnil(node.left) { 
		m.preorder_keys(node.left) 
	} 
	if !isnil(node.right) { 
		m.preorder_keys(node.right) 
	} 
} 

pub fn (m mut map) keys() []string {
	m._keys = [''; m.size] 
	m.key_i = 0 
	if isnil(m.root) {
		return m._keys
	} 
	m.preorder_keys(m.root) 
	return m._keys
}

fn (m map) get(key string, out voidptr) bool {
	if isnil(m.root) {
		return false 
	} 
	return m.root.find(key, out, m.element_size) 
}

pub fn (n mut Node) delete(key string, element_size int) { 
	if n.key == key {
		C.memset(n.val, 0, element_size)
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
	m.root.delete(key, m.element_size) 
	m.size-- 
} 

pub fn (m map) exists(key string) bool {
	panic('map.exists(key) was removed from the language. Use `key in map` instead.') 
	return false 
}

fn (m map) _exists(key string) bool {
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

pub fn (m map) free() {
	// C.free(m.table)
	// C.free(m.keys_table)
}

pub fn (m map_string) str() string {
	// return 'not impl'
	if m.size == 0 {
		return '{}'
	}
	// TODO use bytes buffer
	mut s := '{\n'
	//for key, val  in m { 
		//val := m[entry.key]
		//s += '  "$entry.key" => "$val"\n'
	//}
	s += '}'
	return s
}
