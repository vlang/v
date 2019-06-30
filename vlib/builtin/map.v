// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

struct map {
	// cap          int
	// keys         []string
	// table        byteptr
	// keys_table   *string
	// table *Entry
	element_size int
	// collisions   []Entry
pub:
	entries      []Entry
	is_sorted    bool
}

struct Entry {
pub:
	key string
	val voidptr
	// linked list for collisions
	// next *Entry
}

fn new_map(cap, elm_size int) map {
	res := map {
		// len: len,
		element_size: elm_size
		// entries:
		// keys: []string
	}
	return res
}

fn (m &map) new_entry(key string, val voidptr) Entry {
	new_e := Entry {
		key: key
		val: malloc(m.element_size)
		// next: 0
	}
	C.memcpy(new_e.val, val, m.element_size)
	return new_e
}

fn (m mut map) _set(key string, val voidptr) {
	e := m.new_entry(key, val)
	for i := 0; i < m.entries.len; i++ {
		entry := m.entries[i]
		if entry.key == key {
			// e := Entry2{key: key, val: val}
			m.entries[i] = e
			return
		}
	}
	m.entries << e// m.new_entry(key, val)
	m.is_sorted = false
}

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

fn compare_map(a, b *Entry) int {
	if a.key < b.key {
		return -1
	}
	if a.key > b.key {
		return 1
	}
	return 0
}

pub fn (m mut map) sort() {
	m.entries.sort_with_compare(compare_map)
	m.is_sorted = true
}

pub fn (m map) keys() []string {
	mut keys := []string{}
	for i := 0; i < m.entries.len; i++ {
		entry := m.entries[i]
		keys << entry.key
	}
	return keys
}

fn (m map) get(key string, out voidptr) bool {
	if m.is_sorted {
		// println('\n\nget "$key" sorted')
		m.bs(key, 0, m.entries.len, out)
		return true
	}
	for i := 0; i < m.entries.len; i++ {
		entry := m.entries[i]
		if entry.key == key {
			C.memcpy(out, entry.val, m.element_size)
			return true
		}
	}
	return false
}

pub fn (m map) exists(key string) bool {
	for i := 0; i < m.entries.len; i++ {
		entry := m.entries[i]
		if entry.key == key {
			return true
		}
	}
	return false
}

pub fn (m map) print() {
	println('<<<<<<<<')
	for i := 0; i < m.entries.len; i++ {
		// entry := m.entries[i]
		// println('$entry.key => $entry.val')
	}
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
	if m.entries.len == 0 {
		return '{}'
	}
	// TODO use bytes buffer
	mut s := '{\n'
	for entry in m.entries {
		val := m[entry.key]
		s += '  "$entry.key" => "$val"\n'
	}
	s += '}'
	return s
}
