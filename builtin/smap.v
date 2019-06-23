// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

struct Entry2 {
	key string
	val string
}

struct smap {
	entries   []Entry2
	is_sorted bool
}

fn new_smap() smap {
	res := smap{}
	return res
}

fn (m mut smap) set(key string, val string) {
	/* 
	for i := 0; i < m.entries.len; i++ {
		entry := m.entries[i]
		if entry.key == key {
			e := Entry2{key: key, val: val}
			m.entries[i] = e
			return
		}
	}
*/
	e := Entry2{key: key, val: val}
	m.entries << e
}

fn (m smap) get(key string) string {
	if m.is_sorted {
		return m.bs(key, 0, m.entries.len)
	}
	for i := 0; i < m.entries.len; i++ {
		entry := m.entries[i]
		if entry.key == key {
			return entry.val
		}
	}
	return ''
}

fn (m smap) bs(query string, start, end int) string {
	mid := start + ((end - start) / 2)
	if end - start == 0 {
		last := m.entries[end]
		return last.val
	}
	if end - start == 1 {
		first := m.entries[start]
		return first.val
	}
	if mid >= m.entries.len {
		return ''
	}
	mid_msg := m.entries[mid]
	if query < mid_msg.key {
		return m.bs(query, start, mid)
	}
	return m.bs(query, mid, end)
}

fn compare_smap(a, b *Entry2) int {
	if a.key < b.key {
		return -1
	}
	if a.key > b.key {
		return 1
	}
	return 0
}

fn (m mut smap) sort() {
	m.entries.sort_with_compare(compare_smap)
	m.is_sorted = true
}

fn (m smap) free() {
	// m.entries.free()
}

fn (m smap) str() string {
	if m.entries.len == 0 {
		return '{}'
	}
	// TODO use bytes buffer
	mut s := '{\n'
	for entry in m.entries {
		s += '  "$entry.key" => "$entry.val"\n'
	}
	s += '}'
	return s
}

