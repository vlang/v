// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

import strings

const (
	initial_size       = 2 << 15
	initial_cap        = initial_size - 1
	load_factor        = 0.5
	probe_offset       = u16(256)
	fnv64_prime        = 1099511628211
	fnv64_offset_basis = 14695981039346656037
   fnv32_prime        = u32(16777619)
	fnv32_offset_basis = u32(2166136261)
)

[inline]
fn fnv1a64(data string) u64 {
    mut hash := fnv64_offset_basis
    for i := 0; i < data.len; i++ {
        hash = (hash ^ u64(data[i])) * fnv64_prime
    }
    return hash
}

pub struct map {
	value_bytes	int
mut:
	info &u16
	key_values &KeyValue
	cap        int
pub mut:
	size   int
}

fn new_map(cap, value_bytes int) map {
	return map {
		value_bytes: value_bytes
		info: &u16(calloc(sizeof(u16) * initial_size))
		key_values: &KeyValue(calloc(sizeof(KeyValue) * initial_size))
		cap: initial_cap
		size: 0
	}
}

fn new_map_init(n, value_bytes int, keys &string, values voidptr) map {
	mut out := new_map(n, value_bytes)
	for i in 0 .. n {
		out.set(keys[i], values + i * value_bytes)
	}
	return out
}

struct KeyValue {
	key   string
mut:
	value voidptr
}

fn new_key_value(key string, value voidptr, value_bytes int) KeyValue {
	mut new_e := KeyValue {
		key: key
		value: malloc(value_bytes)
	}
	C.memcpy(new_e.value, value, value_bytes)
	return new_e
} 

fn (m mut map) set(key string, value voidptr) {
	// The load factor is 0.5.
	// It will be adjustable  in the future and with 
	// a higher default settings to lower memory usage.
	if (m.size << 1) == (m.cap - 1) { 
		m.rehash()
	}

	// Hash-function will be swapped for wyhash
	hash := fnv1a64(key)
	mut info := u16((hash >> 56) | probe_offset)
	mut index := hash & m.cap

	// While probe count is less
	for info < m.info[index] {
		index = (index + 1) & m.cap
		info += probe_offset
	}

	// While we might have a match
	for info == m.info[index] {
		if key == m.key_values[index].key {
			C.memcpy(m.key_values[index].value, value, m.value_bytes)
			return
		}
		index = (index + 1) & m.cap
		info += probe_offset
	}

	// Match is not possible anymore.
	// Probe until an empty index is found.
	// Swap when probe count is higher/richer (Robin Hood).
	mut current_key := key
	mut current_value := value
	for m.info[index] != 0 {
		if info > m.info[index] {
			tmp_kv := m.key_values[index] 
			tmp_info := m.info[index]
			m.key_values[index] = new_key_value(current_key, current_value, m.value_bytes)
			m.info[index] = info
			current_key = tmp_kv.key
			current_value = tmp_kv.value
			info = tmp_info
		}
		index = (index + 1) & m.cap
		info += probe_offset
	}

	// Should almost never happen
	if (info & 0xFF00) == 0xFF00 {
		m.rehash()
		m.set(current_key, current_value)
		return
	}

	m.info[index] = info
	m.key_values[index] = new_key_value(current_key, current_value, m.value_bytes)
	m.size++
}

fn (m mut map) rehash() {
	old_cap := m.cap
	m.cap = ((m.cap + 1) << 1) - 1
	mut new_key_values :=  &KeyValue(calloc(sizeof(KeyValue) * (m.cap + 1)))
	mut new_info := &u16(calloc(sizeof(u16) * (m.cap + 1)))
	for i in 0..(old_cap + 1) {
		if m.info[i] != 0 {
			key := m.key_values[i].key
			value := m.key_values[i].value
			hash := fnv1a64(key)
			mut info := u16((hash >> 56) | probe_offset)
			mut index := hash & m.cap
			// While probe count is less
			for info < new_info[index] {
				index = (index + 1) & m.cap
				info += probe_offset
			}

			// While we might have a match
			for info == new_info[index] {
				if key == new_key_values[index].key {
					new_key_values[index].value = value
					return
				}
				index = (index + 1) & m.cap
				info += probe_offset
			}

			// Match is not possible anymore.
			// Probe until an empty index is found.
			// Swap when probe count is higher/richer (Robin Hood).
			mut current_key := key
			mut current_value := value
			for new_info[index] != 0 {
				if info > new_info[index] {
					tmp_kv := new_key_values[index] 
					tmp_info := new_info[index]
					new_key_values[index] = new_key_value(current_key, current_value, m.value_bytes)
					new_info[index] = info
					current_key = tmp_kv.key
					current_value = tmp_kv.value
					info = tmp_info
				}
				index = (index + 1) & m.cap
				info += probe_offset
			}

			// Should almost never happen
			if (info & 0xFF00) == 0xFF00 {
				m.rehash()
				m.set(current_key, current_value)
				return
			}

			new_info[index] = info
			new_key_values[index] = new_key_value(current_key, current_value, m.value_bytes)

		}
	}
	m.key_values = new_key_values
	m.info = new_info
}

pub fn (m &map) keys() []string {
	mut keys := [''].repeat(m.size)
	if m.value_bytes == 0 {
		return keys
	}
	mut j := 0
	for i in 0..(m.cap + 1) {
		if m.info[i] != 0 {
			keys[j] = m.key_values[i].key
			j++
		}
	}
	return keys
}

fn (m map) get(key string, out voidptr) bool {
	hash := fnv1a64(key)
	mut index := hash & m.cap
	mut info := u16((hash >> 56) | probe_offset)

	for info < m.info[index] {
		index = (index + 1) & m.cap
		info += probe_offset
	}

	for info == m.info[index] {
		if key == m.key_values[index].key {
			C.memcpy(out, m.key_values[index].value, m.value_bytes)
			return true
		}
		index = (index + 1) & m.cap
		info += probe_offset
	}
	return false
}


pub fn (m mut map) delete(key string) {
	hash := fnv1a64(key)
	mut index := hash & m.cap
	mut info := u16((hash >> 56) | probe_offset)

	for info < m.info[index] {
		index = (index + 1) & m.cap
		info += probe_offset
	}

	// Perform backwards shifting
	for info == m.info[index] {
		if key == m.key_values[index].key {
			mut old_index := index
			index = (index + 1) & m.cap
			mut current_info := m.info[index]
			for (current_info >> 8) > 1 {
				m.info[old_index] = current_info - probe_offset
				m.key_values[old_index] = m.key_values[index]
				old_index = index
				index = (index + 1) & m.cap
				current_info = m.info[index]
			}
			m.info[old_index] = 0
			m.size--
			return
		}
		index = (index + 1) & m.cap
		info += probe_offset
	}
}

fn (m map) exists(key string) bool {
	if m.value_bytes == 0 {
		return false
	}

	hash := fnv1a64(key)
	mut index := hash & m.cap
	mut info := u16((hash >> 56) | probe_offset)

	for info < m.info[index] {
		index = (index + 1) & m.cap
		info += probe_offset
	}

	for info == m.info[index] {
		if key == m.key_values[index].key {
			return true
		}
		index = (index + 1) & m.cap
		info += probe_offset
	}
	return false
}

pub fn (m map) print() {
	println('<<<<<<<<')
	// for i := 0; i < m.entries.len; i++ {
	// entry := m.entries[i]
	// println('$entry.key => $entry.val')
	// }
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

pub fn (m mut map) free() {
	// if m.root == 0 {
	// 	return
	// }
	// m.root.free()
	// C.free(m.table)
	// C.free(m.keys_table)
}

pub fn (m map_string) str() string {
	if m.size == 0 {
		return '{}'
	}
	mut sb := strings.new_builder(50)
	sb.writeln('{')
	for key, val in m {
		sb.writeln('  "$key" => "$val"')
	}
	sb.writeln('}')
	return sb.str()
}
