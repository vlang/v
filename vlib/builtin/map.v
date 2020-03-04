// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

import (
	strings
	hash.wyhash
)

const (
	// Number of bits from the hash stored for each entry
	hashbits = 24
	// Number of bits from the hash stored for rehasing
	cached_hashbits = 16
	// Initial log-number of buckets in the hashtable
	init_log_capicity = 5
	// Initial number of buckets in the hashtable
	init_capicity = 1<<init_log_capicity
	// Initial load-factor
	init_load_factor = 0.8
	// Initial range cap
	init_range_cap = init_capicity - 1
	// Bitmask to select all the hashbits
	hash_mask = u32(0x00FFFFFF)
	// Used for incrementing the probe-count
	probe_inc = u32(0x01000000)
	// Bitmask for maximum probe count
	max_probe = u32(0xFF000000)
)

pub struct map {
	// Byte size of value
	value_bytes int
mut:
	// Index of the highest index in the hashtable
	range_cap   u32
	// Number of cached hashbits left for rehasing
	window      byte
	// Used for right-shifting out used hashbits
	shift       byte
	// Pointer to Key-value memory
	key_values  &KeyValue
	// Pointer to probe_hash memory. Each Key-value has a
	// corresponding probe_hash-DWORD. Upper-bits are the
	// probe-count and lower-bits are bits from the hash.
	probe_hash  &u32
	// Measure that decides when to increase the capacity
	load_factor f32
pub mut:
	// Number of key-values currently in the hashmap
	size        int
}

struct KeyValue {
	key   string
mut:
	value voidptr
}

fn new_map(n, value_bytes int) map {
	probe_hash_bytes := sizeof(u32) * init_capicity
	key_value_bytes := sizeof(KeyValue) * init_capicity
	memory := vcalloc(key_value_bytes + probe_hash_bytes)
	return map{
		value_bytes: value_bytes
		range_cap: init_range_cap
		shift: init_log_capicity
		window: cached_hashbits
		key_values: &KeyValue(memory)
		probe_hash: &u32(memory + key_value_bytes)
		load_factor: init_load_factor
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

fn (m mut map) set(key string, value voidptr) {
	// load_factor can be adjusted.
	if (f32(m.size) / f32(m.range_cap)) > m.load_factor {
		m.expand()
	}
	hash := wyhash.wyhash_c(key.str, u64(key.len), 0)
	mut probe_hash := u32(((hash>>m.shift) & hash_mask) | probe_inc)
	mut index := hash & m.range_cap
	// While probe count is less
	for probe_hash < m.probe_hash[index] {
		index = (index + 1) & m.range_cap
		probe_hash += probe_inc
	}
	// While we might have a match
	for probe_hash == m.probe_hash[index] {
		if key == m.key_values[index].key {
			C.memcpy(m.key_values[index].value, value, m.value_bytes)
			return
		}
		index = (index + 1) & m.range_cap
		probe_hash += probe_inc
	}
	// Match is not possible anymore.
	// Probe until an empty index is found.
	// Swap when probe count is higher/richer (Robin Hood).
	mut current_kv := KeyValue{
		key:key
		value:malloc(m.value_bytes)
	}
	C.memcpy(current_kv.value, value, m.value_bytes)
	for m.probe_hash[index] != 0 {
		if probe_hash > m.probe_hash[index] {
			// Swap probe_hash
			tmp_probe_hash := m.probe_hash[index]
			m.probe_hash[index] = probe_hash
			probe_hash = tmp_probe_hash
			// Swap KeyValue
			tmp_kv := m.key_values[index]
			m.key_values[index] = current_kv
			current_kv = tmp_kv
		}
		index = (index + 1) & m.range_cap
		probe_hash += probe_inc
	}
	// Should almost never happen
	if (probe_hash & max_probe) == max_probe {
		m.expand()
		m.set(current_kv.key, current_kv.value)
		return
	}
	m.probe_hash[index] = probe_hash
	m.key_values[index] = current_kv
	m.size++
}

fn (m mut map) expand() {
	old_range_cap := m.range_cap
	// double the size of the hashmap
	m.range_cap = ((m.range_cap + 1)<<1) - 1
	// check if no hashbits are left
	if m.window == 0 {
		m.shift += cached_hashbits
		m.rehash(old_range_cap)
		m.window = cached_hashbits
	}
	else {
		m.cached_rehash(old_range_cap)
	}
	m.window--
}

fn (m mut map) rehash(old_range_cap u32) {
	probe_hash_bytes := sizeof(u32) * (m.range_cap + 1)
	key_value_bytes := sizeof(KeyValue) * (m.range_cap + 1)
	memory := vcalloc(probe_hash_bytes + key_value_bytes)
	mut new_key_values := &KeyValue(memory)
	mut new_probe_hash := &u32(memory + key_value_bytes)
	for i := u32(0); i < old_range_cap + 1; i++ {
		if m.probe_hash[i] != 0 {
			mut kv := m.key_values[i]
			hash := wyhash.wyhash_c(kv.key.str, u64(kv.key.len), 0)
			mut probe_hash := u32(((hash>>m.shift) & hash_mask) | probe_inc)
			mut index := hash & m.range_cap
			// While probe count is less
			for probe_hash < new_probe_hash[index] {
				index = (index + 1) & m.range_cap
				probe_hash += probe_inc
			}
			// Probe until an empty index is found.
			// Swap when probe count is higher/richer (Robin Hood).
			for new_probe_hash[index] != 0 {
				if probe_hash > new_probe_hash[index] {
					// Swap probe_hash
					tmp_probe_hash := new_probe_hash[index]
					new_probe_hash[index] = probe_hash
					probe_hash = tmp_probe_hash
					// Swap KeyValue
					tmp_kv := new_key_values[index]
					new_key_values[index] = kv
					kv = tmp_kv
				}
				index = (index + 1) & m.range_cap
				probe_hash += probe_inc
			}
			// Should almost never happen
			if (probe_hash & max_probe) == max_probe {
				m.expand()
				m.set(kv.key, kv.value)
				return
			}
			new_probe_hash[index] = probe_hash
			new_key_values[index] = kv
		}
	}
	unsafe{
		free(m.key_values)
	}
	m.key_values = new_key_values
	m.probe_hash = new_probe_hash
}

fn (m mut map) cached_rehash(old_range_cap u32) {
	probe_hash_bytes := sizeof(u32) * (m.range_cap + 1)
	key_value_bytes := sizeof(KeyValue) * (m.range_cap + 1)
	memory := vcalloc(probe_hash_bytes + key_value_bytes)
	mut new_probe_hash := &u32(memory + key_value_bytes)
	mut new_key_values := &KeyValue(memory)
	for i := u32(0); i < old_range_cap + 1; i++ {
		if m.probe_hash[i] != 0 {
			mut kv := m.key_values[i]
			mut probe_hash := m.probe_hash[i]
			original := u64(i - ((probe_hash>>hashbits) - 1)) & (m.range_cap>>1)
			hash := original | (probe_hash<<m.shift)
			probe_hash = (probe_hash & hash_mask) | probe_inc
			mut index := hash & m.range_cap
			// While probe count is less
			for probe_hash < new_probe_hash[index] {
				index = (index + 1) & m.range_cap
				probe_hash += probe_inc
			}
			// Probe until an empty index is found.
			// Swap when probe count is higher/richer (Robin Hood).
			for new_probe_hash[index] != 0 {
				if probe_hash > new_probe_hash[index] {
					// Swap probe_hash
					tmp_probe_hash := new_probe_hash[index]
					new_probe_hash[index] = probe_hash
					probe_hash = tmp_probe_hash
					// Swap KeyValue
					tmp_kv := new_key_values[index]
					new_key_values[index] = kv
					kv = tmp_kv
				}
				index = (index + 1) & m.range_cap
				probe_hash += probe_inc
			}
			// Should almost never happen
			if (probe_hash & max_probe) == max_probe {
				m.expand()
				m.set(kv.key, kv.value)
				return
			}
			new_probe_hash[index] = probe_hash
			new_key_values[index] = kv
		}
	}
	unsafe{
		free(m.key_values)
	}
	m.key_values = new_key_values
	m.probe_hash = new_probe_hash
}

pub fn (m mut map) delete(key string) {
	hash := wyhash.wyhash_c(key.str, u64(key.len), 0)
	mut index := hash & m.range_cap
	mut probe_hash := u32(((hash>>m.shift) & hash_mask) | probe_inc)
	for probe_hash < m.probe_hash[index] {
		index = (index + 1) & m.range_cap
		probe_hash += probe_inc
	}
	// Perform backwards shifting
	for probe_hash == m.probe_hash[index] {
		if key == m.key_values[index].key {
			mut old_index := index
			index = (index + 1) & m.range_cap
			mut current_probe_hash := m.probe_hash[index]
			for (current_probe_hash>>hashbits) > 1 {
				m.probe_hash[old_index] = current_probe_hash - probe_inc
				m.key_values[old_index] = m.key_values[index]
				old_index = index
				index = (index + 1) & m.range_cap
				current_probe_hash = m.probe_hash[index]
			}
			m.probe_hash[old_index] = 0
			m.size--
			return
		}
		index = (index + 1) & m.range_cap
		probe_hash += probe_inc
	}
}

fn (m map) get(key string, out voidptr) bool {
	hash := wyhash.wyhash_c(key.str, u64(key.len), 0)
	mut index := hash & m.range_cap
	mut probe_hash := u32(((hash>>m.shift) & hash_mask) | probe_inc)
	for probe_hash < m.probe_hash[index] {
		index = (index + 1) & m.range_cap
		probe_hash += probe_inc
	}
	for probe_hash == m.probe_hash[index] {
		if key == m.key_values[index].key {
			C.memcpy(out, m.key_values[index].value, m.value_bytes)
			return true
		}
		index = (index + 1) & m.range_cap
		probe_hash += probe_inc
	}
	return false
}

fn (m map) exists(key string) bool {
	if m.value_bytes == 0 {
		return false
	}
	hash := wyhash.wyhash_c(key.str, u64(key.len), 0)
	mut index := hash & m.range_cap
	mut probe_hash := u32(((hash>>m.shift) & hash_mask) | probe_inc)
	for probe_hash < m.probe_hash[index] {
		index = (index + 1) & m.range_cap
		probe_hash += probe_inc
	}
	for probe_hash == m.probe_hash[index] {
		if key == m.key_values[index].key {
			return true
		}
		index = (index + 1) & m.range_cap
		probe_hash += probe_inc
	}
	return false
}

pub fn (m &map) keys() []string {
	mut keys := [''].repeat(m.size)
	if m.value_bytes == 0 {
		return keys
	}
	mut j := 0
	for i := u32(0); i < m.range_cap + 1; i++ {
		if m.probe_hash[i] != 0 {
			keys[j] = m.key_values[i].key
			j++
		}
	}
	return keys
}

pub fn (m mut map) set_load_factor(new_load_factor f32) {
	if new_load_factor > 1.0 {
		m.load_factor = 1.0
	}
	else if new_load_factor < 0.1 {
		m.load_factor = 0.1
	}
	else {
		m.load_factor = new_load_factor
	}
}

pub fn (m mut map) free() {
	unsafe{
		free(m.key_values)
	}
}

pub fn (m map) print() {
	println('TODO')
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
