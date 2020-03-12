// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

import (
	strings
	hash.wyhash
)

/*
This is a very fast hashmap implementation. It has several properties 
that in combination makes it very fast. Here is a short explanation of
each property. After reading this you should have a basic understanding
of how it works:

1.	Hash-function (Wyhash). Wyhash is the fastest hash-function
	passing SMHasher, so it was an easy choice.

2.	Open addressing (Robin Hood Hashing). With this method a hash 
	collision is resolved by probing. As opposed to linear probing,
	Robin Hood hashing has simple but clever twist: As new keys are 
	inserted, old keys are shifted around in a way such that all keys 
	stay reasonably close to the slot they originally hash to.

3.	Memory layout. Key-value pairs are stored in a "DenseArray",
	with an average of rougly 6.25% unused memory as opposed to
	most other dynamic array implementation with a growth factor
	of 1.5 or 2. The key-values keep their index in the array -
	they and are not probed. Instead, this implementation uses another
	array "metas" storing "metas" (meta-data). Each Key-value has
	a corresponding meta. A meta stores a reference to its key-value, and
	its index in "metas" is determined by the hash of the key and probing.
	A meta also stores bits from the hash (for faster rehashing etc.)
	and how far away it is from the index it was originally hashed to 
	(probe count).  

	meta (64 bit) = probe_count (8 bits) | hashbits (24 bits) | kv_index (32 bit)
	metas = [meta, 0, meta, 0, meta, meta, meta, 0, ...]
	key_values = [kv, kv, kv, kv, kv, ...]

4.	Power of two. TODO: explain
5.	Extra metas. TODO: explain
6.	Cached rehashing TODO: explain
7.	Load-factor. TODO: explain
8.	Deletion. TODO: explain
*/

const (
	// Number of bits from the hash stored for each entry
	hashbits = 24
	// Number of bits from the hash stored for rehasing
	cached_hashbits = 16
	// Initial log-number of buckets in the hashtable
	init_log_capicity = 5
	// Initial number of buckets in the hashtable
	init_capicity = 1 << init_log_capicity
	// Initial max load-factor
	init_max_load_factor = 0.8
	// Minimum Load-factor. 
	// Number is picked to make delete O(1) amortized
	min_load_factor = 0.3
	// Initial range cap
	init_cap = init_capicity - 2
	// Used for incrementing `extra_metas` when max
	// probe count is too high, to avoid overflow
	extra_metas_inc = 4
	// Bitmask to select all the hashbits
	hash_mask = u32(0x00FFFFFF)
	// Used for incrementing the probe-count
	probe_inc = u32(0x01000000) 
	// Bitmask for maximum probe count
	max_probe = u32(0xFF000000)
)

struct KeyValue {
	key   string
mut:
	value voidptr
}

// Dynamic array with very low growth factor
struct DenseArray {
mut:
	data &KeyValue
	cap  u32
	size u32
}

[inline]
fn new_dense_array() DenseArray {
	unsafe {
		return DenseArray {
			data: &KeyValue(malloc(8 * sizeof(KeyValue)))
			cap: 8
			size: 0
		}
	}
}

// Push element to array and return index
// The growth-factor is roughly 12.5 `(x + (x >> 3))`
[inline]
fn (d mut DenseArray) push(kv KeyValue) u32 {
	if d.cap == d.size {
		d.cap += d.cap >> 3
		d.data = &KeyValue(realloc(d.data, sizeof(KeyValue) * d.cap))
	}
	push_index := d.size
	d.data[push_index] = kv
	d.size++
	return push_index
}

// Move all zeros to the end of the array
// and resize array
fn (d mut DenseArray) zeros_to_end() {
	mut count := u32(0)
	for i in 0..d.size {
		if d.data[i].key.str != 0 {
			d.data[count] = d.data[i]
			count++
		}
	}
	d.size = count
	d.cap = if count < 8 {8} else {count}
	d.data = &KeyValue(realloc(d.data, sizeof(KeyValue) * d.cap))
}

pub struct map {
	// Byte size of value
	value_bytes     int
mut:
	// Index of the highest index in the hashtable
	cap             u32
	// Number of cached hashbits left for rehasing
	window          byte
	// Used for right-shifting out used hashbits
	shift           byte
	// Pointer to Key-value memory
	key_values      DenseArray
	// Pointer to meta-data
	metas           &u32
	// Measure that decides when to increase the capacity
	max_load_factor f32
	// Extra metas that allows for no ranging when incrementing
	// index in the hashmap
	extra_metas     u32
pub mut:
	// Number of key-values currently in the hashmap
	size            int
}

fn new_map(n, value_bytes int) map {
	return map{
		value_bytes: value_bytes
		cap: init_cap
		window: cached_hashbits
		shift: init_log_capicity
		key_values: new_dense_array()
		metas: &u32(vcalloc(sizeof(u32) * (init_capicity + extra_metas_inc)))
		max_load_factor: init_max_load_factor
		extra_metas: extra_metas_inc
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
	if (f32(m.size << 1) / f32(m.cap)) > m.max_load_factor {
		m.expand()
	}
	hash := wyhash.wyhash_c(key.str, u64(key.len), 0)
	mut meta := u32(((hash >> m.shift) & hash_mask) | probe_inc)
	mut index := hash & m.cap
	// While probe count is less
	for meta < m.metas[index] {
		index += 2
		meta += probe_inc
	}
	// While we might have a match
	for meta == m.metas[index] {
		kv_index := m.metas[index + 1]
		if key == m.key_values.data[kv_index].key {
			C.memcpy(m.key_values.data[kv_index].value, value, m.value_bytes)
			return
		}
		index += 2
		meta += probe_inc
	}
	// Match is not possible anymore.
	// Probe until an empty index is found.
	// Swap when probe count is higher/richer (Robin Hood).
	kv := KeyValue{
		key: key 
		value: malloc(m.value_bytes)
	}
	C.memcpy(kv.value, value, m.value_bytes)
	mut kv_index := m.key_values.push(kv)
	for m.metas[index] != 0 {
		if meta > m.metas[index] {
			tmp_meta := m.metas[index]
			m.metas[index] = meta
			meta = tmp_meta
			tmp_index := m.metas[index + 1]
			m.metas[index + 1] = kv_index
			kv_index = tmp_index
		}
		index += 2
		meta += probe_inc
	}
	probe_count := (meta >> hashbits) - 1
	if (probe_count << 1) == m.extra_metas {
		// Should almost never happen
		if (meta & max_probe) == max_probe {
			m.expand()
			m.set(kv.key, kv.value)
			return
		}
		m.extra_metas += extra_metas_inc
		mem_size := (m.cap + 2 + m.extra_metas)
		m.metas = &u32(realloc(m.metas, sizeof(u32) * mem_size))
		memset(m.metas + mem_size - extra_metas_inc, 0, sizeof(u32) * extra_metas_inc)
	}  
	m.metas[index] = meta
	m.metas[index + 1] = kv_index
	m.size++
}

// Doubles the size of the hashmap
fn (m mut map) expand() {
	old_cap := m.cap
	m.cap = ((m.cap + 2)<<1) - 2
	// Check if any hashbits are left
	if m.window == 0 {
		m.shift += cached_hashbits
		m.rehash()
		m.window = cached_hashbits
	}
	else {
		m.cached_rehash(old_cap)
	}
	m.window--
}

// Halves the size of the hashmap
fn (m mut map) shrink() {
	m.key_values.zeros_to_end()
	m.cap = ((m.cap + 2)>>1) - 2
	if m.window == 16 {
		m.shift -= cached_hashbits
		m.window = 0
	}
	m.rehash()
	m.window++
}

fn (m mut map) rehash() {
	meta_bytes := sizeof(u32) * (m.cap + 2 + m.extra_metas)
	m.metas = &u32(realloc(m.metas, meta_bytes))
	C.memset(m.metas, 0, meta_bytes)
	for i := u32(0); i < m.key_values.size; i++ {
		if m.key_values.data[i].key.str == 0 {
			continue
		}
		kv := m.key_values.data[i]
		hash := wyhash.wyhash_c(kv.key.str, u64(kv.key.len), 0)
		mut meta := u32(((hash>>m.shift) & hash_mask) | probe_inc)
		mut index := hash & m.cap
		// While probe count is less
		for meta < m.metas[index] {
			index += 2
			meta += probe_inc
		}
		// Match is not possible anymore.
		// Probe until an empty index is found.
		// Swap when probe count is higher/richer (Robin Hood).
		mut kv_index := i
		for m.metas[index] != 0 {
			if meta > m.metas[index] {
				tmp_meta := m.metas[index]
				m.metas[index] = meta
				meta = tmp_meta
				tmp_index := m.metas[index + 1]
				m.metas[index + 1] = kv_index
				kv_index = tmp_index
			}
			index += 2
			meta += probe_inc
		}
		probe_count := (meta >> hashbits) - 1
		if (probe_count << 1) == m.extra_metas {
			// Should almost never happen
			if (meta & max_probe) == max_probe {
				m.expand()
				return
			}
			m.extra_metas += extra_metas_inc
			mem_size := (m.cap + 2 + m.extra_metas)
			m.metas = &u32(realloc(m.metas, sizeof(u32) * mem_size))
			memset(m.metas + mem_size - extra_metas_inc, 0, sizeof(u32) * extra_metas_inc)
		} 
		m.metas[index] = meta
		m.metas[index + 1] = kv_index
	}
}

fn (m mut map) cached_rehash(old_cap u32) {
	mut new_meta := &u32(vcalloc(sizeof(u32) * (m.cap + 2 + m.extra_metas)))
	old_extra_metas := m.extra_metas
	for i := 0; i <= old_cap + old_extra_metas; i += 2 {
		if m.metas[i] == 0 {
			continue
		}
		old_meta := m.metas[i]
		old_probe_count := u64((old_meta>>hashbits) - 1) << 1
		old_index := (i - old_probe_count) & (m.cap >> 1)
		mut index := u64(old_index) | (old_meta << m.shift) & m.cap
		mut meta := (old_meta & hash_mask) | probe_inc
		// While probe count is less
		for meta < new_meta[index] {
			index += 2
			meta += probe_inc
		}
		// Match is not possible anymore.
		// Probe until an empty index is found.
		// Swap when probe count is higher/richer (Robin Hood).
		mut kv_index := m.metas[i + 1]
		for new_meta[index] != 0 {
			if meta > new_meta[index] {
				tmp_meta := new_meta[index]
				new_meta[index] = meta
				meta = tmp_meta
				tmp_index := new_meta[index + 1]
				new_meta[index + 1] = kv_index
				kv_index = tmp_index
			}
			index += 2
			meta += probe_inc
		}
		probe_count := (meta >> hashbits) - 1
		if (probe_count << 1) == m.extra_metas {
			// Should almost never happen
			if (meta & max_probe) == max_probe {
				free(new_meta)
				m.expand()
				return
			}
			m.extra_metas += extra_metas_inc
			mem_size := (m.cap + 2 + m.extra_metas)
			new_meta = &u32(realloc(new_meta, sizeof(u32) * mem_size))
			memset(new_meta + mem_size - extra_metas_inc, 0, sizeof(u32) * extra_metas_inc)
		} 
		new_meta[index] = meta
		new_meta[index + 1] = kv_index
	}
	unsafe{
		free(m.metas)
	}
	m.metas = new_meta
}

fn (m map) get(key string, out voidptr) bool {
	hash := wyhash.wyhash_c(key.str, u64(key.len), 0)
	mut index := hash & m.cap
	mut meta := u32(((hash>>m.shift) & hash_mask) | probe_inc)
	for meta < m.metas[index] {
		index += 2
		meta += probe_inc
	}
	for meta == m.metas[index] {
		kv_index := m.metas[index + 1]
		if key == m.key_values.data[kv_index].key {
			C.memcpy(out, m.key_values.data[kv_index].value, m.value_bytes)
			return true
		}
		index += 2
		meta += probe_inc
	}
	return false
}

pub fn (m map) exists(key string) bool {
	if m.value_bytes == 0 {
		return false
	}
	hash := wyhash.wyhash_c(key.str, u64(key.len), 0)
	mut index := hash & m.cap
	mut meta := u32(((hash>>m.shift) & hash_mask) | probe_inc)
	for meta < m.metas[index] {
		index += 2
		meta += probe_inc
	}
	for meta == m.metas[index] {
		kv_index := m.metas[index + 1]
		if key == m.key_values.data[kv_index].key {
			return true
		}
		index += 2
		meta += probe_inc
	}
	return false
}

pub fn (m mut map) delete(key string) {
	hash := wyhash.wyhash_c(key.str, u64(key.len), 0)
	mut index := hash & m.cap
	mut meta := u32(((hash>>m.shift) & hash_mask) | probe_inc)
	for meta < m.metas[index] {
		index += 2
		meta += probe_inc
	}
	// Perform backwards shifting
	for meta == m.metas[index] {
		kv_index := m.metas[index + 1]
		if key == m.key_values.data[kv_index].key {
			memset(&m.key_values.data[kv_index], 0, sizeof(KeyValue))
			mut old_index := index
			index += 2
			mut cur_meta := m.metas[index]
			mut cur_index := m.metas[index + 1]
			for (cur_meta >> hashbits) > 1 {
				m.metas[old_index] = cur_meta - probe_inc
				m.metas[old_index + 1] = cur_index
				old_index = index
				index += 2
				cur_meta = m.metas[index]
				cur_index = m.metas[index + 1]
			}
			m.metas[old_index] = 0
			m.size--
			if m.cap == 14 {return}
			if (f32(m.size << 1) / f32(m.cap)) < min_load_factor {
				m.shrink()
			}
			return
		}
		index += 2
		meta += probe_inc
	}
}

pub fn (m &map) keys() []string {
	mut keys := [''].repeat(m.size)
	if m.value_bytes == 0 {
		return keys
	}
	mut j := 0
	for i := u32(0); i < m.key_values.size; i++ {
		if m.key_values.data[i].key.str == 0 {
			continue
		}
		keys[j] = m.key_values.data[i].key
		j++
	}
	return keys
}

pub fn (m map) free() {
	unsafe {
		free(m.metas)
		free(m.key_values.data)
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
