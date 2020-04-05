// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

import (
	strings
	hash.wyhash
)
/*
This is a very fast hashmap implementation. It has several properties that in
combination makes it very fast. Here is a short explanation of each property.
After reading this you should have a basic understanding of how it works:

1. |Hash-function (Wyhash)|. Wyhash is the fastest hash-function passing SMHash-
er, so it was an easy choice.

2. |Open addressing (Robin Hood Hashing)|. With this method, a hash collision is
resolved by probing. As opposed to linear probing, Robin Hood hashing has a sim-
ple but clever twist: As new keys are inserted, old keys are shifted around in a
way such that all keys stay reasonably close to the slot they originally hash to.

3. |Memory layout|. Key-value pairs are stored in a `DenseArray`, with an avera-
ge of roughly 6.25% unused memory, as opposed to most other dynamic array imple-
mentations with a growth factor of 1.5 or 2. The key-values keep their index in
the array - they are not probed. Instead, this implementation uses another array
"metas" storing "meta"s (meta-data). Each Key-value has a corresponding meta. A
meta stores a reference to its key-value, and its index in "metas" is determined
by the hash of the key and probing. A meta also stores bits from the hash (for
faster rehashing etc.) and how far away it is from the index it was originally
hashed to (probe_count). probe_count is 0 if empty, 1 if not probed, 2 if probed
by 1, etc..

meta (64 bit) =  kv_index (32 bit) | probe_count (8 bits) | hashbits (24 bits)
metas = [meta, 0, meta, 0, meta, meta, meta, 0, ...]
key_values = [kv, kv, kv, kv, kv, ...]

4. |Power of two size array|. The size of metas is a power of two. This makes it
possible to find a bucket from a hash code by using "hash & (SIZE -1)" instead
of "abs(hash) % SIZE". Modulo is extremely expensive so using '&' is a big perf-
ormance improvement. The general concern with this is that you only use the low-
er bits of the hash and that can cause more collisions. This is solved by using
good hash-function.

5. |Extra metas|. The hashmap keeps track of the highest probe_count. The trick
is to allocate extra_metas > max(probe_count), so you never have to do any boun-
ds-checking because the extra metas ensures that an element will never go beyond
the last index.

6. |Cached rehashing|. When the load_factor of the map exceeds the max_load_fac-
tor the size of metas is doubled and all the elements need to be "rehashed" to
find the index in the new array. Instead of rehashing completely, it simply uses
the hashbits stored in the meta.
*/


const (
// Number of bits from the hash stored for each entry
	hashbits = 24
	// Number of bits from the hash stored for rehashing
	max_cached_hashbits = 16
	// Initial log-number of buckets in the hashtable
	init_log_capicity = 5
	// Initial number of buckets in the hashtable
	init_capicity = 1<<init_log_capicity
	// Maximum load-factor (size / capacity)
	max_load_factor = 0.8
	// Initial highest even index in metas
	init_cap = init_capicity - 2
	// Used for incrementing `extra_metas` when max
	// probe count is too high, to avoid overflow
	extra_metas_inc = 4
	// Bitmask to select all the hashbits
	hash_mask = u32(0x00FFFFFF)
	// Used for incrementing the probe-count
	probe_inc = u32(0x01000000)
)

struct KeyValue {
	key   string
mut:
	value voidptr
}

// Dynamic array with very low growth factor
struct DenseArray {
mut:
	cap     u32
	size    u32
	deletes u32
	data    &KeyValue
}

[inline]
fn new_dense_array() DenseArray {
	unsafe{
		return DenseArray{
			cap: 8
			size: 0
			deletes: 0
			data: &KeyValue(malloc(8 * sizeof(KeyValue)))
		}
	}
}

// Push element to array and return index
// The growth-factor is roughly 12.5 `(x + (x >> 3))`
[inline]
fn (d mut DenseArray) push(kv KeyValue) u32 {
	if d.cap == d.size {
		d.cap += d.cap>>3
		d.data = &KeyValue(C.realloc(d.data, sizeof(KeyValue) * d.cap))
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
	for i in 0 .. d.size {
		if d.data[i].key.str != 0 {
			tmp := d.data[count]
			d.data[count] = d.data[i]
			d.data[i] = tmp
			count++
		}
	}
	d.deletes = 0
	d.size = count
	d.cap = if count < 8 { 8 } else { count }
	d.data = &KeyValue(C.realloc(d.data, sizeof(KeyValue) * d.cap))
}

pub struct map {
	// Byte size of value
	value_bytes int
mut:
// highest even index in the hashtable
	cap         u32
	// Number of cached hashbits left for rehasing
	cached_hashbits      byte
	// Used for right-shifting out used hashbits
	shift       byte
	// Array storing key-values (ordered)
	key_values  DenseArray
	// Pointer to meta-data:
	// Odd indices stores index in `key_values`.
	// Even indices stores probe_count and hashbits.
	metas       &u32
	// Extra metas that allows for no ranging when incrementing
	// index in the hashmap
	extra_metas u32
pub mut:
// Number of key-values currently in the hashmap
	size        int
}

fn new_map(n, value_bytes int) map {
	return map{
		value_bytes: value_bytes
		cap: init_cap
		cached_hashbits: max_cached_hashbits
		shift: init_log_capicity
		key_values: new_dense_array()
		metas: &u32(vcalloc(sizeof(u32) * (init_capicity + extra_metas_inc)))
		extra_metas: extra_metas_inc
		size: 0
	}
}

fn new_map_init(n, value_bytes int, keys &string, values voidptr) map {
	mut out := new_map(n, value_bytes)
	for i in 0 .. n {
		out.set(keys[i], byteptr(values) + i * value_bytes)
	}
	return out
}

[inline]
fn (m map) key_to_index(key string) (u32,u32) {
	hash := wyhash.wyhash_c(key.str, u64(key.len), 0)
	index := hash & m.cap
	meta := ((hash>>m.shift) & hash_mask) | probe_inc
	return u32(index),u32(meta)
}

[inline]
fn (m map) meta_less(_index u32, _metas u32) (u32,u32) {
	mut index := _index
	mut meta := _metas
	for meta < m.metas[index] {
		index += 2
		meta += probe_inc
	}
	return index,meta
}

[inline]
fn (m mut map) meta_greater(_index u32, _metas u32, kvi u32) {
	mut meta := _metas
	mut index := _index
	mut kv_index := kvi
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
	m.metas[index] = meta
	m.metas[index + 1] = kv_index
	probe_count := (meta>>hashbits) - 1
	if (probe_count<<1) == m.extra_metas {
		m.extra_metas += extra_metas_inc
		mem_size := (m.cap + 2 + m.extra_metas)
		m.metas = &u32(C.realloc(m.metas, sizeof(u32) * mem_size))
		C.memset(m.metas + mem_size - extra_metas_inc, 0, sizeof(u32) * extra_metas_inc)
		// Should almost never happen
		if probe_count == 252 {
			panic('Probe overflow')
		}
	}
}

fn (m mut map) set(key string, value voidptr) {
	load_factor := f32(m.size<<1) / f32(m.cap)
	if load_factor > max_load_factor {
		m.expand()
	}
	mut index,mut meta := m.key_to_index(key)
	index,meta = m.meta_less(index, meta)
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
	// Match not possible anymore
	kv := KeyValue{
		key: key
		value: malloc(m.value_bytes)
	}
	C.memcpy(kv.value, value, m.value_bytes)
	kv_index := m.key_values.push(kv)
	m.meta_greater(index, meta, kv_index)
	m.size++
}

// Doubles the size of the hashmap
fn (m mut map) expand() {
	old_cap := m.cap
	m.cap = ((m.cap + 2)<<1) - 2
	// Check if any hashbits are left
	if m.cached_hashbits == 0 {
		m.shift += max_cached_hashbits
		m.cached_hashbits = max_cached_hashbits
		m.rehash()
	}
	else {
		m.cached_rehash(old_cap)
	}
	m.cached_hashbits--
}

fn (m mut map) rehash() {
	meta_bytes := sizeof(u32) * (m.cap + 2 + m.extra_metas)
	m.metas = &u32(C.realloc(m.metas, meta_bytes))
	C.memset(m.metas, 0, meta_bytes)
	for i := u32(0); i < m.key_values.size; i++ {
		if m.key_values.data[i].key.str == 0 {
			continue
		}
		kv := m.key_values.data[i]
		mut index,mut meta := m.key_to_index(kv.key)
		index,meta = m.meta_less(index, meta)
		m.meta_greater(index, meta, i)
	}
}

fn (m mut map) cached_rehash(old_cap u32) {
	old_metas := m.metas
	m.metas = &u32(vcalloc(sizeof(u32) * (m.cap + 2 + m.extra_metas)))
	old_extra_metas := m.extra_metas
	for i := u32(0); i <= old_cap + old_extra_metas; i += 2 {
		if old_metas[i] == 0 {
			continue
		}
		old_meta := old_metas[i]
		old_probe_count := ((old_meta>>hashbits) - 1)<<1
		old_index := (i - old_probe_count) & (m.cap>>1)
		mut index := (old_index | (old_meta<<m.shift)) & m.cap
		mut meta := (old_meta & hash_mask) | probe_inc
		index,meta = m.meta_less(index, meta)
		kv_index := old_metas[i + 1]
		m.meta_greater(index, meta, kv_index)
	}
	unsafe{
		free(old_metas)
	}
}

fn (m map) get3(key string, zero voidptr) voidptr {
	mut index,mut meta := m.key_to_index(key)
	index,meta = m.meta_less(index, meta)
	for meta == m.metas[index] {
		kv_index := m.metas[index + 1]
		if key == m.key_values.data[kv_index].key {
			out := malloc(m.value_bytes)
			C.memcpy(out, m.key_values.data[kv_index].value, m.value_bytes)
			return out
		}
		index += 2
		meta += probe_inc
	}
	return zero
}

fn (m map) exists(key string) bool {
	if m.value_bytes == 0 {
		return false
	}
	mut index,mut meta := m.key_to_index(key)
	index,meta = m.meta_less(index, meta)
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
	mut index,mut meta := m.key_to_index(key)
	index,meta = m.meta_less(index, meta)
	// Perform backwards shifting
	for meta == m.metas[index] {
		kv_index := m.metas[index + 1]
		if key == m.key_values.data[kv_index].key {
			for (m.metas[index + 2]>>hashbits) > 1 {
				m.metas[index] = m.metas[index + 2] - probe_inc
				m.metas[index + 1] = m.metas[index + 3]
				index += 2
			}
			m.size--
			m.metas[index] = 0
			m.key_values.deletes++
			if m.key_values.size <= 32 {
				return
			}
			C.memset(&m.key_values.data[kv_index], 0, sizeof(KeyValue))
			// Clean up key_values if too many have been deleted
			if m.key_values.deletes >= (m.key_values.size >> 1) {
				m.key_values.zeros_to_end()
				m.rehash()
				m.key_values.deletes = 0
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

[unsafe_fn]
pub fn (m map) free() {
	free(m.metas)
	for i := u32(0); i < m.key_values.size; i++ {
		if m.key_values.data[i].key.str == 0 {
			continue
		}
		m.key_values.data[i].key.free()
	}
	free(m.key_values.data)
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
	for key, val in m {
		sb.writeln('  "$key" => "$val"')
	}
	sb.writeln('}')
	return sb.str()
}
