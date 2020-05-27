// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

import strings
import hash.wyhash

fn C.memcmp(byteptr, byteptr, int) int

/*
This is a highly optimized hashmap implementation. It has several traits that
in combination makes it very fast and memory efficient. Here is a short expl-
anation of each trait. After reading this you should have a basic understand-
ing of how it functions:

1. Hash-function: Wyhash. Wyhash is the fastest hash-function for short keys
passing SMHasher, so it was an obvious choice.

2. Open addressing: Robin Hood Hashing. With this method, a hash-collision is
resolved by probing. As opposed to linear probing, Robin Hood hashing has a
simple but clever twist: As new keys are inserted, old keys are shifted arou-
nd in a way such that all keys stay reasonably close to the slot they origin-
ally hash to. A new key may displace a key already inserted if its probe cou-
nt is larger than that of the key at the current position.

3. Memory layout: key-value pairs are stored in a `DenseArray`. This is a dy-
namic array with a very low volume of unused memory, at the cost of more rea-
llocations when inserting elements. It also preserves the order of the key-v-
alues. This array is named `key_values`. Instead of probing a new key-value,
this map probes two 32-bit numbers collectively. The first number has its 8
most significant bits reserved for the probe-count and the remaining 24 bits
are cached bits from the hash which are utilized for faster re-hashing. This
number is often referred to as `meta`. The other 32-bit number is the index
at which the key-value was pushed to in `key_values`. Both of these numbers
are stored in a sparse array `metas`. The `meta`s and `kv_index`s are stored
at even and odd indices, respectively:

metas = [meta, kv_index, 0, 0, meta, kv_index, 0, 0, meta, kv_index, ...]
key_values = [kv, kv, kv, ...]

4. The size of metas is a power of two. This enables the use of bitwise AND
to convert the 64-bit hash to a bucket/index that doesn't overflow metas. If
the size is power of two you can use "hash & (SIZE - 1)" instead of "hash %
SIZE". Modulo is extremely expensive so using '&' is a big performance impro-
vement. The general concern with this approach is that you only make use of
the lower bits of the hash which can cause more collisions. This is solved by
using a well-dispersed hash-function.

5. The hashmap keeps track of the highest probe_count. The trick is to alloc-
ate `extra_metas` > max(probe_count), so you never have to do any bounds-che-
cking since the extra meta memory ensures that a meta will never go beyond
the last index.

6. Cached rehashing. When the `load_factor` of the map exceeds the `max_load_
factor` the size of metas is doubled and all the key-values are "rehashed" to
find the index for their meta's in the new array. Instead of rehashing compl-
etely, it simply uses the cached-hashbits stored in the meta, resulting in
much faster rehashing.
*/

const (
	// Number of bits from the hash stored for each entry
	hashbits            = 24
	// Number of bits from the hash stored for rehashing
	max_cached_hashbits = 16
	// Initial log-number of buckets in the hashtable
	init_log_capicity   = 5
	// Initial number of buckets in the hashtable
	init_capicity       = 1 << init_log_capicity
	// Maximum load-factor (size / capacity)
	max_load_factor     = 0.8
	// Initial highest even index in metas
	init_cap            = init_capicity - 2
	// Used for incrementing `extra_metas` when max
	// probe count is too high, to avoid overflow
	extra_metas_inc     = 4
	// Bitmask to select all the hashbits
	hash_mask           = u32(0x00FFFFFF)
	// Used for incrementing the probe-count
	probe_inc           = u32(0x01000000)
)

// This function is intended to be fast when
// the strings are very likely to be equal
// TODO: add branch prediction hints
[inline]
fn fast_string_eq(a, b string) bool {
	if a.len != b.len {
		return false
	}
	return C.memcmp(a.str, b.str, b.len) == 0
}

// Dynamic array with very low growth factor
struct DenseArray {
	value_bytes int
mut:
	cap         u32
	size        u32
	deletes     u32
	keys        &string
	values      byteptr
}

[inline]
[unsafe_fn]
fn new_dense_array(value_bytes int) DenseArray {
	return DenseArray{
		value_bytes: value_bytes
		cap: 8
		size: 0
		deletes: 0
		keys: &string(malloc(8 * sizeof(string)))
		values: malloc(8 * value_bytes)
	}
}

// Push element to array and return index
// The growth-factor is roughly 1.125 `(x + (x >> 3))`
[inline]
fn (mut d DenseArray) push(key string, value voidptr) u32 {
	if d.cap == d.size {
		d.cap += d.cap >> 3
		d.keys = &string(C.realloc(d.keys, sizeof(string) * d.cap))
		d.values = C.realloc(d.values, u32(d.value_bytes) * d.cap)
	}
	push_index := d.size
	d.keys[push_index] = key
	C.memcpy(d.values + push_index * u32(d.value_bytes), value, d.value_bytes)
	d.size++
	return push_index
}

fn (d DenseArray) get(i int) voidptr {
	$if !no_bounds_checking? {
		if i < 0 || i >= int(d.size) {
			panic('DenseArray.get: index out of range (i == $i, d.len == $d.size)')
		}
	}
	return byteptr(d.keys) + i * int(sizeof(string))
}

// Move all zeros to the end of the array
// and resize array
fn (mut d DenseArray) zeros_to_end() {
	mut tmp_value := malloc(d.value_bytes)
	mut count := u32(0)
	for i in 0 .. d.size {
		if d.keys[i].str != 0 {
			// swap keys
			tmp_key := d.keys[count]
			d.keys[count] = d.keys[i]
			d.keys[i] = tmp_key
			// swap values (TODO: optimize)
			C.memcpy(tmp_value, d.values + count * u32(d.value_bytes), d.value_bytes)
			C.memcpy(d.values + count * u32(d.value_bytes), d.values + i * d.value_bytes, d.value_bytes)
			C.memcpy(d.values + i * d.value_bytes, tmp_value, d.value_bytes)
			count++
		}
	}
	free(tmp_value)
	d.deletes = 0
	d.size = count
	d.cap = if count < 8 { u32(8) } else { count }
	d.keys = &string(C.realloc(d.keys, sizeof(string) * d.cap))
	d.values = C.realloc(d.values, u32(d.value_bytes) * d.cap)
}

pub struct map {
	// Byte size of value
	value_bytes     int
mut:
	// highest even index in the hashtable
	cap             u32
	// Number of cached hashbits left for rehasing
	cached_hashbits byte
	// Used for right-shifting out used hashbits
	shift           byte
	// Array storing key-values (ordered)
	key_values      DenseArray
	// Pointer to meta-data:
	// Odd indices store kv_index.
	// Even indices store probe_count and hashbits.
	metas           &u32
	// Extra metas that allows for no ranging when incrementing
	// index in the hashmap
	extra_metas     u32
pub mut:
	// Number of key-values currently in the hashmap
	size            int
}

fn new_map_1(value_bytes int) map {
	return map{
		value_bytes: value_bytes
		cap: init_cap
		cached_hashbits: max_cached_hashbits
		shift: init_log_capicity
		key_values: new_dense_array(value_bytes)
		metas: &u32(vcalloc(sizeof(u32) * (init_capicity + extra_metas_inc)))
		extra_metas: extra_metas_inc
		size: 0
	}
}

fn new_map_init(n, value_bytes int, keys &string, values voidptr) map {
	mut out := new_map_1(value_bytes)
	for i in 0 .. n {
		out.set(keys[i], byteptr(values) + i * value_bytes)
	}
	return out
}

[inline]
fn (m &map) key_to_index(key string) (u32,u32) {
	hash := wyhash.wyhash_c(key.str, u64(key.len), 0)
	index := hash & m.cap
	meta := ((hash >> m.shift) & hash_mask) | probe_inc
	return u32(index),u32(meta)
}

[inline]
fn (m &map) meta_less(_index u32, _metas u32) (u32,u32) {
	mut index := _index
	mut meta := _metas
	for meta < m.metas[index] {
		index += 2
		meta += probe_inc
	}
	return index,meta
}

[inline]
fn (mut m map) meta_greater(_index u32, _metas u32, kvi u32) {
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
	probe_count := (meta >> hashbits) - 1
	m.ensure_extra_metas(probe_count)
}

[inline]
fn (mut m map) ensure_extra_metas(probe_count u32) {
	if (probe_count << 1) == m.extra_metas {
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

fn (mut m map) set(k string, value voidptr) {
	key := k.clone()
	load_factor := f32(m.size << 1) / f32(m.cap)
	if load_factor > max_load_factor {
		m.expand()
	}
	mut index,mut meta := m.key_to_index(key)
	index,meta = m.meta_less(index, meta)
	// While we might have a match
	for meta == m.metas[index] {
		kv_index := m.metas[index + 1]
		if fast_string_eq(key, m.key_values.keys[kv_index]) {
			C.memcpy(m.key_values.values + kv_index * u32(m.value_bytes), value, m.value_bytes)
			return
		}
		index += 2
		meta += probe_inc
	}
	kv_index := m.key_values.push(key, value)
	m.meta_greater(index, meta, kv_index)
	m.size++
}

// Doubles the size of the hashmap
fn (mut m map) expand() {
	old_cap := m.cap
	m.cap = ((m.cap + 2) << 1) - 2
	// Check if any hashbits are left
	if m.cached_hashbits == 0 {
		m.shift += max_cached_hashbits
		m.cached_hashbits = max_cached_hashbits
		m.rehash()
	}
	else {
		m.cached_rehash(old_cap)
		m.cached_hashbits--
	}
}

fn (mut m map) rehash() {
	meta_bytes := sizeof(u32) * (m.cap + 2 + m.extra_metas)
	m.metas = &u32(C.realloc(m.metas, meta_bytes))
	C.memset(m.metas, 0, meta_bytes)
	for i := u32(0); i < m.key_values.size; i++ {
		if m.key_values.keys[i].str == 0 {
			continue
		}
		mut index,mut meta := m.key_to_index(m.key_values.keys[i])
		index,meta = m.meta_less(index, meta)
		m.meta_greater(index, meta, i)
	}
}

fn (mut m map) cached_rehash(old_cap u32) {
	old_metas := m.metas
	m.metas = &u32(vcalloc(sizeof(u32) * (m.cap + 2 + m.extra_metas)))
	old_extra_metas := m.extra_metas
	for i := u32(0); i <= old_cap + old_extra_metas; i += 2 {
		if old_metas[i] == 0 {
			continue
		}
		old_meta := old_metas[i]
		old_probe_count := ((old_meta >> hashbits) - 1) << 1
		old_index := (i - old_probe_count) & (m.cap >> 1)
		mut index := (old_index | (old_meta << m.shift)) & m.cap
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
	for {
		if meta == m.metas[index] {
			kv_index := m.metas[index + 1]
			if fast_string_eq(key, m.key_values.keys[kv_index]) {
				return voidptr(m.key_values.values + kv_index * u32(m.value_bytes))
			}
		}
		index += 2
		meta += probe_inc
		if meta > m.metas[index] { break }
	}
	return zero
}

fn (m map) exists(key string) bool {
	mut index,mut meta := m.key_to_index(key)
	for {
		if meta == m.metas[index] {
			kv_index := m.metas[index + 1]
			if fast_string_eq(key, m.key_values.keys[kv_index]) {
				return  true
			}
		}
		index += 2
		meta += probe_inc
		if meta > m.metas[index] { break }
	}
	return false
}

pub fn (mut m map) delete(key string) {
	mut index,mut meta := m.key_to_index(key)
	index,meta = m.meta_less(index, meta)
	// Perform backwards shifting
	for meta == m.metas[index] {
		kv_index := m.metas[index + 1]
		if fast_string_eq(key, m.key_values.keys[kv_index]) {
			for (m.metas[index + 2] >> hashbits) > 1 {
				m.metas[index] = m.metas[index + 2] - probe_inc
				m.metas[index + 1] = m.metas[index + 3]
				index += 2
			}
			m.size--
			m.metas[index] = 0
			m.key_values.deletes++
			// Mark key as deleted
			m.key_values.keys[kv_index].free()
			C.memset(&m.key_values.keys[kv_index], 0, sizeof(string))
			if m.key_values.size <= 32 {
				return
			}
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

// TODO: add optimization in case of no deletes
pub fn (m &map) keys() []string {
	mut keys := [''].repeat(m.size)
	mut j := 0
	for i := u32(0); i < m.key_values.size; i++ {
		if m.key_values.keys[i].str == 0 {
			continue
		}
		keys[j] = m.key_values.keys[i].clone()
		j++
	}
	return keys
}

[unsafe_fn]
pub fn (d DenseArray) clone() DenseArray {
	res := DenseArray {
		value_bytes: d.value_bytes
		cap:         d.cap
		size:        d.size
		deletes:     d.deletes
		keys:        &string(malloc(d.cap * sizeof(string)))
		values:      byteptr(malloc(d.cap * u32(d.value_bytes)))
	}
	C.memcpy(res.keys, d.keys, d.cap * sizeof(string))
	C.memcpy(res.values, d.values, d.cap * u32(d.value_bytes))
	return res
}

[unsafe_fn]
pub fn (m map) clone() map {
	metas_size := sizeof(u32) * (m.cap + 2 + m.extra_metas)
	res := map{
		value_bytes:     m.value_bytes
		cap:             m.cap
		cached_hashbits: m.cached_hashbits
		shift:           m.shift
		key_values:      m.key_values.clone()
		metas:           &u32(malloc(metas_size))
		extra_metas:     m.extra_metas
		size:            m.size
	}
	C.memcpy(res.metas, m.metas, metas_size)
	return res
}

[unsafe_fn]
pub fn (m &map) free() {
	free(m.metas)
	for i := u32(0); i < m.key_values.size; i++ {
		if m.key_values.keys[i].str == 0 {
			continue
		}
		m.key_values.keys[i].free()
	}
	free(m.key_values.keys)
	free(m.key_values.values)
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
