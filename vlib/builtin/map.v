// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module builtin

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
	// Maximum load-factor (len / capacity)
	max_load_factor     = 0.8
	// Initial highest even index in metas
	init_even_index     = init_capicity - 2
	// Used for incrementing `extra_metas` when max
	// probe count is too high, to avoid overflow
	extra_metas_inc     = 4
	// Bitmask to select all the hashbits
	hash_mask           = u32(0x00FFFFFF)
	// Used for incrementing the probe-count
	probe_inc           = u32(0x01000000)
)

// DenseArray represents a dynamic array with very low growth factor
struct DenseArray {
	key_bytes   int
	value_bytes int
mut:
	cap     int
	len     int
	deletes u32 // count
	// array allocated (with `cap` bytes) on first deletion
	// has non-zero element when key deleted
	all_deleted &u8
	keys        &u8
	values      &u8
}

[inline]
fn new_dense_array(key_bytes int, value_bytes int) DenseArray {
	cap := 8
	return DenseArray{
		key_bytes: key_bytes
		value_bytes: value_bytes
		cap: cap
		len: 0
		deletes: 0
		all_deleted: 0
		keys: unsafe { malloc(cap * key_bytes) }
		values: unsafe { malloc(cap * value_bytes) }
	}
}

[inline]
fn (d &DenseArray) key(i int) voidptr {
	return unsafe { voidptr(d.keys + i * d.key_bytes) }
}

// for cgen
[inline]
fn (d &DenseArray) value(i int) voidptr {
	return unsafe { voidptr(d.values + i * d.value_bytes) }
}

[inline]
fn (d &DenseArray) has_index(i int) bool {
	return d.deletes == 0 || unsafe { d.all_deleted[i] } == 0
}

// Make space to append an element and return index
// The growth-factor is roughly 1.125 `(x + (x >> 3))`
[inline]
fn (mut d DenseArray) expand() int {
	old_cap := d.cap
	old_key_size := d.key_bytes * old_cap
	old_value_size := d.value_bytes * old_cap
	if d.cap == d.len {
		d.cap += d.cap >> 3
		unsafe {
			d.keys = realloc_data(d.keys, old_key_size, d.key_bytes * d.cap)
			d.values = realloc_data(d.values, old_value_size, d.value_bytes * d.cap)
			if d.deletes != 0 {
				d.all_deleted = realloc_data(d.all_deleted, old_cap, d.cap)
				vmemset(voidptr(d.all_deleted + d.len), 0, d.cap - d.len)
			}
		}
	}
	push_index := d.len
	unsafe {
		if d.deletes != 0 {
			d.all_deleted[push_index] = 0
		}
	}
	d.len++
	return push_index
}

type MapHashFn = fn (voidptr) u64

type MapEqFn = fn (voidptr, voidptr) bool

type MapCloneFn = fn (voidptr, voidptr)

type MapFreeFn = fn (voidptr)

// map is the internal representation of a V `map` type.
pub struct map {
	// Number of bytes of a key
	key_bytes int
	// Number of bytes of a value
	value_bytes int
mut:
	// Highest even index in the hashtable
	even_index u32
	// Number of cached hashbits left for rehashing
	cached_hashbits u8
	// Used for right-shifting out used hashbits
	shift u8
	// Array storing key-values (ordered)
	key_values DenseArray
	// Pointer to meta-data:
	// - Odd indices store kv_index.
	// - Even indices store probe_count and hashbits.
	metas &u32
	// Extra metas that allows for no ranging when incrementing
	// index in the hashmap
	extra_metas     u32
	has_string_keys bool
	hash_fn         MapHashFn
	key_eq_fn       MapEqFn
	clone_fn        MapCloneFn
	free_fn         MapFreeFn
pub mut:
	// Number of key-values currently in the hashmap
	len int
}

fn map_eq_string(a voidptr, b voidptr) bool {
	return fast_string_eq(*unsafe { &string(a) }, *unsafe { &string(b) })
}

fn map_eq_int_1(a voidptr, b voidptr) bool {
	return unsafe { *&u8(a) == *&u8(b) }
}

fn map_eq_int_2(a voidptr, b voidptr) bool {
	return unsafe { *&u16(a) == *&u16(b) }
}

fn map_eq_int_4(a voidptr, b voidptr) bool {
	return unsafe { *&u32(a) == *&u32(b) }
}

fn map_eq_int_8(a voidptr, b voidptr) bool {
	return unsafe { *&u64(a) == *&u64(b) }
}

fn map_clone_string(dest voidptr, pkey voidptr) {
	unsafe {
		s := *&string(pkey)
		(*&string(dest)) = s.clone()
	}
}

fn map_clone_int_1(dest voidptr, pkey voidptr) {
	unsafe {
		*&u8(dest) = *&u8(pkey)
	}
}

fn map_clone_int_2(dest voidptr, pkey voidptr) {
	unsafe {
		*&u16(dest) = *&u16(pkey)
	}
}

fn map_clone_int_4(dest voidptr, pkey voidptr) {
	unsafe {
		*&u32(dest) = *&u32(pkey)
	}
}

fn map_clone_int_8(dest voidptr, pkey voidptr) {
	unsafe {
		*&u64(dest) = *&u64(pkey)
	}
}

fn map_free_string(pkey voidptr) {
	unsafe {
		(*&string(pkey)).free()
	}
}

fn map_free_nop(_ voidptr) {
}

fn new_map(key_bytes int, value_bytes int, hash_fn MapHashFn, key_eq_fn MapEqFn, clone_fn MapCloneFn, free_fn MapFreeFn) map {
	metasize := int(sizeof(u32) * (init_capicity + extra_metas_inc))
	// for now assume anything bigger than a pointer is a string
	has_string_keys := key_bytes > sizeof(voidptr)
	return map{
		key_bytes: key_bytes
		value_bytes: value_bytes
		even_index: init_even_index
		cached_hashbits: max_cached_hashbits
		shift: init_log_capicity
		key_values: new_dense_array(key_bytes, value_bytes)
		metas: unsafe { &u32(vcalloc_noscan(metasize)) }
		extra_metas: extra_metas_inc
		len: 0
		has_string_keys: has_string_keys
		hash_fn: hash_fn
		key_eq_fn: key_eq_fn
		clone_fn: clone_fn
		free_fn: free_fn
	}
}

fn new_map_init(hash_fn MapHashFn, key_eq_fn MapEqFn, clone_fn MapCloneFn, free_fn MapFreeFn, n int, key_bytes int, value_bytes int, keys voidptr, values voidptr) map {
	mut out := new_map(key_bytes, value_bytes, hash_fn, key_eq_fn, clone_fn, free_fn)
	// TODO pre-allocate n slots
	mut pkey := &u8(keys)
	mut pval := &u8(values)
	for _ in 0 .. n {
		unsafe {
			out.set(pkey, pval)
			pkey = pkey + key_bytes
			pval = pval + value_bytes
		}
	}
	return out
}

pub fn (mut m map) move() map {
	r := *m
	unsafe {
		vmemset(m, 0, int(sizeof(map)))
	}
	return r
}

// clear clears the map without deallocating the allocated data.
// It does it by setting the map length to `0`
// Example: a.clear() // `a.len` and `a.key_values.len` is now 0
pub fn (mut m map) clear() {
	m.len = 0
	m.key_values.len = 0
}

[inline]
fn (m &map) key_to_index(pkey voidptr) (u32, u32) {
	hash := m.hash_fn(pkey)
	index := hash & m.even_index
	meta := ((hash >> m.shift) & hash_mask) | probe_inc
	return u32(index), u32(meta)
}

[inline]
fn (m &map) meta_less(_index u32, _metas u32) (u32, u32) {
	mut index := _index
	mut meta := _metas
	for meta < unsafe { m.metas[index] } {
		index += 2
		meta += probe_inc
	}
	return index, meta
}

[inline]
fn (mut m map) meta_greater(_index u32, _metas u32, kvi u32) {
	mut meta := _metas
	mut index := _index
	mut kv_index := kvi
	for unsafe { m.metas[index] } != 0 {
		if meta > unsafe { m.metas[index] } {
			unsafe {
				tmp_meta := m.metas[index]
				m.metas[index] = meta
				meta = tmp_meta
				tmp_index := m.metas[index + 1]
				m.metas[index + 1] = kv_index
				kv_index = tmp_index
			}
		}
		index += 2
		meta += probe_inc
	}
	unsafe {
		m.metas[index] = meta
		m.metas[index + 1] = kv_index
	}
	probe_count := (meta >> hashbits) - 1
	m.ensure_extra_metas(probe_count)
}

[inline]
fn (mut m map) ensure_extra_metas(probe_count u32) {
	if (probe_count << 1) == m.extra_metas {
		size_of_u32 := sizeof(u32)
		old_mem_size := (m.even_index + 2 + m.extra_metas)
		m.extra_metas += extra_metas_inc
		mem_size := (m.even_index + 2 + m.extra_metas)
		unsafe {
			x := realloc_data(&u8(m.metas), int(size_of_u32 * old_mem_size), int(size_of_u32 * mem_size))
			m.metas = &u32(x)
			vmemset(m.metas + mem_size - extra_metas_inc, 0, int(sizeof(u32) * extra_metas_inc))
		}
		// Should almost never happen
		if probe_count == 252 {
			panic('Probe overflow')
		}
	}
}

// Insert new element to the map. The element is inserted if its key is
// not equivalent to the key of any other element already in the container.
// If the key already exists, its value is changed to the value of the new element.
fn (mut m map) set(key voidptr, value voidptr) {
	load_factor := f32(u32(m.len) << 1) / f32(m.even_index)
	if load_factor > max_load_factor {
		m.expand()
	}
	mut index, mut meta := m.key_to_index(key)
	index, meta = m.meta_less(index, meta)
	// While we might have a match
	for meta == unsafe { m.metas[index] } {
		kv_index := int(unsafe { m.metas[index + 1] })
		pkey := unsafe { m.key_values.key(kv_index) }
		if m.key_eq_fn(key, pkey) {
			unsafe {
				pval := m.key_values.value(kv_index)
				vmemcpy(pval, value, m.value_bytes)
			}
			return
		}
		index += 2
		meta += probe_inc
	}
	kv_index := m.key_values.expand()
	unsafe {
		pkey := m.key_values.key(kv_index)
		pvalue := m.key_values.value(kv_index)
		m.clone_fn(pkey, key)
		vmemcpy(&u8(pvalue), value, m.value_bytes)
	}
	m.meta_greater(index, meta, u32(kv_index))
	m.len++
}

// Doubles the size of the hashmap
fn (mut m map) expand() {
	old_cap := m.even_index
	m.even_index = ((m.even_index + 2) << 1) - 2
	// Check if any hashbits are left
	if m.cached_hashbits == 0 {
		m.shift += max_cached_hashbits
		m.cached_hashbits = max_cached_hashbits
		m.rehash()
	} else {
		m.cached_rehash(old_cap)
		m.cached_hashbits--
	}
}

// A rehash is the reconstruction of the hash table:
// All the elements in the container are rearranged according
// to their hash value into the newly sized key-value container.
// Rehashes are performed when the load_factor is going to surpass
// the max_load_factor in an operation.
fn (mut m map) rehash() {
	meta_bytes := sizeof(u32) * (m.even_index + 2 + m.extra_metas)
	unsafe {
		// TODO: use realloc_data here too
		x := v_realloc(&u8(m.metas), int(meta_bytes))
		m.metas = &u32(x)
		vmemset(m.metas, 0, int(meta_bytes))
	}
	for i := 0; i < m.key_values.len; i++ {
		if !m.key_values.has_index(i) {
			continue
		}
		pkey := unsafe { m.key_values.key(i) }
		mut index, mut meta := m.key_to_index(pkey)
		index, meta = m.meta_less(index, meta)
		m.meta_greater(index, meta, u32(i))
	}
}

// This method works like rehash. However, instead of rehashing the
// key completely, it uses the bits cached in `metas`.
fn (mut m map) cached_rehash(old_cap u32) {
	old_metas := m.metas
	metasize := int(sizeof(u32) * (m.even_index + 2 + m.extra_metas))
	m.metas = unsafe { &u32(vcalloc(metasize)) }
	old_extra_metas := m.extra_metas
	for i := u32(0); i <= old_cap + old_extra_metas; i += 2 {
		if unsafe { old_metas[i] } == 0 {
			continue
		}
		old_meta := unsafe { old_metas[i] }
		old_probe_count := ((old_meta >> hashbits) - 1) << 1
		old_index := (i - old_probe_count) & (m.even_index >> 1)
		mut index := (old_index | (old_meta << m.shift)) & m.even_index
		mut meta := (old_meta & hash_mask) | probe_inc
		index, meta = m.meta_less(index, meta)
		kv_index := unsafe { old_metas[i + 1] }
		m.meta_greater(index, meta, kv_index)
	}
	unsafe { free(old_metas) }
}

// This method is used for assignment operators. If the argument-key
// does not exist in the map, it's added to the map along with the zero/default value.
// If the key exists, its respective value is returned.
fn (mut m map) get_and_set(key voidptr, zero voidptr) voidptr {
	for {
		mut index, mut meta := m.key_to_index(key)
		for {
			if meta == unsafe { m.metas[index] } {
				kv_index := int(unsafe { m.metas[index + 1] })
				pkey := unsafe { m.key_values.key(kv_index) }
				if m.key_eq_fn(key, pkey) {
					pval := unsafe { m.key_values.value(kv_index) }
					return unsafe { &u8(pval) }
				}
			}
			index += 2
			meta += probe_inc
			if meta > unsafe { m.metas[index] } {
				break
			}
		}
		// Key not found, insert key with zero-value
		m.set(key, zero)
	}
	return unsafe { nil }
}

// If `key` matches the key of an element in the container,
// the method returns a reference to its mapped value.
// If not, a zero/default value is returned.
fn (m &map) get(key voidptr, zero voidptr) voidptr {
	mut index, mut meta := m.key_to_index(key)
	for {
		if meta == unsafe { m.metas[index] } {
			kv_index := int(unsafe { m.metas[index + 1] })
			pkey := unsafe { m.key_values.key(kv_index) }
			if m.key_eq_fn(key, pkey) {
				pval := unsafe { m.key_values.value(kv_index) }
				return unsafe { &u8(pval) }
			}
		}
		index += 2
		meta += probe_inc
		if meta > unsafe { m.metas[index] } {
			break
		}
	}
	return zero
}

// If `key` matches the key of an element in the container,
// the method returns a reference to its mapped value.
// If not, a zero pointer is returned.
// This is used in `x := m['key'] or { ... }`
fn (m &map) get_check(key voidptr) voidptr {
	mut index, mut meta := m.key_to_index(key)
	for {
		if meta == unsafe { m.metas[index] } {
			kv_index := int(unsafe { m.metas[index + 1] })
			pkey := unsafe { m.key_values.key(kv_index) }
			if m.key_eq_fn(key, pkey) {
				pval := unsafe { m.key_values.value(kv_index) }
				return unsafe { &u8(pval) }
			}
		}
		index += 2
		meta += probe_inc
		if meta > unsafe { m.metas[index] } {
			break
		}
	}
	return 0
}

// Checks whether a particular key exists in the map.
fn (m &map) exists(key voidptr) bool {
	mut index, mut meta := m.key_to_index(key)
	for {
		if meta == unsafe { m.metas[index] } {
			kv_index := int(unsafe { m.metas[index + 1] })
			pkey := unsafe { m.key_values.key(kv_index) }
			if m.key_eq_fn(key, pkey) {
				return true
			}
		}
		index += 2
		meta += probe_inc
		if meta > unsafe { m.metas[index] } {
			break
		}
	}
	return false
}

[inline]
fn (mut d DenseArray) delete(i int) {
	if d.deletes == 0 {
		d.all_deleted = vcalloc(d.cap) // sets to 0
	}
	d.deletes++
	unsafe {
		d.all_deleted[i] = 1
	}
}

// Removes the mapping of a particular key from the map.
[unsafe]
pub fn (mut m map) delete(key voidptr) {
	mut index, mut meta := m.key_to_index(key)
	index, meta = m.meta_less(index, meta)
	// Perform backwards shifting
	for meta == unsafe { m.metas[index] } {
		kv_index := int(unsafe { m.metas[index + 1] })
		pkey := unsafe { m.key_values.key(kv_index) }
		if m.key_eq_fn(key, pkey) {
			for (unsafe { m.metas[index + 2] } >> hashbits) > 1 {
				unsafe {
					m.metas[index] = m.metas[index + 2] - probe_inc
					m.metas[index + 1] = m.metas[index + 3]
				}
				index += 2
			}
			m.len--
			m.key_values.delete(kv_index)
			unsafe {
				m.metas[index] = 0
				m.free_fn(pkey)
				// Mark key as deleted
				vmemset(pkey, 0, m.key_bytes)
			}
			if m.key_values.len <= 32 {
				return
			}
			// Clean up key_values if too many have been deleted
			if m.key_values.deletes >= (m.key_values.len >> 1) {
				m.key_values.zeros_to_end()
				m.rehash()
			}
			return
		}
		index += 2
		meta += probe_inc
	}
}

// Returns all keys in the map.
pub fn (m &map) keys() array {
	mut keys := __new_array(m.len, 0, m.key_bytes)
	mut item := unsafe { &u8(keys.data) }
	if m.key_values.deletes == 0 {
		for i := 0; i < m.key_values.len; i++ {
			unsafe {
				pkey := m.key_values.key(i)
				m.clone_fn(item, pkey)
				item = item + m.key_bytes
			}
		}
		return keys
	}
	for i := 0; i < m.key_values.len; i++ {
		if !m.key_values.has_index(i) {
			continue
		}
		unsafe {
			pkey := m.key_values.key(i)
			m.clone_fn(item, pkey)
			item = item + m.key_bytes
		}
	}
	return keys
}

// Returns all values in the map.
pub fn (m &map) values() array {
	mut values := __new_array(m.len, 0, m.value_bytes)
	mut item := unsafe { &u8(values.data) }

	if m.key_values.deletes == 0 {
		unsafe {
			vmemcpy(item, m.key_values.values, m.value_bytes * m.key_values.len)
		}
		return values
	}

	for i := 0; i < m.key_values.len; i++ {
		if !m.key_values.has_index(i) {
			continue
		}
		unsafe {
			pvalue := m.key_values.value(i)
			vmemcpy(item, pvalue, m.value_bytes)
			item = item + m.value_bytes
		}
	}
	return values
}

// warning: only copies keys, does not clone
[unsafe]
fn (d &DenseArray) clone() DenseArray {
	res := DenseArray{
		key_bytes: d.key_bytes
		value_bytes: d.value_bytes
		cap: d.cap
		len: d.len
		deletes: d.deletes
		all_deleted: 0
		values: 0
		keys: 0
	}
	unsafe {
		if d.deletes != 0 {
			res.all_deleted = memdup(d.all_deleted, d.cap)
		}
		res.keys = memdup(d.keys, d.cap * d.key_bytes)
		res.values = memdup(d.values, d.cap * d.value_bytes)
	}
	return res
}

// clone returns a clone of the `map`.
[unsafe]
pub fn (m &map) clone() map {
	metasize := int(sizeof(u32) * (m.even_index + 2 + m.extra_metas))
	res := map{
		key_bytes: m.key_bytes
		value_bytes: m.value_bytes
		even_index: m.even_index
		cached_hashbits: m.cached_hashbits
		shift: m.shift
		key_values: unsafe { m.key_values.clone() }
		metas: unsafe { &u32(malloc_noscan(metasize)) }
		extra_metas: m.extra_metas
		len: m.len
		has_string_keys: m.has_string_keys
		hash_fn: m.hash_fn
		key_eq_fn: m.key_eq_fn
		clone_fn: m.clone_fn
		free_fn: m.free_fn
	}
	unsafe { vmemcpy(res.metas, m.metas, metasize) }
	if !m.has_string_keys {
		return res
	}
	// clone keys
	for i in 0 .. m.key_values.len {
		if !m.key_values.has_index(i) {
			continue
		}
		m.clone_fn(res.key_values.key(i), m.key_values.key(i))
	}
	return res
}

// free releases all memory resources occupied by the `map`.
[unsafe]
pub fn (m &map) free() {
	unsafe { free(m.metas) }
	if m.key_values.deletes == 0 {
		for i := 0; i < m.key_values.len; i++ {
			unsafe {
				pkey := m.key_values.key(i)
				m.free_fn(pkey)
			}
		}
	} else {
		for i := 0; i < m.key_values.len; i++ {
			if !m.key_values.has_index(i) {
				continue
			}
			unsafe {
				pkey := m.key_values.key(i)
				m.free_fn(pkey)
			}
		}
		unsafe { free(m.key_values.all_deleted) }
	}
	unsafe {
		free(m.key_values.keys)
		free(m.key_values.values)
	}
}
