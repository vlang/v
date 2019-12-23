// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module hashmap
/*
	This is work in progress.
	A very early test version of the Hashmap with a fixed size.
	Only works with string keys and int values for now.

	I added this to improve performance of the V compiler,
	which uses lots of O(log n) map get's. Turned out with N < 10 000
	the performance gains are basically non-existent.
*/


struct Hashmap {
	cap           int
	keys          []string
	table         []Hashmapentry
	elm_size      int
pub mut:
	nr_collisions int
}

struct Hashmapentry {
mut:
	key  string
	val  int
	next &Hashmapentry // linked list for collisions
}

const (
	min_cap = 2<<10
	max_cap = 2<<20
)

const(
	fnv64_prime        = 1099511628211
	fnv64_offset_basis = u64(14695981039346656037)
)

const(
    fnv32_offset_basis = u32(2166136261)
    fnv32_prime        = u32(16777619)
)

pub fn new_hashmap(planned_nr_items int) Hashmap {
	mut cap := planned_nr_items * 5
	if cap < min_cap {
		cap = min_cap
	}
	if cap > max_cap {
		cap = max_cap
	}
	return Hashmap{
		cap: cap
		elm_size: 4
		table: make(cap, cap, sizeof(Hashmapentry))
	}
}

pub fn (m mut Hashmap) set(key string, val int) {
	// mut hash := int(b_fabs(key.hash()))
	// idx := hash % m.cap
	idx := int(fnv1a32(key) % m.cap)
	if m.table[idx].key.len != 0 {
		// println('\nset() idx=$idx key="$key" hash="$hash" val=$val')
		m.nr_collisions++
		// println('collision:' + m.table[idx].key)
		mut e := &m.table[idx]
		for e.next != 0 {
			e = e.next
		}
		e.next = &Hashmapentry{
			key,val,0}
	}
	else {
		m.table[idx] = Hashmapentry{
			key,val,0}
	}
}

pub fn (m &Hashmap) get(key string) int {
	// mut hash := int(b_fabs(key.hash()))
	// idx := hash % m.cap
	idx := int(fnv1a32(key) % m.cap)
	mut e := &m.table[idx]
	for e.next != 0 {
		// todo unsafe {
		if e.key == key {
			return e.val
		}
		e = e.next
	}
	return e.val
}

[inline]
fn b_fabs(v int) f64 {
	return if v < 0 { -v } else { v }
}

// inline functions here for speed
// rather than full impl in vlib
[inline]
fn fnv1a32(data string) u32 {
    mut hash := fnv32_offset_basis
    for i := 0; i < data.len; i++ {
        hash = (hash ^ u32(data[i])) * fnv32_prime
    }
    return hash
}

[inline]
fn fnv1a64(data string) u64 {
    mut hash := fnv64_offset_basis
    for i := 0; i < data.len; i++ {
        hash = (hash ^ u64(data[i])) * fnv64_prime
    }
    return hash
}
