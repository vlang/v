// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module builtin

/*
	This is work in progress.
	A very early test version of the hashmap with a fixed size.
	Only works with string keys and int values for now.
	
	I added this to improve performance of the V compiler,
	which uses lots of O(log n) map get's. Turned out with N < 10 000
	the performance gains are basically non-existent.
*/

struct hashmap {
	cap          int
	keys         []string
	table        []hashmapentry
	elm_size int
pub:
	nr_collisions int
}

struct hashmapentry {
        key string
        val int
        next &hashmapentry  // linked list for collisions
}

const (
	min_cap = 2 << 10
	max_cap = 2 << 20
)

fn new_hashmap(planned_nr_items int) hashmap {
	mut cap := planned_nr_items * 5
	if cap < min_cap {
		cap = min_cap
	}	
	if cap > max_cap {
		cap = max_cap
	}	
	return hashmap{
		cap: cap
		elm_size: 4
		table: make(cap, cap, sizeof(hashmapentry))
	}	
}	

fn (m mut hashmap) set(key string, val int) {
	mut hash := int(b_fabs( key.hash() ))
	idx := hash % m.cap
	if m.table[idx].key.len != 0 {
		//println('\nset() idx=$idx key="$key" hash="$hash" val=$val')
		m.nr_collisions++
		//println('collision:' + m.table[idx].key)
		mut e := &m.table[idx]
		for e.next != 0 {
			e = e.next
		}	
		e.next = &hashmapentry{key, val, 0}
	} else {
		m.table[idx] = hashmapentry{key, val, 0}
	}
}	

fn (m mut hashmap) get(key string) int {
	hash := int(b_fabs( key.hash() ))
	idx := hash % m.cap
	mut e := &m.table[idx]
	for e.next != 0 { // todo unsafe {
		if e.key == key {
			return e.val
		}	
		e = e.next
	}	
	return e.val
}

[inline] fn b_fabs(v int) f64 { return if v < 0 { -v } else { v } }
