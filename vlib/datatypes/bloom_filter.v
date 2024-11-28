module datatypes

// Bloom filter is used to test whether a given element is part of a set.  Lookups will occasionally generate false positives, but never false  negatives.

@[heap]
struct BloomFilter[T] {
	// TODO: V bug
	hash_func fn (T) u32 = unsafe { nil } // hash function, input [T] , output u32
	// hash_func     fn (T) u32 = empty_cb // hash function, input [T] , output u32
	table_size    int // every entry is one-bit, packed into `table`
	num_functions int // 1~16
mut:
	table []u8
}

/*
TODO maybe allow pointing to generic fns?
fn empty_cb[T](x T) u32 {
	panic('empty BloomFilter.hash_func callback')
}
*/

// Salt values(random values).  These salts are XORed with the output of the hash function to give multiple unique hashes.
const salts = [
	// vfmt off
		u32(0xefd8c55b),0xa1c57493,0x174c3763,0xc26e60d4,
		0x9ec387fe,0xdcdc9e97,0xfc495ddc,0x6a1fa748,
		0x8d82a03b,0x38dc692a,0x97d0f42d,0x048a2be3,
		0x9b5d83aa,0x2380d32f,0x2437552f,0xcc622295,
	// vfmt on
]

fn (b &BloomFilter[T]) free() {
	unsafe {
		free(b.table)
	}
}

// new_bloom_filter_fast creates a new bloom_filter. `table_size` is 16384, and `num_functions` is 4.
pub fn new_bloom_filter_fast[T](hash_func fn (T) u32) &BloomFilter[T] {
	return &BloomFilter[T]{
		hash_func:     hash_func
		table_size:    16384
		num_functions: 4
		table:         []u8{len: (16384 + 7) / 8}
	}
}

// new_bloom_filter creates a new bloom_filter. `table_size` should be greater than 0, and `num_functions` should be 1~16.
pub fn new_bloom_filter[T](hash_func fn (T) u32, table_size int, num_functions int) !&BloomFilter[T] {
	if table_size <= 0 {
		return error('table_size should great that 0')
	}
	if num_functions < 1 || num_functions > salts.len {
		return error('num_functions should between 1~${salts.len}')
	}

	return &BloomFilter[T]{
		hash_func:     hash_func
		table_size:    table_size
		num_functions: num_functions
		table:         []u8{len: (table_size + 7) / 8}
	}
}

// adds the element to bloom filter.
pub fn (mut b BloomFilter[T]) add(element T) {
	hash := b.hash_func(element)

	for i in 0 .. b.num_functions {
		subhash := hash ^ salts[i]
		index := int(subhash % u32(b.table_size))
		bb := u8((1 << (index % 8)))
		b.table[index / 8] |= bb
	}
}

// checks the element is exists.
pub fn (b &BloomFilter[T]) exists(element T) bool {
	hash := b.hash_func(element)
	for i in 0 .. b.num_functions {
		subhash := hash ^ salts[i]
		index := int(subhash % u32(b.table_size))
		bb := b.table[index / 8]
		bit := 1 << (index % 8)
		if bb & bit == 0 {
			return false
		}
	}

	return true
}

// @union returns the union of the two bloom filters.
pub fn (l &BloomFilter[T]) @union(r &BloomFilter[T]) !&BloomFilter[T] {
	if l.table_size != r.table_size || l.num_functions != r.num_functions
		|| l.hash_func != r.hash_func {
		return error('Both filters must be created with the same values.')
	}

	mut new_f := BloomFilter[T]{
		hash_func:     l.hash_func
		table_size:    l.table_size
		num_functions: l.num_functions
		table:         []u8{len: (l.table_size + 7) / 8}
	}
	for i in 0 .. l.table.len {
		new_f.table[i] = l.table[i] | r.table[i]
	}

	return &new_f
}

// intersection returns the intersection of bloom filters.
pub fn (l &BloomFilter[T]) intersection(r &BloomFilter[T]) !&BloomFilter[T] {
	if l.table_size != r.table_size || l.num_functions != r.num_functions
		|| l.hash_func != r.hash_func {
		return error('Both filters must be created with the same values.')
	}

	mut new_f := BloomFilter[T]{
		hash_func:     l.hash_func
		table_size:    l.table_size
		num_functions: l.num_functions
		table:         []u8{len: (l.table_size + 7) / 8}
	}
	for i in 0 .. l.table.len {
		new_f.table[i] = l.table[i] & r.table[i]
	}

	return &new_f
}
