module dlmalloc

// This is an internal test, so `v test` type-checks every function body of the
// module, including the allocator paths that no other vlib code calls.
fn test_least_bit_isolates_the_lowest_set_bit() {
	assert least_bit(0b1011_0100) == 0b100
	assert least_bit(u32(1) << 31) == u32(1) << 31
	assert least_bit(0) == 0
}

fn test_left_bits_sets_every_bit_above_the_given_one() {
	assert left_bits(1) == 0xffff_fffe
	assert left_bits(0b100) == 0xffff_fff8
	assert left_bits(u32(1) << 8) == 0xffff_fe00
	assert left_bits(u32(1) << 30) == u32(1) << 31
	assert left_bits(u32(1) << 31) == 0
	assert left_bits(0) == 0
}

fn test_chunk_prev_steps_back_by_prev_foot() {
	mut buf := [8]uint{}
	unsafe {
		mut c := &Chunk(&buf[4])
		c.prev_foot = 2 * sizeof(uint)
		assert voidptr(c.prev()) == voidptr(&buf[2])
	}
}

fn test_tree_bins_keep_chunks_reachable_by_their_size_bits() {
	mut dl := new(get_system_allocator())
	// All of these sizes share one tree bin (768..1023 bytes).
	sizes := [uint(0x300), 0x3f0, 0x380, 0x340, 0x3c0, 0x310, 0x3a0, 0x3e0]
	mut chunks := []TreeChunk{len: sizes.len}
	idx := dl.compute_tree_index(sizes[0])
	unsafe {
		for i, size in sizes {
			assert dl.compute_tree_index(size) == idx
			chunks[i].chunk.head = size
			dl.insert_large_chunk(&chunks[i], size)
		}
		for i, size in sizes {
			// Descend like `tmalloc_large`: one bit of the size per level.
			mut t := *dl.treebin_at(idx)
			mut size_bits := size << leftshift_for_tree_index(idx)
			for !isnil(t) && t.chunk().size() != size {
				t = t.child[int((size_bits >> (sizeof(uint) * 8 - 1)) & 1)]
				size_bits <<= 1
			}
			assert voidptr(t) == voidptr(&chunks[i])
		}
	}
}

fn test_new_starts_with_empty_bins() {
	dl := new(get_system_allocator())
	assert dl.smallmap == 0
	assert dl.treemap == 0
	assert dl.footprint == 0
}

fn test_global_allocator_round_trip() {
	$if windows {
		// The Windows system allocator is a stub that never returns memory.
		return
	}
	unsafe {
		p := &u8(malloc(100))
		assert !isnil(p)
		assert uint(p) % malloc_alignment() == 0
		for i in 0 .. 100 {
			p[i] = u8(i)
		}
		q := &u8(realloc(p, 100, 5000))
		assert !isnil(q)
		for i in 0 .. 100 {
			assert q[i] == u8(i)
		}
		z := &u8(calloc(300))
		assert !isnil(z)
		for i in 0 .. 300 {
			assert z[i] == 0
		}
		a := memalign(1000, 256)
		assert !isnil(a)
		assert uint(a) % 256 == 0
		free(a)
		free(z)
		free(q)
	}
}

fn test_stress_with_the_system_allocator() {
	$if windows {
		return
	}
	mut dl := new(get_system_allocator())
	stress(mut dl, 30_000, 0x2545f491, 3 * 1024 * 1024)
}

fn test_stress_with_contiguous_and_scattered_segments() {
	// Hands out regions directly above, directly below and away from the
	// previous ones, so that the extend, prepend and add-segment paths of
	// `sys_alloc` all run, whatever the platform's mmap does.
	mut arena := new_test_arena(32 * 1024 * 1024)
	mut dl := new(arena.allocator())
	stress(mut dl, 20_000, 0x9e3779b9, 512 * 1024)
	assert arena.calls > 6
	arena.release()
}

fn test_realloc_releases_the_old_block_when_it_moves() {
	mut arena := new_test_arena(1024 * 1024)
	mut dl := new(arena.allocator())
	unsafe {
		p := dl.malloc(20_000)
		blocker := dl.malloc(16)
		// On a fresh heap `blocker` directly follows `p`, so `p` cannot grow in place.
		assert uint(blocker) == uint(p) + chunk_from_mem(p).size()
		q := dl.realloc(p, 40_000)
		assert !isnil(q)
		assert q != p
		// the old block is free, and is handed out again
		assert !chunk_from_mem(blocker).pinuse()
		assert dl.malloc(20_000) == p
	}
	arena.release()
}

// Block is one live allocation of `stress`.
struct Block {
mut:
	ptr  &u8 = unsafe { nil }
	size uint
	seed u8
}

struct Rng {
mut:
	state u32
}

fn (mut r Rng) next() u32 {
	r.state ^= r.state << 13
	r.state ^= r.state >> 17
	r.state ^= r.state << 5
	return r.state
}

fn (mut r Rng) size(max_huge uint) uint {
	kind := r.next() % 16
	return if kind < 9 {
		// small bins, up to just above `max_small_request`
		uint(1 + r.next() % 248)
	} else if kind < 14 {
		// tree bins
		uint(233 + r.next() % 16_000)
	} else if kind < 15 {
		uint(16_000 + r.next() % 200_000)
	} else {
		uint(1 + r.next() % u32(max_huge))
	}
}

fn pattern(seed u8, i uint) u8 {
	return u8(uint(seed) * 7 + i * 13 + (i >> 9))
}

// next_sample visits every byte near the ends of a block, and one byte per KiB
// in between, so that big blocks stay cheap to fill and check.
fn next_sample(i uint, n uint) uint {
	if i < 64 || i + 64 >= n {
		return i + 1
	}
	next := (i | 1023) + 1
	return if next + 64 < n { next } else { n - 64 }
}

fn fill(b Block) {
	mut i := uint(0)
	for i < b.size {
		unsafe {
			b.ptr[i] = pattern(b.seed, i)
		}
		i = next_sample(i, b.size)
	}
}

// check verifies the first `n` bytes of `b`, sampled as `fill` wrote them.
fn check(b Block, n uint) {
	mut i := uint(0)
	for i < n {
		got := unsafe { b.ptr[i] }
		if got != pattern(b.seed, i) {
			assert false, 'block ${voidptr(b.ptr)} of ${b.size} bytes is corrupted at offset ${i}'
		}
		i = next_sample(i, b.size)
	}
}

fn check_new_block(blocks []Block, idx int, b Block, align uint) {
	assert !isnil(b.ptr), 'allocation of ${b.size} bytes failed'
	assert uint(b.ptr) % align == 0
	c := chunk_from_mem(b.ptr)
	assert c.cinuse()
	assert c.size() >= b.size + chunk_overhead()
	assert c.next().pinuse()
	lo := uint(b.ptr)
	hi := lo + b.size
	for j, other in blocks {
		if j == idx || isnil(other.ptr) {
			continue
		}
		olo := uint(other.ptr)
		assert hi <= olo || olo + other.size <= lo, 'blocks overlap'
	}
}

@[unsafe]
fn stress(mut dl Dlmalloc, rounds int, seed u32, max_huge uint) {
	mut rng := Rng{seed}
	mut blocks := []Block{len: 256}
	for round in 0 .. rounds {
		idx := int(rng.next() % u32(blocks.len))
		mut b := blocks[idx]
		op := rng.next() % 8
		if isnil(b.ptr) {
			b.size = rng.size(max_huge)
			b.seed = u8(round)
			mut align := malloc_alignment()
			unsafe {
				if op == 0 {
					b.ptr = dl.calloc(b.size)
					for i in 0 .. b.size {
						assert b.ptr[i] == 0
					}
				} else if op == 1 {
					align = uint(32) << (rng.next() % 8)
					b.ptr = dl.memalign(align, b.size)
				} else {
					b.ptr = dl.malloc(b.size)
				}
			}
			check_new_block(blocks, idx, b, align)
			fill(b)
		} else if op < 3 {
			check(b, b.size)
			new_size := rng.size(max_huge)
			mut moved := b
			moved.ptr = unsafe { dl.realloc(b.ptr, new_size) }
			b.ptr = moved.ptr
			b.size = new_size
			check_new_block(blocks, idx, b, malloc_alignment())
			// the old contents, in the old block's sampling layout
			check(moved, if moved.size < new_size { moved.size } else { new_size })
			b.seed = u8(round)
			fill(b)
		} else {
			check(b, b.size)
			unsafe { dl.free_(b.ptr) }
			b.ptr = unsafe { nil }
		}
		blocks[idx] = b
	}
	for b in blocks {
		if !isnil(b.ptr) {
			check(b, b.size)
			unsafe { dl.free_(b.ptr) }
		}
	}
	assert dl.footprint <= dl.max_footprint
}

// TestArena is a system allocator over one buffer.
@[heap]
struct TestArena {
mut:
	buf   []u8
	start uint
	end   uint
	lo    uint
	hi    uint
	calls int
}

fn new_test_arena(size uint) &TestArena {
	buf := []u8{len: int(size)}
	start := align_up(uint(buf.data), default_granularity())
	end := uint(buf.data) + size
	middle := align_up(start + (end - start) / 2, default_granularity())
	return &TestArena{
		buf:   buf
		start: start
		end:   end
		lo:    middle
		hi:    middle
	}
}

fn (a &TestArena) allocator() Allocator {
	return Allocator{
		alloc:            arena_alloc
		remap:            system_remap
		free_part:        arena_free_part
		free_:            arena_free
		can_release_part: fn (_ voidptr, _ u32) bool {
			return true
		}
		allocates_zeros:  fn (_ voidptr) bool {
			return false
		}
		page_size:        system_page_size
		data:             voidptr(a)
	}
}

fn (mut a TestArena) release() {
	unsafe { a.buf.free() }
}

@[unsafe]
fn arena_alloc(data voidptr, size uint) (voidptr, uint, u32) {
	mut a := unsafe { &TestArena(data) }
	a.calls++
	mut addr := uint(0)
	match a.calls % 3 {
		1 {
			// directly above the highest region
			addr = a.hi
		}
		2 {
			// directly below the lowest region
			if a.lo - a.start < size {
				return unsafe { nil }, 0, 0
			}
			a.lo -= size
			return voidptr(a.lo), size, 0
		}
		else {
			// with a gap above the highest region
			addr = a.hi + default_granularity()
		}
	}
	if a.end < size || addr > a.end - size {
		return unsafe { nil }, 0, 0
	}
	a.hi = addr + size
	return voidptr(addr), size, 0
}

// Released memory stays readable, so it is overwritten to make any later use
// of it by the allocator visible.
fn arena_free(_ voidptr, ptr voidptr, size uint) bool {
	unsafe { vmemset(ptr, 0xa5, int(size)) }
	return true
}

fn arena_free_part(_ voidptr, ptr voidptr, oldsize uint, newsize uint) bool {
	unsafe { vmemset(voidptr(uint(ptr) + newsize), 0xa5, int(oldsize - newsize)) }
	return true
}
