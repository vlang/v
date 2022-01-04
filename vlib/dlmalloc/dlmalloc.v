// This is a version of dlmalloc.c ported to V. You can find the original
// source at ftp://g.oswego.edu/pub/misc/malloc.c
//
// The original source was written by Doug Lea and released to the public domain
module dlmalloc

import math.bits

const (
	n_small_bins           = 32
	n_tree_bins            = 32
	small_bin_shift        = 3
	tree_bin_shift         = 8
	default_granularity    = 64 * 1024
	default_trim_threshold = 2 * 1024 * 1024
	max_release_check_rate = 4095
	malloc_alignment       = sizeof(usize) * 2
	chunk_overhead         = sizeof(usize)
	mmap_chnk_overhead     = 2 * sizeof(usize)
	min_large_size         = 1 << tree_bin_shift
	max_small_size         = min_large_size - 1
	max_small_request      = max_small_size - (malloc_alignment - 1) - chunk_overhead
	min_chunk_size         = align_up(sizeof(Chunk), malloc_alignment)
	chunk_mem_offset       = 2 * sizeof(usize)
	min_request            = min_chunk_size - chunk_overhead - 1
	top_foot_size          = align_offset_usize(chunk_mem_offset) + pad_request(sizeof(Segment)) +
		min_chunk_size
	max_request            = calc_max_request()
	mmap_foot_pad          = 4 * sizeof(usize)
)

fn usize_leading_zeros(x usize) usize {
	if sizeof(usize) == 8 {
		return usize(bits.leading_zeros_64(u64(x)))
	} else {
		return usize(bits.leading_zeros_32(u32(x)))
	}
}

fn min_sys_alloc_space() usize {
	return ((~0 - (dlmalloc.default_granularity + dlmalloc.top_foot_size +
		dlmalloc.malloc_alignment) + 1) & ~dlmalloc.malloc_alignment) - dlmalloc.chunk_overhead + 1
}

fn calc_max_request() usize {
	x := min_sys_alloc_space()
	y := (~dlmalloc.min_chunk_size + 1) << 2
	if x < y {
		return x
	} else {
		return y
	}
}

fn pad_request(amt usize) usize {
	return align_up(amt + dlmalloc.chunk_overhead, dlmalloc.malloc_alignment)
}

fn align_offset_usize(addr usize) usize {
	return align_up(addr, dlmalloc.malloc_alignment) - addr
}

fn is_aligned(a usize) bool {
	return a & (dlmalloc.malloc_alignment - 1) == 0
}

fn is_small(s usize) bool {
	return s >> dlmalloc.small_bin_shift < dlmalloc.n_small_bins
}

fn small_index2size(idx u32) usize {
	return usize(idx) << dlmalloc.small_bin_shift
}

fn small_index(size usize) u32 {
	return u32(size >> dlmalloc.small_bin_shift)
}

fn align_up(a usize, alignment usize) usize {
	return (a + (alignment - 1)) & ~(alignment - 1)
}

fn left_bits(x u32) u32 {
	return (x << 1) | (~(x << 1)) + 1
}

fn least_bit(x u32) u32 {
	return x & (~x + 1)
}

fn leftshift_for_tree_index(x u32) u32 {
	y := usize(x)
	if y == dlmalloc.n_tree_bins - 1 {
		return 0
	} else {
		return u32(sizeof(usize) * 8 - 1 - ((y >> 1) + dlmalloc.tree_bin_shift - 2))
	}
}

[unsafe]
fn align_as_chunk(ptr_ voidptr) &Chunk {
	ptr := usize(ptr_)
	chunk := ptr + dlmalloc.chunk_mem_offset
	return &Chunk(ptr + align_offset_usize(chunk))
}

fn request_2_size(req usize) usize {
	if req < dlmalloc.min_request {
		return dlmalloc.min_chunk_size
	} else {
		return pad_request(req)
	}
}

fn overhead_for(req usize) usize {
	if req < dlmalloc.min_request {
		return dlmalloc.min_chunk_size
	} else {
		return pad_request(req)
	}
}

// In order for dlmalloc to efficently manage memory, it needs a way to communicate with the underlying platform.
// This `Allocator` type provides an interface for this communication.
//
//
// Why not `interface?` Interfaces require memory allocation so it is simpler to pass a struct.
pub struct Allocator {
	alloc            fn (voidptr, usize) (voidptr, usize, u32)
	remap            fn (voidptr, voidptr, usize, usize, bool) voidptr
	free_part        fn (voidptr, voidptr, usize, usize) bool
	free             fn (voidptr, voidptr, usize) bool
	can_release_part fn (voidptr, u32) bool
	allocates_zeros  fn (voidptr) bool
	page_size        fn (voidptr) usize // not a constant field because some platforms might have different page sizes depending on configs
	data             voidptr
}

pub struct Dlmalloc {
	system_allocator Allocator
mut:
	// bin maps
	smallmap u32 // bin map for small bins
	treemap  u32 // bin map  for tree bins

	smallbins      [66]&Chunk // small bins, it is actually (n_small_bins + 1) * 2
	treebins       [n_tree_bins]&TreeChunk
	dvsize         usize
	topsize        usize
	dv             &Chunk
	top            &Chunk
	footprint      usize
	max_footprint  usize
	seg            Segment
	trim_check     u32
	least_addr     voidptr
	release_checks usize
}

[heap]
struct Chunk {
mut:
	prev_foot usize
	head      usize
	prev      &Chunk
	next      &Chunk
}

[heap]
struct Segment {
mut:
	base  voidptr
	size  usize
	next  &Segment
	flags u32
}

[heap]
struct TreeChunk {
mut:
	chunk  Chunk
	child  [2]voidptr
	parent voidptr
	index  u32
}

const (
	pinuse         = 1 << 0
	cinuse         = 1 << 1
	flag4          = 1 << 2
	inuse          = pinuse | cinuse
	flag_bits      = pinuse | cinuse | flag4
	fencepost_head = inuse | sizeof(usize)
)

fn (c &Chunk) size() usize {
	return c.head & ~dlmalloc.flag_bits
}

fn (c &Chunk) mmapped() bool {
	return c.head & dlmalloc.inuse == 0
}

fn (c &Chunk) next() &Chunk {
	mut me := usize(c)
	me = me + c.size()
	return &Chunk(me)
}

fn (c &Chunk) prev() &Chunk {
	mut me := usize(c)
	me = me + c.prev_foot
	return &Chunk(me)
}

fn (c &Chunk) cinuse() bool {
	return c.head & dlmalloc.cinuse != 0
}

fn (c &Chunk) pinuse() bool {
	return c.head & dlmalloc.pinuse != 0
}

fn (mut c Chunk) clear_pinuse() {
	c.head &= ~dlmalloc.pinuse
}

fn (c &Chunk) inuse() bool {
	return c.head & dlmalloc.inuse != dlmalloc.pinuse
}

fn (mut c Chunk) set_inuse(size usize) {
	c.head = (c.head & dlmalloc.pinuse) | size | dlmalloc.cinuse
	mut next := c.plus_offset(size)
	next.head |= dlmalloc.pinuse
}

fn (mut c Chunk) set_inuse_and_pinuse(size usize) {
	c.head = dlmalloc.pinuse | size | dlmalloc.cinuse
	mut next := c.plus_offset(size)
	next.head |= dlmalloc.pinuse
}

fn (mut c Chunk) set_size_and_pinuse_of_inuse_chunk(size usize) {
	c.head = size | dlmalloc.pinuse | dlmalloc.cinuse
}

fn (mut c Chunk) set_size_and_pinuse_of_free_chunk(size usize) {
	c.head = size | dlmalloc.pinuse
	c.set_foot(size)
}

fn (mut c Chunk) set_free_with_pinuse(size usize, n_ &Chunk) {
	mut n := n_
	n.clear_pinuse()
	c.set_size_and_pinuse_of_free_chunk(size)
}

fn (c &Chunk) set_foot(size usize) {
	mut next := c.plus_offset(size)
	next.prev_foot = size
}

fn (c &Chunk) plus_offset(offset usize) &Chunk {
	return &Chunk(usize(c) + offset)
}

fn (c &Chunk) minus_offset(offset usize) &Chunk {
	return &Chunk(usize(c) - offset)
}

fn (c &Chunk) to_mem() voidptr {
	return voidptr(usize(c) + dlmalloc.chunk_mem_offset)
}

fn chunk_from_mem(mem_ voidptr) &Chunk {
	mem := usize(mem_)
	return &Chunk((mem - dlmalloc.chunk_mem_offset))
}

fn (tree &TreeChunk) leftmost_child() &TreeChunk {
	left := &TreeChunk(tree.child[0])
	if isnil(left) {
		return tree.child[1]
	} else {
		return left
	}
}

fn (tree &TreeChunk) chunk() &Chunk {
	return &tree.chunk
}

[unsafe]
fn (tree &TreeChunk) next() &TreeChunk {
	unsafe {
		return &TreeChunk(tree.chunk().next)
	}
}

[unsafe]
fn (tree &TreeChunk) prev() &TreeChunk {
	unsafe {
		return &TreeChunk(tree.chunk().prev)
	}
}

const extern = 1 << 0

fn (seg &Segment) is_extern() bool {
	return seg.flags & dlmalloc.extern != 0
}

fn (seg &Segment) can_release_part(sys_alloc &Allocator) bool {
	return sys_alloc.can_release_part(sys_alloc.data, seg.flags >> 1)
}

fn (seg &Segment) sys_flags() u32 {
	return seg.flags >> 1
}

fn (seg &Segment) holds(addr voidptr) bool {
	return seg.base <= addr && addr < seg.top()
}

fn (seg &Segment) top() voidptr {
	return voidptr(usize(seg.base) + seg.size)
}

[unsafe]
pub fn (dl &Dlmalloc) calloc_must_clear(ptr voidptr) bool {
	return !dl.system_allocator.allocates_zeros(dl.system_allocator.data)
		|| !chunk_from_mem(ptr).mmapped()
}

[unsafe]
fn (mut dl Dlmalloc) smallbin_at(idx u32) &Chunk {
	return dl.smallbins[idx * 2]
}

[unsafe]
fn (mut dl Dlmalloc) treebin_at(idx u32) &&TreeChunk {
	return &dl.treebins[idx]
}

fn (dl &Dlmalloc) compute_tree_index(size usize) u32 {
	x := size >> dlmalloc.tree_bin_shift
	if x == 0 {
		return 0
	} else if x > 0xffff {
		return dlmalloc.n_tree_bins - 1
	} else {
		k := sizeof(usize) * 8 - 1 - usize_leading_zeros(x)
		return u32((k << 1) + (size >> (k + dlmalloc.tree_bin_shift - 1) & 1))
	}
}

[unsafe]
fn (mut dl Dlmalloc) unlink_chunk(chunk &Chunk, size usize) {
	unsafe {
		if is_small(size) {
			dl.unlink_small_chunk(chunk, size)
		} else {
			dl.unlink_large_chunk(&TreeChunk(chunk))
		}
	}
}

[unsafe]
fn (mut dl Dlmalloc) unlink_small_chunk(chunk_ &Chunk, size usize) {
	mut chunk := chunk_
	mut f := chunk.prev
	mut b := chunk.next
	idx := small_index(size)

	if b == f {
		unsafe { dl.clear_smallmap(idx) }
	} else {
		f.next = b
		b.prev = f
	}
}

[unsafe]
fn (mut dl Dlmalloc) unlink_large_chunk(chunk_ &TreeChunk) {
	unsafe {
		mut chunk := chunk_
		mut xp := &TreeChunk(chunk.parent)
		mut r := &TreeChunk(voidptr(0))
		if chunk.next() != chunk {
			mut f := chunk.prev()
			r = chunk.next()
			f.chunk.next = r.chunk()
			r.chunk.prev = f.chunk()
		} else {
			mut rp := &&TreeChunk(&chunk.child[1])
			if isnil(*rp) {
				rp = &&TreeChunk(&chunk.child[0])
			}

			r = *rp
			if !isnil(*rp) {
				for {
					mut cp := &&TreeChunk(&rp.child[1])
					if isnil(*cp) {
						cp = &&TreeChunk(&rp.child[0])
					}
					if isnil(*cp) {
						break
					}
					rp = cp
				}
				r = *rp
				*rp = &TreeChunk(voidptr(0))
			}
		}

		if isnil(xp) {
			return
		}

		mut h := dl.treebin_at(chunk.index)
		if chunk == *h {
			*h = r
			if isnil(r) {
				dl.clear_treemap(chunk.index)
			}
		} else {
			if xp.child[0] == chunk {
				xp.child[0] = r
			} else {
				xp.child[1] = r
			}
		}

		if !isnil(r) {
			r.parent = xp
			mut c0 := &TreeChunk(chunk.child[0])
			if !isnil(c0) {
				r.child[0] = c0
				c0.parent = r
			}
			mut c1 := &TreeChunk(chunk.child[1])
			if !isnil(c1) {
				r.child[1] = c1
				c1.parent = r
			}
		}
	}
}

[unsafe]
fn (mut dl Dlmalloc) unlink_first_small_chunk(head_ &Chunk, next_ &Chunk, idx u32) {
	mut next := next_
	mut head := head_
	mut ptr := next.prev
	if head == ptr {
		unsafe { dl.clear_smallmap(idx) }
	} else {
		ptr.next = head
		head.prev = ptr
	}
}

[unsafe]
fn (mut dl Dlmalloc) replace_dv(chunk &Chunk, size usize) {
	dvs := dl.dvsize
	if dvs != 0 {
		dv := dl.dv
		unsafe {
			dl.insert_small_chunk(dv, dvs)
		}
	}
	dl.dvsize = size
	dl.dv = chunk
}

[unsafe]
fn (mut dl Dlmalloc) insert_chunk(chunk &Chunk, size usize) {
	unsafe {
		if is_small(size) {
			dl.insert_small_chunk(chunk, size)
		} else {
			dl.insert_large_chunk(&TreeChunk(chunk), size)
		}
	}
}

[unsafe]
fn (mut dl Dlmalloc) insert_small_chunk(chunk_ &Chunk, size usize) {
	mut chunk := chunk_
	idx := small_index(size)
	unsafe {
		mut head := dl.smallbin_at(idx)
		mut f := head
		if !dl.smallmap_is_marked(idx) {
			dl.mark_smallmap(idx)
		} else {
			f = head.prev
		}
		head.prev = chunk
		f.next = chunk
		chunk.prev = f
		chunk.next = head
	}
}

[unsafe]
fn (mut dl Dlmalloc) insert_large_chunk(chunk_ &TreeChunk, size usize) {
	mut chunk := chunk_
	unsafe {
		idx := dl.compute_tree_index(size)
		mut h := dl.treebin_at(idx)

		chunk.index = idx
		chunk.child[0] = voidptr(0)
		chunk.child[1] = voidptr(0)

		mut chunkc := chunk.chunk()
		if !dl.treemap_is_marked(idx) {
			dl.mark_treemap(idx)
			*h = chunk
			chunk.parent = voidptr(h)
			chunkc.prev = chunkc
			chunkc.next = chunkc
		} else {
			mut t := *h
			mut k := size << leftshift_for_tree_index(idx)
			for {
				if t.chunk().size() != size {
					c_ := &t.child[(k >> sizeof(usize) * 8 - 1) & 1]
					mut c := &&TreeChunk(c_)
					k <<= 1
					if !isnil(c) {
						t = *c
					} else {
						*c = chunk
						chunk.parent = t
						chunkc.next = chunkc
						chunkc.prev = chunkc
						break
					}
				} else {
					tc := t.chunk()
					f := tc.prev
					f.next = chunkc
					tc.prev = chunkc
					chunkc.prev = f
					chunkc.next = tc
					chunk.parent = voidptr(0)
					break
				}
			}
		}
	}
}

[unsafe]
fn (mut dl Dlmalloc) clear_smallmap(idx u32) {
	dl.smallmap &= ~(1 << idx)
}

[unsafe]
fn (mut dl Dlmalloc) mark_smallmap(idx u32) {
	dl.smallmap |= 1 << idx
}

[unsafe]
fn (mut dl Dlmalloc) smallmap_is_marked(idx u32) bool {
	return dl.smallmap & (1 << idx) != 0
}

[unsafe]
fn (mut dl Dlmalloc) clear_treemap(idx u32) {
	dl.treemap &= ~(1 << idx)
}

[unsafe]
fn (mut dl Dlmalloc) mark_treemap(idx u32) {
	dl.treemap |= 1 << idx
}

[unsafe]
fn (mut dl Dlmalloc) treemap_is_marked(idx u32) bool {
	return dl.treemap & (1 << idx) != 0
}

[unsafe]
pub fn (mut dl Dlmalloc) malloc(size usize) voidptr {
	mut nb := usize(0)
	unsafe {
		if size <= dlmalloc.max_small_request {
			nb = request_2_size(size)
			mut idx := small_index(nb)
			smallbits := dl.smallmap >> idx
			if smallbits & 0b11 != 0 {
				idx += ~smallbits & 1

				b := dl.smallbin_at(idx)
				mut p := b.prev
				dl.unlink_first_small_chunk(b, p, idx)
				smallsize := small_index2size(idx)
				p.set_inuse_and_pinuse(smallsize)
				ret := p.to_mem()
				return ret
			}

			if nb > dl.dvsize {
				// if there's some other bin with some memory, then we just use
				// the next smallest bin
				if smallbits != 0 {
					leftbits := (smallbits << idx) & left_bits(1 << idx)
					leastbit := least_bit(leftbits)
					i := u32(bits.trailing_zeros_32(leastbit))
					mut b := dl.smallbin_at(i)
					mut p := b.prev
					dl.unlink_first_small_chunk(b, p, i)
					smallsize := small_index2size(i)
					rsize := smallsize - nb
					if sizeof(usize) != 4 && rsize < dlmalloc.min_chunk_size {
						p.set_inuse_and_pinuse(smallsize)
					} else {
						p.set_size_and_pinuse_of_inuse_chunk(nb)
						mut r := p.plus_offset(nb)
						r.set_size_and_pinuse_of_free_chunk(size)
						dl.replace_dv(r, rsize)
					}
					ret := p.to_mem()
					return ret
				} else if dl.treemap != 0 {
					panic('todo: tmalloc_small')
				}
			}
		} else if size >= dlmalloc.max_request {
			return voidptr(0)
		} else {
			panic('todo: tlmalloc_large')
		}
		return dl.sys_alloc(size)
	}
}

[unsafe]
fn (mut dl Dlmalloc) init_bins() {
	unsafe {
		for i in 0 .. dlmalloc.n_small_bins {
			mut bin := dl.smallbin_at(i)
			bin.prev = bin
			bin.next = bin
		}
	}
}

[unsafe]
fn (mut dl Dlmalloc) init_top(ptr &Chunk, size_ usize) {
	offset := align_offset_usize(ptr.to_mem())
	mut p := ptr.plus_offset(offset)
	size := size_ - offset
	dl.top = p
	dl.topsize = size
	p.head = size | dlmalloc.pinuse
	p.plus_offset(size).head = dlmalloc.top_foot_size
	dl.trim_check = dlmalloc.default_trim_threshold
}

[unsafe]
fn (mut dl Dlmalloc) sys_alloc(size usize) voidptr {
	asize := align_up(size + dlmalloc.top_foot_size + dlmalloc.malloc_alignment, dlmalloc.default_granularity)
	unsafe {
		tbase, mut tsize, flags := dl.system_allocator.alloc(dl.system_allocator.data,
			asize)
		if isnil(tbase) {
			return tbase
		}
		dl.footprint += tsize
		dl.max_footprint = if dl.max_footprint > dl.footprint {
			dl.max_footprint
		} else {
			dl.footprint
		}
		if isnil(dl.top) {
			if isnil(dl.least_addr) || tbase < dl.least_addr {
				dl.least_addr = tbase
			}
			dl.seg.base = tbase
			dl.seg.size = tsize
			dl.seg.flags = flags
			dl.release_checks = dlmalloc.max_release_check_rate
			dl.init_bins()
			tsize_ := tsize - dlmalloc.top_foot_size
			dl.init_top(&Chunk(tbase), tsize_)
		} else {
			mut sp := &dl.seg
			for !isnil(sp) && tbase != sp.top() {
				sp = sp.next
			}

			if !isnil(sp) && !sp.is_extern() && sp.sys_flags() == flags && sp.holds(dl.top) {
				sp.size += tsize
				ptr := dl.top
				size_ := dl.topsize + tsize
				dl.init_top(ptr, size_)
			} else {
				if tbase < dl.least_addr {
					dl.least_addr = tbase
				} else {
					dl.least_addr = dl.least_addr
				}
				sp = &dl.seg
				for !isnil(sp) && sp.base != voidptr(usize(tbase) + tsize) {
					sp = sp.next
				}

				if !isnil(sp) && !sp.is_extern() && sp.sys_flags() == flags {
					oldbase := sp.base
					sp.base = tbase
					sp.size = tsize
					return dl.prepend_alloc(tbase, oldbase, size)
				} else {
					dl.add_segment(tbase, tsize, flags)
				}
			}
		}
	}
	return voidptr(0)
}

[unsafe]
fn (mut dl Dlmalloc) prepend_alloc(newbase voidptr, oldbase voidptr, size usize) voidptr {
	unsafe {
		mut p := align_as_chunk(newbase)
		mut oldfirst := align_as_chunk(oldbase)
		psize := usize(oldfirst) - usize(p)
		mut q := p.plus_offset(size)
		mut qsize := psize - size
		p.set_size_and_pinuse_of_inuse_chunk(size)

		if oldfirst == dl.top {
			dl.topsize += qsize
			tsize := dl.topsize
			dl.top = q
			q.head = tsize | dlmalloc.pinuse
		} else if oldfirst == dl.dv {
			dl.dvsize += qsize
			dsize := dl.dvsize
			dl.dv = q
			q.set_size_and_pinuse_of_free_chunk(dsize)
		} else {
			if !oldfirst.inuse() {
				nsize := oldfirst.size()
				dl.unlink_chunk(oldfirst, nsize)
				oldfirst = oldfirst.plus_offset(nsize)
				qsize += nsize
			}
			q.set_free_with_pinuse(qsize, oldfirst)
			dl.insert_chunk(q, qsize)
		}

		ret := p.to_mem()
		return ret
	}
}

[unsafe]
fn (mut dl Dlmalloc) add_segment(tbase voidptr, tsize usize, flags u32) {
}
