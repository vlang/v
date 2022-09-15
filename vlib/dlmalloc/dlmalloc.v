// This is a version of dlmalloc.c ported to V. You can find the original
// source at ftp://g.oswego.edu/pub/misc/malloc.c
//
// The original source was written by Doug Lea and released to the public domain
//
//
// # Why dlmalloc?
//
// This library does not rely on C code. The primary purpose is for use in freestanding
// build mode and for WASM target.
//
// dlmalloc is not the most performant allocator. It's main purpose is to be
// easily portable and easy to learn. Here we have straight port of C and dlmalloc-rs
// versions of dlmalloc.
module dlmalloc

import math.bits

/*
$if debug ? {
	#include "valgrind.h"
}

fn //C.VALGRIND_MALLOCLIKE_BLOCK(addr voidptr, size usize, rzb usize,is_zeroed bool)
fn //C.VALGRIND_FREELIKE_BLOCK(addr voidptr, rzB usize)
fn //C.VALGRIND_MAKE_MEM_UNDEFINED(addr voidptr, size usize)
*/

pub const (
	n_small_bins           = 32
	n_tree_bins            = 32
	small_bin_shift        = 3
	tree_bin_shift         = 8

	max_release_check_rate = 4095
)

fn usize_leading_zeros(x usize) usize {
	if sizeof(usize) == 8 {
		return usize(bits.leading_zeros_64(u64(x)))
	} else {
		return usize(bits.leading_zeros_32(u32(x)))
	}
}

[inline]
fn default_granularity() usize {
	return 64 * 1024
}

[inline]
fn default_trim_threshold() usize {
	return 2 * 1024 * 1024
}

[inline]
fn malloc_alignment() usize {
	return sizeof(usize) * 2
}

[inline]
fn chunk_overhead() usize {
	return sizeof(usize)
}

[inline]
fn min_large_size() usize {
	return 1 << dlmalloc.tree_bin_shift
}

[inline]
fn mmap_chunk_overhead() usize {
	return 2 * sizeof(usize)
}

[inline]
fn max_small_size() usize {
	return min_large_size() - 1
}

[inline]
fn max_small_request() usize {
	return max_small_size() - (malloc_alignment() - 1) - chunk_overhead()
}

[inline]
fn min_chunk_size() usize {
	return align_up(sizeof(Chunk), malloc_alignment())
}

[inline]
fn chunk_mem_offset() usize {
	return 2 * sizeof(usize)
}

[inline]
fn min_request() usize {
	return min_chunk_size() - chunk_overhead() - 1
}

[inline]
fn top_foot_size() usize {
	return align_offset_usize(chunk_mem_offset()) + pad_request(sizeof(Segment)) + min_chunk_size()
}

[inline]
fn max_request() usize {
	return calc_max_request()
}

[inline]
fn mmap_foot_pad() usize {
	return 4 * sizeof(usize)
}

fn min_sys_alloc_space() usize {
	return ((~0 - (default_granularity() + top_foot_size() + malloc_alignment()) +
		1) & ~malloc_alignment()) - chunk_overhead() + 1
}

fn calc_max_request() usize {
	x := min_sys_alloc_space()
	y := (~min_chunk_size() + 1) << 2

	if x < y {
		return x
	} else {
		return y
	}
}

fn pad_request(amt usize) usize {
	return align_up(amt + chunk_overhead(), malloc_alignment())
}

fn align_offset_usize(addr usize) usize {
	return align_up(addr, malloc_alignment()) - addr
}

fn is_aligned(a usize) bool {
	return a & (malloc_alignment() - 1) == 0
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
	if a % alignment == 0 {
		return a
	} else {
		return a - (a % alignment) + alignment
	}
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
	chunk := ptr + chunk_mem_offset()
	return &Chunk(ptr + align_offset_usize(chunk))
}

fn request_2_size(req usize) usize {
	if req < min_request() {
		return min_chunk_size()
	} else {
		return pad_request(req)
	}
}

fn overhead_for(c &Chunk) usize {
	if c.mmapped() {
		return mmap_chunk_overhead()
	} else {
		return chunk_overhead()
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
	free_            fn (voidptr, voidptr, usize) bool
	can_release_part fn (voidptr, u32) bool
	allocates_zeros  fn (voidptr) bool
	page_size        fn (voidptr) usize // not a constant field because some platforms might have different page sizes depending on configs
	data             voidptr
}

pub struct Dlmalloc {
	system_allocator Allocator
	max_request      usize = 4294901657
mut:
	// bin maps
	smallmap u32 // bin map for small bins
	treemap  u32 // bin map  for tree bins

	smallbins      [66]&Chunk // small bins, it is actually (n_small_bins + 1) * 2
	treebins       [n_tree_bins]&TreeChunk
	dvsize         usize
	topsize        usize
	dv             &Chunk = unsafe { nil }
	top            &Chunk = unsafe { nil }
	footprint      usize
	max_footprint  usize
	seg            Segment
	trim_check     u32
	least_addr     voidptr
	release_checks usize
}

pub fn new(system_allocator Allocator) Dlmalloc {
	return Dlmalloc{
		smallmap: 0
		treemap: 0
		smallbins: unsafe { [(dlmalloc.n_small_bins + 1) * 2]&Chunk{} }
		treebins: unsafe { [dlmalloc.n_tree_bins]&TreeChunk{} }
		dvsize: 0
		topsize: 0
		dv: unsafe { nil }
		top: unsafe { nil }
		footprint: 0
		max_footprint: 0
		seg: Segment{unsafe { nil }, 0, unsafe { nil }, 0}
		trim_check: 0
		least_addr: unsafe { nil }
		release_checks: 0
		system_allocator: system_allocator
		max_request: 4294901657
	}
}

[heap]
struct Chunk {
mut:
	prev_foot usize
	head      usize
	prev      &Chunk = unsafe { nil }
	next      &Chunk = unsafe { nil }
}

[heap]
struct Segment {
mut:
	base  voidptr
	size  usize
	next  &Segment = unsafe { nil }
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
	pinuse    = 1 << 0
	cinuse    = 1 << 1
	flag4     = 1 << 2
	inuse     = pinuse | cinuse
	flag_bits = pinuse | cinuse | flag4
)

fn fencepost_head() usize {
	return dlmalloc.inuse | sizeof(usize)
}

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
	mut n := unsafe { n_ }
	n.clear_pinuse()
	c.set_size_and_pinuse_of_free_chunk(size)
}

fn (c &Chunk) set_foot(size usize) {
	mut next := c.plus_offset(size)
	next.prev_foot = size
}

fn (c &Chunk) plus_offset(offset usize) &Chunk {
	return &Chunk((usize(c) + offset))
}

fn (c &Chunk) minus_offset(offset usize) &Chunk {
	return &Chunk(usize(c) - offset)
}

fn (c &Chunk) to_mem() voidptr {
	return voidptr(usize(c) + chunk_mem_offset())
}

fn chunk_from_mem(mem_ voidptr) &Chunk {
	mem := usize(mem_)
	return &Chunk((mem - chunk_mem_offset()))
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

fn (tree &TreeChunk) size(treemap u32) usize {
	return tree.chunk.head & ~dlmalloc.flag_bits
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
	unsafe {
		return &Chunk(&dl.smallbins[idx * 2])
	}
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
	mut chunk := unsafe { chunk_ }
	mut f := chunk.prev
	mut b := chunk.next
	idx := small_index(size)

	if voidptr(b) == voidptr(f) {
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
		mut r := &TreeChunk(nil)
		if voidptr(chunk.next()) != voidptr(chunk) {
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
				*rp = &TreeChunk(nil)
			}
		}

		if isnil(xp) {
			return
		}

		mut h := dl.treebin_at(chunk.index)
		if voidptr(chunk) == voidptr(*h) {
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
	mut next := unsafe { next_ }
	mut head := unsafe { head_ }
	println('Unlink first small')
	mut ptr := next.prev
	if voidptr(head) == voidptr(ptr) {
		unsafe { dl.clear_smallmap(idx) }
	} else {
		ptr.next = head
		head.prev = ptr
	}
}

// calloc is the same as `malloc`, except if the allocation succeeds it's guaranteed
// to point to `size` bytes of zeros.
[unsafe]
pub fn (mut dl Dlmalloc) calloc(size usize) voidptr {
	unsafe {
		ptr := dl.malloc(size)
		if !isnil(ptr) && dl.calloc_must_clear(ptr) {
			vmemset(ptr, 0, int(size))
		}
		return ptr
	}
}

// free_ behaves as libc free, but operates within the given space
[unsafe]
pub fn (mut dl Dlmalloc) free_(mem voidptr) {
	unsafe {
		// C.VALGRIND_FREELIKE_BLOCK(mem, 0)
		mut p := chunk_from_mem(mem)

		mut psize := p.size()

		next := p.plus_offset(psize)

		if !p.pinuse() {
			prevsize := p.prev_foot

			if p.mmapped() {
				psize += prevsize + mmap_foot_pad()
				if dl.system_allocator.free_(dl.system_allocator.data, voidptr(usize(p) - prevsize),
					psize)
				{
					dl.footprint -= psize
				}

				return
			}

			prev := p.minus_offset(prevsize)
			psize += prevsize
			p = prev
			if voidptr(p) != voidptr(dl.dv) {
				dl.unlink_chunk(p, prevsize)
			} else if (next.head & dlmalloc.inuse) == dlmalloc.inuse {
				dl.dvsize = psize
				p.set_free_with_pinuse(psize, next)

				return
			}
		}
		// consolidate forward if we can
		if !next.cinuse() {
			if voidptr(next) == voidptr(dl.top) {
				dl.topsize += psize
				p.head = 0

				tsize := dl.topsize
				dl.top = p
				p.head = tsize | dlmalloc.pinuse
				if voidptr(p) == voidptr(dl.dv) {
					dl.dv = nil
					dl.dvsize = 0
				}

				if dl.should_trim(tsize) {
					dl.sys_trim(0)
				}

				return
			} else if voidptr(next) == voidptr(dl.dv) {
				dl.dvsize += psize
				dsize := dl.dvsize
				dl.dv = p
				p.set_size_and_pinuse_of_free_chunk(dsize)

				return
			} else {
				nsize := next.size()
				psize += nsize
				dl.unlink_chunk(next, nsize)
				p.set_size_and_pinuse_of_free_chunk(psize)

				if voidptr(p) == voidptr(dl.dv) {
					dl.dvsize = psize
					return
				}
			}
		} else {
			p.set_free_with_pinuse(psize, next)
		}

		if is_small(psize) {
			dl.insert_small_chunk(p, psize)
		} else {
			dl.insert_large_chunk(&TreeChunk(p), psize)
			dl.release_checks -= 1
			if dl.release_checks == 0 {
				dl.release_unused_segments()
			}
		}
	}
}

fn (dl Dlmalloc) should_trim(size usize) bool {
	return size > dl.trim_check
}

[unsafe]
fn (mut dl Dlmalloc) sys_trim(pad_ usize) bool {
	unsafe {
		mut pad := pad_
		mut released := usize(0)
		if pad < dl.max_request && !isnil(dl.top) {
			pad += top_foot_size()
			if dl.topsize > pad {
				unit := usize(default_granularity())
				extra := ((dl.topsize - pad + unit - 1) / unit - 1) * unit
				mut sp := dl.segment_holding(dl.top)

				if !sp.is_extern() {
					if sp.can_release_part(&dl.system_allocator) {
						if sp.size >= extra && !dl.has_segment_link(sp) {
							newsize := sp.size - extra
							if dl.system_allocator.free_part(dl.system_allocator.data,
								sp.base, sp.size, newsize)
							{
								released = extra
							}
						}
					}
				}

				if released != 0 {
					sp.size -= released
					dl.footprint -= released
					top := dl.top
					topsize := dl.topsize - released
					dl.init_top(top, topsize)
				}
			}

			released += dl.release_unused_segments()

			if released == 0 && dl.topsize > dl.trim_check {
				dl.trim_check = 1 << 31
			}
		}
		return released != 0
	}
}

[unsafe]
fn (mut dl Dlmalloc) release_unused_segments() usize {
	unsafe {
		mut released := usize(0)
		mut nsegs := usize(0)
		mut pred := &dl.seg
		mut sp := pred.next
		for !isnil(sp) {
			base := sp.base
			size := sp.size
			next := sp.next

			nsegs += 1

			if sp.can_release_part(&dl.system_allocator) && !sp.is_extern() {
				mut p := align_as_chunk(base)
				psize := p.size()
				chunk_top := voidptr(usize(p) + psize)
				top := voidptr(usize(base) + (size - top_foot_size()))
				if !p.inuse() && chunk_top >= top {
					mut tp := &TreeChunk(p)
					if voidptr(p) == voidptr(dl.dv) {
						dl.dv = nil
						dl.dvsize = 0
					} else {
						dl.unlink_large_chunk(tp)
					}

					if dl.system_allocator.free_(dl.system_allocator.data, base, size) {
						released += size
						dl.footprint -= size
						sp = pred
						sp.next = next
					} else {
						// back out if we can't unmap
						dl.insert_large_chunk(tp, psize)
					}
				}
			}
			pred = sp
			sp = next
		}
		dl.release_checks = if nsegs > dlmalloc.max_release_check_rate {
			nsegs
		} else {
			dlmalloc.max_release_check_rate
		}
		return released
	}
}

[unsafe]
fn (dl &Dlmalloc) has_segment_link(ptr &Segment) bool {
	mut sp := &dl.seg
	for !isnil(sp) {
		if ptr.holds(sp) {
			return true
		}
		sp = sp.next
	}
	return false
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
	mut chunk := unsafe { chunk_ }
	idx := small_index(size)
	unsafe {
		mut head := dl.smallbin_at(idx)
		mut f := head
		if !dl.smallmap_is_marked(idx) {
			dl.mark_smallmap(idx)
		} else {
			f = head.prev
		}

		assert !isnil(f)
		assert !isnil(head)
		head.prev = chunk
		f.next = chunk
		chunk.prev = f
		chunk.next = head
	}
}

[unsafe]
fn (mut dl Dlmalloc) insert_large_chunk(chunk_ &TreeChunk, size usize) {
	unsafe {
		mut chunk := chunk_
		idx := dl.compute_tree_index(size)
		mut h := dl.treebin_at(idx)

		chunk.index = idx
		chunk.child[0] = nil
		chunk.child[1] = nil

		mut chunkc := chunk.chunk()
		if !dl.treemap_is_marked(idx) {
			dl.mark_treemap(idx)
			*h = chunk
			chunk.parent = voidptr(h)
			assert !isnil(chunkc)
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
					assert !isnil(chunkc)
					tc.prev = chunkc
					chunkc.prev = f
					chunkc.next = tc
					chunk.parent = nil
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
	return (dl.smallmap & (1 << idx)) != 0
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

pub fn (mut dl Dlmalloc) malloc(size usize) voidptr {
	unsafe {
		p := dl.malloc_real(size)
		if !isnil(p) {
			// C.VALGRIND_MALLOCLIKE_BLOCK(p, size, 0,false)
		}
		return p
	}
}

/// malloc behaves as libc malloc, but operates within the given space
[unsafe]
fn (mut dl Dlmalloc) malloc_real(size usize) voidptr {
	mut nb := usize(0)
	unsafe {
		if size <= max_small_request() {
			nb = request_2_size(size)
			mut idx := small_index(nb)
			smallbits := dl.smallmap >> idx
			if smallbits & 0b11 != 0 {
				idx += ~smallbits & 1

				b := dl.smallbin_at(idx)
				mut p := b.prev
				smallsize := small_index2size(idx)

				dl.unlink_first_small_chunk(b, p, idx)

				p.set_inuse_and_pinuse(smallsize)

				ret := p.to_mem()

				return ret
			}

			if nb > dl.dvsize {
				// if there's some other bin with some memory, then we just use
				// the next smallest bin

				// todo(playXE): Find out why in the world this part of code does not work in
				// some programs (esp. x.json2). Theoretically disabling this path just
				// makes fragmentation a little worser but nothing really bad should happen
				if false && smallbits != 0 {
					leftbits := (smallbits << idx) & left_bits(1 << idx)
					leastbit := least_bit(leftbits)
					i := u32(bits.trailing_zeros_32(leastbit))
					mut b := dl.smallbin_at(i)
					mut p := b.prev
					dl.unlink_first_small_chunk(b, p, i)
					smallsize := small_index2size(i)
					rsize := smallsize - nb
					if sizeof(usize) != 4 && rsize < min_chunk_size() {
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
					mem := dl.tmalloc_small(nb)
					if !isnil(mem) {
						return mem
					}
				}
			}
		} else if size >= dl.max_request {
			return nil
		} else {
			nb = pad_request(size)
			if dl.treemap != 0 {
				mem := dl.tmalloc_large(nb)
				if !isnil(mem) {
					return mem
				}
			}
		}

		// use the `dv` node if we can, splitting it if necessary or otherwise
		// exhausting the entire chunk
		if nb <= dl.dvsize {
			rsize := dl.dvsize - nb
			mut p := dl.dv
			if rsize >= min_chunk_size() {
				dl.dv = p.plus_offset(nb)
				dl.dvsize = rsize
				mut r := dl.dv
				r.set_size_and_pinuse_of_free_chunk(rsize)
				p.set_size_and_pinuse_of_inuse_chunk(nb)
			} else {
				dvs := dl.dvsize
				dl.dvsize = 0
				dl.dv = nil
				p.set_inuse_and_pinuse(dvs)
			}
			ret := p.to_mem()

			return ret
		}
		// Split the top node if we can
		if nb < dl.topsize {
			dl.topsize -= nb
			rsize := dl.topsize
			mut p := dl.top
			dl.top = p.plus_offset(nb)
			mut r := dl.top
			r.head = rsize | dlmalloc.pinuse
			p.set_size_and_pinuse_of_inuse_chunk(nb)
			ret := p.to_mem()

			return ret
		}

		return dl.sys_alloc(nb)
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
	// C.VALGRIND_MAKE_MEM_UNDEFINED(p.plus_offset(sizeof(usize)),sizeof(usize))
	p.head = size | dlmalloc.pinuse
	// C.VALGRIND_MAKE_MEM_UNDEFINED(p.plus_offset(size + sizeof(usize)),sizeof(usize))
	p.plus_offset(size).head = top_foot_size()
	dl.trim_check = u32(default_trim_threshold())
}

[unsafe]
fn (mut dl Dlmalloc) sys_alloc(size usize) voidptr {
	page_size := dl.system_allocator.page_size(dl.system_allocator.data)
	asize := align_up(align_up(size + top_foot_size() + malloc_alignment(), default_granularity()),
		page_size)
	unsafe {
		alloc := dl.system_allocator.alloc
		tbase, mut tsize, flags := alloc(dl.system_allocator.data, asize)

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
			tsize_ := tsize - top_foot_size()
			dl.init_top(&Chunk(tbase), tsize_)
		} else {
			mut sp := &dl.seg
			for !isnil(sp) && voidptr(tbase) != voidptr(sp.top()) {
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

		if size < dl.topsize {
			dl.topsize -= size
			rsize := dl.topsize
			mut p := dl.top
			dl.top = p.plus_offset(size)
			mut r := dl.top
			r.head = rsize | dlmalloc.pinuse
			p.set_size_and_pinuse_of_inuse_chunk(size)
			ret := p.to_mem()

			return ret
		}
	}
	return unsafe { nil }
}

[unsafe]
fn (mut dl Dlmalloc) tmalloc_small(size usize) voidptr {
	unsafe {
		leastbit := least_bit(dl.treemap)
		i := bits.trailing_zeros_32(leastbit)
		mut v := *dl.treebin_at(u32(i))
		mut t := v
		mut rsize := t.size(dl.treemap)
		for {
			t = t.leftmost_child()
			if isnil(t) {
				break
			}

			trem := t.chunk().size() - size
			if trem < rsize {
				rsize = trem
				v = t
			}
		}

		mut vc := v.chunk()
		r := &TreeChunk(vc.plus_offset(size))
		dl.unlink_large_chunk(v)
		if rsize < min_chunk_size() {
			vc.set_inuse_and_pinuse(rsize + size)
		} else {
			mut rc := r.chunk()
			vc.set_size_and_pinuse_of_inuse_chunk(size)
			rc.set_size_and_pinuse_of_free_chunk(rsize)
			dl.replace_dv(rc, rsize)
		}

		return vc.to_mem()
	}
}

[unsafe]
fn (mut dl Dlmalloc) tmalloc_large(size usize) voidptr {
	unsafe {
		mut v := &TreeChunk(nil)
		mut rsize := ~size + 1
		idx := dl.compute_tree_index(size)
		mut t := *dl.treebin_at(idx)
		if !isnil(t) {
			mut sizebits := size << leftshift_for_tree_index(idx)
			mut rst := voidptr(u64(0))
			for {
				csize := t.chunk().size()
				if csize >= size && csize - size < rsize {
					v = t
					rsize = csize - size
					if rsize == 0 {
						break
					}
				}

				rt := t.child[1]
				t = t.child[(sizebits >> (sizeof(usize) * 8 - 1)) & 1]
				if !isnil(rt) && voidptr(rt) != voidptr(t) {
					rst = rt
				}
				if isnil(t) {
					t = rst
					break
				}
				sizebits <<= 1
			}
		}

		if isnil(t) && isnil(v) {
			leftbits := left_bits(1 << idx) & dl.treemap
			if leftbits != 0 {
				leastbit := least_bit(leftbits)
				i := bits.trailing_zeros_32(leastbit)
				t = *dl.treebin_at(u32(i))
			}
		}
		// Find the smallest of this tree or subtree
		for !isnil(t) {
			csize := t.chunk().size()
			if csize >= size && csize - size < rsize {
				rsize = csize - size
				v = t
			}
			t = t.leftmost_child()
		}

		if isnil(v) || (dl.dvsize >= size && !(rsize < dl.dvsize - size)) {
			return nil
		}

		mut vc := v.chunk()
		mut r := vc.plus_offset(size)
		dl.unlink_large_chunk(v)
		if rsize < min_chunk_size() {
			vc.set_inuse_and_pinuse(rsize + size)
		} else {
			vc.set_size_and_pinuse_of_inuse_chunk(size)
			r.set_size_and_pinuse_of_free_chunk(rsize)
			dl.insert_chunk(r, rsize)
		}

		return vc.to_mem()
	}
}

[unsafe]
fn (mut dl Dlmalloc) prepend_alloc(newbase voidptr, oldbase voidptr, size usize) voidptr {
	unsafe {
		mut p := align_as_chunk(newbase)
		mut oldfirst := align_as_chunk(oldbase)
		psize := usize(oldfirst) - usize(p)
		mut q := p.plus_offset(size)
		mut qsize := psize - size
		// C.VALGRIND_MAKE_MEM_UNDEFINED(p.plus_offset(sizeof(usize)),size)
		p.set_size_and_pinuse_of_inuse_chunk(size)

		if qsize >= sizeof(TreeChunk) {
			// C.VALGRIND_MAKE_MEM_UNDEFINED(q, sizeof(TreeChunk))
		} else {
			// C.VALGRIND_MAKE_MEM_UNDEFINED(q,sizeof(Chunk))
		}
		// C.VALGRIND_MAKE_MEM_UNDEFINED(q.plus_offset(qsize),sizeof(usize))

		if voidptr(oldfirst) == voidptr(dl.top) {
			dl.topsize += qsize
			tsize := dl.topsize
			dl.top = q
			q.head = tsize | dlmalloc.pinuse
		} else if voidptr(oldfirst) == voidptr(dl.dv) {
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
	// TODO: what in the world is this function doing????
	unsafe {
		old_top := dl.top
		mut oldsp := dl.segment_holding(old_top)
		old_end := oldsp.top()
		ssize := pad_request(sizeof(Segment))
		mut offset := ssize + sizeof(usize) * 4 + malloc_alignment() - 1
		rawsp := voidptr(usize(old_end) - offset)
		offset = align_offset_usize((&Chunk(rawsp)).to_mem())
		asp := voidptr(usize(rawsp) + offset)
		csp := if asp < voidptr(usize(old_top) + min_chunk_size()) { old_top } else { asp }
		mut sp := &Chunk(csp)
		mut ss := &Segment(sp.to_mem())
		mut tnext := sp.plus_offset(ssize)
		mut p := tnext
		mut nfences := 0

		size := tsize - top_foot_size()
		dl.init_top(&Chunk(tbase), size)

		sp.set_size_and_pinuse_of_inuse_chunk(ssize)
		*ss = dl.seg
		dl.seg.base = tbase
		dl.seg.size = tsize
		dl.seg.flags = flags
		dl.seg.next = ss

		for {
			nextp := p.plus_offset(sizeof(usize))
			p.head = fencepost_head()
			nfences += 1
			if nextp.head < old_end {
				p = nextp
			} else {
				break
			}
		}
		// TODO: why 2?
		assert nfences >= 2
		if voidptr(csp) != voidptr(old_top) {
			mut q := &Chunk(old_top)
			psize := usize(csp) - usize(old_top)
			tn := q.plus_offset(psize)
			q.set_free_with_pinuse(psize, tn)

			dl.insert_chunk(q, psize)
		}
	}
}

[unsafe]
fn (mut dl Dlmalloc) segment_holding(ptr voidptr) &Segment {
	mut sp := &dl.seg
	for !isnil(sp) {
		if sp.base <= ptr && ptr < sp.top() {
			return sp
		}
		sp = sp.next
	}
	return &Segment(0)
}

// realloc behaves as libc realloc, but operates within the given space
[unsafe]
pub fn (mut dl Dlmalloc) realloc(oldmem voidptr, bytes usize) voidptr {
	if bytes >= dl.max_request {
		return unsafe { nil }
	}
	unsafe {
		nb := request_2_size(bytes)
		mut oldp := chunk_from_mem(oldmem)
		newp := dl.try_realloc_chunk(oldp, nb, true)
		if !isnil(newp) {
			return newp.to_mem()
		}

		ptr := dl.malloc(bytes)
		if !isnil(ptr) {
			oc := oldp.size() - overhead_for(oldp)
			copy_bytes := if oc < bytes { oc } else { bytes }
			vmemcpy(ptr, oldmem, int(copy_bytes))
		}

		return ptr
	}
}

// memaligns allocates memory aligned to `alignment_`. Only call this with power-of-two alignment
// and alignment > dlmalloc.malloc_alignment
[unsafe]
pub fn (mut dl Dlmalloc) memalign(alignment_ usize, bytes usize) voidptr {
	mut alignment := alignment_
	if alignment < min_chunk_size() {
		alignment = min_chunk_size()
	}

	if bytes >= max_request() - alignment {
		return unsafe { nil }
	}
	unsafe {
		nb := request_2_size(bytes)
		req := nb + alignment + min_chunk_size() - chunk_overhead()
		mem := dl.malloc(req)
		if isnil(mem) {
			return mem
		}

		mut p := chunk_from_mem(mem)
		if usize(mem) & (alignment - 1) != 0 {
			// Here we find an aligned sopt inside the chunk. Since we need to
			// give back leading space in a chunk of at least `min_chunk_size`,
			// if the first calculation places us at a spot with less than
			// `min_chunk_size` leader we can move to the next aligned spot.
			// we've allocated enough total room so that this is always possible
			br_ := (usize(mem) + alignment - 1) & (~alignment + 1)
			br := chunk_from_mem(voidptr(br_))
			mut pos := voidptr(u64(0))
			if usize(br) - usize(p) > min_chunk_size() {
				pos = voidptr(br)
			} else {
				pos = voidptr(usize(br) + alignment)
			}

			mut newp := &Chunk(pos)
			leadsize := usize(pos) - usize(p)
			newsize := p.size() - leadsize

			if p.mmapped() {
				newp.prev_foot = p.prev_foot + leadsize
				newp.head = newsize
			} else {
				newp.set_inuse(newsize)
				p.set_inuse(leadsize)
				dl.dispose_chunk(p, leadsize)
			}
			p = newp
		}

		if !p.mmapped() {
			size := p.size()
			if size > nb + min_chunk_size() {
				remainder_size := size - nb
				mut remainder := p.plus_offset(nb)
				p.set_inuse(nb)
				remainder.set_inuse(remainder_size)
				dl.dispose_chunk(remainder, remainder_size)
			}
		}

		// C.VALGRIND_MALLOCLIKE_BLOCK(p.to_mem(), bytes, 0, false)
		return p.to_mem()
	}
}

[unsafe]
fn (mut dl Dlmalloc) try_realloc_chunk(p_ &Chunk, nb usize, can_move bool) &Chunk {
	unsafe {
		mut p := p_
		oldsize := p.size()
		mut next := p.plus_offset(oldsize)
		if p.mmapped() {
			return dl.mmap_resize(p, nb, can_move)
		} else if oldsize >= nb {
			rsize := oldsize - nb
			if rsize >= min_chunk_size() {
				mut r := p.plus_offset(nb)
				p.set_inuse(nb)
				r.set_inuse(rsize)
				dl.dispose_chunk(r, rsize)
			}
			return p
		} else if voidptr(next) == voidptr(dl.top) {
			if oldsize + dl.topsize <= nb {
				return nil
			}

			newsize := oldsize + dl.topsize
			newtopsize := newsize - nb
			mut newtop := p.plus_offset(nb)
			p.set_inuse(nb)
			newtop.head = newtopsize | dlmalloc.pinuse
			dl.top = newtop
			dl.topsize = newtopsize
			return p
		} else if voidptr(next) == voidptr(dl.dv) {
			dvs := dl.dvsize
			if oldsize + dvs < nb {
				return nil
			}

			dsize := oldsize + dvs - nb
			if dsize >= min_chunk_size() {
				mut r := p.plus_offset(nb)
				mut n := r.plus_offset(dsize)
				p.set_inuse(nb)
				r.set_size_and_pinuse_of_free_chunk(dsize)
				n.clear_pinuse()
				dl.dvsize = dsize
				dl.dv = r
			} else {
				newsize := oldsize + dvs
				p.set_inuse(newsize)
				dl.dvsize = 0
				dl.dv = nil
			}
			return p
		} else if !next.cinuse() {
			nextsize := next.size()
			if oldsize + nextsize < nb {
				return nil
			}
			rsize := oldsize + nextsize - nb
			dl.unlink_chunk(next, nextsize)
			if rsize < min_chunk_size() {
				newsize := oldsize + nextsize
				p.set_inuse(newsize)
			} else {
				r := p.plus_offset(nb)
				p.set_inuse(nb)
				r.set_inuse(rsize)
				dl.dispose_chunk(r, rsize)
			}
			return p
		} else {
			return nil
		}
	}
}

[unsafe]
fn (mut dl Dlmalloc) mmap_resize(oldp_ &Chunk, nb usize, can_move bool) &Chunk {
	mut oldp := unsafe { oldp_ }
	oldsize := oldp.size()
	if is_small(nb) {
		return unsafe { nil }
	}
	// Keep the old chunk if it's big enough but not too big
	if oldsize >= nb + sizeof(usize) && (oldsize - nb) <= (default_granularity() << 1) {
		return oldp
	}

	offset := oldp.prev_foot
	oldmmsize := oldsize + offset + mmap_foot_pad()
	newmmsize := dl.mmap_align(nb + 6 * sizeof(usize) + malloc_alignment() - 1)

	ptr := dl.system_allocator.remap(dl.system_allocator.data, voidptr(usize(oldp) - offset),
		oldmmsize, newmmsize, can_move)
	if isnil(ptr) {
		return unsafe { nil }
	}

	mut newp := &Chunk(voidptr(usize(ptr) + offset))
	psize := newmmsize - offset - mmap_foot_pad()
	newp.head = psize
	newp.plus_offset(psize).head = fencepost_head()
	newp.plus_offset(psize + sizeof(usize)).head = 0
	if ptr < dl.least_addr {
		dl.least_addr = ptr
	}
	dl.footprint = dl.footprint + newmmsize - oldmmsize
	if dl.footprint > dl.max_footprint {
		dl.max_footprint = dl.footprint
	}
	return newp
}

fn (dl &Dlmalloc) mmap_align(a usize) usize {
	return align_up(a, dl.system_allocator.page_size(dl.system_allocator.data))
}

[unsafe]
fn (mut dl Dlmalloc) dispose_chunk(p_ &Chunk, psize_ usize) {
	mut p := unsafe { p_ }
	mut psize := psize_
	unsafe {
		mut next := p.plus_offset(psize)
		if !p.pinuse() {
			prevsize := p.prev_foot
			if p.mmapped() {
				psize += prevsize + mmap_foot_pad()

				if dl.system_allocator.free_(dl.system_allocator.data, voidptr(usize(p) - prevsize),
					psize)
				{
					dl.footprint -= psize
				}
				return
			}

			prev := p.minus_offset(prevsize)
			psize += prevsize
			p = prev
			if voidptr(p) != voidptr(dl.dv) {
				dl.unlink_chunk(p, prevsize)
			} else if next.head & dlmalloc.inuse == dlmalloc.inuse {
				dl.dvsize = psize
				p.set_free_with_pinuse(psize, next)
				return
			}
		}

		if !next.cinuse() {
			if voidptr(next) == voidptr(dl.top) {
				dl.topsize += psize
				tsize := dl.topsize
				dl.top = p
				p.head = tsize | dlmalloc.pinuse
				if voidptr(p) == voidptr(dl.dv) {
					dl.dv = nil
					dl.dvsize = 0
				}
				return
			} else if voidptr(next) == voidptr(dl.dv) {
				dl.dvsize += psize
				dvsize := dl.dvsize
				dl.dv = p
				p.set_size_and_pinuse_of_free_chunk(dvsize)
				return
			} else {
				nsize := next.size()
				psize += nsize
				dl.unlink_chunk(next, nsize)
				p.set_size_and_pinuse_of_free_chunk(psize)
				if voidptr(p) == voidptr(dl.dv) {
					dl.dvsize = psize
					return
				}
			}
		} else {
			p.set_free_with_pinuse(psize, next)
		}
		dl.insert_chunk(p, psize)
	}
}
