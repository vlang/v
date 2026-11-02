// Copyright (c) 2020 Igalia, S.L.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// walloc is a small malloc implementation for use in WebAssembly targets.
// It is a pure V port of thirdparty/walloc/walloc.c: same chunked page
// layout, same small object size classes, same best-fit large object
// allocator with splitting and merging.
//
// The allocator manages the wasm linear memory directly. On wasm32 (C
// backend) it uses the clang memory.size/grow builtins and the `__heap_base`
// linker symbol. On every other target it manages a simulated linear memory
// (a page-aligned host block), so the exact same algorithm is exercised by
// `v test vlib/walloc/` on the host.
//
// The freestanding `wasm_bare` builtin wires a global instance of this
// allocator to the C `malloc`/`free` symbols (see
// vlib/builtin/wasm_bare/libc_impl.c.v).
@[has_globals]
module walloc

$if wasm32 {
	#insert "@VEXEROOT/vlib/walloc/walloc_heap_base.h"
}

$if wasm32 {
	fn C.__builtin_wasm_memory_size(mem uint) uint
	fn C.__builtin_wasm_memory_grow(mem uint, delta uint) uint
	__global C.__heap_base u8
}

// Raw libc allocation, used only by the host simulation of linear memory.
// The signatures match vlib/builtin/cfns.c.v exactly.
fn C.malloc(size uint) voidptr
fn C.memset(dst voidptr, c i32, n uint) voidptr

const chunk_size = 256
const chunk_mask = chunk_size - 1
const page_size = 65536
const page_mask = page_size - 1
const granule_size = 8
const granule_size_log_2 = 3
const page_header_size = 256
const first_allocatable_chunk = 1
const num_small_kinds = 10

// Chunk kinds 0..9 are the indices into `small_granules` below.
const kind_free_large = u8(254)
const kind_large = u8(255)

// Small object payload sizes, in granules (8 bytes each).
const small_granules = [u8(1), 2, 3, 4, 5, 6, 8, 10, 16, 32]!

// A free small object is just a next pointer; live ones hold user data.
struct FreeNode {
mut:
	next &FreeNode = unsafe { nil }
}

// A free or live large object: header followed by the payload.
struct LargeObject {
mut:
	next &LargeObject = unsafe { nil }
	size uint
}

// Walloc manages one linear memory, either the real wasm one (on wasm32) or
// a simulated page-aligned host block (everywhere else, for testing).
pub struct Walloc {
mut:
	small_freelists [10]&FreeNode
	large_objects   &LargeObject = unsafe { nil }
	heap_size       uint
	pending_compact bool
	// Host-only simulation of the wasm linear memory.
	sim_base      &u8 = unsafe { nil }
	sim_pages     uint
	sim_cap_pages uint
	sim_heap_off  uint
}

// new_walloc returns a fresh allocator. On wasm32 it manages the real linear
// memory; on the host it manages a simulated one (initially 2 pages of a
// 256 page reservation), so the wasm paths are testable without wasm hardware.
pub fn new_walloc() Walloc {
	$if wasm32 {
		return Walloc{}
	} $else {
		cap_pages := uint(256)
		total := cap_pages * uint(page_size) + uint(page_size)
		raw := C.malloc(total)
		if raw == unsafe { nil } {
			unsafe {
				panic('walloc: could not reserve simulated linear memory')
			}
		}
		aligned := (uint(raw) + uint(page_size) - 1) & ~(uint(page_size) - 1)
		base := unsafe { &u8(voidptr(aligned)) }
		C.memset(base, 0, cap_pages * uint(page_size))
		return Walloc{
			sim_base:      base
			sim_pages:     2
			sim_cap_pages: cap_pages
			sim_heap_off:  uint(page_size)
		}
	}
}

// linear_pages returns the current linear memory size, in pages.
@[unsafe]
fn (w Walloc) linear_pages() uint {
	$if wasm32 {
		return unsafe { C.__builtin_wasm_memory_size(0) }
	} $else {
		return w.sim_pages
	}
}

// linear_grow extends the linear memory by `delta_pages` pages and returns
// the previous size in pages, or ~uint(0) on failure. New pages read as zero.
@[unsafe]
fn (mut w Walloc) linear_grow(delta_pages uint) uint {
	$if wasm32 {
		return unsafe { C.__builtin_wasm_memory_grow(0, delta_pages) }
	} $else {
		unsafe {
			if w.sim_pages + delta_pages > w.sim_cap_pages {
				return ~uint(0)
			}
			old := w.sim_pages
			C.memset(voidptr(uint(w.sim_base) + old * uint(page_size)), 0, delta_pages * uint(page_size))
			w.sim_pages = old + delta_pages
			return old
		}
	}
}

// heap_base_off is the linear-memory offset where static data ends.
@[unsafe]
fn (w Walloc) heap_base_off() uint {
	$if wasm32 {
		return unsafe { uint(&C.__heap_base) }
	} $else {
		return w.sim_heap_off
	}
}

// ptr_at translates a linear-memory offset into a pointer.
@[inline; unsafe]
fn (w Walloc) ptr_at(off uint) &u8 {
	$if wasm32 {
		// On wasm32 linear-memory offsets are addresses.
		return unsafe { &u8(voidptr(off)) }
	} $else {
		return unsafe { w.sim_base + off }
	}
}

@[inline; unsafe]
fn wmax(a uint, b uint) uint {
	return if a < b { b } else { a }
}

@[inline; unsafe]
fn walign(val uint, alignment uint) uint {
	return (val + alignment - 1) & ~(alignment - 1)
}

// page_of maps a heap pointer to its 64KiB page base. This works on absolute
// addresses too, because every linear memory base is page aligned.
@[inline; unsafe]
fn page_of(ptr voidptr) &u8 {
	return unsafe { &u8(uint(ptr) & ~uint(page_mask)) }
}

@[inline; unsafe]
fn chunk_index_of(ptr voidptr) uint {
	return (uint(ptr) & uint(page_mask)) / uint(chunk_size)
}

// The page header is the 256 chunk-kind bytes at the page start.
@[inline; unsafe]
fn chunk_kind_at(page &u8, idx uint) u8 {
	return unsafe { page[idx] }
}

@[inline; unsafe]
fn set_chunk_kind_at(page &u8, idx uint, kind u8) {
	unsafe {
		page[idx] = kind
	}
}

@[inline; unsafe]
fn chunk_at(page &u8, idx uint) &u8 {
	return unsafe { page + idx * uint(chunk_size) }
}

@[inline; unsafe]
fn large_header_size() uint {
	return uint(sizeof(LargeObject))
}

@[inline; unsafe]
fn large_payload(obj &LargeObject) &u8 {
	return unsafe { &u8(obj) + large_header_size() }
}

@[inline; unsafe]
fn large_from_payload(ptr voidptr) &LargeObject {
	return unsafe { &LargeObject(&u8(ptr) - large_header_size()) }
}

fn granules_to_kind(granules uint) u8 {
	for i, g in small_granules {
		if granules <= uint(g) {
			return u8(i)
		}
	}
	return kind_large
}

@[inline; unsafe]
fn kind_to_granules(kind u8) uint {
	if kind < u8(num_small_kinds) {
		return uint(small_granules[int(kind)])
	}
	return ~uint(0)
}

// allocate_pages reserves whole pages holding at least `payload_size` bytes
// after the page header. It returns the first allocatable page and the number
// of pages the reservation spans.
@[unsafe]
fn (mut w Walloc) allocate_pages(payload_size uint) (&u8, uint) {
	unsafe {
		needed := payload_size + uint(page_header_size)
		heap_bytes := w.linear_pages() * uint(page_size)
		mut base := heap_bytes
		mut preallocated := uint(0)
		mut grow := uint(0)
		if w.heap_size == 0 {
			// First call: claim whatever the toolchain already mapped past the
			// static data (already zeroed, page aligned use starts at `base`).
			heap_base := walign(w.heap_base_off(), uint(page_size))
			preallocated = heap_bytes - heap_base
			w.heap_size = preallocated
			base -= preallocated
		}
		if preallocated < needed {
			// Always grow the heap by at least 50%.
			grow = walign(wmax(w.heap_size / 2, needed - preallocated), uint(page_size))
			assert grow != 0
			if w.linear_grow(grow / uint(page_size)) == ~uint(0) {
				return &u8(unsafe { nil }), 0
			}
			w.heap_size += grow
		}
		ret := w.ptr_at(base)
		size := grow + preallocated
		assert size != 0
		assert size & uint(page_mask) == 0
		return ret, size / uint(page_size)
	}
}

@[unsafe]
fn allocate_chunk(page &u8, idx uint, kind u8) &u8 {
	unsafe {
		set_chunk_kind_at(page, idx, kind)
		return chunk_at(page, idx)
	}
}

// maybe_repurpose_head moves a free large object that fits in a single chunk
// back to the 32-granule small object freelist.
@[unsafe]
fn (mut w Walloc) maybe_repurpose_head() {
	unsafe {
		if w.large_objects.size < uint(chunk_size) {
			idx := chunk_index_of(w.large_objects)
			ptr := allocate_chunk(page_of(w.large_objects), idx, u8(num_small_kinds - 1))
			w.large_objects = w.large_objects.next
			head := &FreeNode(ptr)
			head.next = w.small_freelists[num_small_kinds - 1]
			w.small_freelists[num_small_kinds - 1] = head
		}
	}
}

// unlink_large removes `target` from the large object freelist.
@[unsafe]
fn (mut w Walloc) unlink_large(target &LargeObject) bool {
	unsafe {
		if w.large_objects == target {
			w.large_objects = target.next
			return true
		}
		mut walk := w.large_objects
		for walk != unsafe { nil } {
			if walk.next == target {
				walk.next = target.next
				return true
			}
			walk = walk.next
		}
		return false
	}
}

// maybe_compact merges adjacent free large objects. It runs at most once per
// large allocation after frees happened, and repeats passes until no pair
// merges, which is equivalent to the single-pass predecessor fixup in the C
// original.
@[unsafe]
fn (mut w Walloc) maybe_compact() {
	unsafe {
		if !w.pending_compact {
			return
		}
		w.pending_compact = false
		mut merged := true
		for merged {
			merged = false
			mut cur := w.large_objects
			for cur != unsafe { nil } {
				end := uint(large_payload(cur)) + cur.size
				assert end & uint(chunk_mask) == 0
				chunk := (end & uint(page_mask)) / uint(chunk_size)
				if chunk >= uint(first_allocatable_chunk) {
					page := page_of(voidptr(end))
					if chunk_kind_at(page, chunk) == kind_free_large {
						tail := &LargeObject(voidptr(end))
						unlinked := w.unlink_large(tail)
						assert unlinked
						if unlinked {
							cur.size += large_header_size() + tail.size
							merged = true
							continue
						}
					}
				}
				cur = cur.next
			}
		}
	}
}

// allocate_large_object reserves `size` payload bytes in a chunk-aligned large
// object with a header. Fitting free objects are reused (best fit) and split;
// otherwise fresh pages are claimed and the remainder is split off.
@[unsafe]
fn (mut w Walloc) allocate_large_object(req_size uint) &LargeObject {
	unsafe {
		mut size := req_size
		w.maybe_compact()
		mut best := &LargeObject(unsafe { nil })
		mut best_prev_node := &LargeObject(unsafe { nil })
		mut best_is_head := true
		mut best_size := ~uint(0)
		mut prev_node := &LargeObject(unsafe { nil })
		mut is_head := true
		mut walk := w.large_objects
		for walk != unsafe { nil } {
			if walk.size >= size && walk.size < best_size {
				best_size = walk.size
				best = walk
				best_prev_node = prev_node
				best_is_head = is_head
				if best_size + large_header_size() == walign(size + large_header_size(), uint(chunk_size)) {
					// Not going to do any better than this; just return it.
					break
				}
			}
			prev_node = walk
			is_head = false
			walk = walk.next
		}
		if best == unsafe { nil } {
			// No free object fits; claim fresh pages and treat them as one object.
			size_with_header := size + large_header_size()
			page, n_allocated := w.allocate_pages(size_with_header)
			if page == unsafe { nil } {
				return unsafe { nil }
			}
			ptr := allocate_chunk(page, uint(first_allocatable_chunk), kind_large)
			best = &LargeObject(ptr)
			page_header := uint(ptr) - uint(page)
			best.next = w.large_objects
			best.size = n_allocated * uint(page_size) - page_header - large_header_size()
			best_size = best.size
			assert best_size >= size_with_header
		}
		set_chunk_kind_at(page_of(best), chunk_index_of(best), kind_large)
		next := best.next
		if best_is_head {
			w.large_objects = next
		} else {
			best_prev_node.next = next
		}
		mut tail_size := (best_size - size) & ~uint(chunk_mask)
		if tail_size != 0 {
			// The best-fitting object has 1 or more aligned chunks free after the
			// requested allocation; split the tail off into a fresh object.
			start_page := page_of(best)
			mut start := uint(large_payload(best))
			end := start + best_size
			if start_page == page_of(voidptr(end - tail_size - 1)) {
				// The allocation does not span a page boundary; yay.
				assert end & uint(chunk_mask) == 0
			} else if size < uint(page_size) - large_header_size() - uint(chunk_size) {
				// The allocation itself is smaller than a page; split off the
				// head, then fall through to maybe split the tail.
				assert end & uint(page_mask) == 0
				first_page_size := uint(page_size) - (start & uint(page_mask))
				head := best
				set_chunk_kind_at(start_page, chunk_index_of(head), kind_free_large)
				head.size = first_page_size
				head.next = w.large_objects
				w.large_objects = head
				w.maybe_repurpose_head()
				next_page := voidptr(uint(start_page) + uint(page_size))
				ptr2 := allocate_chunk(page_of(next_page), uint(first_allocatable_chunk),
					kind_large)
				best = &LargeObject(ptr2)
				best.size = best_size - first_page_size - uint(chunk_size) - large_header_size()
				assert best.size >= size
				start = uint(large_payload(best))
				tail_size = (best.size - size) & ~uint(chunk_mask)
			} else {
				// A large object spanning more than one page consumes all of its
				// tail pages, so round the split up to a page boundary.
				assert end & uint(page_mask) == 0
				first_page_size := uint(page_size) - (start & uint(page_mask))
				tail_pages_size := walign(size - first_page_size, uint(page_size))
				size = first_page_size + tail_pages_size
				tail_size = best_size - size
			}
			best.size -= tail_size
			mut tail_idx := chunk_index_of(voidptr(end - tail_size))
			for tail_idx < uint(first_allocatable_chunk) && tail_size != 0 {
				// Never split inside a page header.
				tail_size -= uint(chunk_size)
				tail_idx++
			}
			if tail_size != 0 {
				page := page_of(voidptr(end - tail_size))
				tail_ptr := allocate_chunk(page, tail_idx, kind_free_large)
				tail := &LargeObject(tail_ptr)
				tail.next = w.large_objects
				tail.size = tail_size - large_header_size()
				assert (uint(large_payload(tail)) + tail.size) & uint(chunk_mask) == 0
				w.large_objects = tail
				w.maybe_repurpose_head()
			}
		}
		assert (uint(large_payload(best)) + best.size) & uint(chunk_mask) == 0
		return best
	}
}

// carve_chunk repurposes one whole free chunk (256 bytes) as small objects of
// the given kind, returning the head of the new freelist chain.
@[unsafe]
fn (w Walloc) carve_chunk(chunk &u8, kind u8) &FreeNode {
	unsafe {
		ptr := allocate_chunk(page_of(chunk), chunk_index_of(chunk), kind)
		end := uint(ptr) + uint(chunk_size)
		size := kind_to_granules(kind) * uint(granule_size)
		mut next := &FreeNode(unsafe { nil })
		mut i := size
		for i <= uint(chunk_size) {
			head := &FreeNode(voidptr(end - i))
			head.next = next
			next = head
			i += size
		}
		return next
	}
}

// obtain_small_objects returns a chain of fresh small objects of one kind,
// either from a spare whole chunk or from a fresh single-chunk large object.
@[unsafe]
fn (mut w Walloc) obtain_small_objects(kind u8) &FreeNode {
	unsafe {
		whole := num_small_kinds - 1
		if w.small_freelists[whole] != unsafe { nil } {
			chunk := w.small_freelists[whole]
			w.small_freelists[whole] = chunk.next
			return w.carve_chunk(&u8(chunk), kind)
		}
		chunk_obj := w.allocate_large_object(0)
		if chunk_obj == unsafe { nil } {
			return unsafe { nil }
		}
		return w.carve_chunk(&u8(chunk_obj), kind)
	}
}

@[unsafe]
fn (mut w Walloc) allocate_small(kind u8) voidptr {
	unsafe {
		if w.small_freelists[int(kind)] == unsafe { nil } {
			freelist := w.obtain_small_objects(kind)
			if freelist == unsafe { nil } {
				return unsafe { nil }
			}
			w.small_freelists[int(kind)] = freelist
		}
		ret := w.small_freelists[int(kind)]
		w.small_freelists[int(kind)] = ret.next
		return voidptr(ret)
	}
}

@[unsafe]
fn (mut w Walloc) allocate_large(size uint) voidptr {
	unsafe {
		obj := w.allocate_large_object(size)
		if obj == unsafe { nil } {
			return unsafe { nil }
		}
		return voidptr(large_payload(obj))
	}
}

// malloc allocates `size` bytes. It returns nil when out of memory.
// The result is aligned to 8 bytes.
@[unsafe]
pub fn (mut w Walloc) malloc(size uint) voidptr {
	unsafe {
		granules := (size + uint(granule_size) - 1) >> uint(granule_size_log_2)
		kind := granules_to_kind(granules)
		if kind == kind_large {
			return w.allocate_large(size)
		}
		return w.allocate_small(kind)
	}
}

// calloc allocates `size` zeroed bytes. It returns nil when out of memory.
@[unsafe]
pub fn (mut w Walloc) calloc(size uint) voidptr {
	unsafe {
		ptr := w.malloc(size)
		if ptr == unsafe { nil } {
			return unsafe { nil }
		}
		C.memset(ptr, 0, size)
		return ptr
	}
}

// free_ releases a block previously returned by malloc/calloc. Freeing nil is
// a no-op. Small objects return to their size-class freelist; large objects
// return to the large freelist and are merged lazily on the next large
// allocation.
@[unsafe]
pub fn (mut w Walloc) free_(ptr voidptr) {
	unsafe {
		if ptr == unsafe { nil } {
			return
		}
		page := page_of(ptr)
		chunk := chunk_index_of(ptr)
		kind := chunk_kind_at(page, chunk)
		if kind == kind_large {
			obj := large_from_payload(ptr)
			obj.next = w.large_objects
			w.large_objects = obj
			allocate_chunk(page, chunk, kind_free_large)
			w.pending_compact = true
		} else {
			assert kind < u8(num_small_kinds)
			obj := &FreeNode(ptr)
			obj.next = w.small_freelists[int(kind)]
			w.small_freelists[int(kind)] = obj
		}
	}
}
