// vgc_d_vgc.c.v - V Garbage Collector: Core types, heap, and allocation
// Translated from Go's runtime GC (golang/go src/runtime/malloc.go, mheap.go, mspan.go, mcache.go, mcentral.go)
// Concurrent tri-color mark-and-sweep garbage collector with size-class allocation.

@[has_globals]
module builtin

#flag -I @VEXEROOT/thirdparty/vgc
#include "vgc_platform.h"

// C interop declarations for platform header
fn C.vgc_get_cache_idx() int
fn C.vgc_set_cache_idx(idx int)
fn C.vgc_atomic_load_u32(ptr &u32) u32
fn C.vgc_atomic_store_u32(ptr &u32, val u32)
fn C.vgc_atomic_load_u64(ptr &u64) u64
fn C.vgc_atomic_store_u64(ptr &u64, val u64)
fn C.vgc_atomic_add_u64(ptr &u64, val u64) u64
fn C.vgc_atomic_sub_u64(ptr &u64, val u64) u64
fn C.vgc_atomic_add_u32(ptr &u32, val u32) u32
fn C.vgc_atomic_cas_u32(ptr &u32, expected &u32, desired u32) bool
fn C.vgc_atomic_exchange_u32(ptr &u32, val u32) u32
fn C.vgc_atomic_fence()
fn C.vgc_os_alloc(size usize) voidptr
fn C.vgc_os_free(ptr voidptr, size usize)
fn C.vgc_os_decommit(ptr voidptr, size usize)
fn C.vgc_get_sp() voidptr
fn C.vgc_get_stack_bounds(lo &usize, hi &usize) int
fn C.vgc_bitmap_get(bits &u8, idx u32) int
fn C.vgc_bitmap_set(bits &u8, idx u32)
fn C.vgc_bitmap_clear(bits &u8, idx u32)
fn C.vgc_bitmap_test_and_set(bits &u8, idx u32) int
fn C.vgc_popcount8(x u8) int
fn C.vgc_size_class(size u32) u8
fn C.vgc_get_class_size(cls int) u32
fn C.vgc_get_class_npages(cls int) u32
fn C.vgc_get_class_nobjs(cls int) u32
fn C.vgc_init_size_tables()
fn C.vgc_mutex_lock(lk &u32)
fn C.vgc_mutex_unlock(lk &u32)
fn C.vgc_start_thread(f voidptr)
fn C.vgc_addr_map_register(base usize, size usize, arena_idx int)
fn C.vgc_addr_to_arena(addr usize) int

// ============================================================
// Constants (translated from Go's runtime constants)
// ============================================================

const vgc_page_shift = 13
const vgc_page_size = 8192
const vgc_max_small_size = u32(32768) // objects > this are "large"
const vgc_num_classes = 68
const vgc_num_span_classes = 136 // 68 * 2 (scan + noscan variants)
const vgc_arena_size = usize(64) * 1024 * 1024 // 64MB per arena
const vgc_pages_per_arena = vgc_arena_size / vgc_page_size
const vgc_max_arenas = 64
const vgc_max_threads = 64
const vgc_tiny_size = 16 // tiny allocator threshold (no-pointer objects < 16 bytes)

// GC phases (translated from Go's _GCoff, _GCmark, _GCmarktermination)
const vgc_phase_off = u32(0)
const vgc_phase_mark = u32(1)
const vgc_phase_mark_term = u32(2)
const vgc_phase_sweep = u32(3)

// ============================================================
// Core types (translated from Go's mspan, mheap, mcache, mcentral)
// ============================================================

// VGC_Span represents a run of contiguous pages containing objects of one size class.
// Translated from Go's runtime.mspan.
struct VGC_Span {
mut:
	base       usize // start address of the span
	npages     u32   // number of pages in this span
	elem_size  u32   // size of each object in bytes
	nelems     u32   // number of elements (objects) in the span
	class_idx  u8    // size class index (0 for large objects)
	noscan     bool  // true if objects contain no pointers (noscan variant)
	in_use     bool  // true if span is allocated to a size class
	has_ptrmap bool  // true if ptrmap is valid (precise scanning available)
	// Pointer bitmap: bit N = word offset N contains a pointer.
	// Covers objects up to 512 bytes (64 words on 64-bit).
	// For larger objects, falls back to conservative scanning.
	ptrmap u64
	// Number of pointer words in the object (for precise scanning)
	ptr_words u8
	// Allocation bitmaps (translated from Go's allocBits/gcmarkBits)
	alloc_bits  &u8 = unsafe { nil } // 1 = allocated
	mark_bits   &u8 = unsafe { nil } // 1 = marked (used during GC)
	alloc_count u32 // number of currently allocated objects
	free_index  u32 // hint: scan from here for free slot
	// Sweep generation (translated from Go's sweepgen)
	sweep_gen u32
	// Linked list pointers for central free lists
	next &VGC_Span = unsafe { nil }
	prev &VGC_Span = unsafe { nil }
}

// VGC_Cache is a per-thread allocation cache.
// Translated from Go's runtime.mcache - eliminates lock contention on hot path.
struct VGC_Cache {
mut:
	alloc [136]&VGC_Span // one span per span class (68 scan + 68 noscan)
	// Tiny allocator for objects < 16 bytes without pointers
	// Translated from Go's mcache.tiny
	tiny        usize
	tiny_offset usize
	tiny_allocs usize
	// Thread info
	registered bool
	stack_base usize // fixed stack boundary for this thread
	stack_lo   usize // lowest stack address (for root scanning)
	stack_hi   usize // highest stack address
	thread_id  u64
	stopped    u32 // 1 if stopped for GC
}

// VGC_Central is a central free list for one span class.
// Translated from Go's runtime.mcentral.
struct VGC_Central {
mut:
	lock    u32 // spinlock
	partial &VGC_Span = unsafe { nil } // spans with free objects (swept)
	full    &VGC_Span = unsafe { nil } // spans with no free objects
}

// VGC_Arena tracks a chunk of memory obtained from the OS.
// Translated from Go's heapArena concept.
struct VGC_Arena {
mut:
	base usize
	size usize
	used usize
	// Map from page index to span (for finding which span owns an address)
	page_span [8192]&VGC_Span // vgc_pages_per_arena entries
}

// VGC_WorkBuf is a work buffer for the mark phase.
// Translated from Go's runtime.workbuf.
struct VGC_WorkBuf {
mut:
	nobj int
	obj  [256]usize // pointers to mark
	next &VGC_WorkBuf = unsafe { nil }
}

// VGC_Heap is the global heap.
// Translated from Go's runtime.mheap.
struct VGC_Heap {
mut:
	lock u32 // spinlock
	// Arenas (memory from OS)
	arenas  [64]VGC_Arena
	narenas int
	// Central free lists (one per span class)
	central [136]VGC_Central
	// Large object spans
	large_alloc &VGC_Span = unsafe { nil }
	// All spans for iteration during GC
	allspans [16384]&VGC_Span
	nspans   int
	// Free spans (completely empty, reusable by page count)
	free_spans_lock u32
	free_spans      [32]&VGC_Span // free spans indexed by npages (1..31, 0=unused)
	// Per-thread caches
	caches     [64]VGC_Cache
	ncaches    int
	cache_lock u32
	// GC state
	gc_phase   u32 // atomic: current GC phase
	gc_enabled u32 // atomic: 1 = GC enabled
	sweep_gen  u32 // current sweep generation
	wb_enabled u32 // atomic: write barrier enabled
	// GC metrics (translated from Go's gcController)
	heap_live   u64 // atomic: bytes of live heap objects (actual object bytes)
	heap_marked u64 // bytes marked in last cycle
	next_gc     u64 // trigger next GC at this heap size
	total_alloc u64 // atomic: total bytes allocated
	gc_cycle    u64 // number of completed GC cycles
	// GC work queues
	work_full  &VGC_WorkBuf = unsafe { nil }
	work_empty &VGC_WorkBuf = unsafe { nil }
	work_lock  u32
	// GC worker coordination
	gc_workers_done  u32 // atomic
	gc_nworkers      int
	gc_stop_flag     u32 // atomic: tells threads to stop for GC
	gc_stopped_count u32 // atomic: threads stopped
	gc_target_stops  u32 // number of threads to stop
	// Sweep state
	sweep_idx  int
	sweep_done u32 // atomic
	// Default GC trigger: collect when heap doubles (GOGC=100 equivalent)
	gc_percent int // like Go's GOGC, default 100
}

// Global heap instance
__global vgc_heap = VGC_Heap{}
// Fast bounds check for pointer validation
__global vgc_arena_lo = usize(0)
__global vgc_arena_hi = usize(0)

// ============================================================
// Initialization
// ============================================================

@[markused]
pub fn vgc_init() {
	C.vgc_init_size_tables()
	vgc_heap.gc_enabled = 1
	vgc_heap.gc_percent = 100
	vgc_heap.next_gc = 4 * 1024 * 1024 // initial trigger at 4MB (like Go)
	vgc_heap.gc_phase = vgc_phase_off
	// Register the main thread
	vgc_register_thread()
}

// ============================================================
// Thread registration (for root scanning)
// ============================================================

fn vgc_register_thread() {
	C.vgc_mutex_lock(&vgc_heap.cache_lock)
	idx := vgc_heap.ncaches
	if idx >= vgc_max_threads {
		C.vgc_mutex_unlock(&vgc_heap.cache_lock)
		return
	}
	vgc_heap.ncaches = idx + 1
	C.vgc_mutex_unlock(&vgc_heap.cache_lock)

	C.vgc_set_cache_idx(idx)
	sp := usize(C.vgc_get_sp())
	mut stack_lo := usize(0)
	mut stack_hi := usize(0)
	mut stack_base := if sp > usize(8) * 1024 * 1024 {
		sp - usize(8) * 1024 * 1024
	} else {
		usize(0)
	}
	if C.vgc_get_stack_bounds(&stack_lo, &stack_hi) != 0 && stack_lo < stack_hi {
		dist_lo := if sp >= stack_lo { sp - stack_lo } else { stack_lo - sp }
		dist_hi := if stack_hi >= sp { stack_hi - sp } else { sp - stack_hi }
		stack_base = if dist_hi <= dist_lo { stack_hi } else { stack_lo }
	}
	unsafe {
		vgc_heap.caches[idx].registered = true
		vgc_heap.caches[idx].stack_base = stack_base
	}
	vgc_refresh_stack_range_for_sp(idx, sp)
}

fn vgc_ensure_registered() {
	if C.vgc_get_cache_idx() < 0 {
		vgc_register_thread()
	}
}

fn vgc_refresh_stack_range() {
	cache_idx := C.vgc_get_cache_idx()
	if cache_idx < 0 {
		return
	}
	vgc_refresh_stack_range_for_sp(cache_idx, usize(C.vgc_get_sp()))
}

fn vgc_refresh_stack_range_for_sp(cache_idx int, sp usize) {
	if cache_idx < 0 || cache_idx >= vgc_max_threads {
		return
	}
	stack_base := unsafe { vgc_heap.caches[cache_idx].stack_base }
	if stack_base <= sp {
		unsafe {
			vgc_heap.caches[cache_idx].stack_lo = stack_base
			vgc_heap.caches[cache_idx].stack_hi = sp
		}
	} else {
		unsafe {
			vgc_heap.caches[cache_idx].stack_lo = sp
			vgc_heap.caches[cache_idx].stack_hi = stack_base
		}
	}
}

// ============================================================
// Span management (translated from Go's mspan operations)
// ============================================================

// Try to get a recycled span from the free list
fn vgc_get_free_span(npages u32) &VGC_Span {
	if npages == 0 || npages >= 32 {
		return unsafe { nil }
	}
	C.vgc_mutex_lock(&vgc_heap.free_spans_lock)
	span := vgc_heap.free_spans[npages]
	if span != unsafe { nil } {
		unsafe {
			vgc_heap.free_spans[npages] = span.next
			span.next = nil
			span.prev = nil
			span.in_use = true
		}
		C.vgc_mutex_unlock(&vgc_heap.free_spans_lock)
		return span
	}
	C.vgc_mutex_unlock(&vgc_heap.free_spans_lock)
	return unsafe { nil }
}

// Return a fully-empty span to the free list for reuse
fn vgc_put_free_span(mut span VGC_Span) {
	npages := span.npages
	if npages == 0 || npages >= 32 {
		return
	}
	// Free bitmaps before zeroing nelems
	bitmap_size := usize((span.nelems + 7) / 8)
	if span.alloc_bits != unsafe { nil } {
		C.vgc_os_free(span.alloc_bits, bitmap_size)
		span.alloc_bits = unsafe { nil }
	}
	if span.mark_bits != unsafe { nil } {
		C.vgc_os_free(span.mark_bits, bitmap_size)
		span.mark_bits = unsafe { nil }
	}
	span.in_use = false
	span.class_idx = 0
	span.elem_size = 0
	span.nelems = 0
	span.alloc_count = 0
	span.free_index = 0
	// Decommit pages to return physical memory to OS
	page_bytes := usize(npages) * vgc_page_size
	C.vgc_os_decommit(voidptr(span.base), page_bytes)
	C.vgc_mutex_lock(&vgc_heap.free_spans_lock)
	unsafe {
		span.next = vgc_heap.free_spans[npages]
		vgc_heap.free_spans[npages] = span
	}
	C.vgc_mutex_unlock(&vgc_heap.free_spans_lock)
}

// Allocate a new span with the given number of pages
fn vgc_span_alloc(npages u32) &VGC_Span {
	// First try to reuse a free span
	recycled := vgc_get_free_span(npages)
	if recycled != unsafe { nil } {
		return recycled
	}

	nbytes := usize(npages) * vgc_page_size

	C.vgc_mutex_lock(&vgc_heap.lock)
	// Try to find space in existing arenas
	mut base := usize(0)
	mut arena_idx := -1
	for i in 0 .. vgc_heap.narenas {
		a := unsafe { &vgc_heap.arenas[i] }
		if a.used + nbytes <= a.size {
			base = a.base + a.used
			arena_idx = i
			unsafe {
				vgc_heap.arenas[i].used += nbytes
			}
			break
		}
	}
	// Allocate new arena if needed
	if base == 0 {
		asize := if nbytes > vgc_arena_size { nbytes } else { vgc_arena_size }
		mem := C.vgc_os_alloc(asize)
		if mem == unsafe { nil } {
			C.vgc_mutex_unlock(&vgc_heap.lock)
			return unsafe { nil }
		}
		arena_idx = vgc_heap.narenas
		if arena_idx >= vgc_max_arenas {
			C.vgc_os_free(mem, asize)
			C.vgc_mutex_unlock(&vgc_heap.lock)
			return unsafe { nil }
		}
		unsafe {
			vgc_heap.arenas[arena_idx].base = usize(mem)
			vgc_heap.arenas[arena_idx].size = asize
			vgc_heap.arenas[arena_idx].used = nbytes
		}
		vgc_heap.narenas = arena_idx + 1
		base = usize(mem)
		// Register in address map for O(1) lookup
		C.vgc_addr_map_register(usize(mem), asize, arena_idx)
		// Update global arena bounds for fast pointer rejection
		if vgc_arena_lo == 0 || base < vgc_arena_lo {
			vgc_arena_lo = base
		}
		arena_end := base + asize
		if arena_end > vgc_arena_hi {
			vgc_arena_hi = arena_end
		}
	}

	// Create span metadata (allocate from OS for metadata to avoid chicken-and-egg)
	span := unsafe { &VGC_Span(C.vgc_os_alloc(sizeof(VGC_Span))) }
	if span == unsafe { nil } {
		C.vgc_mutex_unlock(&vgc_heap.lock)
		return unsafe { nil }
	}
	unsafe {
		C.memset(span, 0, sizeof(VGC_Span))
		span.base = base
		span.npages = npages
		span.in_use = true
	}
	// Register span in arena's page map
	if arena_idx >= 0 {
		page_start := (base - vgc_heap.arenas[arena_idx].base) / vgc_page_size
		for p in 0 .. npages {
			pidx := page_start + p
			if pidx < vgc_pages_per_arena {
				unsafe {
					vgc_heap.arenas[arena_idx].page_span[pidx] = span
				}
			}
		}
	}

	// Track in allspans
	if vgc_heap.nspans < 16384 {
		unsafe {
			vgc_heap.allspans[vgc_heap.nspans] = span
		}
		vgc_heap.nspans++
	}

	C.vgc_mutex_unlock(&vgc_heap.lock)
	return span
}

// Initialize a span for a specific size class
fn vgc_span_init(mut span VGC_Span, class_idx u8, noscan bool) {
	size := C.vgc_get_class_size(int(class_idx))
	npages := C.vgc_get_class_npages(int(class_idx))
	nobjs := C.vgc_get_class_nobjs(int(class_idx))

	span.class_idx = class_idx
	span.noscan = noscan
	span.elem_size = size
	span.npages = npages
	span.nelems = nobjs
	span.free_index = 0
	span.alloc_count = 0

	// Allocate bitmaps: ceil(nobjs/8) bytes each
	bitmap_size := (nobjs + 7) / 8
	span.alloc_bits = unsafe { &u8(C.vgc_os_alloc(usize(bitmap_size))) }
	span.mark_bits = unsafe { &u8(C.vgc_os_alloc(usize(bitmap_size))) }
	if span.alloc_bits != unsafe { nil } {
		unsafe { C.memset(span.alloc_bits, 0, bitmap_size) }
	}
	if span.mark_bits != unsafe { nil } {
		unsafe { C.memset(span.mark_bits, 0, bitmap_size) }
	}
}

// Find a free slot in a span and allocate it
fn vgc_span_alloc_obj(mut span VGC_Span) voidptr {
	if span.alloc_bits == unsafe { nil } {
		return unsafe { nil }
	}
	start_idx := span.free_index
	nbytes := (span.nelems + 7) >> 3
	start_byte := start_idx >> 3
	end_byte := (start_idx + 7) >> 3
	for pass in 0 .. 2 {
		mut byte_idx := if pass == 0 { start_byte } else { u32(0) }
		limit := if pass == 0 { nbytes } else { end_byte }
		for byte_idx < limit {
			bit_base := byte_idx << 3
			mut b := unsafe { span.alloc_bits[byte_idx] }
			if b == 0xFF {
				byte_idx++
				continue
			}
			start_bit := if byte_idx == start_byte { start_idx & 7 } else { u32(0) }
			for bit := start_bit; bit < u32(8); bit++ {
				i := bit_base + bit
				if i >= span.nelems {
					break
				}
				mask := u8(1) << bit
				if (b & mask) == 0 {
					b |= mask
					unsafe {
						span.alloc_bits[byte_idx] = b
					}
					span.alloc_count++
					span.free_index = i + 1
					addr := span.base + usize(i) * usize(span.elem_size)
					return unsafe { voidptr(addr) }
				}
			}
			byte_idx++
		}
	}
	return unsafe { nil } // span is full
}

// ============================================================
// Central free list operations (translated from Go's mcentral)
// ============================================================

// Get a span with free objects for the given span class
fn vgc_central_get_span(span_class int) &VGC_Span {
	central := unsafe { &vgc_heap.central[span_class] }
	C.vgc_mutex_lock(&central.lock)

	// Try partial list first (spans with free objects)
	mut span := central.partial
	if span != unsafe { nil } {
		// Remove from partial list
		unsafe {
			vgc_heap.central[span_class].partial = span.next
		}
		if span.next != unsafe { nil } {
			unsafe {
				span.next.prev = nil
			}
		}
		unsafe {
			span.next = nil
			span.prev = nil
		}
		C.vgc_mutex_unlock(&central.lock)
		return span
	}

	C.vgc_mutex_unlock(&central.lock)

	// No spans available - allocate a new one
	class_idx := u8(span_class / 2)
	noscan := (span_class % 2) == 1
	npages := C.vgc_get_class_npages(int(class_idx))
	new_span := vgc_span_alloc(npages)
	if new_span == unsafe { nil } {
		return unsafe { nil }
	}
	unsafe {
		vgc_span_init(mut new_span, class_idx, noscan)
	}

	return new_span
}

// Return a span to the central free list
fn vgc_central_return_span(span_class int, mut span VGC_Span) {
	central := unsafe { &vgc_heap.central[span_class] }
	C.vgc_mutex_lock(&central.lock)

	if span.alloc_count < span.nelems {
		// Has free objects - add to partial
		unsafe {
			span.next = vgc_heap.central[span_class].partial
			span.prev = nil
		}
		if span.next != unsafe { nil } {
			unsafe {
				span.next.prev = span
			}
		}
		unsafe {
			vgc_heap.central[span_class].partial = span
		}
	} else {
		// Full - add to full list
		unsafe {
			span.next = vgc_heap.central[span_class].full
			span.prev = nil
		}
		if span.next != unsafe { nil } {
			unsafe {
				span.next.prev = span
			}
		}
		unsafe {
			vgc_heap.central[span_class].full = span
		}
	}

	C.vgc_mutex_unlock(&central.lock)
}

// ============================================================
// Cache operations (translated from Go's mcache)
// ============================================================

fn vgc_cache_get_span(cache_idx int, span_class int) &VGC_Span {
	span := unsafe { vgc_heap.caches[cache_idx].alloc[span_class] }
	if span != unsafe { nil } {
		// Check if span has free objects
		if span.alloc_count < span.nelems {
			return span
		}
		// Span is full - return to central and get a new one
		unsafe {
			vgc_central_return_span(span_class, mut span)
		}
	}
	// Get fresh span from central
	new_span := vgc_central_get_span(span_class)
	unsafe {
		vgc_heap.caches[cache_idx].alloc[span_class] = new_span
	}
	return new_span
}

// ============================================================
// Main allocation entry points
// (translated from Go's runtime.mallocgc)
// ============================================================

fn vgc_malloc(n usize) voidptr {
	return vgc_malloc_typed_opts(n, 0, 0, true)
}

// vgc_malloc_typed allocates with a precise pointer map.
// ptrmap: bitmap where bit N means word offset N is a pointer.
// ptr_words: number of pointer words in the object.
// If ptrmap==0 && ptr_words==0, falls back to conservative scanning.
fn vgc_malloc_typed(n usize, ptrmap u64, ptr_words u8) voidptr {
	return vgc_malloc_typed_opts(n, ptrmap, ptr_words, true)
}

fn vgc_malloc_typed_opts(n usize, ptrmap u64, ptr_words u8, zero_fill bool) voidptr {
	if n == 0 {
		return unsafe { nil }
	}

	vgc_ensure_registered()
	cache_idx := C.vgc_get_cache_idx()

	// Large allocation (> 32KB) - get dedicated span
	if n > usize(vgc_max_small_size) {
		vgc_maybe_gc()
		return vgc_alloc_large(n, false, zero_fill)
	}

	// Small allocation - use size class and cache
	class_idx := C.vgc_size_class(u32(n))
	if class_idx == 0 {
		vgc_maybe_gc()
		return vgc_alloc_large(n, false, zero_fill)
	}

	span_class := int(class_idx) * 2 // scan variant
	span := vgc_cache_get_span(cache_idx, span_class)
	if span == unsafe { nil } {
		return unsafe { nil }
	}

	// Set precise pointer map on span (first typed allocation wins)
	if ptrmap != 0 && !span.has_ptrmap {
		unsafe {
			(&VGC_Span(span)).has_ptrmap = true
			(&VGC_Span(span)).ptrmap = ptrmap
			(&VGC_Span(span)).ptr_words = ptr_words
		}
	}

	ptr := unsafe { vgc_span_alloc_obj(mut span) }
	if ptr != unsafe { nil } {
		// Track actual object bytes, not page bytes
		C.vgc_atomic_add_u64(&vgc_heap.heap_live, u64(span.elem_size))
		C.vgc_atomic_add_u64(&vgc_heap.total_alloc, u64(n))
		if zero_fill {
			unsafe { C.memset(ptr, 0, n) }
		}
		// Periodic GC check - only when span fills up (amortize cost)
		if span.alloc_count >= span.nelems {
			vgc_maybe_gc()
		}
	}
	return ptr
}

// Amortized GC trigger check - avoids atomic loads on every allocation
fn vgc_maybe_gc() {
	if C.vgc_atomic_load_u32(&vgc_heap.gc_enabled) != 0 {
		heap_live := C.vgc_atomic_load_u64(&vgc_heap.heap_live)
		next_gc := C.vgc_atomic_load_u64(&vgc_heap.next_gc)
		if heap_live >= next_gc {
			vgc_gc_start()
		}
		if C.vgc_atomic_load_u32(&vgc_heap.gc_stop_flag) != 0 {
			vgc_safepoint()
		}
	}
}

fn vgc_malloc_noscan(n usize) voidptr {
	return vgc_malloc_noscan_opts(n, true)
}

fn vgc_malloc_noscan_opts(n usize, zero_fill bool) voidptr {
	if n == 0 {
		return unsafe { nil }
	}

	vgc_ensure_registered()
	cache_idx := C.vgc_get_cache_idx()

	if n > usize(vgc_max_small_size) {
		vgc_maybe_gc()
		return vgc_alloc_large(n, true, zero_fill)
	}

	class_idx := C.vgc_size_class(u32(n))
	if class_idx == 0 {
		return vgc_alloc_large(n, true, zero_fill)
	}

	// Tiny allocator for very small objects (translated from Go's mcache tiny allocator)
	if n < vgc_tiny_size && cache_idx >= 0 {
		cache := unsafe { &vgc_heap.caches[cache_idx] }
		if cache.tiny != 0 {
			// Align up for the allocation
			mut off := cache.tiny_offset
			if n >= 8 {
				off = (off + 7) & ~usize(7)
			} else if n >= 4 {
				off = (off + 3) & ~usize(3)
			} else if n >= 2 {
				off = (off + 1) & ~usize(1)
			}
			if off + n <= vgc_tiny_size {
				ptr := unsafe { voidptr(cache.tiny + off) }
				unsafe {
					vgc_heap.caches[cache_idx].tiny_offset = off + n
					vgc_heap.caches[cache_idx].tiny_allocs++
				}
				C.vgc_atomic_add_u64(&vgc_heap.total_alloc, u64(n))
				return ptr
			}
		}
		// Allocate a new tiny block
		span_class := int(class_idx) * 2 + 1 // noscan
		span := vgc_cache_get_span(cache_idx, span_class)
		if span != unsafe { nil } {
			ptr := unsafe { vgc_span_alloc_obj(mut span) }
			if ptr != unsafe { nil } {
				if zero_fill {
					unsafe { C.memset(ptr, 0, usize(span.elem_size)) }
				}
				unsafe {
					vgc_heap.caches[cache_idx].tiny = usize(ptr)
					vgc_heap.caches[cache_idx].tiny_offset = n
					vgc_heap.caches[cache_idx].tiny_allocs++
				}
				C.vgc_atomic_add_u64(&vgc_heap.heap_live, u64(span.elem_size))
				C.vgc_atomic_add_u64(&vgc_heap.total_alloc, u64(n))
				return ptr
			}
		}
	}

	span_class := int(class_idx) * 2 + 1 // noscan variant
	span := vgc_cache_get_span(cache_idx, span_class)
	if span == unsafe { nil } {
		return unsafe { nil }
	}

	ptr := unsafe { vgc_span_alloc_obj(mut span) }
	if ptr != unsafe { nil } {
		C.vgc_atomic_add_u64(&vgc_heap.heap_live, u64(span.elem_size))
		C.vgc_atomic_add_u64(&vgc_heap.total_alloc, u64(n))
		if zero_fill {
			unsafe { C.memset(ptr, 0, n) }
		}
	}
	return ptr
}

// Allocate a large object (> 32KB) with its own span
fn vgc_alloc_large(n usize, noscan bool, zero_fill bool) voidptr {
	npages := u32((n + vgc_page_size - 1) / vgc_page_size)
	span := vgc_span_alloc(npages)
	if span == unsafe { nil } {
		return unsafe { nil }
	}

	unsafe {
		span.class_idx = 0
		span.noscan = noscan
		span.elem_size = u32(n)
		span.nelems = 1
		span.alloc_count = 1

		// Single-element bitmap
		span.alloc_bits = &u8(C.vgc_os_alloc(1))
		span.mark_bits = &u8(C.vgc_os_alloc(1))
		if span.alloc_bits != nil {
			span.alloc_bits[0] = 1
		}
		if span.mark_bits != nil {
			span.mark_bits[0] = 0
		}
	}
	// Add to large allocation list
	C.vgc_mutex_lock(&vgc_heap.lock)
	unsafe {
		span.next = vgc_heap.large_alloc
		vgc_heap.large_alloc = span
	}
	C.vgc_mutex_unlock(&vgc_heap.lock)

	C.vgc_atomic_add_u64(&vgc_heap.heap_live, u64(n))
	C.vgc_atomic_add_u64(&vgc_heap.total_alloc, u64(n))

	ptr := unsafe { voidptr(span.base) }
	if zero_fill {
		unsafe { C.memset(ptr, 0, n) }
	}
	return ptr
}

// Realloc for VGC-managed memory
fn vgc_realloc(old_ptr voidptr, new_size usize) voidptr {
	if old_ptr == unsafe { nil } {
		return vgc_malloc(new_size)
	}
	if new_size == 0 {
		return unsafe { nil }
	}
	// Find the span owning this pointer to get old size
	old_span := vgc_find_span(old_ptr)
	if old_span == unsafe { nil } {
		// Unknown object - just malloc new
		return vgc_malloc(new_size)
	}
	old_size := usize(old_span.elem_size)
	if new_size <= old_size {
		return old_ptr // fits in current allocation
	}
	// Preserve the original scan policy so raw buffers do not become scan objects.
	mut new_ptr := unsafe { nil }
	if old_span.noscan {
		new_ptr = vgc_malloc_noscan_opts(new_size, false)
	} else if old_span.has_ptrmap {
		new_ptr = vgc_malloc_typed_opts(new_size, old_span.ptrmap, old_span.ptr_words,
			false)
	} else {
		new_ptr = vgc_malloc_typed_opts(new_size, 0, 0, false)
	}
	if new_ptr != unsafe { nil } {
		copy_size := if old_size < new_size { old_size } else { new_size }
		unsafe { C.memcpy(new_ptr, old_ptr, copy_size) }
	}
	return new_ptr
}

// Free is mostly a no-op for GC, but can hint at deallocation
fn vgc_free(ptr voidptr) {
	if ptr == unsafe { nil } {
		return
	}
	// In a GC environment, explicit free is optional.
	// The object will be collected if unreachable.
	// However, we can mark it as free immediately for reuse.
	span := vgc_find_span(ptr)
	if span == unsafe { nil } {
		return
	}
	if span.elem_size == 0 {
		return
	}
	obj_idx := u32((usize(ptr) - span.base) / usize(span.elem_size))
	if obj_idx < span.nelems && span.alloc_bits != unsafe { nil } {
		if C.vgc_bitmap_get(span.alloc_bits, obj_idx) != 0 {
			C.vgc_bitmap_clear(span.alloc_bits, obj_idx)
			unsafe {
				span.alloc_count--
				if obj_idx < span.free_index {
					span.free_index = obj_idx
				}
			}
			C.vgc_atomic_sub_u64(&vgc_heap.heap_live, u64(span.elem_size))
		}
	}
}

// Calloc (zero-initialized allocation)
fn vgc_calloc(n usize) voidptr {
	return vgc_malloc(n) // vgc_malloc already zeroes memory
}

// Typed memdup: allocate with pointer map and copy source data.
// Used by HEAP_vgc() macro for struct allocations with known layout.
@[markused]
fn vgc_memdup_typed(src voidptr, n isize, ptrmap u64, ptr_words u8) voidptr {
	if src == unsafe { nil } || n <= 0 {
		return unsafe { nil }
	}
	mem := vgc_malloc_typed(usize(n), ptrmap, ptr_words)
	if mem != unsafe { nil } {
		unsafe { C.memcpy(mem, src, n) }
	}
	return mem
}

// ============================================================
// Span lookup (find which span owns an address) - O(1) via address map
// ============================================================

fn vgc_find_span(ptr voidptr) &VGC_Span {
	addr := usize(ptr)
	arena_idx := C.vgc_addr_to_arena(addr)
	if arena_idx < 0 || arena_idx >= vgc_heap.narenas {
		return unsafe { nil }
	}
	a := unsafe { &vgc_heap.arenas[arena_idx] }
	if addr < a.base || addr >= a.base + a.size {
		return unsafe { nil }
	}
	page_idx := (addr - a.base) / vgc_page_size
	if page_idx < vgc_pages_per_arena {
		return a.page_span[page_idx]
	}
	return unsafe { nil }
}

// Get the allocation size of an object
fn vgc_get_obj_size(ptr voidptr) usize {
	span := vgc_find_span(ptr)
	if span == unsafe { nil } {
		return 0
	}
	return usize(span.elem_size)
}

// Check if an address is within the GC heap - O(1) with fast bounds reject
fn vgc_is_heap_ptr(addr usize) bool {
	// Fast reject: most words on the stack are NOT heap pointers
	if addr < vgc_arena_lo || addr >= vgc_arena_hi {
		return false
	}
	arena_idx := C.vgc_addr_to_arena(addr)
	if arena_idx < 0 || arena_idx >= vgc_heap.narenas {
		return false
	}
	a := unsafe { &vgc_heap.arenas[arena_idx] }
	return addr >= a.base && addr < a.base + a.used
}

// Safepoint: called when GC needs threads to stop
fn vgc_safepoint() {
	cache_idx := C.vgc_get_cache_idx()
	if cache_idx < 0 {
		return
	}
	// Update the live stack range for root scanning.
	vgc_refresh_stack_range_for_sp(cache_idx, usize(C.vgc_get_sp()))
	// Mark ourselves as stopped
	C.vgc_atomic_store_u32(&vgc_heap.caches[cache_idx].stopped, 1)
	C.vgc_atomic_add_u32(&vgc_heap.gc_stopped_count, 1)

	// Wait until GC is done with stop-the-world phase
	for C.vgc_atomic_load_u32(&vgc_heap.gc_stop_flag) != 0 {
		C.vgc_atomic_fence()
	}

	C.vgc_atomic_store_u32(&vgc_heap.caches[cache_idx].stopped, 0)
}
