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
fn C.vgc_counter_load(ptr &u64) u64
fn C.vgc_counter_add(ptr &u64, val u64)
fn C.vgc_count_alloc(live &u64, allocated &u64, size u64, n u64)
fn C.vgc_os_alloc(size usize) voidptr
fn C.vgc_os_alloc_heap(size usize) voidptr
fn C.vgc_os_free(ptr voidptr, size usize)
fn C.vgc_os_decommit(ptr voidptr, size usize)
fn C.vgc_os_recommit(ptr voidptr, size usize) int
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
fn C.vgc_mutex_trylock(lk &u32) int
fn C.vgc_thread_yield()
fn C.vgc_set_thread_exit_hook(f fn (voidptr))
fn C.vgc_watch_thread_exit(idx int)
fn C.vgc_call_with_registers_spilled(f fn (voidptr, voidptr), arg voidptr)
fn C.vgc_start_thread(f voidptr)
fn C.vgc_num_cpus() int

// ============================================================
// Constants (translated from Go's runtime constants)
// ============================================================

const vgc_page_shift = 13
const vgc_page_size = 8192
const vgc_max_small_size = u32(32768) // objects > this are "large"
const vgc_num_classes = 68
const vgc_num_span_classes = 136 // 68 * 2 (scan + noscan variants)
// These MUST stay compile-time integer constants (no `usize()` cast, no division by
// another const). Otherwise V emits them as runtime globals initialized inside
// `_vinit()`, and the allocations that run earlier in `_vinit()` (const strings, etc.)
// see them as 0: arenas get sized to a single page instead of 64MB.
// The number of arenas has no limit: their table grows with the heap.
const vgc_arena_size = 64 * 1024 * 1024 // 64MB per arena (an object larger than that gets its own)
// The arena map finds the arena of an address in constant time, however many arenas there are.
// It cuts the address space into granules of 64 MB, the smallest arena size, so a granule
// overlaps at most two arenas: one that ends in it and one that starts in it. It covers the low
// 2^48 bytes (every user address on x86-64, on arm64 with 48-bit virtual addresses, on macOS and
// on Windows) in two levels: 1024 chunks of 4096 granules. A chunk (64 KB, for 256 GB of
// address space) is allocated when an arena first falls into it. Entries are only added, under
// vgc_heap.lock, and never change after that, so a lookup takes no lock.
const vgc_granule_shift = 26
const vgc_arena_map_l2_bits = 12
const vgc_arena_map_l2_size = 4096
const vgc_arena_map_granules = 4096 * 1024 // 2^22 granules of 2^26 bytes
const vgc_max_threads = 64
const vgc_tiny_size = 16 // tiny allocator threshold (no-pointer objects < 16 bytes)
// A span holds at most 1024 objects (8-byte objects in one page): 128 bytes of bits.
const vgc_bitmap_bytes = 128
const vgc_meta_chunk_size = 256 * 1024 // span metadata is carved out of chunks of this size

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
	// Cache index + 1 of the thread that allocates from the span, or that is setting it up;
	// 0 when no thread does. Only that thread changes the span's alloc bits, and the sweep
	// skips the span.
	cached_by int
	// Linked list pointers for central free lists
	next &VGC_Span = unsafe { nil }
	prev &VGC_Span = unsafe { nil }
	// Storage for alloc_bits (the first half) and mark_bits (the second half).
	bits [256]u8
}

// VGC_Cache is a per-thread allocation cache.
// Translated from Go's runtime.mcache - eliminates lock contention on hot path.
// Each cache starts a cache line of its own (128 bytes on Apple's arm64 cores, two lines on
// x86-64): its owner writes it on every allocation, and with caches packed next to each other
// threads slowed each other down through the lines they shared, by up to a quarter of their
// cycles with 8 threads, depending on where VGC_Heap happened to put the array.
@[aligned: 128]
struct VGC_Cache {
mut:
	alloc [136]&VGC_Span // one span per span class (68 scan + 68 noscan)
	// Tiny allocator for objects < 16 bytes without pointers
	// Translated from Go's mcache.tiny
	tiny        usize
	tiny_offset usize
	tiny_allocs usize
	// What the threads that had this slot allocated: the bytes of the objects that they allocated
	// minus the ones that they freed, and the bytes that they asked for. The slot's thread adds to
	// them with C.vgc_count_alloc and C.vgc_counter_add, macros without a lock, an atomic
	// read-modify-write or a call; other threads read them at any time (see vgc_heap_live). They
	// outlive the thread: the next thread that gets the slot continues them.
	live_bytes  u64
	alloc_bytes u64
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
	// Map from page index to span (for finding which span owns an address): one entry for each
	// of the size / vgc_page_size pages, stored right after the arena.
	page_span &&VGC_Span = unsafe { nil }
}

// VGC_Granule holds the (at most two) arenas that overlap a granule of the arena map.
struct VGC_Granule {
mut:
	arena [2]&VGC_Arena
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
	// Arenas (memory from OS), in a table of arenas_cap entries that doubles when it is full.
	// Only code that holds `lock` reads the table: the old one is freed when it grows.
	arenas     &&VGC_Arena = unsafe { nil }
	narenas    int
	arenas_cap int
	// Arenas before this index have no room left for a span.
	arena_open int
	// The arena map (see vgc_arena_map_granules), and whether an arena is missing from it.
	arena_map         [1024]&VGC_Granule
	arena_map_partial bool
	// Central free lists (one per span class)
	central [136]VGC_Central
	// Large object spans
	large_alloc &VGC_Span = unsafe { nil }
	// The rest of the current chunk for span metadata, and the bytes of all chunks
	meta_next  usize
	meta_left  usize
	meta_bytes u64
	// Free spans (completely empty, reusable by page count)
	free_spans_lock u32
	free_spans      [32]&VGC_Span // free spans indexed by npages (1..31, 0=unused)
	// Free large spans (npages >= 32): single list, matched by exact page count.
	// Shares free_spans_lock. Without this, large objects are never reclaimed,
	// leaking both physical memory and arena address space.
	free_large &VGC_Span = unsafe { nil }
	// Per-thread caches
	caches     [64]VGC_Cache
	ncaches    int
	cache_lock u32
	// GC state
	gc_phase   u32 // atomic: current GC phase
	gc_enabled u32 // atomic: 1 = GC enabled
	sweep_gen  u32 // current sweep generation
	wb_enabled u32 // atomic: write barrier enabled
	// GC metrics (translated from Go's gcController). The live heap and the bytes allocated are
	// counted per cache slot (VGC_Cache.live_bytes and alloc_bytes), see vgc_heap_live and
	// vgc_total_alloc.
	heap_marked u64 // atomic: bytes marked in last cycle
	live_at_gc  u64 // atomic: the sum of the slots' live_bytes when the last collection ended
	next_gc     u64 // trigger next GC at this heap size
	alloc_at_gc u64 // atomic: vgc_total_alloc() when the last collection ended
	gc_cycle    u64 // number of completed GC cycles
	// Heap memory (see vgc_heap_usage), all atomic: the size of all arenas, the part of them
	// that spans were carved from, and the pages of free spans that went back to the OS.
	arena_bytes    u64
	arena_used     u64
	unmapped_bytes u64
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
	sweep_done u32 // atomic
	// Default GC trigger: collect when heap doubles (GOGC=100 equivalent)
	gc_percent int // like Go's GOGC, default 100
}

// Global heap instance.
// @[cinit] forces the initializer into C static (load-time) initialization instead
// of `_vinit()`. Without it, `_vinit()` runs *after* `vgc_init()` (see cmain.v) and
// re-assigns `vgc_heap = VGC_Heap{}`, wiping out `gc_enabled`/`next_gc` and every span
// allocated during const initialization. The result is that the GC stays disabled and
// the heap grows without bound.

@[cinit]
__global vgc_heap = VGC_Heap{}

// Fast bounds check for pointer validation. Also @[cinit]: otherwise `_vinit()` resets
// these to 0 after the first arena has already been mapped during const init.

@[cinit]
__global vgc_arena_lo = usize(0)

@[cinit]
__global vgc_arena_hi = usize(0)

// ============================================================
// Initialization
// ============================================================

@[markused]
pub fn vgc_init() {
	C.vgc_init_size_tables()
	vgc_heap.gc_enabled = 1
	vgc_heap.gc_percent = 100
	vgc_heap.next_gc = 256 * 1024 * 1024 // favor throughput over early collections
	vgc_heap.gc_phase = vgc_phase_off
	C.vgc_set_thread_exit_hook(vgc_on_thread_exit)
	// Register the main thread
	vgc_register_thread()
}

// ============================================================
// Thread registration (for root scanning)
// ============================================================

fn vgc_register_thread() {
	C.vgc_mutex_lock(&vgc_heap.cache_lock)
	// Reuse the slot of a thread that has exited.
	mut idx := 0
	for idx < vgc_heap.ncaches && unsafe { vgc_heap.caches[idx].registered } {
		idx++
	}
	if idx >= vgc_max_threads {
		C.vgc_mutex_unlock(&vgc_heap.cache_lock)
		// Without a cache slot the allocator would index caches[-1].
		C.fputs(c'vgc: more than 64 threads allocate at the same time\n', C.stderr)
		C.abort()
	}
	if idx == vgc_heap.ncaches {
		vgc_heap.ncaches = idx + 1
	}
	vgc_reset_cache(idx)
	unsafe {
		vgc_heap.caches[idx].registered = true
	}
	C.vgc_mutex_unlock(&vgc_heap.cache_lock)

	C.vgc_set_cache_idx(idx)
	C.vgc_watch_thread_exit(idx)
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
		vgc_heap.caches[idx].stack_base = stack_base
	}
	vgc_refresh_stack_range_for_sp(idx, sp)
}

// vgc_on_thread_exit runs when a registered thread exits, with its cache index + 1.
fn vgc_on_thread_exit(value voidptr) {
	vgc_unregister_thread(int(isize(value)) - 1)
}

// vgc_unregister_thread frees the slot of an exiting thread, so that the collector no longer
// scans its stack, which the system is about to unmap. The spans that the thread had cached
// stay in use until a sweep finds them empty, like the spans that a thread filled.
fn vgc_unregister_thread(idx int) {
	if idx < 0 || idx >= vgc_max_threads {
		return
	}
	// A running collection holds cache_lock while it scans the stacks, and waits for this
	// thread to stop.
	for C.vgc_mutex_trylock(&vgc_heap.cache_lock) == 0 {
		if C.vgc_atomic_load_u32(&vgc_heap.gc_stop_flag) != 0 {
			vgc_safepoint_at(idx)
		} else {
			C.vgc_thread_yield()
		}
	}
	for span_class in 0 .. vgc_num_span_classes {
		span := unsafe { vgc_heap.caches[idx].alloc[span_class] }
		if span != unsafe { nil } {
			unsafe {
				span.cached_by = 0
			}
		}
	}
	vgc_reset_cache(idx)
	C.vgc_mutex_unlock(&vgc_heap.cache_lock)
	C.vgc_set_cache_idx(-1)
}

// vgc_reset_cache empties the cache in slot idx, for a thread that gets the slot or gives it up,
// but keeps its counters, which other threads read at any time. The caller holds cache_lock.
fn vgc_reset_cache(idx int) {
	mut cache := unsafe { &vgc_heap.caches[idx] }
	unsafe { C.memset(&cache.alloc[0], 0, sizeof(cache.alloc)) }
	cache.tiny = 0
	cache.tiny_offset = 0
	cache.tiny_allocs = 0
	cache.registered = false
	cache.stack_base = 0
	cache.stack_lo = 0
	cache.stack_hi = 0
	cache.thread_id = 0
	cache.stopped = 0
}

// vgc_live_sum returns the sum of the live_bytes counters of all cache slots.
fn vgc_live_sum() u64 {
	mut sum := u64(0)
	for i in 0 .. vgc_heap.ncaches {
		sum += C.vgc_counter_load(unsafe { &vgc_heap.caches[i].live_bytes })
	}
	return sum
}

// vgc_heap_live returns the bytes of the heap objects that the last collection found live, plus
// the ones allocated since then, minus the ones freed since then. It adds up the threads'
// counters, so a thread that allocates touches no memory that other threads write.
fn vgc_heap_live() u64 {
	// The counters only wrap around: the difference is right, and it can be negative when
	// more was freed than allocated since the collection.
	since_gc := i64(vgc_live_sum() - C.vgc_atomic_load_u64(&vgc_heap.live_at_gc))
	live := i64(C.vgc_atomic_load_u64(&vgc_heap.heap_marked)) + since_gc
	return if live > 0 { u64(live) } else { u64(0) }
}

// vgc_total_alloc returns the bytes allocated since the program started.
fn vgc_total_alloc() u64 {
	mut sum := u64(0)
	for i in 0 .. vgc_heap.ncaches {
		sum += C.vgc_counter_load(unsafe { &vgc_heap.caches[i].alloc_bytes })
	}
	return sum
}

fn vgc_ensure_registered() {
	if C.vgc_get_cache_idx() < 0 {
		vgc_register_thread()
	}
}

// vgc_enter starts an allocator operation: it registers the thread, and stops it while a
// collection runs, before it touches its cache or a span. Returns the thread's cache index.
@[inline]
fn vgc_enter() int {
	vgc_ensure_registered()
	if C.vgc_atomic_load_u32(&vgc_heap.gc_stop_flag) != 0 {
		vgc_safepoint()
	}
	return C.vgc_get_cache_idx()
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
fn vgc_get_free_span(npages u32, owner int) &VGC_Span {
	mut span := vgc_take_free_span(npages, owner)
	if span == unsafe { nil } {
		return span
	}
	// The pages of a free span were given back to the OS; Windows faults on them until they
	// are committed again.
	if C.vgc_os_recommit(voidptr(span.base), usize(npages) * vgc_page_size) == 0 {
		vgc_push_free_span(mut span)
		return unsafe { nil }
	}
	C.vgc_atomic_sub_u64(&vgc_heap.unmapped_bytes, u64(npages) * vgc_page_size)
	return span
}

fn vgc_take_free_span(npages u32, owner int) &VGC_Span {
	if npages == 0 {
		return unsafe { nil }
	}
	// Large spans: walk the dedicated list looking for an exact page-count match.
	if npages >= 32 {
		C.vgc_mutex_lock(&vgc_heap.free_spans_lock)
		mut prev := unsafe { &VGC_Span(nil) }
		mut span := vgc_heap.free_large
		for span != unsafe { nil } {
			if span.npages == npages {
				unsafe {
					if prev != nil {
						prev.next = span.next
					} else {
						vgc_heap.free_large = span.next
					}
					span.next = nil
					span.prev = nil
					span.in_use = true
					span.cached_by = owner
				}
				C.vgc_mutex_unlock(&vgc_heap.free_spans_lock)
				return span
			}
			prev = span
			span = span.next
		}
		C.vgc_mutex_unlock(&vgc_heap.free_spans_lock)
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
			span.cached_by = owner
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
	if npages == 0 {
		return
	}
	span.alloc_bits = unsafe { nil }
	span.mark_bits = unsafe { nil }
	span.in_use = false
	span.class_idx = 0
	span.elem_size = 0
	span.nelems = 0
	span.alloc_count = 0
	span.free_index = 0
	span.noscan = false
	span.has_ptrmap = false
	span.ptrmap = 0
	span.ptr_words = 0
	// Decommit pages to return physical memory to OS
	page_bytes := usize(npages) * vgc_page_size
	C.vgc_os_decommit(voidptr(span.base), page_bytes)
	C.vgc_atomic_add_u64(&vgc_heap.unmapped_bytes, u64(page_bytes))
	vgc_push_free_span(mut span)
}

// vgc_push_free_span puts an empty span, whose pages are decommitted, on its free list.
fn vgc_push_free_span(mut span VGC_Span) {
	npages := span.npages
	span.in_use = false
	span.cached_by = 0
	C.vgc_mutex_lock(&vgc_heap.free_spans_lock)
	if npages >= 32 {
		// Large span: keep on the dedicated list, matched by exact page count.
		unsafe {
			span.next = vgc_heap.free_large
			vgc_heap.free_large = span
		}
	} else {
		unsafe {
			span.next = vgc_heap.free_spans[npages]
			vgc_heap.free_spans[npages] = span
		}
	}
	C.vgc_mutex_unlock(&vgc_heap.free_spans_lock)
}

// Allocate a new span with the given number of pages. `owner` is the cache index + 1 of the
// thread that sets it up: the sweep leaves the span alone until that thread is done with it.
fn vgc_span_alloc(npages u32, owner int) &VGC_Span {
	// First try to reuse a free span
	recycled := vgc_get_free_span(npages, owner)
	if recycled != unsafe { nil } {
		return recycled
	}

	nbytes := usize(npages) * vgc_page_size

	C.vgc_mutex_lock(&vgc_heap.lock)
	// Try to find space in existing arenas
	mut base := usize(0)
	mut arena := unsafe { &VGC_Arena(nil) }
	for i in vgc_heap.arena_open .. vgc_heap.narenas {
		mut a := unsafe { vgc_heap.arenas[i] }
		if a.used + nbytes <= a.size {
			base = a.base + a.used
			a.used += nbytes
			arena = a
			C.vgc_atomic_add_u64(&vgc_heap.arena_used, u64(nbytes))
			break
		}
	}
	for vgc_heap.arena_open < vgc_heap.narenas {
		open := unsafe { vgc_heap.arenas[vgc_heap.arena_open] }
		if open.used < open.size {
			break
		}
		vgc_heap.arena_open++
	}
	// Allocate new arena if needed
	if base == 0 {
		arena = vgc_new_arena(nbytes)
		if arena == unsafe { nil } {
			C.vgc_mutex_unlock(&vgc_heap.lock)
			return unsafe { nil }
		}
		arena.used = nbytes
		C.vgc_atomic_add_u64(&vgc_heap.arena_used, u64(nbytes))
		base = arena.base
	}

	// Create span metadata (allocate from OS for metadata to avoid chicken-and-egg)
	span := unsafe { &VGC_Span(vgc_meta_alloc(sizeof(VGC_Span))) }
	if span == unsafe { nil } {
		C.vgc_mutex_unlock(&vgc_heap.lock)
		return unsafe { nil }
	}
	unsafe {
		C.memset(span, 0, sizeof(VGC_Span))
		span.base = base
		span.npages = npages
		span.in_use = true
		span.cached_by = owner
	}
	// Register span in arena's page map
	page_start := (base - arena.base) / vgc_page_size
	for p in 0 .. usize(npages) {
		unsafe {
			arena.page_span[page_start + p] = span
		}
	}

	C.vgc_mutex_unlock(&vgc_heap.lock)
	return span
}

// vgc_new_arena gets an arena for a span of nbytes from the OS: vgc_arena_size bytes, or
// nbytes for a larger object. The caller holds vgc_heap.lock.
fn vgc_new_arena(nbytes usize) &VGC_Arena {
	if vgc_heap.narenas == vgc_heap.arenas_cap {
		new_cap := if vgc_heap.arenas_cap == 0 { 8 } else { vgc_heap.arenas_cap * 2 }
		table := unsafe { &&VGC_Arena(C.vgc_os_alloc(usize(new_cap) * sizeof(voidptr))) }
		if table == unsafe { nil } {
			return unsafe { nil }
		}
		if vgc_heap.arenas != unsafe { nil } {
			unsafe {
				C.memcpy(table, vgc_heap.arenas, usize(vgc_heap.narenas) * sizeof(voidptr))
			}
			C.vgc_os_free(vgc_heap.arenas, usize(vgc_heap.arenas_cap) * sizeof(voidptr))
		}
		vgc_heap.arenas = table
		vgc_heap.arenas_cap = new_cap
	}
	asize := if nbytes > vgc_arena_size { nbytes } else { usize(vgc_arena_size) }
	mem := C.vgc_os_alloc_heap(asize)
	if mem == unsafe { nil } {
		return unsafe { nil }
	}
	// The arena and its page map share one allocation, zeroed by the OS.
	npages := asize / vgc_page_size
	desc := C.vgc_os_alloc(sizeof(VGC_Arena) + npages * sizeof(voidptr))
	if desc == unsafe { nil } {
		C.vgc_os_free(mem, asize)
		return unsafe { nil }
	}
	mut arena := unsafe { &VGC_Arena(desc) }
	arena.base = usize(mem)
	arena.size = asize
	arena.page_span = unsafe { &&VGC_Span(usize(desc) + sizeof(VGC_Arena)) }
	unsafe {
		vgc_heap.arenas[vgc_heap.narenas] = arena
	}
	vgc_heap.narenas++
	if !vgc_arena_map_add(arena) {
		vgc_heap.arena_map_partial = true
	}
	C.vgc_atomic_add_u64(&vgc_heap.arena_bytes, u64(asize))
	// Update global arena bounds for fast pointer rejection
	if vgc_arena_lo == 0 || arena.base < vgc_arena_lo {
		vgc_arena_lo = arena.base
	}
	if arena.base + asize > vgc_arena_hi {
		vgc_arena_hi = arena.base + asize
	}
	return arena
}

// vgc_arena_map_add enters an arena in the arena map. It returns false when the arena lies above
// the map, or a chunk of the map cannot be allocated; vgc_find_arena then searches the table for
// it. The caller holds vgc_heap.lock.
fn vgc_arena_map_add(arena &VGC_Arena) bool {
	first := arena.base >> vgc_granule_shift
	last := (arena.base + arena.size - 1) >> vgc_granule_shift
	if last >= vgc_arena_map_granules {
		return false
	}
	for g in first .. last + 1 {
		mut chunk := vgc_heap.arena_map[g >> vgc_arena_map_l2_bits]
		if chunk == unsafe { nil } {
			chunk = unsafe { &VGC_Granule(C.vgc_os_alloc(vgc_arena_map_l2_size * sizeof(VGC_Granule))) }
			if chunk == unsafe { nil } {
				return false
			}
			C.vgc_atomic_fence()
			vgc_heap.arena_map[g >> vgc_arena_map_l2_bits] = chunk
		}
		mut granule := unsafe { &chunk[g & (vgc_arena_map_l2_size - 1)] }
		slot := if granule.arena[0] == unsafe { nil } { 0 } else { 1 }
		if granule.arena[slot] != unsafe { nil } {
			return false
		}
		// A lookup that finds the arena must see its fields.
		C.vgc_atomic_fence()
		granule.arena[slot] = arena
	}
	return true
}

// vgc_meta_alloc returns zeroed memory for the metadata of a span. Spans are recycled, never
// freed, so the metadata comes from large chunks; a page of its own for each span (and one for
// each of its two bitmaps) used more memory than the span's objects. The caller holds
// vgc_heap.lock.
fn vgc_meta_alloc(size usize) voidptr {
	n := (size + 15) & ~usize(15)
	if vgc_heap.meta_left < n {
		chunk := C.vgc_os_alloc(usize(vgc_meta_chunk_size))
		if chunk == unsafe { nil } {
			return unsafe { nil }
		}
		vgc_heap.meta_next = usize(chunk)
		vgc_heap.meta_left = usize(vgc_meta_chunk_size)
		vgc_heap.meta_bytes += u64(vgc_meta_chunk_size)
	}
	p := vgc_heap.meta_next
	vgc_heap.meta_next += n
	vgc_heap.meta_left -= n
	return voidptr(p)
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

	// Bitmaps: ceil(nobjs/8) bytes each, in the span itself
	bitmap_size := (nobjs + 7) / 8
	vgc_span_set_bitmaps(mut span)
	unsafe {
		C.memset(span.alloc_bits, 0, bitmap_size)
		C.memset(span.mark_bits, 0, bitmap_size)
	}
}

fn vgc_span_set_bitmaps(mut span VGC_Span) {
	span.alloc_bits = unsafe { &u8(&span.bits[0]) }
	span.mark_bits = unsafe { &u8(&span.bits[vgc_bitmap_bytes]) }
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

// Get a span with free objects for the given span class, for the cache with index owner - 1.
fn vgc_central_get_span(span_class int, owner int) &VGC_Span {
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
			span.cached_by = owner
		}
		C.vgc_mutex_unlock(&central.lock)
		return span
	}

	C.vgc_mutex_unlock(&central.lock)

	// No spans available - allocate a new one
	class_idx := u8(span_class / 2)
	noscan := (span_class % 2) == 1
	npages := C.vgc_get_class_npages(int(class_idx))
	new_span := vgc_span_alloc(npages, owner)
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
	span.cached_by = 0
	// A sweep that runs now must not file the span a second time.
	span.sweep_gen = vgc_heap.sweep_gen
	vgc_central_file_span(span_class, mut span)
	C.vgc_mutex_unlock(&central.lock)
}

// vgc_central_file_span puts a span that no thread allocates from on the partial or the full
// list of its class. The caller holds the central lock.
fn vgc_central_file_span(span_class int, mut span VGC_Span) {
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
	new_span := vgc_central_get_span(span_class, cache_idx + 1)
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

	cache_idx := vgc_enter()

	// Large allocation (> 32KB) - get dedicated span
	if n > usize(vgc_max_small_size) {
		vgc_maybe_gc()
		return vgc_alloc_large(n, false, zero_fill, cache_idx)
	}

	// Small allocation - use size class and cache
	class_idx := C.vgc_size_class(u32(n))
	if class_idx == 0 {
		vgc_maybe_gc()
		return vgc_alloc_large(n, false, zero_fill, cache_idx)
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
		cache := unsafe { &vgc_heap.caches[cache_idx] }
		C.vgc_count_alloc(&cache.live_bytes, &cache.alloc_bytes, u64(span.elem_size), u64(n))
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
		heap_live := vgc_heap_live()
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

	cache_idx := vgc_enter()

	if n > usize(vgc_max_small_size) {
		vgc_maybe_gc()
		return vgc_alloc_large(n, true, zero_fill, cache_idx)
	}

	class_idx := C.vgc_size_class(u32(n))
	if class_idx == 0 {
		vgc_maybe_gc()
		return vgc_alloc_large(n, true, zero_fill, cache_idx)
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
				// A slot in the tiny block is no new object.
				C.vgc_counter_add(&cache.alloc_bytes, u64(n))
				return ptr
			}
		}
		// Allocate a new tiny block. The block is always vgc_tiny_size bytes (objects
		// are packed into it up to that offset), so it must come from the size class
		// that fits vgc_tiny_size - NOT the class for `n`, which may be smaller and
		// would let packing overflow the slot into the next object.
		tiny_class := C.vgc_size_class(u32(vgc_tiny_size))
		span_class := int(tiny_class) * 2 + 1 // noscan
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
				C.vgc_count_alloc(&cache.live_bytes, &cache.alloc_bytes, u64(span.elem_size),
					u64(n))
				if span.alloc_count >= span.nelems {
					vgc_maybe_gc()
				}
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
		cache := unsafe { &vgc_heap.caches[cache_idx] }
		C.vgc_count_alloc(&cache.live_bytes, &cache.alloc_bytes, u64(span.elem_size), u64(n))
		if zero_fill {
			unsafe { C.memset(ptr, 0, n) }
		}
		// Strings and other pointer-free data count toward the trigger like any allocation;
		// check it when a span fills up, as for objects with pointers.
		if span.alloc_count >= span.nelems {
			vgc_maybe_gc()
		}
	}
	return ptr
}

// Allocate a large object (> 32KB) with its own span
fn vgc_alloc_large(n usize, noscan bool, zero_fill bool, cache_idx int) voidptr {
	npages := u32((n + vgc_page_size - 1) / vgc_page_size)
	span := vgc_span_alloc(npages, cache_idx + 1)
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
		vgc_span_set_bitmaps(mut span)
		span.alloc_bits[0] = 1
		span.mark_bits[0] = 0
	}
	// Add to large allocation list
	C.vgc_mutex_lock(&vgc_heap.lock)
	unsafe {
		span.next = vgc_heap.large_alloc
		vgc_heap.large_alloc = span
		// Set up: the sweep may free it once it is garbage.
		span.cached_by = 0
	}
	C.vgc_mutex_unlock(&vgc_heap.lock)

	cache := unsafe { &vgc_heap.caches[cache_idx] }
	C.vgc_count_alloc(&cache.live_bytes, &cache.alloc_bytes, u64(n), u64(n))

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
		new_ptr = vgc_malloc_typed_opts(new_size, old_span.ptrmap, old_span.ptr_words, false)
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
	cache_idx := vgc_enter()
	span := vgc_find_span(ptr)
	if span == unsafe { nil } {
		return
	}
	if span.elem_size == 0 {
		return
	}
	// Never eagerly free noscan objects. The tiny allocator packs several sub-16-byte
	// noscan objects (e.g. short map key strings) into one slot that shares a single
	// alloc bit, so clearing it here would mark still-live neighbours as free and let
	// the next allocation overwrite them. GC marking reclaims dead noscan objects safely.
	if span.noscan {
		return
	}
	// Only the thread that allocates from a span changes its alloc bits. A span that no
	// thread allocates from changes only under its central lock; leave the objects of other
	// threads' spans to the collector.
	if span.cached_by == cache_idx + 1 {
		vgc_free_obj(span, ptr, cache_idx)
		return
	}
	if span.cached_by != 0 {
		return
	}
	span_class := int(span.class_idx) * 2 + if span.noscan { 1 } else { 0 }
	central := unsafe { &vgc_heap.central[span_class] }
	C.vgc_mutex_lock(&central.lock)
	if span.cached_by == 0 {
		vgc_free_obj(span, ptr, cache_idx)
	}
	C.vgc_mutex_unlock(&central.lock)
}

// vgc_free_obj frees the object at ptr for the thread with cache index cache_idx.
fn vgc_free_obj(span &VGC_Span, ptr voidptr, cache_idx int) {
	obj_idx := u32((usize(ptr) - span.base) / usize(span.elem_size))
	if obj_idx < span.nelems && span.alloc_bits != unsafe { nil } {
		if C.vgc_bitmap_get(span.alloc_bits, obj_idx) != 0 {
			C.vgc_bitmap_clear(span.alloc_bits, obj_idx)
			unsafe {
				(&VGC_Span(span)).alloc_count--
				if obj_idx < span.free_index {
					(&VGC_Span(span)).free_index = obj_idx
				}
			}
			// The counter wraps around: adding the two's complement subtracts.
			C.vgc_counter_add(unsafe { &vgc_heap.caches[cache_idx].live_bytes }, u64(0) - u64(span.elem_size))
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
	mem := vgc_malloc_typed_opts(usize(n), ptrmap, ptr_words, false)
	if mem != unsafe { nil } {
		unsafe { C.memcpy(mem, src, n) }
	}
	return mem
}

// Memdup variants that skip zero-fill when the destination will be overwritten.
@[markused]
fn vgc_memdup(src voidptr, n isize) voidptr {
	if src == unsafe { nil } || n <= 0 {
		return unsafe { nil }
	}
	mem := vgc_malloc_typed_opts(usize(n), 0, 0, false)
	if mem != unsafe { nil } {
		unsafe { C.memcpy(mem, src, n) }
	}
	return mem
}

@[markused]
fn vgc_memdup_noscan(src voidptr, n isize) voidptr {
	if src == unsafe { nil } || n <= 0 {
		return unsafe { nil }
	}
	mem := vgc_malloc_noscan_opts(usize(n), false)
	if mem != unsafe { nil } {
		unsafe { C.memcpy(mem, src, n) }
	}
	return mem
}

// ============================================================
// Span lookup (find which span owns an address) - O(1) via address map
// ============================================================

// vgc_next_span returns the first span at or after page `page` of arena `arena`, and the
// position after it; nil after the last span. The spans tile the used part of each arena, and
// a recycled span keeps its page count, so the page map holds every span exactly once. The
// position is returned rather than kept in an iterator, because the compiler may move a local
// whose address is taken to the heap, and the collector must not allocate.
fn vgc_next_span(arena int, page usize) (&VGC_Span, int, usize) {
	mut ai := arena
	mut p := page
	for ai < vgc_heap.narenas {
		a := unsafe { vgc_heap.arenas[ai] }
		used_pages := a.used / vgc_page_size
		for p < used_pages {
			span := unsafe { a.page_span[p] }
			if span == unsafe { nil } || span.npages == 0 {
				p++
				continue
			}
			return span, ai, p + usize(span.npages)
		}
		ai++
		p = 0
	}
	return unsafe { nil }, ai, p
}

@[direct_array_access]
fn vgc_find_span(ptr voidptr) &VGC_Span {
	addr := usize(ptr)
	// Fast reject: most probed words are not heap pointers at all.
	if addr < vgc_arena_lo || addr >= vgc_arena_hi {
		return unsafe { nil }
	}
	// The arena map lookup of vgc_find_arena, without a call: marking calls this for every word
	// that it scans.
	g := addr >> vgc_granule_shift
	if g < vgc_arena_map_granules {
		chunk := vgc_heap.arena_map[g >> vgc_arena_map_l2_bits]
		if chunk != unsafe { nil } {
			granule := unsafe { &chunk[g & (vgc_arena_map_l2_size - 1)] }
			mut a := granule.arena[0]
			if a != unsafe { nil } && addr - a.base < a.size {
				return unsafe { a.page_span[(addr - a.base) >> vgc_page_shift] }
			}
			a = granule.arena[1]
			if a != unsafe { nil } && addr - a.base < a.size {
				return unsafe { a.page_span[(addr - a.base) >> vgc_page_shift] }
			}
		}
	}
	if !vgc_heap.arena_map_partial {
		return unsafe { nil }
	}
	a := vgc_find_arena(addr)
	if a == unsafe { nil } {
		return unsafe { nil }
	}
	return unsafe { a.page_span[(addr - a.base) >> vgc_page_shift] }
}

// vgc_find_arena returns the arena that holds addr, or nil.
@[direct_array_access]
fn vgc_find_arena(addr usize) &VGC_Arena {
	g := addr >> vgc_granule_shift
	if g < vgc_arena_map_granules {
		chunk := vgc_heap.arena_map[g >> vgc_arena_map_l2_bits]
		if chunk != unsafe { nil } {
			granule := unsafe { &chunk[g & (vgc_arena_map_l2_size - 1)] }
			for a in granule.arena {
				if a != unsafe { nil } && addr - a.base < a.size {
					return a
				}
			}
		}
	}
	if !vgc_heap.arena_map_partial {
		return unsafe { nil }
	}
	// An arena that the map could not hold is searched for in the table.
	mut found := unsafe { &VGC_Arena(nil) }
	C.vgc_mutex_lock(&vgc_heap.lock)
	for i in 0 .. vgc_heap.narenas {
		b := unsafe { vgc_heap.arenas[i] }
		if addr >= b.base && addr < b.base + b.size {
			found = b
			break
		}
	}
	C.vgc_mutex_unlock(&vgc_heap.lock)
	return found
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
	a := vgc_find_arena(addr)
	return a != unsafe { nil } && addr < a.base + a.used
}

// Safepoint: called when GC needs threads to stop
fn vgc_safepoint() {
	cache_idx := C.vgc_get_cache_idx()
	if cache_idx < 0 {
		return
	}
	vgc_safepoint_at(cache_idx)
}

fn vgc_safepoint_at(cache_idx int) {
	// Wait below a frame that holds this thread's callee-saved registers, so that the
	// collector's scan of this stack sees them.
	C.vgc_call_with_registers_spilled(vgc_safepoint_wait, voidptr(isize(cache_idx)))
}

// vgc_safepoint_wait stops the thread with cache index arg while a collection runs. The
// collector scans its stack from regs, where the thread's registers are saved, upwards: the
// frames below are vgc's own.
fn vgc_safepoint_wait(arg voidptr, regs voidptr) {
	cache_idx := int(isize(arg))
	// Update the live stack range for root scanning.
	vgc_refresh_stack_range_for_sp(cache_idx, usize(regs))
	// Mark ourselves as stopped
	C.vgc_atomic_store_u32(&vgc_heap.caches[cache_idx].stopped, 1)
	C.vgc_atomic_add_u32(&vgc_heap.gc_stopped_count, 1)

	// Wait until GC is done with stop-the-world phase
	mut spins := 0
	for C.vgc_atomic_load_u32(&vgc_heap.gc_stop_flag) != 0 {
		if spins < 64 {
			spins++
			C.vgc_atomic_fence()
		} else {
			C.vgc_thread_yield()
		}
	}

	C.vgc_atomic_store_u32(&vgc_heap.caches[cache_idx].stopped, 0)
}
