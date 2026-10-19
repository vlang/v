// vgc_gc_d_vgc.c.v - V Garbage Collector: Mark, sweep, and orchestration
// Translated from Go's runtime GC (mgc.go, mgcmark.go, mgcsweep.go, mgcwork.go, mgcpacer.go)
// Concurrent tri-color mark-and-sweep with parallel marking.

@[has_globals]
module builtin

fn C.v3_vgc_mark_global_arrays()
fn C.vgc_data_ranges(lo &usize, hi &usize, max int) int

// ============================================================
// GC Orchestration (translated from Go's runtime.gcStart, gcMarkDone, gcMarkTermination)
// ============================================================

// vgc_gc_start triggers a garbage collection cycle.
// Translated from Go's gcStart() in mgc.go.
// Flow: sweep termination (STW) -> mark (parallel) -> mark termination (STW) -> sweep (concurrent)
fn vgc_gc_start() {
	// Only one GC at a time
	mut expected := vgc_phase_off
	if !C.vgc_atomic_cas_u32(&vgc_heap.gc_phase, &expected, vgc_phase_mark) {
		return
	}
	// Threads cannot register or exit while their stacks are scanned.
	C.vgc_mutex_lock(&vgc_heap.cache_lock)
	// The collection runs below a frame that holds this thread's callee-saved registers, so
	// that pointers the callers only kept in registers are seen by the stack scan.
	C.vgc_call_with_registers_spilled(vgc_gc_run, unsafe { nil })
	C.vgc_mutex_unlock(&vgc_heap.cache_lock)
}

// vgc_collect runs a full collection now (gc_collect()), unless the collector is disabled.
// When another thread is collecting already, it waits for that collection instead.
fn vgc_collect() {
	if C.vgc_atomic_load_u32(&vgc_heap.gc_enabled) == 0 {
		return
	}
	vgc_ensure_registered()
	if C.vgc_atomic_load_u32(&vgc_heap.gc_stop_flag) != 0 {
		vgc_safepoint()
	}
	vgc_gc_start()
	if C.vgc_atomic_load_u32(&vgc_heap.gc_stop_flag) != 0 {
		vgc_safepoint()
	}
}

// vgc_enable turns the automatic and the explicit collections on or off (gc_enable() and
// gc_disable()).
fn vgc_enable(on bool) {
	C.vgc_atomic_store_u32(&vgc_heap.gc_enabled, if on { u32(1) } else { u32(0) })
}

fn vgc_is_enabled() bool {
	return C.vgc_atomic_load_u32(&vgc_heap.gc_enabled) != 0
}

fn vgc_gc_run(_ voidptr) {
	// === Phase 1: Sweep Termination (STW) ===
	// Ensure any previous sweep is complete
	vgc_sweep_finish()

	// Stop the world for root scanning
	// Set stop flag - threads will stop at next safepoint (allocation)
	self_idx := C.vgc_get_cache_idx()
	mut targets := u32(0)
	for i in 0 .. vgc_heap.ncaches {
		if i != self_idx && unsafe { vgc_heap.caches[i].registered } {
			targets++
		}
	}
	vgc_heap.gc_target_stops = targets // all threads except GC thread
	C.vgc_atomic_store_u32(&vgc_heap.gc_stopped_count, 0)
	C.vgc_atomic_store_u32(&vgc_heap.gc_stop_flag, 1)

	// Wait for threads to stop (with timeout to avoid deadlock)
	mut wait_iters := 0
	for C.vgc_atomic_load_u32(&vgc_heap.gc_stopped_count) < vgc_heap.gc_target_stops {
		C.vgc_atomic_fence()
		wait_iters++
		if wait_iters > 1000000 {
			break // Don't wait forever - proceed with what we have
		}
	}

	// Clear mark bits on all spans (prepare for new cycle)
	vgc_clear_mark_bits()

	// === Phase 2: Mark (parallel) ===
	// Enable write barrier (for concurrent correctness)
	C.vgc_atomic_store_u32(&vgc_heap.wb_enabled, 1)

	// The thread starting GC does not pass through a safepoint.
	vgc_refresh_stack_range()

	// Scan roots: stacks + globals (conservative scanning)
	vgc_mark_roots()

	// Parallel mark: drain the work queue
	vgc_parallel_mark()

	// === Phase 3: Mark Termination (brief STW) ===
	C.vgc_atomic_store_u32(&vgc_heap.gc_phase, vgc_phase_mark_term)

	// Final drain of work queue
	vgc_drain_mark_work()

	// Disable write barrier
	C.vgc_atomic_store_u32(&vgc_heap.wb_enabled, 0)

	// Compute live bytes from mark bits
	marked := vgc_count_marked()
	C.vgc_atomic_store_u64(&vgc_heap.heap_marked, marked)
	// Reset heap_live to match what we actually found alive
	C.vgc_atomic_store_u64(&vgc_heap.heap_live, marked)

	// === Phase 4: Sweep ===
	vgc_heap.sweep_gen++
	C.vgc_atomic_store_u32(&vgc_heap.sweep_done, 0)
	C.vgc_atomic_store_u32(&vgc_heap.gc_phase, vgc_phase_sweep)

	// Sweep synchronously - it's fast and avoids race conditions
	vgc_do_sweep()

	// Update GC trigger for next cycle (translated from Go's gcController.endCycle)
	vgc_update_trigger()

	vgc_heap.gc_cycle++
	// Resume the threads only now. Nothing marks the objects that they allocate, so they
	// must not allocate before the sweep is done, and the sweep must not change a span
	// while a thread allocates from it.
	C.vgc_atomic_store_u32(&vgc_heap.gc_stop_flag, 0)
	C.vgc_atomic_store_u32(&vgc_heap.gc_phase, vgc_phase_off)
}

// ============================================================
// Mark phase (translated from Go's mgcmark.go)
// ============================================================

// Clear all mark bits before a new GC cycle
fn vgc_clear_mark_bits() {
	mut arena := 0
	mut page := usize(0)
	for {
		span, next_arena, next_page := vgc_next_span(arena, page)
		if span == unsafe { nil } {
			break
		}
		arena = next_arena
		page = next_page
		if !span.in_use {
			continue
		}
		if span.mark_bits != unsafe { nil } {
			bitmap_size := (span.nelems + 7) / 8
			unsafe { C.memset(span.mark_bits, 0, bitmap_size) }
		}
	}
}

// Conservative root scanning: scan the globals, registered global arrays and thread stacks.
// Translated from Go's markroot() / scanblock() - but using conservative scanning
// since V compiles to C and we don't have precise type info at runtime.
fn vgc_mark_roots() {
	vgc_mark_data_segments()
	C.v3_vgc_mark_global_arrays()
	// Scan each registered thread's stack
	for i in 0 .. vgc_heap.ncaches {
		cache := unsafe { &vgc_heap.caches[i] }
		if !cache.registered {
			continue
		}
		if cache.stack_lo > 0 && cache.stack_hi > 0 && cache.stack_hi > cache.stack_lo {
			vgc_scan_range(cache.stack_lo, cache.stack_hi)
		}
		// The thread keeps packing small objects into its tiny block.
		if cache.tiny != 0 {
			vgc_shade(cache.tiny)
		}
	}
}

// vgc_mark_data_segments scans the writable segments of the executable: every global and
// const, including the ones that are initialized at run time and point into the GC heap.
fn vgc_mark_data_segments() {
	mut lo := [16]usize{}
	mut hi := [16]usize{}
	n := C.vgc_data_ranges(&lo[0], &hi[0], 16)
	for i in 0 .. n {
		vgc_scan_range(lo[i], hi[i])
	}
}

// Scan a memory range conservatively, looking for pointers into the GC heap.
// Each word-aligned value that looks like a heap pointer is treated as a root.
// Translated from Go's scanblock() with conservative pointer finding.
fn vgc_scan_range(lo usize, hi usize) {
	// Align to word boundaries
	start := (lo + sizeof(usize) - 1) & ~(usize(sizeof(usize)) - 1)
	mut addr := start
	for addr + sizeof(usize) <= hi {
		val := unsafe { *(&usize(voidptr(addr))) }
		if val != 0 {
			vgc_shade(val)
		}
		addr += sizeof(usize)
	}
}

// Shade marks an object grey (discovered but not yet scanned).
// Translated from Go's shade() in mgcmark.go.
fn vgc_shade(addr usize) {
	if addr < vgc_arena_lo || addr >= vgc_arena_hi {
		return
	}
	span := vgc_find_span(voidptr(addr))
	if span == unsafe { nil } || !span.in_use {
		return
	}
	if span.elem_size == 0 {
		return
	}
	// Find which object this address belongs to
	obj_idx := u32((addr - span.base) / usize(span.elem_size))
	if obj_idx >= span.nelems {
		return
	}
	// Check if object is allocated
	if span.alloc_bits == unsafe { nil } || C.vgc_bitmap_get(span.alloc_bits, obj_idx) == 0 {
		return
	}
	// Mark it (grey -> will be scanned)
	if span.mark_bits != unsafe { nil } {
		if C.vgc_bitmap_test_and_set(span.mark_bits, obj_idx) == 0 {
			// Newly marked - add to work queue for scanning (only if it may contain pointers)
			if !span.noscan {
				obj_addr := span.base + usize(obj_idx) * usize(span.elem_size)
				vgc_work_put(obj_addr)
			}
		}
	}
}

// Mark the heap. Currently single-threaded on purpose.
//
// Parallel marking (spawning helper threads that all run vgc_drain_mark_work) is
// NOT correct yet: vgc_bitmap_test_and_set() on mark_bits is a plain non-atomic
// read-modify-write, so two workers marking objects whose mark bits live in the same
// byte can lose a mark and let a live object be swept. Until the mark bitmap uses
// atomic ops (and the rest of the parallel path is audited), force a single worker.
// This keeps marking race-free and makes the unlocked arena reads in vgc_find_span
// safe (no concurrent readers, and the sole mutator is blocked inside gc_start).
fn vgc_parallel_mark() {
	vgc_heap.gc_nworkers = 1
	C.vgc_atomic_store_u32(&vgc_heap.gc_workers_done, 0)
	vgc_drain_mark_work()
}

// Drain the mark work queue - scan grey objects and mark their referents.
// Uses precise pointer maps when available (from vgc_malloc_typed),
// falls back to conservative scanning otherwise.
fn vgc_drain_mark_work() {
	for {
		obj_addr := vgc_work_get()
		if obj_addr == 0 {
			break
		}
		span := vgc_find_span(voidptr(obj_addr))
		if span == unsafe { nil } || span.noscan {
			continue // noscan objects don't contain pointers
		}
		if span.has_ptrmap {
			// Precise scanning: only check known pointer word offsets
			vgc_scan_precise(obj_addr, span.ptrmap, span.ptr_words)
		} else {
			// Conservative fallback: scan every word
			obj_size := usize(span.elem_size)
			vgc_scan_range(obj_addr, obj_addr + obj_size)
		}
	}
}

// Precise pointer scanning: use the pointer bitmap to scan only
// word offsets known to contain pointers. Much faster than conservative.
fn vgc_scan_precise(obj_addr usize, ptrmap u64, ptr_words u8) {
	mut mask := ptrmap
	word_size := sizeof(usize)
	for mask != 0 {
		// Find lowest set bit (next pointer offset)
		mut bit := u8(0)
		mut m := mask
		// Count trailing zeros to find the bit position
		if m & 0xFFFFFFFF == 0 {
			bit += 32
			m >>= 32
		}
		if m & 0xFFFF == 0 {
			bit += 16
			m >>= 16
		}
		if m & 0xFF == 0 {
			bit += 8
			m >>= 8
		}
		if m & 0xF == 0 {
			bit += 4
			m >>= 4
		}
		if m & 0x3 == 0 {
			bit += 2
			m >>= 2
		}
		if m & 0x1 == 0 {
			bit += 1
		}
		// Read the pointer at this offset
		ptr_addr := obj_addr + usize(bit) * word_size
		val := unsafe { *(&usize(voidptr(ptr_addr))) }
		if val != 0 {
			vgc_shade(val)
		}
		// Clear this bit and continue
		mask &= mask - 1
	}
}

// ============================================================
// Work queue (translated from Go's mgcwork.go)
// ============================================================

@[inline]
fn vgc_can_use_work_fastpath() bool {
	return vgc_heap.ncaches <= 1 && vgc_heap.gc_nworkers <= 1
}

// Add a pointer to the mark work queue
fn vgc_work_put(addr usize) {
	if vgc_can_use_work_fastpath() {
		mut buf := vgc_heap.work_full
		if buf == unsafe { nil } || buf.nobj >= 256 {
			mut new_buf := vgc_heap.work_empty
			if new_buf != unsafe { nil } {
				unsafe {
					vgc_heap.work_empty = new_buf.next
				}
			} else {
				new_buf = unsafe { &VGC_WorkBuf(C.vgc_os_alloc(usize(sizeof(VGC_WorkBuf)))) }
				if new_buf == unsafe { nil } {
					return
				}
			}
			unsafe {
				new_buf.nobj = 0
				new_buf.next = vgc_heap.work_full
				vgc_heap.work_full = new_buf
			}
			buf = new_buf
		}
		unsafe {
			buf.obj[buf.nobj] = addr
			buf.nobj++
		}
		return
	}

	C.vgc_mutex_lock(&vgc_heap.work_lock)

	// Get or create a work buffer
	mut buf := vgc_heap.work_full
	if buf == unsafe { nil } || buf.nobj >= 256 {
		// Need a new buffer
		mut new_buf := vgc_heap.work_empty
		if new_buf != unsafe { nil } {
			unsafe {
				vgc_heap.work_empty = new_buf.next
			}
		} else {
			new_buf = unsafe { &VGC_WorkBuf(C.vgc_os_alloc(usize(sizeof(VGC_WorkBuf)))) }
			if new_buf == unsafe { nil } {
				C.vgc_mutex_unlock(&vgc_heap.work_lock)
				return
			}
		}
		unsafe {
			new_buf.nobj = 0
			new_buf.next = vgc_heap.work_full
			vgc_heap.work_full = new_buf
		}
		buf = new_buf
	}

	unsafe {
		buf.obj[buf.nobj] = addr
		buf.nobj++
	}
	C.vgc_mutex_unlock(&vgc_heap.work_lock)
}

// Get a pointer from the mark work queue
fn vgc_work_get() usize {
	if vgc_can_use_work_fastpath() {
		mut buf := vgc_heap.work_full
		if buf == unsafe { nil } || buf.nobj == 0 {
			return 0
		}
		unsafe {
			buf.nobj--
			addr := buf.obj[buf.nobj]
			if buf.nobj == 0 {
				vgc_heap.work_full = buf.next
				buf.next = vgc_heap.work_empty
				vgc_heap.work_empty = buf
			}
			return addr
		}
	}

	C.vgc_mutex_lock(&vgc_heap.work_lock)

	mut buf := vgc_heap.work_full
	if buf == unsafe { nil } || buf.nobj == 0 {
		C.vgc_mutex_unlock(&vgc_heap.work_lock)
		return 0
	}

	unsafe {
		buf.nobj--
		addr := buf.obj[buf.nobj]

		// If buffer is empty, move to empty list
		if buf.nobj == 0 {
			vgc_heap.work_full = buf.next
			buf.next = vgc_heap.work_empty
			vgc_heap.work_empty = buf
		}

		C.vgc_mutex_unlock(&vgc_heap.work_lock)
		return addr
	}
}

// ============================================================
// Write barrier (translated from Go's gcWriteBarrier / wbBufFlush)
// ============================================================

// Write barrier: called when a pointer field is written during mark phase.
// Uses Dijkstra-style insertion barrier - shade the new pointer.
fn vgc_write_barrier(new_val voidptr) {
	if C.vgc_atomic_load_u32(&vgc_heap.wb_enabled) == 0 {
		return
	}
	if new_val == unsafe { nil } {
		return
	}
	// Shade the new pointer (mark it grey)
	vgc_shade(usize(new_val))
}

// ============================================================
// Sweep phase (translated from Go's mgcsweep.go)
// ============================================================

// Sweep all spans synchronously.
fn vgc_do_sweep() {
	// The sweep files every span again, by the free slots it leaves in it: a span that a
	// thread filled becomes partial when some of its objects are garbage, and its slots are
	// reused instead of a new span.
	for span_class in 0 .. vgc_num_span_classes {
		central := unsafe { &vgc_heap.central[span_class] }
		C.vgc_mutex_lock(&central.lock)
		unsafe {
			vgc_heap.central[span_class].partial = nil
			vgc_heap.central[span_class].full = nil
		}
		C.vgc_mutex_unlock(&central.lock)
	}
	vgc_sweep_spans()
	C.vgc_atomic_store_u32(&vgc_heap.sweep_done, 1)
}

// vgc_sweep_spans sweeps and files every span that the current sweep has not handled yet.
fn vgc_sweep_spans() {
	// Keep the arenas still while walking them.
	C.vgc_mutex_lock(&vgc_heap.lock)
	mut arena := 0
	mut page := usize(0)
	for {
		span, next_arena, next_page := vgc_next_span(arena, page)
		if span == unsafe { nil } {
			break
		}
		arena = next_arena
		page = next_page
		vgc_sweep_and_file_span(span)
	}
	C.vgc_mutex_unlock(&vgc_heap.lock)
}

// vgc_sweep_and_file_span sweeps a span that no thread allocates from, and puts it on the
// free list when it is empty, or on its central list.
fn vgc_sweep_and_file_span(span &VGC_Span) {
	span_class := int(span.class_idx) * 2 + if span.noscan { 1 } else { 0 }
	central := unsafe { &vgc_heap.central[span_class] }
	C.vgc_mutex_lock(&central.lock)
	// A thread allocates from a cached span; it is swept after the thread gives it back. A
	// span that a thread gave back during this sweep is on a list already.
	if !span.in_use || span.cached_by != 0 || span.sweep_gen == vgc_heap.sweep_gen {
		C.vgc_mutex_unlock(&central.lock)
		return
	}
	mut mspan := unsafe { &VGC_Span(span) }
	mspan.sweep_gen = vgc_heap.sweep_gen
	vgc_sweep_span(span)
	if span.in_use && span.class_idx > 0 {
		vgc_central_file_span(span_class, mut mspan)
	}
	C.vgc_mutex_unlock(&central.lock)
}

// Sweep a single span: free unmarked objects.
// Translated from Go's mspan.sweep() in mgcsweep.go.
fn vgc_sweep_span(span &VGC_Span) {
	if span.alloc_bits == unsafe { nil } || span.mark_bits == unsafe { nil } {
		return
	}

	// Sweep using byte-level operations for speed
	nbytes := (span.nelems + 7) / 8
	mut freed := u32(0)
	mut new_free_index := span.nelems // will be set to lowest freed index

	for b in 0 .. nbytes {
		alloc_byte := unsafe { span.alloc_bits[b] }
		mark_byte := unsafe { span.mark_bits[b] }
		// allocated but not marked = garbage
		garbage := alloc_byte & ~mark_byte
		if garbage != 0 {
			freed += u32(C.vgc_popcount8(garbage))
			// Clear the garbage bits from alloc bitmap
			unsafe {
				span.alloc_bits[b] = alloc_byte & mark_byte
			}
			// Track lowest freed index for free_index hint
			base_idx := b * 8
			if u32(base_idx) < new_free_index {
				new_free_index = u32(base_idx)
			}
		}
	}

	if freed > 0 {
		unsafe {
			(&VGC_Span(span)).alloc_count -= freed
			if new_free_index < span.free_index {
				(&VGC_Span(span)).free_index = new_free_index
			}
		}
	}

	// If span is completely empty, recycle it to the free span pool
	if span.alloc_count == 0 && span.npages > 0 {
		mut mspan := unsafe { &VGC_Span(span) }
		vgc_put_free_span(mut mspan)
	}
}

// Ensure all sweeping from previous cycle is done
fn vgc_sweep_finish() {
	if C.vgc_atomic_load_u32(&vgc_heap.sweep_done) == 0 && vgc_heap.gc_cycle > 0 {
		// Sweep any remaining spans
		vgc_sweep_spans()
		C.vgc_atomic_store_u32(&vgc_heap.sweep_done, 1)
	}
}

// ============================================================
// GC Pacer (translated from Go's mgcpacer.go gcController)
// ============================================================

// Count total marked bytes across all spans using byte-level popcount
fn vgc_count_marked() u64 {
	mut total := u64(0)
	mut arena := 0
	mut page := usize(0)
	for {
		span, next_arena, next_page := vgc_next_span(arena, page)
		if span == unsafe { nil } {
			break
		}
		arena = next_arena
		page = next_page
		if !span.in_use || span.mark_bits == unsafe { nil } {
			continue
		}
		nbytes := (span.nelems + 7) / 8
		mut count := u32(0)
		for b in 0 .. nbytes {
			count += u32(C.vgc_popcount8(unsafe { span.mark_bits[b] }))
		}
		// Clamp to nelems (last byte may have extra bits)
		if count > span.nelems {
			count = span.nelems
		}
		total += u64(count) * u64(span.elem_size)
	}
	return total
}

// Update the GC trigger point for the next cycle.
// Translated from Go's gcControllerState.endCycle() / heapGoal().
// Uses GOGC logic: trigger when heap grows to (1 + GOGC/100) * marked
fn vgc_update_trigger() {
	marked := C.vgc_atomic_load_u64(&vgc_heap.heap_marked)
	gc_percent := u64(vgc_heap.gc_percent)

	mut goal := marked + marked * gc_percent / 100
	// Avoid very small heap goals that force frequent full cycles on bursty workloads.
	if goal < 256 * 1024 * 1024 {
		goal = 256 * 1024 * 1024
	}
	C.vgc_atomic_store_u64(&vgc_heap.next_gc, goal)
}

// ============================================================
// Heap usage reporting
// ============================================================

fn vgc_heap_usage() (usize, usize, usize, usize, usize) {
	live := C.vgc_atomic_load_u64(&vgc_heap.heap_live)
	total_alloc := C.vgc_atomic_load_u64(&vgc_heap.total_alloc)
	// Count actual in-use span pages as heap size
	mut in_use_bytes := usize(0)
	mut arena := 0
	mut page := usize(0)
	for {
		span, next_arena, next_page := vgc_next_span(arena, page)
		if span == unsafe { nil } {
			break
		}
		arena = next_arena
		page = next_page
		if span.in_use {
			in_use_bytes += usize(span.npages) * vgc_page_size
		}
	}
	free_bytes := if in_use_bytes > usize(live) { in_use_bytes - usize(live) } else { usize(0) }
	return in_use_bytes, free_bytes, usize(live), usize(total_alloc), usize(vgc_heap.gc_cycle)
}

fn vgc_memory_use() usize {
	mut total := usize(0)
	for i in 0 .. vgc_heap.narenas {
		total += vgc_heap.arenas[i].used
	}
	return total
}
