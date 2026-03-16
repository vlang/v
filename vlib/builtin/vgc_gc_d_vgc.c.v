// vgc_gc_d_vgc.c.v - V Garbage Collector: Mark, sweep, and orchestration
// Translated from Go's runtime GC (mgc.go, mgcmark.go, mgcsweep.go, mgcwork.go, mgcpacer.go)
// Concurrent tri-color mark-and-sweep with parallel marking.

@[has_globals]
module builtin

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

	// === Phase 1: Sweep Termination (STW) ===
	// Ensure any previous sweep is complete
	vgc_sweep_finish()

	// Stop the world for root scanning
	// Set stop flag - threads will stop at next safepoint (allocation)
	vgc_heap.gc_target_stops = u32(vgc_heap.ncaches - 1) // all threads except GC thread
	C.vgc_atomic_store_u32(&vgc_heap.gc_stopped_count, 0)
	C.vgc_atomic_store_u32(&vgc_heap.gc_stop_flag, 1)

	// Wait for threads to stop (with timeout to avoid deadlock)
	// In practice, threads stop at their next allocation safepoint
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

	// === Phase 2: Mark (parallel using spawn) ===
	// Enable write barrier (for concurrent correctness)
	C.vgc_atomic_store_u32(&vgc_heap.wb_enabled, 1)

	// Scan roots: stacks + globals (conservative scanning)
	vgc_mark_roots()

	// Resume threads - they can allocate but write barrier is on
	C.vgc_atomic_store_u32(&vgc_heap.gc_stop_flag, 0)
	C.vgc_atomic_fence()

	// Parallel mark: drain the work queue using spawn workers
	vgc_parallel_mark()

	// === Phase 3: Mark Termination (brief STW) ===
	C.vgc_atomic_store_u32(&vgc_heap.gc_phase, vgc_phase_mark_term)

	// Process any remaining write barrier buffer entries
	vgc_drain_wb_buf()

	// Final drain of work queue
	vgc_drain_mark_work()

	// Disable write barrier
	C.vgc_atomic_store_u32(&vgc_heap.wb_enabled, 0)

	// Count marked bytes for pacer
	marked := vgc_count_marked()
	C.vgc_atomic_store_u64(&vgc_heap.heap_marked, marked)

	// === Phase 4: Sweep (concurrent) ===
	vgc_heap.sweep_gen++
	C.vgc_atomic_store_u32(&vgc_heap.sweep_done, 0)
	vgc_heap.sweep_idx = 0
	C.vgc_atomic_store_u32(&vgc_heap.gc_phase, vgc_phase_sweep)

	// Sweep concurrently in a background thread
	C.vgc_start_thread(vgc_concurrent_sweep)

	// Update GC trigger for next cycle (translated from Go's gcController.endCycle)
	vgc_update_trigger()

	vgc_heap.gc_cycle++
	C.vgc_atomic_store_u32(&vgc_heap.gc_phase, vgc_phase_off)
}

// ============================================================
// Mark phase (translated from Go's mgcmark.go)
// ============================================================

// Clear all mark bits before a new GC cycle
fn vgc_clear_mark_bits() {
	for i in 0 .. vgc_heap.nspans {
		span := unsafe { vgc_heap.allspans[i] }
		if span == unsafe { nil } || !span.in_use {
			continue
		}
		if span.mark_bits != unsafe { nil } {
			bitmap_size := (span.nelems + 7) / 8
			unsafe { C.memset(span.mark_bits, 0, bitmap_size) }
		}
	}
}

// Conservative root scanning: scan thread stacks and look for pointers into the heap.
// Translated from Go's markroot() / scanblock() - but using conservative scanning
// since V compiles to C and we don't have precise type info at runtime.
fn vgc_mark_roots() {
	// Scan each registered thread's stack
	for i in 0 .. vgc_heap.ncaches {
		cache := unsafe { &vgc_heap.caches[i] }
		if !cache.registered {
			continue
		}
		if cache.stack_lo > 0 && cache.stack_hi > 0 && cache.stack_hi > cache.stack_lo {
			vgc_scan_range(cache.stack_lo, cache.stack_hi)
		}
	}
	// Scan global data/BSS segments would go here
	// For now, conservative scanning of stacks catches most roots
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
		if vgc_is_heap_ptr(val) {
			vgc_shade(val)
		}
		addr += sizeof(usize)
	}
}

// Shade marks an object grey (discovered but not yet scanned).
// Translated from Go's shade() in mgcmark.go.
fn vgc_shade(addr usize) {
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
			// Newly marked - add to work queue for scanning
			obj_addr := span.base + usize(obj_idx) * usize(span.elem_size)
			vgc_work_put(obj_addr)
		}
	}
}

// Parallel mark using spawn (V threads).
// Translated from Go's gcDrain() with multiple workers.
fn vgc_parallel_mark() {
	// Use up to 4 workers (like Go's dedicated mark workers)
	nworkers := if vgc_heap.ncaches < 4 { 1 } else { 4 }
	vgc_heap.gc_nworkers = nworkers
	C.vgc_atomic_store_u32(&vgc_heap.gc_workers_done, 0)

	if nworkers <= 1 {
		// Single-threaded mark
		vgc_drain_mark_work()
		return
	}

	// Start mark workers as OS threads
	for _ in 0 .. nworkers {
		C.vgc_start_thread(vgc_mark_worker)
	}

	// Wait for all workers to finish
	for C.vgc_atomic_load_u32(&vgc_heap.gc_workers_done) < u32(nworkers) {
		C.vgc_atomic_fence()
	}
}

// Mark worker function - runs in a spawned thread.
// Translated from Go's gcDrain() loop.
fn vgc_mark_worker() {
	vgc_ensure_registered()
	vgc_drain_mark_work()
	C.vgc_atomic_add_u32(&vgc_heap.gc_workers_done, 1)
}

// Drain the mark work queue - scan grey objects and mark their referents.
// Translated from Go's gcDrain() in mgcmark.go.
fn vgc_drain_mark_work() {
	for {
		obj_addr := vgc_work_get()
		if obj_addr == 0 {
			break
		}
		// Scan the object conservatively for pointers
		span := vgc_find_span(voidptr(obj_addr))
		if span == unsafe { nil } || span.noscan {
			continue // noscan objects don't contain pointers
		}
		// Scan the object's memory for heap pointers
		obj_size := usize(span.elem_size)
		vgc_scan_range(obj_addr, obj_addr + obj_size)
	}
}

// ============================================================
// Work queue (translated from Go's mgcwork.go)
// ============================================================

// Add a pointer to the mark work queue
fn vgc_work_put(addr usize) {
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
// In a full implementation, this would be compiler-inserted.
// For now, it's called explicitly and buffered.
fn vgc_write_barrier(new_val voidptr) {
	if C.vgc_atomic_load_u32(&vgc_heap.wb_enabled) == 0 {
		return
	}
	if new_val == unsafe { nil } {
		return
	}
	addr := usize(new_val)
	if !vgc_is_heap_ptr(addr) {
		return
	}
	// Shade the new pointer (mark it grey)
	vgc_shade(addr)
}

// Drain buffered write barrier entries
fn vgc_drain_wb_buf() {
	// In this implementation, write barrier calls shade() directly,
	// so there's nothing buffered to drain.
}

// ============================================================
// Sweep phase (translated from Go's mgcsweep.go)
// ============================================================

// Sweep all spans concurrently.
// Translated from Go's bgsweep() goroutine.
fn vgc_concurrent_sweep() {
	for {
		span_idx := vgc_heap.sweep_idx
		if span_idx >= vgc_heap.nspans {
			break
		}
		vgc_heap.sweep_idx = span_idx + 1

		span := unsafe { vgc_heap.allspans[span_idx] }
		if span == unsafe { nil } || !span.in_use {
			continue
		}
		vgc_sweep_span(span)
	}
	C.vgc_atomic_store_u32(&vgc_heap.sweep_done, 1)
}

// Sweep a single span: free unmarked objects.
// Translated from Go's mspan.sweep() in mgcsweep.go.
fn vgc_sweep_span(span &VGC_Span) {
	if span.alloc_bits == unsafe { nil } || span.mark_bits == unsafe { nil } {
		return
	}

	mut freed := u32(0)
	for i in 0 .. span.nelems {
		if C.vgc_bitmap_get(span.alloc_bits, i) != 0 {
			// Object is allocated
			if C.vgc_bitmap_get(span.mark_bits, i) == 0 {
				// Not marked - free it
				C.vgc_bitmap_clear(span.alloc_bits, i)
				freed++
				// Update free_index hint
				if i < span.free_index {
					unsafe {
						(&VGC_Span(span)).free_index = i
					}
				}
			}
		}
	}

	if freed > 0 {
		unsafe {
			(&VGC_Span(span)).alloc_count -= freed
		}
		// Return freed memory to heap accounting
		freed_bytes := u64(freed) * u64(span.elem_size)
		C.vgc_atomic_sub_u64(&vgc_heap.heap_live, freed_bytes)
	}
}

// Ensure all sweeping from previous cycle is done
fn vgc_sweep_finish() {
	if C.vgc_atomic_load_u32(&vgc_heap.sweep_done) == 0 && vgc_heap.gc_cycle > 0 {
		// Sweep any remaining spans
		for vgc_heap.sweep_idx < vgc_heap.nspans {
			idx := vgc_heap.sweep_idx
			vgc_heap.sweep_idx = idx + 1
			span := unsafe { vgc_heap.allspans[idx] }
			if span != unsafe { nil } && span.in_use {
				vgc_sweep_span(span)
			}
		}
		C.vgc_atomic_store_u32(&vgc_heap.sweep_done, 1)
	}
}

// ============================================================
// GC Pacer (translated from Go's mgcpacer.go gcController)
// ============================================================

// Count total marked bytes across all spans
fn vgc_count_marked() u64 {
	mut total := u64(0)
	for i in 0 .. vgc_heap.nspans {
		span := unsafe { vgc_heap.allspans[i] }
		if span == unsafe { nil } || !span.in_use || span.mark_bits == unsafe { nil } {
			continue
		}
		mut count := u32(0)
		for j in 0 .. span.nelems {
			if C.vgc_bitmap_get(span.mark_bits, j) != 0 {
				count++
			}
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
	// Minimum 4MB trigger
	if goal < 4 * 1024 * 1024 {
		goal = 4 * 1024 * 1024
	}
	C.vgc_atomic_store_u64(&vgc_heap.next_gc, goal)
}

// ============================================================
// Heap usage reporting
// ============================================================

fn vgc_heap_usage() (usize, usize, usize, usize, usize) {
	// Calculate total arena size
	mut total_size := usize(0)
	mut total_used := usize(0)
	for i in 0 .. vgc_heap.narenas {
		a := unsafe { &vgc_heap.arenas[i] }
		total_size += a.size
		total_used += a.used
	}
	free_bytes := total_size - total_used
	live := C.vgc_atomic_load_u64(&vgc_heap.heap_live)
	total_alloc := C.vgc_atomic_load_u64(&vgc_heap.total_alloc)
	return total_size, usize(free_bytes), usize(live), usize(total_alloc), usize(vgc_heap.gc_cycle)
}

fn vgc_memory_use() usize {
	mut total := usize(0)
	for i in 0 .. vgc_heap.narenas {
		total += vgc_heap.arenas[i].used
	}
	return total
}
