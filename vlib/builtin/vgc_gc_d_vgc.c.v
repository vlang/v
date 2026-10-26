// vgc_gc_d_vgc.c.v - V Garbage Collector: Mark, sweep, and orchestration
// Translated from Go's runtime GC (mgc.go, mgcmark.go, mgcsweep.go, mgcwork.go, mgcpacer.go)
// Concurrent tri-color mark-and-sweep with parallel marking.

@[has_globals]
module builtin

fn C.v3_vgc_mark_global_arrays()
fn C.vgc_data_ranges(lo &uint, hi &uint, max int) int

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

// vgc_stop_wait_ns bounds the wait for the other threads to stop. A thread that is blocked
// (in a join, a lock or a system call) does not reach a safepoint, and the collection
// proceeds without it after this time. The previous wait, 1M spins, took about 1-2.5 ms.
const vgc_stop_wait_ns = u64(1_000_000)

// vgc_wait_for_stops waits until every other registered thread acknowledged the stop request
// `epoch` at a safepoint, or until vgc_stop_wait_ns passed. The caller holds cache_lock, so no
// thread registers or exits meanwhile.
fn vgc_wait_for_stops(self_idx int, epoch u32) {
	mut deadline := u64(0)
	mut round := 0
	for i in 0 .. vgc_heap.ncaches {
		if i == self_idx || !unsafe { vgc_heap.caches[i].registered } {
			continue
		}
		for C.vgc_atomic_load_u32(&vgc_heap.caches[i].stop_epoch) != epoch {
			round++
			if round >= 16 {
				// Read the clock only once spinning did not suffice.
				now := C.vgc_now_ns()
				if deadline == 0 {
					deadline = now + vgc_stop_wait_ns
				} else if now >= deadline {
					return
				}
			}
			C.vgc_backoff(round)
		}
	}
}

// vgc_gc_run collects. This thread's stack is scanned from regs, where its registers are saved,
// upwards: the frames below are the collector's, and they hold copies of the stack and arena
// bounds, which can be the address of an object.
fn vgc_gc_run(_ voidptr, regs voidptr) {
	// === Phase 1: Sweep Termination (STW) ===
	// Ensure any previous sweep is complete
	vgc_sweep_finish()

	// Stop the world for root scanning
	// Set stop flag - threads will stop at next safepoint (allocation)
	self_idx := C.vgc_get_cache_idx()
	mut epoch := C.vgc_atomic_load_u32(&vgc_heap.gc_stop_epoch) + 1
	if epoch == 0 {
		epoch = 1
	}
	C.vgc_atomic_store_u32(&vgc_heap.gc_stop_epoch, epoch)
	C.vgc_atomic_store_u32(&vgc_heap.gc_stop_flag, 1)
	vgc_wait_for_stops(self_idx, epoch)

	// Clear mark bits on all spans (prepare for new cycle)
	vgc_clear_mark_bits()

	// === Phase 2: Mark (parallel) ===
	// Enable write barrier (for concurrent correctness)
	C.vgc_atomic_store_u32(&vgc_heap.wb_enabled, 1)

	// The thread starting GC does not pass through a safepoint.
	vgc_refresh_stack_range_for_sp(self_idx, uint(regs))

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
	// The live heap is what we actually found alive, plus what the threads allocate from now on
	// (see vgc_heap_live).
	C.vgc_atomic_store_u64(&vgc_heap.heap_marked, marked)
	C.vgc_atomic_store_u64(&vgc_heap.live_at_gc, vgc_live_sum())

	// === Phase 4: Sweep ===
	vgc_heap.sweep_gen++
	C.vgc_atomic_store_u32(&vgc_heap.sweep_done, 0)
	C.vgc_atomic_store_u32(&vgc_heap.gc_phase, vgc_phase_sweep)

	// Sweep synchronously - it's fast and avoids race conditions
	vgc_do_sweep()

	// Update GC trigger for next cycle (translated from Go's gcController.endCycle)
	vgc_update_trigger()

	vgc_heap.gc_cycle++
	C.vgc_atomic_store_u64(&vgc_heap.alloc_at_gc, vgc_total_alloc())
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
	// Keep the arena table still while walking it.
	C.vgc_mutex_lock(&vgc_heap.lock)
	mut arena := 0
	mut page := uint(0)
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
	C.vgc_mutex_unlock(&vgc_heap.lock)
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
// vgc's own globals are left out. They hold the bounds of the stacks and of the arenas, which
// can be the address of an object (the first one in an arena that starts where a stack ends),
// and the spans that the caches allocate from, but no pointer that the program holds. The only
// objects that they keep alive are the tiny blocks, which vgc_mark_roots marks itself.
fn vgc_mark_data_segments() {
	mut lo := [16]uint{}
	mut hi := [16]uint{}
	n := C.vgc_data_ranges(&lo[0], &hi[0], 16)
	skip_lo := [uint(&vgc_heap), uint(&vgc_arena_lo), uint(&vgc_arena_hi)]!
	skip_hi := [uint(&vgc_heap) + sizeof(VGC_Heap), uint(&vgc_arena_lo) + sizeof(uint),
		uint(&vgc_arena_hi) + sizeof(uint)]!
	for i in 0 .. n {
		vgc_scan_range_skipping(lo[i], hi[i], &skip_lo[0], &skip_hi[0], skip_lo.len)
	}
}

// vgc_scan_range_skipping scans [lo, hi) without the nskip ranges [skip_lo[k], skip_hi[k]).
fn vgc_scan_range_skipping(lo uint, hi uint, skip_lo &uint, skip_hi &uint, nskip int) {
	if lo >= hi {
		return
	}
	if nskip == 0 {
		vgc_scan_range(lo, hi)
		return
	}
	s_lo := unsafe { skip_lo[0] }
	s_hi := unsafe { skip_hi[0] }
	rest_lo := unsafe { &skip_lo[1] }
	rest_hi := unsafe { &skip_hi[1] }
	if s_hi <= lo || s_lo >= hi {
		vgc_scan_range_skipping(lo, hi, rest_lo, rest_hi, nskip - 1)
		return
	}
	vgc_scan_range_skipping(lo, s_lo, rest_lo, rest_hi, nskip - 1)
	vgc_scan_range_skipping(s_hi, hi, rest_lo, rest_hi, nskip - 1)
}

// Scan a memory range conservatively, looking for pointers into the GC heap.
// Each word-aligned value that looks like a heap pointer is treated as a root.
// Translated from Go's scanblock() with conservative pointer finding.
fn vgc_scan_range(lo uint, hi uint) {
	// Align to word boundaries
	start := (lo + sizeof(uint) - 1) & ~(uint(sizeof(uint)) - 1)
	mut addr := start
	for addr + sizeof(uint) <= hi {
		val := unsafe { *(&uint(voidptr(addr))) }
		if val != 0 {
			vgc_shade(val)
		}
		addr += sizeof(uint)
	}
}

// Shade marks an object grey (discovered but not yet scanned).
// Translated from Go's shade() in mgcmark.go.
fn vgc_shade(addr uint) {
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
	obj_idx := u32((addr - span.base) / uint(span.elem_size))
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
				obj_addr := span.base + uint(obj_idx) * uint(span.elem_size)
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
			obj_size := uint(span.elem_size)
			vgc_scan_range(obj_addr, obj_addr + obj_size)
		}
	}
}

// Precise pointer scanning: use the pointer bitmap to scan only
// word offsets known to contain pointers. Much faster than conservative.
fn vgc_scan_precise(obj_addr uint, ptrmap u64, ptr_words u8) {
	mut mask := ptrmap
	word_size := sizeof(uint)
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
		ptr_addr := obj_addr + uint(bit) * word_size
		val := unsafe { *(&uint(voidptr(ptr_addr))) }
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
fn vgc_work_put(addr uint) {
	if vgc_can_use_work_fastpath() {
		mut buf := vgc_heap.work_full
		if buf == unsafe { nil } || buf.nobj >= 256 {
			mut new_buf := vgc_heap.work_empty
			if new_buf != unsafe { nil } {
				unsafe {
					vgc_heap.work_empty = new_buf.next
				}
			} else {
				new_buf = unsafe { &VGC_WorkBuf(C.vgc_os_alloc(uint(sizeof(VGC_WorkBuf)))) }
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
			new_buf = unsafe { &VGC_WorkBuf(C.vgc_os_alloc(uint(sizeof(VGC_WorkBuf)))) }
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
fn vgc_work_get() uint {
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
	vgc_shade(uint(new_val))
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
	// Keep the arena table still while walking it.
	C.vgc_mutex_lock(&vgc_heap.lock)
	mut arena := 0
	mut page := uint(0)
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
	C.vgc_mutex_lock(&vgc_heap.lock)
	mut arena := 0
	mut page := uint(0)
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
	C.vgc_mutex_unlock(&vgc_heap.lock)
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

// vgc_heap_usage returns heap_size, free_bytes, total_bytes, unmapped_bytes and bytes_since_gc,
// with the meanings of Boehm's GC_get_heap_usage_safe():
// - heap_size: the heap memory obtained from the OS, without the part returned to it. vgc gets
//   it in arenas, and gives the pages of every span that a collection empties back to the OS.
// - free_bytes: the part of heap_size in free blocks: the arenas' parts that no span was carved
//   from yet. Like Boehm's heap blocks that were never allocated, they are mapped but untouched.
// - total_bytes: the bytes allocated since the program started.
// - unmapped_bytes: the pages of the free spans, which went back to the OS.
// - bytes_since_gc: the bytes allocated since the last collection.
fn vgc_heap_usage() (uint, uint, uint, uint, uint) {
	obtained := C.vgc_atomic_load_u64(&vgc_heap.arena_bytes)
	used := C.vgc_atomic_load_u64(&vgc_heap.arena_used)
	unmapped := C.vgc_atomic_load_u64(&vgc_heap.unmapped_bytes)
	total := vgc_total_alloc()
	at_gc := C.vgc_atomic_load_u64(&vgc_heap.alloc_at_gc)
	heap_size := if obtained > unmapped { obtained - unmapped } else { u64(0) }
	free := if obtained > used { obtained - used } else { u64(0) }
	since_gc := if total > at_gc { total - at_gc } else { u64(0) }
	return uint(heap_size), uint(free), uint(total), uint(unmapped), uint(since_gc)
}

// vgc_memory_use returns the heap memory in use: the pages of the spans that are not free, which
// is heap_size - free_bytes, as with Boehm's GC_get_memory_use().
fn vgc_memory_use() uint {
	used := C.vgc_atomic_load_u64(&vgc_heap.arena_used)
	unmapped := C.vgc_atomic_load_u64(&vgc_heap.unmapped_bytes)
	return if used > unmapped { uint(used - unmapped) } else { uint(0) }
}
