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
// Flow: sweep termination (STW) -> full STW mark -> sweep -> resume.
// Invoked via the vgc_run_gc_spilled trampoline (vgc_platform.h), which has
// already recorded THIS thread's stack range with callee-saved registers
// spilled — so vgc_gc_start must NOT re-record the collector's own range.
@[export: 'vgc_gc_start']
fn vgc_gc_start() {
	// Only one GC at a time
	mut expected := vgc_phase_off
	if !C.vgc_atomic_cas_u32(&vgc_heap.gc_phase, &expected, vgc_phase_mark) {
		return
	}

	// Take free_spans_lock for the WHOLE cycle, BEFORE stopping the world, so no
	// mutator is ever frozen mid-vgc_get_free_span holding it (which, if the lock
	// were then stolen, would resume into a corrupted free list and hand out a span
	// with a garbage base). Acquired while mutators still run, a current holder
	// releases promptly; thereafter the collector owns it and vgc_put_free_span
	// (collector-only) runs lock-free. Released right after sweep, before resume.
	// (Lock order: free_spans_lock then cache_lock; no mutator holds free_spans_lock
	// and then waits on cache_lock, so no deadlock.)
	C.vgc_mutex_lock(&vgc_heap.free_spans_lock)

	// === Phase 1: Sweep Termination (STW) ===
	// Ensure any previous sweep is complete
	vgc_sweep_finish()

	// === Stop the world via OS-level mach suspend ===
	// Suspend every live registered mutator (except this collector). Unlike the
	// cooperative alloc-path safepoint (which cannot stop a thread blocked in a
	// syscall or spinning in a tight non-allocating loop), mach thread_suspend
	// stops a thread in ANY state. Mechanism validated standalone in
	// bench/parallel-alloc/suspend_world.c.
	self_idx := C.vgc_get_cache_idx()
	if vgc_watch_addr != 0 {
		vgc_watch_cycles++ // DIAGNOSTIC: count collections observed since the watch was set
	}
	C.vgc_trace(5, self_idx, u64(vgc_heap.gc_cycle), u64(vgc_heap.ncaches)) // GC_BEG
	C.vgc_atomic_store_u32(&vgc_heap.gc_stop_flag, 0) // OS-suspend, not cooperative

	// Serialize thread registration/deregistration with STW: hold cache_lock
	// across the whole cycle. Acquired BEFORE any suspend (so no holder is
	// frozen), it blocks new vgc_register_thread/vgc_thread_exit_cb until the
	// cycle ends — closing the thread-create-vs-STW race where a thread that
	// registers just as STW starts would run unsuspended and allocate white.
	C.vgc_mutex_lock(&vgc_heap.cache_lock)

	// Acquire EVERY allocator lock BEFORE stopping the world, so no mutator is ever
	// mach-suspended mid-critical-section. This is the root fix for the thread-churn
	// segv class: previously the collector suspended first and then STOLE (zeroed)
	// these locks "to avoid deadlocking on a frozen owner", but a mutator frozen
	// mid-vgc_span_alloc / vgc_central_get_span / vgc_get_free_span would, on resume,
	// find its lock zeroed and run concurrently with another allocator -> two threads
	// bump the same arena / pop the same free span -> a span with a garbage base ->
	// SIGSEGV in the allocation/bitmap memset. Acquiring up-front (while mutators are
	// still RUNNING and will release promptly) means every allocator structure is
	// quiescent and consistent for the whole cycle, and the frozen-owner problem
	// cannot arise. Safe against deadlock: the collector never RE-ENTERS vgc_heap.lock
	// or central[].lock (mutator-only; verified), and vgc_put_free_span is lock-free
	// (the collector already holds free_spans_lock, taken at the top of this fn); a
	// mutator never holds two allocator locks at once, so acquisition order is free.
	// (cache_lock: the registration gate, already held. work_lock: re-entered by the
	// mark; no mutator holds it under full-STW, so it is reset, not pre-acquired.)
	C.vgc_mutex_lock(&vgc_heap.lock)
	for i in 0 .. 136 {
		C.vgc_mutex_lock(&vgc_heap.central[i].lock)
	}

	for i in 0 .. vgc_heap.ncaches {
		c := unsafe { &vgc_heap.caches[i] }
		reg := if c.registered { u64(1) } else { u64(0) }
		C.vgc_trace(6, i, reg, u64(c.mach_port)) // SUSP? (decision inputs for EVERY slot)
		if c.registered && i != self_idx && c.mach_port != 0 {
			C.vgc_suspend_thread(c.mach_port)
			C.vgc_trace(7, i, u64(c.mach_port), 0) // SUSP! (actually suspended)
		}
	}
	// work_lock is re-entered by vgc_work_put/get during mark; no mutator holds it
	// under full-STW (the write barrier never fires while mutators are frozen), so a
	// plain reset is enough (it is not pre-acquired since the collector re-enters it).
	C.vgc_atomic_store_u32(&vgc_heap.work_lock, 0)

	// Clear mark bits on all spans (prepare for new cycle)
	vgc_clear_mark_bits()
	vgc_watch_snapshot(0) // STAGE 0: post-clear (expect found+in_use+alloc, mark=0)

	// Scan each suspended thread's roots: refresh its stack range from the
	// actual suspended SP and shade its register-resident roots (the exact roots
	// a stack-only scan misses; validated in bench/parallel-alloc/stw_root_scan.c).
	vgc_scan_suspended_roots(self_idx)
	vgc_watch_snapshot(1) // STAGE 1: post suspended-thread reg/stack roots (main's reg holds c)

	// === Phase 2: Mark (parallel) ===
	// Enable write barrier (for concurrent correctness)
	C.vgc_atomic_store_u32(&vgc_heap.wb_enabled, 1)

	// NOTE: the collector's own stack range was already recorded (with
	// registers spilled) by the vgc_run_gc_spilled trampoline before this
	// function was entered. Do NOT call vgc_refresh_stack_range() here — that
	// would overwrite the range with a higher SP and drop the spilled regs.

	// Scan roots: stacks + globals (conservative scanning)
	vgc_mark_roots()
	vgc_watch_snapshot(2) // STAGE 2: post stack roots

	// Shade in-flight spawn-argument structs. These are live (a freshly created
	// thread is about to read them) but unreachable from any scanned stack during
	// the create->start handoff, because the child is not yet registered. Reading
	// the array without the lock is safe here: the world is stopped, so no mutator
	// can be mid add/remove except one frozen holding the lock, and an entry is
	// only counted after its slot is written. (See vgc_spawn_roots.)
	for i in 0 .. vgc_nspawn_roots {
		root := usize(vgc_spawn_roots[i])
		vgc_shade(root)
		// DIAGNOSTIC (root-scan-miss localizer): is the watched object the spawn
		// arg itself (bit0), or reachable from it via arg->...->c (bit1)? Pins
		// whether the spawn-root drain actually reaches c.
		if vgc_watch_addr != 0 && root != 0 {
			if root == vgc_watch_addr {
				vgc_watch_in_spawn |= 1
			} else {
				vgc_watch_scan_obj(root)
			}
		}
	}
	vgc_watch_snapshot(3) // STAGE 3: post spawn-root shade

	// FULL stop-the-world mark+sweep: the world stays stopped from here through
	// the end of sweep. The previous design resumed mutators here and marked
	// concurrently, but objects allocated DURING the concurrent mark were not
	// alloc-blacked and stack-local pointer writes carry no write barrier, so a
	// freshly-allocated-but-live object (e.g. `last` in a tight alloc loop) was
	// swept -> use-after-free. Keeping the world stopped through sweep removes
	// that entire unsound-concurrency window. (Concurrency is a later perf
	// optimization that must come with alloc-black + a correct barrier.)

	// Parallel mark: drain the work queue (workers only; mutators are stopped)
	vgc_parallel_mark()
	vgc_watch_snapshot(4) // STAGE 4: post parallel-mark drain

	// === Phase 3: Mark Termination (brief STW) ===
	C.vgc_atomic_store_u32(&vgc_heap.gc_phase, vgc_phase_mark_term)

	// Final drain of work queue
	vgc_drain_mark_work()
	vgc_watch_snapshot(5) // STAGE 5: post final drain (mark term)

	// Disable write barrier
	C.vgc_atomic_store_u32(&vgc_heap.wb_enabled, 0)

	// Compute live bytes from mark bits
	marked := vgc_count_marked()
	C.vgc_atomic_store_u64(&vgc_heap.heap_marked, marked)
	// Reset heap_live to match what we actually found alive. The per-thread
	// live_delta/alloc_delta (un-flushed accounting) are now stale — `marked` is
	// the true live set — so flush each thread's total-alloc stat and zero both
	// deltas. Safe: the world is stopped, so no mutator is mid-update.
	for ci in 0 .. vgc_heap.ncaches {
		unsafe {
			vgc_heap.total_alloc += vgc_heap.caches[ci].alloc_delta
			vgc_heap.caches[ci].alloc_delta = 0
			vgc_heap.caches[ci].live_delta = 0
		}
	}
	C.vgc_atomic_store_u64(&vgc_heap.heap_live, marked)

	// === Phase 4: Sweep ===
	vgc_heap.sweep_gen++
	C.vgc_atomic_store_u32(&vgc_heap.sweep_done, 0)
	vgc_heap.sweep_idx = 0
	C.vgc_atomic_store_u32(&vgc_heap.gc_phase, vgc_phase_sweep)

	// Sweep synchronously - it's fast and avoids race conditions
	vgc_watch_snapshot(6) // STAGE 6: immediately pre-sweep (the verdict mark+alloc state)
	C.vgc_trace(9, self_idx, u64(vgc_heap.gc_cycle), 0) // SWEEP0
	vgc_do_sweep()
	C.vgc_trace(10, self_idx, u64(vgc_heap.gc_cycle), 0) // SWEEP1

	// Drop mcache slots whose cached span sweep just recycled to the pool (and the
	// tiny cursor). Must run while the world is still stopped, after sweep. Without
	// this, threads resume holding pointers to freed/decommitted spans -> reuse breaks
	// (unbounded arena growth) and the next alloc faults. See vgc_fixup_caches.
	vgc_fixup_caches()

	// Mark + sweep done — release every allocator lock held across the cycle before
	// resuming, so resumed mutators don't block on them. (Reverse acquisition order.)
	for i in 0 .. 136 {
		C.vgc_mutex_unlock(&vgc_heap.central[i].lock)
	}
	C.vgc_mutex_unlock(&vgc_heap.lock)
	C.vgc_mutex_unlock(&vgc_heap.free_spans_lock)

	// Resume the world: mark + sweep are complete, every live object survived.
	C.vgc_atomic_fence()
	for i in 0 .. vgc_heap.ncaches {
		c := unsafe { &vgc_heap.caches[i] }
		if c.registered && i != self_idx && c.mach_port != 0 {
			C.vgc_resume_thread(c.mach_port)
			C.vgc_trace(11, i, u64(c.mach_port), 0) // RESUME
		}
	}
	C.vgc_mutex_unlock(&vgc_heap.cache_lock) // release the registration gate

	// Update GC trigger for next cycle (translated from Go's gcController.endCycle)
	vgc_update_trigger()

	vgc_heap.gc_cycle++
	C.vgc_trace(12, self_idx, u64(vgc_heap.gc_cycle), 0) // GC_END
	C.vgc_atomic_store_u32(&vgc_heap.gc_phase, vgc_phase_off)
}

// Scan the roots of every suspended mutator: refresh its stack range from its
// real (suspended) SP and shade its register-resident roots. Mach
// thread_get_state yields both, so register roots come for free — closing the
// stack-only-scan gap that dropped live values. (stw_root_scan.c prototype.)
fn vgc_scan_suspended_roots(self_idx int) {
	for i in 0 .. vgc_heap.ncaches {
		c := unsafe { &vgc_heap.caches[i] }
		if !c.registered || i == self_idx || c.mach_port == 0 {
			continue
		}
		mut sp := usize(0)
		mut regs := [31]usize{}
		n := C.vgc_thread_regs(c.mach_port, &sp, &regs[0], 31)
		C.vgc_trace(8, i, u64(sp), u64(n)) // SCAN (sp + reg count actually captured)
		if n > 0 && sp != 0 {
			vgc_refresh_stack_range_for_sp(i, sp) // [sp, stack_base]
			for k in 0 .. n {
				vgc_shade(regs[k]) // register-resident roots
			}
			// DIAGNOSTIC (root-scan-miss localizer): is the watched object held
			// only in this suspended thread's registers (e.g. main spin-waiting
			// on c with c in a callee-saved reg)? Bounded to n captured regs.
			if vgc_watch_addr != 0 {
				for k in 0 .. n {
					if regs[k] == vgc_watch_addr {
						vgc_watch_in_reg = u32(i + 1)
						break
					}
				}
			}
		}
	}
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
fn C.vgc_data_segments(los &usize, his &usize, max_ranges int) int

// vgc_fixup_caches clears every per-thread mcache slot whose cached span was
// recycled by THIS cycle's sweep, plus the tiny-allocator cursor. The collector
// sweeps ALL spans, including the one a thread currently has cached in
// caches[i].alloc[class]; if sweep finds that span empty it recycles it to the
// free-span pool (in_use=false, bitmaps freed, pages decommitted) while the mcache
// still points at it. Left alone, that stale pointer (a) makes the next alloc
// sub-allocate into freed/decommitted memory, and (b) breaks span reuse —
// vgc_cache_get_span sees alloc_count(0) < nelems(0) == false, mis-treats the pooled
// span as "full", and the allocator creates fresh spans despite a full pool ->
// unbounded arena growth. Nulling the slot makes the thread refill cleanly from
// central / the pool after resume. Spans that survived sweep (still in_use) stay
// cached. MUST be called after vgc_do_sweep and before the world is resumed; it only
// reads span.in_use and writes mcache fields (no central-list surgery -> no
// double-membership, no span-init race).
fn vgc_fixup_caches() {
	for i in 0 .. vgc_heap.ncaches {
		mut c := unsafe { &vgc_heap.caches[i] }
		// The tiny cursor points into a span that may have been swept+decommitted this
		// cycle; always drop it (worst case abandons the current tiny block's tail).
		c.tiny = 0
		c.tiny_offset = 0
		for sc in 0 .. 136 {
			span := unsafe { c.alloc[sc] }
			if span != unsafe { nil } && !span.in_use {
				unsafe {
					c.alloc[sc] = nil
				}
			}
		}
	}
}

fn vgc_mark_roots() {
	// Scan the global / BSS data segments (V __global roots). Without this, an
	// object reachable only through a global (e.g. rand.default_rng) is reclaimed,
	// and a later access — typically a module's at-exit _deinit — dereferences
	// freed memory and SIGSEGVs. Conservative, like the stack scan.
	mut seg_lo := [8]usize{}
	mut seg_hi := [8]usize{}
	nseg := C.vgc_data_segments(&seg_lo[0], &seg_hi[0], 8)
	// Scan the WHOLE data segment, including the collector's own `vgc_heap` struct.
	// It is tempting to skip vgc_heap (it is large and is "just" collector metadata),
	// but doing so intermittently reclaims live data: vgc_heap's per-thread caches
	// hold pointers (the tiny-allocator cursor / the in-flight mcache spans' object
	// memory) that conservatively root objects the stack+register scan does not fully
	// cover during thread create/exit churn. Excluding it removes that protection and
	// G-CHURN fails ~1-in-3 with reclaimed anchor nodes (verified by isolation). The
	// scan is cheap now that span reuse is fixed (vgc_heap stays small and bounded),
	// so correctness wins: scan it all.
	for k in 0 .. nseg {
		if seg_lo[k] > 0 && seg_hi[k] > seg_lo[k] {
			vgc_scan_range(seg_lo[k], seg_hi[k])
		}
	}

	// Scan each registered thread's stack
	for i in 0 .. vgc_heap.ncaches {
		cache := unsafe { &vgc_heap.caches[i] }
		if !cache.registered {
			continue
		}
		if cache.stack_lo > 0 && cache.stack_hi > 0 && cache.stack_hi > cache.stack_lo {
			vgc_scan_range(cache.stack_lo, cache.stack_hi)
			// DIAGNOSTIC (root-scan-miss localizer): does THIS thread's stack hold
			// a pointer to the watched object? Bounded to the stack ranges only.
			if vgc_watch_addr != 0 {
				vgc_watch_scan_range(cache.stack_lo, cache.stack_hi, i)
			}
		}
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
	// NOTE: the watched-object check is intentionally NOT done here — vgc_shade is
	// the hottest path (called per scanned word) and a per-word compare perturbed
	// timing enough to mask the (timing-sensitive) residual. `marked` and `swept`
	// for the watched object are derived in vgc_sweep_span instead (once per span).
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

// Parallel mark using OS threads.
// Translated from Go's gcDrain() with multiple workers.
fn vgc_parallel_mark() {
	// Single-threaded mark for the minimal STW collector: spawning mark-worker
	// threads during a collection would have them hit the registration barrier
	// (gc_phase != off) and deadlock, and the work-queue lock-stealing under STW
	// assumes a single marker. Collection is rare (Perceus front line front-loads
	// frees), so single-threaded mark is acceptable; parallel mark is a later
	// optimization that must reintroduce a GC-worker exemption from the barrier.
	mut nworkers := 1
	vgc_heap.gc_nworkers = nworkers
	C.vgc_atomic_store_u32(&vgc_heap.gc_workers_done, 0)

	if nworkers <= 1 {
		vgc_drain_mark_work()
		return
	}

	// Start helper workers and let the current GC thread participate as well.
	for _ in 1 .. nworkers {
		C.vgc_start_thread(vgc_mark_worker)
	}
	vgc_drain_mark_work()
	C.vgc_atomic_add_u32(&vgc_heap.gc_workers_done, 1)

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
	for i in 0 .. vgc_heap.nspans {
		span := unsafe { vgc_heap.allspans[i] }
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

	// Sweep using byte-level operations for speed
	nbytes := (span.nelems + 7) / 8
	mut freed := u32(0)
	mut new_free_index := span.nelems // will be set to lowest freed index

	// DIAGNOSTIC: compute the watched object's bit position within this span (if any).
	mut watch_byte := u32(0xffffffff)
	mut watch_bit := u8(0)
	if vgc_watch_addr != 0 && span.elem_size != 0 && vgc_watch_addr >= span.base
		&& vgc_watch_addr < span.base + usize(span.nelems) * usize(span.elem_size) {
		w_idx := u32((vgc_watch_addr - span.base) / usize(span.elem_size))
		watch_byte = w_idx >> 3
		watch_bit = u8(1) << (w_idx & 7)
	}

	for b in 0 .. nbytes {
		alloc_byte := unsafe { span.alloc_bits[b] }
		mark_byte := unsafe { span.mark_bits[b] }
		// allocated but not marked = garbage
		garbage := alloc_byte & ~mark_byte
		if b == watch_byte {
			// DIAGNOSTIC (per-span, cheap): record this cycle's mark/sweep verdict
			// for the watched object. marked = its mark bit is set at sweep time;
			// swept = it was allocated-but-unmarked and is being freed now.
			if (mark_byte & watch_bit) != 0 {
				vgc_watch_marked = 1
			}
			if (garbage & watch_bit) != 0 {
				vgc_watch_swept = 1
			}
		}
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

	// If span is completely empty, recycle it to the free span pool. But FIRST
	// unlink it from any central partial/full list it is on — vgc_put_free_span
	// reuses span.next for the free_spans chain, so recycling a still-linked span
	// would splice free_spans into the central list and a later vgc_central_get_span
	// would traverse a garbage node -> wild span -> SIGSEGV. Safe to touch the
	// central lists here: vgc_gc_start holds every central[].lock across the cycle.
	//
	// sweep_gen GUARD: never reclaim a span ACQUIRED this same GC cycle. A mutator
	// that just got a fresh/recycled span (vgc_span_alloc/central_get_span set
	// span.sweep_gen = gc_cycle) but was suspended mid-vgc_span_init — after
	// mark_bits=mmap, with alloc_count still 0 and the span already in allspans —
	// would otherwise look "empty" here and be freed (mark_bits munmap'd, base
	// decommitted); on resume the mutator's span_init memset hits the freed page ->
	// SIGSEGV. Giving an in-flight span a one-cycle grace closes that window (a span
	// genuinely emptied this cycle is reclaimed on the next, a bounded delay).
	if span.alloc_count == 0 && span.npages > 0 && span.sweep_gen != u32(vgc_heap.gc_cycle) {
		mut mspan := unsafe { &VGC_Span(span) }
		if mspan.on_central != 0 {
			sc := int(mspan.class_idx) * 2 + if mspan.noscan { 1 } else { 0 }
			unsafe {
				if mspan.prev != nil {
					mspan.prev.next = mspan.next
				} else if mspan.on_central == 1 {
					vgc_heap.central[sc].partial = mspan.next
				} else {
					vgc_heap.central[sc].full = mspan.next
				}
				if mspan.next != nil {
					mspan.next.prev = mspan.prev
				}
				mspan.on_central = 0
				mspan.next = nil
				mspan.prev = nil
			}
		}
		vgc_put_free_span(mut mspan)
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

// Count total marked bytes across all spans using byte-level popcount
fn vgc_count_marked() u64 {
	mut total := u64(0)
	for i in 0 .. vgc_heap.nspans {
		span := unsafe { vgc_heap.allspans[i] }
		if span == unsafe { nil } || !span.in_use || span.mark_bits == unsafe { nil } {
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
	for i in 0 .. vgc_heap.nspans {
		span := unsafe { vgc_heap.allspans[i] }
		if span != unsafe { nil } && span.in_use {
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
