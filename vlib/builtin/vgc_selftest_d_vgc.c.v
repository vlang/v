module builtin

// vgc_residual4_selftest — deterministic, self-contained white-box self-check for the
// two load-bearing fixes to the multi-threaded-churn allocator bug (vgc_malloc returning
// NULL under heavy concurrent alloc/free -> `&T{}` null -> caller null-deref). It lives
// inside module builtin so it can reach the private span allocator + the mcache-protection
// pass; it is driven by bench/parallel-alloc/vgc_residual4_test.v (a module-main test —
// an in-builtin _test.v that references vgc symbols trips a `v test` double-compile of
// builtin). NOT @[markused]: no ordinary program calls it, so -skip-unused prunes it from
// production binaries; the test runner references it so it survives there. Only compiled
// under -d vgc / -gc e (the `_d_vgc` filename suffix). Returns 0 on success, else the id
// of the first failed check.
pub fn vgc_residual4_selftest() u32 {
	// ── Bug C ── vgc_span_alloc_obj's two-pass scan must cover the START byte's LOW
	// bits. The wrap pass used to re-apply the free_index start_bit offset to the start
	// byte, so a span with a free LOW slot but a high free_index (the fill-then-stale-
	// cross-thread-free state) reported "full" -> nil. Single-byte-bitmap spans hit it
	// whenever free_index == nelems.
	mut s := VGC_Span{
		base:      usize(0x100000)
		npages:    1
		elem_size: 16
		nelems:    4
		in_use:    true
	}
	s.alloc_bits = unsafe { &s.alloc_buf[0] }
	s.alloc_buf[0] = u8(0b00001101) // slots 0,2,3 allocated; slot 1 FREE
	s.alloc_count = 3
	s.free_index = 4 // past the free low slot
	p := unsafe { vgc_span_alloc_obj(mut s) }
	if p != unsafe { voidptr(s.base + usize(1) * usize(s.elem_size)) } {
		return 1 // pre-fix: returns nil (scan skipped bits [0,4) in both passes)
	}
	if s.alloc_count != 4 {
		return 2
	}
	// A genuinely full single-byte span must still report nil (no false positive).
	mut f := VGC_Span{
		base:      usize(0x200000)
		npages:    1
		elem_size: 16
		nelems:    4
		in_use:    true
	}
	f.alloc_bits = unsafe { &f.alloc_buf[0] }
	f.alloc_buf[0] = u8(0b00001111)
	f.alloc_count = 4
	f.free_index = 4
	if unsafe { vgc_span_alloc_obj(mut f) } != unsafe { nil } {
		return 3
	}

	// ── Bug B ── vgc_protect_cached_spans must stamp every mcache-RESIDENT span's
	// sweep_gen with the current gc_cycle so this cycle's sweep skips it (else an empty
	// cached span is reclaimed + zeroed by vgc_put_free_span while a suspended owner
	// still references it -> nelems=0 -> nil -> null-deref). A span NOT in any cache
	// must be left alone (so genuinely-dead spans still reclaim).
	idx := C.vgc_get_cache_idx()
	if idx < 0 {
		return 4 // self-check thread must be vgc-registered
	}
	// No allocation happens between vgc_span_alloc and vgc_put_free_span below, and
	// vgc_span_alloc never triggers a collection, so single-threaded no real GC fires
	// in this window to race our scratch spans — no collector-pin needed.
	class_idx := u8(C.vgc_size_class(u32(64)))
	sc := int(class_idx) * 2 // scan variant
	np := u32(C.vgc_get_class_npages(int(class_idx)))
	mut span := vgc_span_alloc(np)
	if span == unsafe { nil } {
		return 5
	}
	unsafe { vgc_span_init(mut span, class_idx, false) }
	old_gen := u32(vgc_heap.gc_cycle)
	span.alloc_count = 0 // empty -> reclaim-eligible
	span.sweep_gen = old_gen - 1 // stale -> WOULD be reclaimed without the stamp
	// install in THIS thread's mcache slot (save + restore; no allocation in the window)
	saved_slot := unsafe { vgc_heap.caches[idx].alloc[sc] }
	unsafe {
		vgc_heap.caches[idx].alloc[sc] = span
	}
	vgc_protect_cached_spans()
	stamped := span.sweep_gen
	unsafe {
		vgc_heap.caches[idx].alloc[sc] = saved_slot
	}
	mut rc := u32(0)
	if stamped != u32(vgc_heap.gc_cycle) {
		rc = 6 // resident span was NOT protected (pre-fix)
	}
	// negative control: a span referenced by no cache slot must NOT be stamped
	mut span2 := vgc_span_alloc(np)
	if span2 != unsafe { nil } {
		unsafe { vgc_span_init(mut span2, class_idx, false) }
		span2.sweep_gen = old_gen - 7
		vgc_protect_cached_spans()
		if rc == 0 && span2.sweep_gen != old_gen - 7 {
			rc = 7
		}
		unsafe { vgc_put_free_span(mut span2) }
	}
	unsafe { vgc_put_free_span(mut span) }
	return rc
}
