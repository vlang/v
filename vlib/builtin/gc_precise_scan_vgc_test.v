// vtest vflags: -gc vgc
// vtest build: !race?
// The precise scan kernel must follow every pointer word declared in the
// allocation's ptrmap, and the span must be marked as precise.
module builtin

struct VPCNode {
	v int
}

struct VPPair {
mut:
	a   &VPCNode = unsafe { nil }
	b   &VPCNode = unsafe { nil }
	pad [120]u64
}

fn test_vgc_precise_scan_kernel_follows_all_pointer_words() {
	$if vgc ? {
		mut p := unsafe { &VPPair(vgc_malloc_typed(uint(sizeof(VPPair)), u64(3), u8(2))) }
		assert p != unsafe { nil }
		p.a = &VPCNode{ v: 11 }
		p.b = &VPCNode{ v: 22 }
		span := vgc_find_span(voidptr(p))
		assert span != unsafe { nil }
		assert span.has_ptrmap
		gc_collect()
		assert p.a.v == 11
		assert p.b.v == 22
	}
}
