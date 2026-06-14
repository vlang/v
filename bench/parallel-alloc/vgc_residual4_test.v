// CX-free deterministic regression for the vgc concurrent-HTTP "residual #4" allocator
// bugs (B: GC reclaiming mcache-resident spans; C: two-pass scan missing the start
// byte's low bits). Both were multi-core null-deref crashes; this drives the white-box
// self-check in module builtin (vgc_residual4_selftest), which exercises the exact span-
// allocator logic with no threads / no timing.
//
//   ../../v -gc e -cc cc test bench/parallel-alloc/vgc_residual4_test.v
//
// Requires -gc e (the vgc backend); the self-check is absent under -gc boehm/none.
module main

fn test_vgc_residual4_allocator_invariants() {
	rc := vgc_residual4_selftest()
	// 0 = ok; 1/2 = Bug C scan miss; 3 = false positive on a full span;
	// 6 = Bug B mcache span not protected; 7 = a non-cached span wrongly stamped;
	// 4/5 = setup precondition (registered thread / span alloc).
	assert rc == 0
}
