module edwards25519

import crypto.internal.subtle

// A precomputed lookup table for fixed-base, constant-time scalar muls.
struct AffineLookupTable {
mut:
	points [8]AffineCached
}

// A dynamic lookup table for variable-base, constant-time scalar muls.
struct ProjLookupTable {
mut:
	points [8]ProjectiveCached
}

// A dynamic lookup table for variable-base, variable-time scalar muls.
struct NafLookupTable5 {
mut:
	points [8]ProjectiveCached
}

// A precomputed lookup table for fixed-base, variable-time scalar muls.
struct NafLookupTable8 {
mut:
	points [64]AffineCached
}

// Constructors.

// Builds a lookup table at runtime. Fast.
fn (mut v ProjLookupTable) from_p3(q Point) {
	// Goal: v.points[i] = (i+1)*Q, i.e., Q, 2Q, ..., 8Q
	// This allows lookup of -8Q, ..., -Q, 0, Q, ..., 8Q
	v.points[0].from_p3(q)
	mut tmp_p3 := Point{}
	mut tmp_p1 := ProjectiveP1{}
	for i := 0; i < 7; i++ {
		// Compute (i+1)*Q as Q + i*Q and convert to a ProjCached
		// This is needlessly complicated because the API has explicit
		// recievers instead of creating stack objects and relying on RVO
		v.points[i + 1].from_p3(tmp_p3.from_p1(tmp_p1.add(q, v.points[i])))
	}
}

// This is not optimised for speed; fixed-base tables should be precomputed.
fn (mut v AffineLookupTable) from_p3(q Point) {
	// Goal: v.points[i] = (i+1)*Q, i.e., Q, 2Q, ..., 8Q
	// This allows lookup of -8Q, ..., -Q, 0, Q, ..., 8Q
	v.points[0].from_p3(q)
	mut tmp_p3 := Point{}
	mut tmp_p1 := ProjectiveP1{}
	for i := 0; i < 7; i++ {
		// Compute (i+1)*Q as Q + i*Q and convert to AffineCached
		v.points[i + 1].from_p3(tmp_p3.from_p1(tmp_p1.add_affine(q, v.points[i])))
	}
}

// Builds a lookup table at runtime. Fast.
fn (mut v NafLookupTable5) from_p3(q Point) {
	// Goal: v.points[i] = (2*i+1)*Q, i.e., Q, 3Q, 5Q, ..., 15Q
	// This allows lookup of -15Q, ..., -3Q, -Q, 0, Q, 3Q, ..., 15Q
	v.points[0].from_p3(q)
	mut q2 := Point{}
	q2.add(q, q)
	mut tmp_p3 := Point{}
	mut tmp_p1 := ProjectiveP1{}
	for i := 0; i < 7; i++ {
		v.points[i + 1].from_p3(tmp_p3.from_p1(tmp_p1.add(q2, v.points[i])))
	}
}

// This is not optimised for speed; fixed-base tables should be precomputed.
fn (mut v NafLookupTable8) from_p3(q Point) {
	v.points[0].from_p3(q)
	mut q2 := Point{}
	q2.add(q, q)
	mut tmp_p3 := Point{}
	mut tmp_p1 := ProjectiveP1{}
	for i := 0; i < 63; i++ {
		v.points[i + 1].from_p3(tmp_p3.from_p1(tmp_p1.add_affine(q2, v.points[i])))
	}
}

// Selectors.

// Set dest to x*Q, where -8 <= x <= 8, in constant time.
fn (mut v ProjLookupTable) select_into(mut dest ProjectiveCached, x i8) {
	// Compute xabs = |x|
	xmask := x >> 7
	xabs := u8((x + xmask) ^ xmask)

	dest.zero()
	for j := 1; j <= 8; j++ {
		// Set dest = j*Q if |x| = j
		cond := subtle.constant_time_byte_eq(xabs, u8(j))
		dest.selected(&v.points[j - 1], dest, cond)
	}
	// Now dest = |x|*Q, conditionally negate to get x*Q
	dest.cond_neg(int(xmask & 1))
}

// Set dest to x*Q, where -8 <= x <= 8, in constant time.
fn (mut v AffineLookupTable) select_into(mut dest AffineCached, x i8) {
	// Compute xabs = |x|
	xmask := x >> 7
	xabs := u8((x + xmask) ^ xmask)

	dest.zero()
	for j := 1; j <= 8; j++ {
		// Set dest = j*Q if |x| = j
		cond := subtle.constant_time_byte_eq(xabs, u8(j))
		dest.selected(v.points[j - 1], dest, cond)
	}
	// Now dest = |x|*Q, conditionally negate to get x*Q
	dest.cond_neg(int(xmask & 1))
}

// Given odd x with 0 < x < 2^4, return x*Q (in variable time).
fn (mut v NafLookupTable5) select_into(mut dest ProjectiveCached, x i8) {
	dest = v.points[x / 2]
}

// Given odd x with 0 < x < 2^7, return x*Q (in variable time).
fn (mut v NafLookupTable8) select_into(mut dest AffineCached, x i8) {
	dest = v.points[x / 2]
}
