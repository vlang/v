module candidates27465

import bitset27465 as bs

pub type Candidates = bs.BitSet[u32]

// set shadows the generic `BitSet[T].set`. Every call to it - both the in-module
// call below and the cross-module call from the test - must resolve to this
// non-generic method (`candidates27465__Candidates_set`), not the generic parent.
pub fn (mut c Candidates) set(digits ...int) {
	for d in digits {
		c.bits |= u32(1) << u32(d)
	}
}

// make exercises an in-module call to the alias method.
pub fn make() Candidates {
	mut c := Candidates{}
	c.set(0, 1, 2)
	return c
}

// full exercises the inherited generic method on the alias type.
pub fn full() Candidates {
	mut c := Candidates{}
	c.set_all()
	return c
}
