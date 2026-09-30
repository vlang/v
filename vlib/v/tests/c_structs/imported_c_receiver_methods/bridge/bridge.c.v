module bridge

#include "@VMODROOT/bridge/counter.h"
pub struct C.Counter {
	value int
}

pub type Counter = C.Counter

pub struct Holder {
pub:
	value C.Counter
}

// make_holder returns a V wrapper with a C value.
pub fn make_holder() Holder {
	return Holder{ value: C.Counter{ value: 17 } }
}

// read returns the wrapped value.
pub fn (c C.Counter) read() int { return c.value }

// read distinguishes alias receivers from their C backing type.
pub fn (c Counter) read() int { return c.value + 100 }

// convert demonstrates explicit generic calls on an imported C receiver.
pub fn (c C.Counter) convert[T](marker T) int { return c.value }

// convert distinguishes explicit generic alias methods from C receiver methods.
pub fn (c Counter) convert[T](marker T) int { return c.value + 200 }

// alias_only exercises a generic method declared only on the alias.
pub fn (c Counter) alias_only[T](marker T) int { return c.value + 300 }

// next returns the same C value through an option.
pub fn (c C.Counter) next() ?C.Counter { return c }

// read_again calls methods on a local copy of the C value.
pub fn (c C.Counter) read_again() int {
	mut value := c
	value = value.next() or { return 0 }
	return value.read()
}

// @union exercises an escaped method on an imported C receiver.
pub fn (c C.Counter) @union(marker int) int { return c.value + marker }

// @select exercises explicit generic arguments on an escaped C receiver method.
pub fn (c C.Counter) @select[T](marker T) int { return c.value }

// @union distinguishes an escaped alias method from its C backing type.
pub fn (c Counter) @union(marker int) int { return c.value + marker + 100 }

// increment mutates the receiver through a bound method value.
pub fn (mut c C.Counter) increment() { c.value++ }

// update mutates the original C receiver by the supplied amount.
pub fn (mut c C.Counter) update(amount int) { c.value += amount }

// same_address checks that a reference receiver retains the original storage.
pub fn (c &C.Counter) same_address(other &C.Counter) bool {
	return voidptr(c) == voidptr(other)
}

// same_generic_address checks that generic reference receivers retain their storage.
pub fn (c &C.Counter) same_generic_address[T](other &C.Counter, marker T) bool {
	return voidptr(c) == voidptr(other)
}

pub struct C.CountingIterator {
mut:
	current int
	end     int
}

// make_iterator constructs an iterator whose protocol method is an imported extension.
pub fn make_iterator(end int) C.CountingIterator {
	return C.CountingIterator{ end: end }
}

// next advances the imported C iterator.
pub fn (mut c C.CountingIterator) next() ?int {
	if c.current >= c.end { return none }
	c.current++
	return c.current
}

// hex formats a counter value, leaving pointer hex formatting unchanged.
pub fn (c C.Counter) hex() string { return 'counter' }

pub struct C.PointerCounter {
	value int
}

// make_pointer_counter returns a value with an explicit pointer receiver method.
pub fn make_pointer_counter() C.PointerCounter { return C.PointerCounter{} }

// hex demonstrates that an imported pointer receiver remains eligible.
pub fn (c &C.PointerCounter) hex() string { return 'pointer counter' }
