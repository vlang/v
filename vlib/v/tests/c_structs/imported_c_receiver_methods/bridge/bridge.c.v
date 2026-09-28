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
