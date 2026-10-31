module right

#include "@VMODROOT/counter.h"
pub struct C.Counter {
	value int
}

pub struct C.GenericCounter {
	value int
}

// used keeps the fixture import explicit.
pub fn used() {}

// hex identifies which receiver declaration was selected.
pub fn (c &C.Counter) hex() string { return 'pointer' }

// hex identifies which receiver declaration was selected.
pub fn (c &C.GenericCounter) hex[T](marker T) string { return 'generic pointer' }
