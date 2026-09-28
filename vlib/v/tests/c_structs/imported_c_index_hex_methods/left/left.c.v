module left

#include "@VMODROOT/counter.h"
pub struct C.Counter {
mut:
	value int
}

pub struct C.GenericCounter {
	value int
}

// make returns the C value used by the imported overloads.
pub fn make() C.Counter { return C.Counter{ value: 17 } }

// make_generic_counter returns the generic method receiver.
pub fn make_generic_counter() C.GenericCounter { return C.GenericCounter{ value: 19 } }

// hex identifies which receiver declaration was selected.
pub fn (c C.Counter) hex() string { return 'value' }

// hex identifies which receiver declaration was selected.
pub fn (c C.GenericCounter) hex[T](marker T) string { return 'generic value' }

// [] reads the counter with an index offset.
pub fn (c C.Counter) [] (index int) int {
	return c.value + index
}

// []=  supports this receiver lookup fixture.
pub fn (mut c C.Counter) []= (index int, value int) {
	c.value = value - index
}
