#include "@VMODROOT/iterator.h"

struct C.MyCStruct {
mut:
	x int
}

fn (mut self C.MyCStruct) next() ?int {
	if self.x >= 10 {
		return none
	}
	self.x++
	return self.x
}

fn test_iterating_over_cstructs() {
	iter := C.MyCStruct{}
	for x in iter {
		println(x)
		assert true
	}
}
