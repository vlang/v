module main

import v.tests.project_with_c_code.mod1

#include "@DIR/relative.h"

fn C.abc() int

fn main() {
	res := mod1.vadd(1, 2)
	println(res)
	assert res == 1003
	res2 := C.abc()
	assert res2 == 142
	dump(res2)
}
