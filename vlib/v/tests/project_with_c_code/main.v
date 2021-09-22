module main

import v.tests.project_with_c_code.mod1

fn main() {
	res := mod1.vadd(1, 2)
	println(res)
	assert res == 1003
}
