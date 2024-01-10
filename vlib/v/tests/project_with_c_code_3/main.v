module main

import v.tests.project_with_c_code_3.mod1

fn main() {
	res := mod1.vadd(1, 2)
	println('mod1.vadd(1, 2): ' + res.str())
	$if js {
		assert res == 2003
	} $else {
		assert res == 1003
	}
	println('mod1.a_common_pure_v_fn(): ' + mod1.a_common_pure_v_fn().str())
	assert mod1.a_common_pure_v_fn() == 987654
}
