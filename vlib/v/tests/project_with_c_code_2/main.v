module main

import v.tests.project_with_c_code_2.modc

// passing array of Vtype to C
// Vtype wraps a C type
fn main() {
	a := [1, 2, 3, 4]
	mut vals := []modc.Vtype{}
	for v in a {
		vals << modc.new_vtype(v)
	}
	modc.call_with_array_param(vals)
}
