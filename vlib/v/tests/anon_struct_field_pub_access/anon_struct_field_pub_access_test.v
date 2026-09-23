module main

import owner
import reader

// The anonymous structs are accessed by the `reader` module, since visibility is
// not enforced inside `_test.v` files. The `v.mod` makes `reader` project code,
// whose diagnostics are reported like those of the test file itself.
fn test_pub_fields_of_anon_struct_fields_are_accessible_from_another_module() {
	mut o := owner.Outer{}
	value0, x0, name0 := reader.read(o)
	assert value0 == 0
	assert x0 == 0
	assert name0 == 'outer'
	reader.bump(mut o)
	reader.bump(mut o)
	value, x, name := reader.read(o)
	assert value == 2
	assert x == 20
	assert name == 'outer'
	made := reader.make(7)
	made_value, made_x, _ := reader.read(made)
	assert made_value == 7
	assert made_x == 0
}
