module types

import os

// Modules can mirror one C struct with different fields (`C.pthread_mutex_t` in `sync`
// and in a module translated from C). A module's own code can use the fields it declares,
// also when another module's public declaration is the canonical one.
fn test_module_uses_the_fields_of_its_own_c_struct_declaration() {
	root := os.join_path(os.vtmp_dir(), 'c_struct_module_views_${os.getpid()}')
	inner := os.join_path(root, 'outer', 'inner')
	os.mkdir_all(inner)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'point.h'), 'struct c_view_point { int x; int y; int z; };\n')!
	os.mkdir_all(os.join_path(root, 'mirror'))!
	os.write_file(os.join_path(root, 'mirror', 'mirror.v'), 'module mirror
#include "${os.join_path(root, 'point.h')}"
pub struct C.c_view_point {
pub mut:
	x i32
	y i32
}
pub fn sum(point &C.c_view_point) i32 {
	return point.x + point.y
}
')!
	os.write_file(os.join_path(root, 'outer', 'outer.v'), 'module outer
import mirror
import outer.inner
pub fn value() i32 {
	_ = mirror.sum
	return inner.z_of_new_point(6)
}
')!
	os.write_file(os.join_path(root, 'outer', 'outer_test.v'), 'module outer
fn test_value() {
	assert value() == 7
}
')!
	os.write_file(os.join_path(inner, 'point.c.v'), '@[translated]
module inner
#include "${os.join_path(root, 'point.h')}"
struct C.c_view_point {
pub mut:
	z i32
}
')!
	os.write_file(os.join_path(inner, 'inner.v'), '@[translated]
module inner
pub fn z_of_new_point(z i32) i32 {
	mut point := C.c_view_point{
		z: 1
	}
	point.z += z
	return point.z
}
')!
	result := os.execute('VMODULES=${os.quoted_path(root)} ${os.quoted_path(@VEXE)} -new-compiler test ${os.quoted_path(os.join_path(root, 'outer', 'outer_test.v'))}')
	assert result.exit_code == 0, result.output
}
