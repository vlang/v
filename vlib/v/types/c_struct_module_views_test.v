module types

import os
import v.parser
import v.pref

fn test_c_struct_visibility_uses_the_current_modules_own_view() {
	root := os.join_path(os.vtmp_dir(), 'c_struct_visibility_views_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	first := os.join_path(root, 'first.c.v')
	second := os.join_path(root, 'second.c.v')
	os.write_file(first, 'module first\npub struct C.ViewPoint { x int }\n')!
	os.write_file(second, 'module second\nstruct C.ViewPoint { y int }\nstruct C.PrivatePoint { z int }\n')!
	for paths in [[first, second], [second, first]] {
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_files(paths)
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		for module_name in ['first', 'second'] {
			tc.cur_file = if module_name == 'first' { first } else { second }
			tc.cur_module = module_name
			assert tc.private_declaration('C.ViewPoint') == none
		}
		tc.cur_file = os.join_path(root, 'third.v')
		tc.cur_module = 'third'
		assert tc.private_declaration('C.PrivatePoint') != none
	}
}

// Modules can mirror one C struct with different fields (`C.pthread_mutex_t` in `sync`
// and in a module translated from C). A module's own code can use the fields it declares,
// with the types it declares them with, also when another module's public declaration is
// the canonical one.
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
pub fn x_value() u32 {
	return inner.x_of_new_point(5)
}
')!
	os.write_file(os.join_path(root, 'outer', 'outer_test.v'), 'module outer
import os
fn test_value() {
	assert os.real_path(os.getenv("VMODULES")) == os.real_path(os.dir(@DIR))
	assert value() == 7
	assert x_value() == 6
}
')!
	os.write_file(os.join_path(inner, 'point.c.v'), '@[translated]
module inner
#include "${os.join_path(root, 'point.h')}"
struct C.c_view_point {
pub mut:
	x u32
	z i32
}
')!
	os.write_file(os.join_path(inner, 'inner_x.v'), 'module inner
pub fn x_of_new_point(x u32) u32 {
	mut point := C.c_view_point{
		x: x
	}
	point.x++
	return read_u32(&point.x)
}
fn read_u32(value &u32) u32 {
	return *value
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
	// Keep synthetic module paths in the child environment.
	parent_vmodules := os.getenv_opt('VMODULES')
	mut process := os.new_process(@VEXE)
	defer { process.close() }
	process.set_args(['-new-compiler', '-no-retry-compilation', 'test',
		os.join_path(root, 'outer', 'outer_test.v')])
	mut environment := os.environ()
	environment['VMODULES'] = root
	process.set_environment(environment)
	process.set_redirect_stdio_merged()
	process.run()
	output := process.stdout_slurp()
	process.wait()
	assert process.code == 0, '${process.err}\n${output}'
	assert os.getenv_opt('VMODULES') == parent_vmodules
}
