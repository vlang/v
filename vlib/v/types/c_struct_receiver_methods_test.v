module types

import os

fn test_c_receiver_method_lookup_keeps_module_visibility_and_ambiguity_checks() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_receiver_methods_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'left'))!
	os.mkdir_all(os.join_path(root, 'right'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'c_receivers' }\n")!
	os.write_file(os.join_path(root, 'left', 'left.c.v'), 'module left
pub struct C.Counter { value int }
pub struct Holder { pub: value C.Counter }
pub fn make_holder() Holder { return Holder{} }
pub fn (c C.Counter) read() int { return 1 }
fn (c C.Counter) private_read() int { return 2 }
')!
	os.write_file(os.join_path(root, 'main.v'), 'module main
import left
fn main() { println(left.make_holder().value.private_read()) }
')!
	private_result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(root)}')
	assert private_result.exit_code != 0, private_result.output
	assert private_result.output.contains('is private'), private_result.output
	os.write_file(os.join_path(root, 'right', 'right.c.v'), 'module right
pub struct C.Counter { value int }
pub fn (c C.Counter) read() int { return 3 }
pub fn used() {}
')!
	os.write_file(os.join_path(root, 'main.v'), 'module main
import left
import right
fn main() { right.used(); println(left.make_holder().value.read()) }
')!
	ambiguous := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(root)}')
	assert ambiguous.exit_code != 0, ambiguous.output
	assert ambiguous.output.contains('unknown function') || ambiguous.output.contains('unknown method'), ambiguous.output
}
