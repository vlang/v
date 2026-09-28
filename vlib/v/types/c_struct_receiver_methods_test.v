module types

import os
import v.flat

fn test_c_backed_alias_inherits_nearest_alias_method() {
	mut tc := TypeChecker.new(&flat.FlatAst{})
	base := Type(Struct{ name: 'C.Counter' })
	inner := Type(Alias{ name: 'Base', base_type: base })
	middle := Type(Alias{ name: 'Middle', base_type: inner })
	outer := Type(Alias{ name: 'Wrapped', base_type: middle })
	tc.fn_ret_types['C.Counter.read'] = Type(int_)
	tc.fn_ret_types['Base.read'] = Type(int_)
	tc.fn_ret_types['Middle.read'] = Type(int_)
	assert tc.c_struct_receiver_method_name(outer, 'read') or { '' } == 'Middle.read'
	tc.fn_ret_types.delete('Middle.read')
	assert tc.c_struct_receiver_method_name(outer, 'read') or { '' } == 'Base.read'
	tc.fn_ret_types['Wrapped.read'] = Type(int_)
	assert tc.c_struct_receiver_method_name(outer, 'read') or { '' } == 'Wrapped.read'
	assert tc.c_struct_receiver_method_name(Type(Pointer{ base_type: outer }), 'read') or { '' } == 'Wrapped.read'
	tc.fn_ret_types.delete('Wrapped.read')
	tc.fn_ret_types.delete('Base.read')
	assert tc.c_struct_receiver_method_name(outer, 'read') or { '' } == 'C.Counter.read'
}

fn test_c_receiver_method_lookup_keeps_module_visibility_and_ambiguity_checks() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_receiver_methods_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'left'))!
	os.mkdir_all(os.join_path(root, 'right'))!
	os.mkdir_all(os.join_path(root, 'facade'))!
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
pub fn (c C.Counter) secret_read() int { return 4 }
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
	for public_module in ['left', 'right'] {
		for method_module in ['left', 'right'] {
			visibility := if method_module == public_module { 'pub ' } else { '' }
			os.write_file(os.join_path(root, method_module, 'choice.c.v'), 'module ${method_module}\n${visibility}fn (c C.Counter) choice() int { return 42 }\n')!
		}
		os.write_file(os.join_path(root, 'main.v'), 'module main\nimport left\nimport right\nfn main() { right.used(); println(left.make_holder().value.choice()) }\n')!
		public_choice := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(root)}')
		assert public_choice.exit_code == 0, public_choice.output
	}
	os.write_file(os.join_path(root, 'facade', 'facade.v'), 'module facade\nimport right\npub fn used() { right.used() }\n')!
	os.write_file(os.join_path(root, 'main.v'), 'module main\nimport left\nimport facade\nfn main() { facade.used(); println(left.make_holder().value.read()) }\n')!
	visible := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(root)}')
	assert visible.exit_code == 0, visible.output
	os.write_file(os.join_path(root, 'main.v'), 'module main\nimport left\nimport facade\nfn main() { facade.used(); println(left.make_holder().value.secret_read()) }\n')!
	hidden := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(root)}')
	assert hidden.exit_code != 0, hidden.output
	assert hidden.output.contains('unknown function') || hidden.output.contains('unknown method'), hidden.output
}

fn test_static_interop_generic_is_not_a_receiver_method() {
	path := os.join_path(os.vtmp_dir(), 'v3_static_interop_generic_${os.getpid()}.v')
	os.write_file(path, 'struct JS.DOMQuad {}\nfn JS.DOMQuad.fromQuad[T](other JS.DOMQuad) T\nfn main() {}\n')!
	defer { os.rm(path) or {} }
	result := os.execute('${os.quoted_path(@VEXE)} -check ${os.quoted_path(path)}')
	assert result.exit_code != 0, result.output
	assert result.output.contains('JS functions cannot be declared as generic'), result.output
}
