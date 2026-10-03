module types

import os
import v.flat

fn test_static_c_declarations_are_not_receiver_methods() {
	root := os.join_path(os.vtmp_dir(), 'v3_static_c_receivers_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'bridge'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'static_c_receivers' }\n")!
	os.write_file(os.join_path(root, 'bridge', 'counter.c.v'), 'module bridge
pub struct C.Counter { value int }
pub fn make() C.Counter { return C.Counter{} }
pub fn C.Counter.read(c C.Counter) int
')!
	for imported in [false, true] {
		declarations := if imported {
			'import bridge\n'
		} else {
			'struct C.Counter { value int }\nfn C.Counter.read(c C.Counter) int\n'
		}
		initializer := if imported { 'bridge.make()' } else { 'C.Counter{}' }
		os.write_file(os.join_path(root, 'main.c.v'), 'module main\n${declarations}
fn main() { value := ${initializer}; println(C.Counter.read(value)) }
')!
		for flags in ['', '-no-parallel'] {
			result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
			assert result.exit_code == 0, result.output
		}
		for body in ['println(value.read())', 'callback := value.read; println(callback())'] {
			os.write_file(os.join_path(root, 'main.c.v'), 'module main\n${declarations}
fn main() { value := ${initializer}; ${body} }
')!
			for flags in ['', '-no-parallel'] {
				result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check',
					root])
				assert result.exit_code != 0, '${imported}: ${body}: ${result.output}'
				assert result.output.contains('unknown function')
					|| result.output.contains('unknown method')
					|| result.output.contains('unknown field'), result.output
			}
		}
	}
}

fn test_static_c_declarations_do_not_hide_actual_receiver_methods() {
	root := os.join_path(os.vtmp_dir(), 'v3_static_c_receiver_choices_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'bridge'))!
	os.mkdir_all(os.join_path(root, 'actual'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'static_c_receiver_choices' }\n")!
	os.write_file(os.join_path(root, 'bridge', 'counter.c.v'), 'module bridge
pub struct C.Counter { value int }
pub fn make() C.Counter { return C.Counter{} }
pub fn C.Counter.read(c C.Counter) int
')!
	os.write_file(os.join_path(root, 'actual', 'counter.c.v'), 'module actual
pub struct C.Counter { value int }
pub fn (c C.Counter) read() int { return 7 }
pub fn used() {}
')!
	for local in [false, true] {
		declarations := if local {
			'fn (c C.Counter) read() int { return 8 }'
		} else {
			'import actual'
		}
		use_import := if local { '' } else { 'actual.used();' }
		os.write_file(os.join_path(root, 'main.c.v'), 'module main
import bridge
${declarations}
fn main() {
 ${use_import}
 value := bridge.make()
 println(value.read())
 callback := value.read
 println(callback())
}
')!
		for flags in ['', '-no-parallel'] {
			result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
			assert result.exit_code == 0, result.output
		}
	}
}

fn test_static_c_receiver_metadata_keeps_canonical_declaration_in_both_orders() {
	for imported_first in [false, true] {
		mut a := flat.FlatAst.new()
		mut declarations := []int{}
		modules := if imported_first { ['bridge', 'main'] } else { ['main', 'bridge'] }
		for module_name in modules {
			declarations << int(a.add_val(.module_decl, module_name))
			mut param := flat.Node{
				kind:  .param
				value: 'c'
				typ:   'C.Counter'
			}
			if module_name == 'main' {
				param.op = .dot
			}
			param_id := a.add_node(param)
			start := a.children.len
			a.children << param_id
			declarations << int(a.add_node(flat.Node{
				kind:           if module_name == 'main' { .fn_decl } else { .c_fn_decl }
				value:          if module_name == 'main' {
					'C.Counter.read'
				} else {
					'Counter.read'
				}
				typ:            'int'
				children_start: start
				children_count: 1
			}))
		}
		mut tc := TypeChecker.new(&a)
		tc.top_level_idx = declarations
		tc.build_fn_name_indexes(&a)
		assert tc.fn_key_is_static_associated('bridge.C.Counter.read')
		assert !tc.fn_key_is_static_associated('C.Counter.read')
		assert !tc.method_can_be_called_on_receiver(Type(Struct{ name: 'C.Counter' }),
			'read', 'bridge.C.Counter.read')
		assert tc.method_can_be_called_on_receiver(Type(Struct{ name: 'C.Counter' }), 'read',
			'C.Counter.read')
	}
}
