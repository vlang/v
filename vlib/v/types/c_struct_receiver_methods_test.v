module types

import os
import v.flat

fn test_imported_value_hex_does_not_accept_pointer_receivers() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_hex_receiver_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'bridge'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'c_hex_receiver' }\n")!
	os.write_file(os.join_path(root, 'bridge', 'counter.c.v'), 'module bridge
pub struct C.Counter { value int }
pub fn make() C.Counter { return C.Counter{} }
pub fn (c C.Counter) hex() string { return "value" }
')!
	os.write_file(os.join_path(root, 'main.v'), 'module main
import bridge
fn main() { value := bridge.make(); ptr := &value; println(ptr.hex()) }
')!
	result := os.exec([@VEXE, '-check', root])
	assert result.exit_code != 0, result.output
}

fn test_imported_c_iterator_extension_visibility() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_iterator_visibility_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'provider'))!
	os.mkdir_all(os.join_path(root, 'extensions'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'c_iterator_visibility' }\n")!
	os.write_file(os.join_path(root, 'provider', 'counter.c.v'), 'module provider
pub struct C.Iterator { current int }
pub fn make() C.Iterator { return C.Iterator{} }
')!
	os.write_file(os.join_path(root, 'main.v'), 'module main
import provider
import extensions as ext
fn main() { iter := provider.make(); for item in iter { println(item) } }
')!
	for visibility in ['pub ', ''] {
		os.write_file(os.join_path(root, 'extensions', 'counter.c.v'), 'module extensions
pub struct C.Iterator { current int }
${visibility}fn (mut c C.Iterator) next() ?int {
 if c.current > 0 { return none }
 c.current++
 return c.current
}
')!
		for flags in ['-W', '-W -no-parallel'] {
			result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
			if visibility.len > 0 {
				assert result.exit_code == 0, result.output
			} else {
				assert result.exit_code != 0, result.output
			}
		}
	}
}

fn test_c_receiver_extension_only_imports_are_used() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_extension_import_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'provider'))!
	os.mkdir_all(os.join_path(root, 'extensions'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'c_extension_import' }\n")!
	os.write_file(os.join_path(root, 'provider', 'counter.c.v'), 'module provider
pub struct C.Counter { value int }
pub struct Holder { pub: value C.Counter }
pub fn make_holder() Holder { return Holder{} }
')!
	os.write_file(os.join_path(root, 'extensions', 'counter.c.v'), 'module extensions
pub struct C.Counter { value int }
pub fn (c C.Counter) read() int { return c.value }
pub fn (c C.Counter) @select[T](marker T) int { return c.value }
')!
	path := os.join_path(root, 'main.v')
	for body in [
		'println(provider.make_holder().value.read())',
		'println(provider.make_holder().value.@select[int](1))',
		'callback := provider.make_holder().value.read; println(callback())',
	] {
		os.write_file(path, 'module main\nimport provider\nimport extensions as ext\nfn main() { ${body} }\n')!
		for flags in ['-W', '-W -no-parallel', '-prod'] {
			result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check', root])
			assert result.exit_code == 0, result.output
		}
	}
	os.write_file(path, 'module main\nimport provider\nimport extensions as ext\nfn main() { _ = provider.make_holder() }\n')!
	unused := os.exec([@VEXE, '-W', '-check', root])
	assert unused.exit_code != 0, unused.output
	assert unused.output.contains('imported but never used'), unused.output
}

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

fn test_c_pointer_alias_inherits_methods_without_losing_pointer_eligibility() {
	mut tc := TypeChecker.new(&flat.FlatAst{})
	base := Type(Struct{ name: 'C.Counter' })
	pointer := Type(Pointer{ base_type: base })
	inner := Type(Alias{ name: 'CounterRef', base_type: pointer })
	outer := Type(Alias{ name: 'OuterRef', base_type: inner })
	tc.fn_ret_types['C.Counter.read'] = Type(int_)
	assert tc.c_struct_receiver_method_name(outer, 'read') or { '' } == 'C.Counter.read'
	tc.fn_ret_types['CounterRef.read'] = Type(int_)
	assert tc.c_struct_receiver_method_name(outer, 'read') or { '' } == 'CounterRef.read'
	tc.fn_ret_types['OuterRef.read'] = Type(int_)
	assert tc.c_struct_receiver_method_name(outer, 'read') or { '' } == 'OuterRef.read'
	tc.fn_ret_types['C.Counter.hex'] = Type(string_)
	tc.fn_param_types['C.Counter.hex'] = [base]
	assert tc.c_struct_receiver_method_name(outer, 'hex') == none
	tc.fn_param_types['C.Counter.hex'] = [pointer]
	assert tc.c_struct_receiver_method_name(outer, 'hex') or { '' } == 'C.Counter.hex'
}

fn test_c_receiver_method_lookup_keeps_module_visibility_and_ambiguity_checks() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_receiver_methods_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'left'))!
	os.mkdir_all(os.join_path(root, 'right'))!
	os.mkdir_all(os.join_path(root, 'facade'))!
	os.mkdir_all(os.join_path(root, 'foo'))!
	os.mkdir_all(os.join_path(root, 'bar', 'foo'))!
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
	private_result := os.exec([@VEXE, '-check', root])
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
	ambiguous := os.exec([@VEXE, '-check', root])
	assert ambiguous.exit_code != 0, ambiguous.output
	assert ambiguous.output.contains('ambiguous method'), ambiguous.output
	for public_module in ['left', 'right'] {
		for method_module in ['left', 'right'] {
			visibility := if method_module == public_module { 'pub ' } else { '' }
			os.write_file(os.join_path(root, method_module, 'choice.c.v'), 'module ${method_module}\n${visibility}fn (c C.Counter) choice() int { return 42 }\n')!
		}
		os.write_file(os.join_path(root, 'main.v'), 'module main\nimport left\nimport right\nfn main() { right.used(); println(left.make_holder().value.choice()) }\n')!
		public_choice := os.exec([@VEXE, '-check', root])
		assert public_choice.exit_code == 0, public_choice.output
	}
	os.write_file(os.join_path(root, 'facade', 'facade.v'), 'module facade\nimport right\npub fn used() { right.used() }\n')!
	os.write_file(os.join_path(root, 'main.v'), 'module main\nimport left\nimport facade\nfn main() { facade.used(); println(left.make_holder().value.read()) }\n')!
	visible := os.exec([@VEXE, '-check', root])
	assert visible.exit_code == 0, visible.output
	os.write_file(os.join_path(root, 'main.v'), 'module main\nimport left\nimport facade\nfn main() { facade.used(); println(left.make_holder().value.secret_read()) }\n')!
	hidden := os.exec([@VEXE, '-check', root])
	assert hidden.exit_code != 0, hidden.output
	assert hidden.output.contains('unknown function') || hidden.output.contains('unknown method'), hidden.output
	os.write_file(os.join_path(root, 'foo', 'foo.c.v'), 'module foo\npub struct C.Counter { value int }\npub fn (c C.Counter) hidden_by_path() int { return 5 }\npub fn used() {}\n')!
	os.write_file(os.join_path(root, 'bar', 'foo', 'foo.v'), 'module foo\npub fn used() {}\n')!
	os.write_file(os.join_path(root, 'facade', 'facade.v'), 'module facade\nimport foo\npub fn used() { foo.used() }\n')!
	os.write_file(os.join_path(root, 'main.v'), 'module main\nimport left\nimport facade\nimport bar.foo\nfn main() { facade.used(); foo.used(); println(left.make_holder().value.hidden_by_path()) }\n')!
	hidden_by_path := os.exec([@VEXE, '-check', root])
	assert hidden_by_path.exit_code != 0, hidden_by_path.output
	assert hidden_by_path.output.contains('unknown function') || hidden_by_path.output.contains('unknown method'), hidden_by_path.output
	for visibility in ['pub ', ''] {
		os.write_file(os.join_path(root, 'left', 'escaped.c.v'), 'module left\npub fn (c C.Counter) @union() int { return 7 }\n')!
		os.write_file(os.join_path(root, 'right', 'escaped.c.v'), 'module right\n${visibility}fn (c C.Counter) @union() int { return 8 }\n')!
		os.write_file(os.join_path(root, 'main.v'), 'module main\nimport left\nimport right\nfn main() { right.used(); println(left.make_holder().value.@union()) }\n')!
		escaped := os.exec([@VEXE, '-check', root])
		if visibility.len > 0 {
			assert escaped.exit_code != 0, escaped.output
			assert escaped.output.contains('ambiguous method'), escaped.output
		} else {
			assert escaped.exit_code == 0, escaped.output
		}
	}
}

fn test_static_interop_generic_is_not_a_receiver_method() {
	path := os.join_path(os.vtmp_dir(), 'v3_static_interop_generic_${os.getpid()}.v')
	os.write_file(path, 'struct JS.DOMQuad {}\nfn JS.DOMQuad.fromQuad[T](other JS.DOMQuad) T\nfn main() {}\n')!
	defer { os.rm(path) or {} }
	result := os.exec([@VEXE, '-check', path])
	assert result.exit_code != 0, result.output
	assert result.output.contains('JS functions cannot be declared as generic'), result.output
}

fn test_c_receiver_method_ambiguity_precedes_implicit_fallbacks() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_receiver_fallbacks_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'left'))!
	os.mkdir_all(os.join_path(root, 'right'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'c_receiver_fallbacks' }\n")!
	for mod in ['left', 'right'] {
		os.write_file(os.join_path(root, mod, 'counter.c.v'), 'module ${mod}
pub struct C.Counter { value int }
pub struct Holder { pub: value C.Counter }
pub fn make_holder() Holder { return Holder{} }
pub fn (c C.Counter) str() string { return "extension" }
pub fn (c C.Counter) clone() C.Counter { return c }
pub fn (c C.Counter) free() {}
')!
	}
	for method in ['str', 'clone', 'free'] {
		os.write_file(os.join_path(root, 'main.v'), 'module main\nimport left\nimport right\nfn main() { _ := right.make_holder(); left.make_holder().value.${method}() }\n')!
		result := os.exec([@VEXE, '-check', root])
		assert result.exit_code != 0, '${method}: ${result.output}'
		assert result.output.contains('ambiguous method'), result.output
	}
}

fn test_imported_c_method_values_keep_visibility_and_receiver_safety() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_method_value_safety_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'bridge'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'c_method_value_safety' }\n")!
	os.write_file(os.join_path(root, 'bridge', 'bridge.c.v'), 'module bridge
pub struct C.Counter { value int }
pub struct Holder { pub: value C.Counter }
pub fn make_holder() Holder { return Holder{} }
fn (c C.Counter) private_read() int { return c.value }
pub fn (c &C.Counter) pointer_read() int { return c.value }
pub fn (mut c C.Counter) increment() { c.value++ }
pub fn (c C.Counter) generic_read[T](marker T) int { return c.value }
')!
	for source, expected in {
		'fn main() { cb := bridge.make_holder().value.private_read; println(cb()) }':                                          'is private'
		'fn main() { value := bridge.make_holder().value; cb := value.pointer_read; println(cb()) }':                          'cannot be used as a variable outside `unsafe`'
		'fn escaped() fn () { mut value := bridge.make_holder().value; return value.increment } fn main() { _ := escaped() }': 'mutable local receiver cannot escape'
		'fn main() { value := bridge.make_holder().value; cb := value.generic_read; println(cb(1)) }':                         'as a generic function value'
	} {
		os.write_file(os.join_path(root, 'main.v'), 'module main\nimport bridge\n${source}\n')!
		result := os.exec([@VEXE, '-check', root])
		assert result.exit_code != 0, result.output
		assert result.output.contains(expected), result.output
	}
}

fn test_imported_c_alias_private_method_values_are_reported_once() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_alias_private_method_value_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'bridge'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'c_alias_private_method_value' }\n")!
	os.write_file(os.join_path(root, 'bridge', 'bridge.c.v'), 'module bridge
pub struct C.Counter { value int }
pub type CounterAlias = C.Counter
pub fn make_alias() CounterAlias { return CounterAlias{} }
fn (c CounterAlias) private_read() int { return c.value }
')!
	os.write_file(os.join_path(root, 'main.v'), 'module main\nimport bridge\nfn main() { value := bridge.make_alias(); cb := value.private_read; println(cb()) }\n')!
	result := os.exec([@VEXE, '-check', root])
	assert result.exit_code != 0, result.output
	assert result.output.count('is private') == 1, result.output
}

fn test_imported_hex_filters_pointer_candidates_before_ambiguity() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_hex_candidates_${os.getpid()}')
	for name in ['left', 'right'] { os.mkdir_all(os.join_path(root, name))! }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'c_hex_candidates' }\n")!
	for generic in [false, true] {
		signature := if generic { '[T](marker T)' } else { '()' }
		os.write_file(os.join_path(root, 'left', 'counter.c.v'), 'module left
pub struct C.Counter { value int }
pub fn make() C.Counter { return C.Counter{} }
pub fn (c C.Counter) hex${signature} string { return "value" }
')!
		for pointer in [true, false] {
			receiver := if pointer { '&C.Counter' } else { 'C.Counter' }
			os.write_file(os.join_path(root, 'right', 'counter.c.v'), 'module right
pub struct C.Counter { value int }
pub fn used() {}
pub fn (c ${receiver}) hex${signature} string { return "pointer" }
')!
			bodies := if generic {
				['println(ptr.hex[int](1))']
			} else {
				['println(ptr.hex())', 'unsafe { callback := ptr.hex; println(callback()) }']
			}
			for body in bodies {
				os.write_file(os.join_path(root, 'main.v'), 'module main\nimport left\nimport right\nfn main() { right.used(); value := left.make(); ptr := &value; ${body} }\n')!
				for flags in ['', '-no-parallel'] {
					result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check',
						root])
					if pointer {
						assert result.exit_code == 0, result.output
					} else {
						assert result.exit_code != 0, result.output
					}
				}
			}
		}
	}
}

fn test_imported_c_index_operators_keep_visibility_and_import_usage() {
	root := os.join_path(os.vtmp_dir(), 'v3_c_index_operators_${os.getpid()}')
	for name in ['provider', 'left', 'right'] { os.mkdir_all(os.join_path(root, name))! }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'c_index_operators' }\n")!
	os.write_file(os.join_path(root, 'provider', 'buffer.c.v'), 'module provider
pub struct C.Buffer { mut: value int }
pub fn make() C.Buffer { return C.Buffer{} }
')!
	for visibility in ['pub ', ''] {
		os.write_file(os.join_path(root, 'left', 'buffer.c.v'), 'module left
pub struct C.Buffer { mut: value int }
${visibility}fn (b C.Buffer) [](index int) int { return b.value + index }
${visibility}fn (mut b C.Buffer) []= (index int, value int) { b.value = value - index }
')!
		for body in ['println(value[0])', 'value[0] = 7', 'value[0] += 3'] {
			os.write_file(os.join_path(root, 'main.v'), 'module main\nimport provider\nimport left as extension\nfn main() { mut value := provider.make(); ${body} }\n')!
			for flags in ['-W', '-W -no-parallel'] {
				result := os.exec([@VEXE, ...(os.split_args(flags) or { panic(err) }), '-check',
					root])
				if visibility.len > 0 {
					assert result.exit_code == 0, result.output
				} else {
					assert result.exit_code != 0, result.output
				}
			}
		}
	}
	// A compound update can select its getter and setter from different imports.
	os.write_file(os.join_path(root, 'left', 'buffer.c.v'), 'module left
pub struct C.Buffer { mut: value int }
pub fn (b C.Buffer) [](index int) int { return b.value + index }
')!
	for visibility in ['pub ', ''] {
		os.write_file(os.join_path(root, 'right', 'buffer.c.v'), 'module right
pub struct C.Buffer { mut: value int }
${visibility}fn (mut b C.Buffer) []= (index int, value int) { b.value = value - index }
')!
		os.write_file(os.join_path(root, 'main.v'), 'module main\nimport provider\nimport left\nimport right\nfn main() { mut value := provider.make(); value[0] += 3 }\n')!
		result := os.exec([@VEXE, '-W', '-check', root])
		if visibility.len > 0 {
			assert result.exit_code == 0, result.output
		} else {
			assert result.exit_code != 0, result.output
		}
	}
	for name in ['left', 'right'] {
		os.write_file(os.join_path(root, name, 'buffer.c.v'), 'module ${name}
pub struct C.Buffer { mut: value int }
pub fn used() {}
pub fn (b C.Buffer) [](index int) int { return b.value + index }
pub fn (mut b C.Buffer) []= (index int, value int) { b.value = value - index }
')!
	}
	for body in ['println(value[0])', 'value[0] = 7', 'value[0] += 3'] {
		os.write_file(os.join_path(root, 'main.v'), 'module main\nimport provider\nimport left\nimport right\nfn main() { left.used(); right.used(); mut value := provider.make(); ${body} }\n')!
		result := os.exec([@VEXE, '-check', root])
		assert result.exit_code != 0, result.output
	}
}

fn test_local_c_hex_pointer_fallbacks_keep_receiver_eligibility() {
	path := os.join_path(os.vtmp_dir(), 'v3_local_c_hex_${os.getpid()}.c.v')
	defer { os.rm(path) or {} }
	for pointer in [true, false] {
		receiver := if pointer { '&C.Counter' } else { 'C.Counter' }
		for generic in [true, false] {
			signature := if generic { '[T](marker T)' } else { '()' }
			body := if generic {
				'println(ptr.hex[int](1))'
			} else {
				'unsafe { callback := ptr.hex; println(callback()) }'
			}
			os.write_file(path, 'struct C.Counter { value int }\nfn (c ${receiver}) hex${signature} string { return "counter" }\nfn main() { value := C.Counter{}; ptr := &value; ${body} }\n')!
			result := os.exec([@VEXE, '-check', path])
			if pointer {
				assert result.exit_code == 0, result.output
			} else {
				assert result.exit_code != 0, result.output
			}
		}
	}
}
