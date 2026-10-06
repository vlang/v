module types

import os
import v.flat
import v.parser
import v.pref

fn generic_param_cache_fixture() !&flat.FlatAst {
	path := os.join_path(os.vtmp_dir(), 'v3_generic_param_cache_${os.getpid()}.v')
	os.write_file(path, 'struct Box[T] { value T }
fn plain(value int) int { return value }
fn identity[U](value U) U { return value }
fn (box Box[T]) get() T { return box.value }
fn main() {
	_ := plain(1)
	_ := identity(2)
}
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	return a
}

fn test_generic_param_cache_keeps_empty_and_inferred_parameters() {
	a := generic_param_cache_fixture()!
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	plain_id := tc.fn_decl_short_name_ids['plain'] or { panic('missing plain declaration') }
	identity_id := tc.fn_decl_short_name_ids['identity'] or { panic('missing identity declaration') }
	get_id := tc.fn_decl_short_name_ids['get'] or { panic('missing get declaration') }
	assert plain_id in tc.enclosing_generic_params_by_node
	assert tc.enclosing_generic_params_by_node[plain_id] == []string{}
	assert 'plain' !in tc.fn_generic_params
	assert tc.enclosing_generic_params_by_node[identity_id] == ['U']
	// The method's generic parameter comes from its receiver rather than a `[T]` fn list.
	assert a.nodes[get_id].generic_params().len == 0
	assert tc.enclosing_generic_params_by_node[get_id] == ['T']
	assert !tc.node_has_enclosing_generic_param(flat.NodeId(plain_id), 'T')
	assert tc.node_has_enclosing_generic_param(flat.NodeId(get_id), 'T')
}

fn test_library_reachability_keeps_inferred_generic_receiver_roots() {
	a := generic_param_cache_fixture()!
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	file := a.nodes[a.file_node_ids[0]].value
	get_id := tc.fn_decl_short_name_ids['get'] or { panic('missing get declaration') }
	get := a.nodes[get_id]
	// No call reaches this method, but its receiver's generic body must still be checked.
	assert get.generic_params().len == 0
	tc.skip_unreachable_library_bodies({
		file: true
	}, []string{}, true)
	assert tc.reachable_library_fns[get.value]
	tc.cur_file = file
	assert !tc.skips_library_body(get)
}

fn test_library_reachability_keeps_top_level_compile_messages() {
	path := os.join_path(os.vtmp_dir(), 'v3_library_compile_messages_${os.getpid()}.v')
	os.write_file(path, 'module dependency
\$compile_error("library error")
\$compile_warn("library warning")
fn unused() {
	\$compile_error("unreachable error")
}
')!
	defer { os.rm(path) or {} }
	for follow_names in [false, true] {
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		tc.diagnostic_files[path] = true
		tc.skip_unreachable_library_bodies({
			path: true
		}, []string{}, follow_names)
		tc.check_semantics()
		assert tc.errors.len == 1, tc.errors.str()
		assert tc.errors[0].msg == 'library error', tc.errors.str()
		assert tc.errors[0].kind == .compile_error
		assert tc.notices.any(it.msg == 'library warning' && it.severity == 'warning:'), tc.notices.str()
	}
}

fn test_library_reachability_follows_references_without_literal_or_declaration_names() {
	root := os.join_path(os.vtmp_dir(), 'v3_library_reference_names_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	main_path := os.join_path(root, 'main.v')
	library_path := os.join_path(root, 'dependency.v')
	os.write_file(main_path, 'module main
import dependency { value_target, CastTarget }
struct Record { field_name int }
fn accept(parameter_name int) {}
fn main() {
	_ := "literal_name"
	_ := Record{field_name: 1}
	callback := value_target
	callback()
	item := dependency.Item{}
	_ := item.method_target
	dependency.direct_target()
	_ := CastTarget(1)
}
')!
	os.write_file(library_path, 'module dependency
pub struct Item {}
pub fn literal_name() {}
pub fn parameter_name() {}
pub fn field_name() {}
pub fn value_target() {}
pub fn direct_target() {}
pub fn (item Item) method_target() {}
pub fn CastTarget(value int) int { return value }
')!
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([main_path, library_path])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.skip_unreachable_library_bodies({
		library_path: true
	}, []string{}, true)
	for name in ['literal_name', 'parameter_name', 'field_name'] {
		assert !tc.reachable_library_fns['dependency.${name}'], name
	}
	for name in ['value_target', 'direct_target', 'Item.method_target', 'CastTarget'] {
		assert tc.reachable_library_fns['dependency.${name}'], name
	}
}

fn test_generic_diagnostic_walks_share_instantiation_without_changing_notices() {
	path := os.join_path(os.vtmp_dir(), 'v3_generic_diagnostic_walks_${os.getpid()}.v')
	os.write_file(path, 'fn diagnostics[T](value T) T {
	\$if T is int {
		\$compile_error("integer specialization")
		\$compile_warn("integer warning")
	}
	return value
}
fn main() { _ := diagnostics(1) }
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut original := TypeChecker.new(a)
	original.collect(a)
	mut shared := TypeChecker.new(a)
	shared.collect(a)
	mut call_id := flat.empty_node
	for idx, node in a.nodes {
		if node.kind == .call && node.children_count > 0
			&& a.child_node(&node, 0).value == 'diagnostics' {
			call_id = flat.NodeId(idx)
			break
		}
	}
	assert call_id != flat.empty_node
	call := a.node(call_id)
	info := CallInfo{ name: 'diagnostics' }
	original.check_instantiated_generic_as_casts(info,
		original.generic_compile_error_instantiation(call, info) or { panic('missing generic instance') })
	original.check_instantiated_generic_noinit_structs(call_id, info,
		original.generic_compile_error_instantiation(call, info) or { panic('missing generic instance') })
	original.check_instantiated_generic_ordering_ops(call, info,
		original.generic_compile_error_instantiation(call, info) or { panic('missing generic instance') })
	original.check_instantiated_generic_compile_errors(call_id, call,
		original.generic_compile_error_instantiation(call, info) or { panic('missing generic instance') })
	original.check_instantiated_generic_compile_warnings(original.generic_compile_error_instantiation(call, info) or { panic('missing generic instance') })
	shared.check_instantiated_generic_diagnostics(call_id, call, info)
	assert original.errors.len == 1, original.errors.str()
	assert original.errors[0].msg == 'integer specialization'
	assert original.notices.any(it.msg == 'integer warning')
	assert shared.errors == original.errors
	assert shared.notices == original.notices
	assert shared.generic_decl_file == original.generic_decl_file
}

fn test_generic_instantiation_falls_back_for_uncached_declarations() {
	a := generic_param_cache_fixture()!
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	for name in ['plain', 'identity'] {
		decl_id := tc.fn_decl_short_name_ids[name] or { panic('missing ${name} declaration') }
		mut call_id := flat.empty_node
		for idx, node in a.nodes {
			if node.kind == .call && node.children_count > 0 {
				callee := a.child_node(&node, 0)
				if callee.kind == .ident && callee.value == name {
					call_id = flat.NodeId(idx)
					break
				}
			}
		}
		assert call_id != flat.empty_node
		call := a.node(call_id)
		info := CallInfo{ name: name }
		// Force both the node-indexed path and the fallback used by generated nodes.
		tc.fn_generic_params.delete(name)
		if name == 'plain' {
			assert tc.generic_compile_error_instantiation(call, info) == none
			tc.enclosing_generic_params_by_node.delete(decl_id)
			assert tc.generic_compile_error_instantiation(call, info) == none
			continue
		}
		cached := tc.generic_compile_error_instantiation(call, info) or {
			panic('missing cached generic instantiation')
		}
		assert cached.generic_params == ['U']
		assert cached.concrete_args == ['int']
		tc.enclosing_generic_params_by_node.delete(decl_id)
		fallback := tc.generic_compile_error_instantiation(call, info) or {
			panic('missing uncached generic instantiation')
		}
		assert fallback.decl_id == cached.decl_id
		assert fallback.generic_params == cached.generic_params
		assert fallback.concrete_args == cached.concrete_args
		assert fallback.symbol_types == cached.symbol_types
	}
}

fn reflected_generic_method_fixture(argument string) !&flat.FlatAst {
	path := os.join_path(os.vtmp_dir(), 'v3_reflected_generic_method_${os.getpid()}.v')
	os.write_file(path, 'module main
struct Context { mut: value int }
struct App {}
fn (_ App) index(mut ctx &Context) {}
fn call[A](app A, mut ptr &Context, mut val Context) {
	\$for method in A.methods {
		if method.name == "index" {
			app.\$method(mut ${argument})
		}
	}
}
fn main() {
	mut val := Context{}
	mut ptr := &val
	call(App{}, mut ptr, mut val)
}
')!
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	return a
}

fn test_recollect_checks_reflected_generic_arguments_again() {
	valid := reflected_generic_method_fixture('ptr')!
	invalid := reflected_generic_method_fixture('val')!
	mut tc := TypeChecker.new(valid)
	tc.collect(valid)
	call_id := tc.fn_decl_short_name_ids['call'] or { panic('missing call declaration') }
	tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()

	mut fresh := TypeChecker.new(invalid)
	fresh.collect(invalid)
	fresh.check_semantics_opt(false)
	assert fresh.errors.len == 1, fresh.errors.str()
	assert fresh.errors[0].kind == .call_arg_mismatch

	tc.collect(invalid)
	assert tc.fn_decl_short_name_ids['call'] == call_id
	tc.check_semantics_opt(false)
	assert tc.errors.len == 1, tc.errors.str()
	assert tc.errors[0].kind == .call_arg_mismatch
	assert tc.errors[0].msg == fresh.errors[0].msg
}

fn test_generic_method_metadata_routes_remain_independent() {
	a := flat.FlatAst.new()
	for route in 0 .. 3 {
		mut tc := TypeChecker.new(&a)
		name := if route == 1 { 'Alias' } else { 'Box' }
		key := if route == 2 { 'Box[u8].read' } else { '${name}.read' }
		tc.fn_ret_types[key] = tc.parse_type('u64')
		if route == 0 {
			tc.struct_generic_params['Box'] = ['T']
		} else if route == 1 {
			tc.type_alias_generic_params['Alias'] = ['T']
		} else {
			tc.register_generated_fn_param_types(key, []Type{})
		}
		info := tc.resolve_generic_struct_method('${name}[u8]', 'read') or { panic('missing route ${route}') }
		assert info.name == key
		assert info.return_type.name() == 'u64'
	}
}

fn test_plain_signatures_and_aliases_do_not_supply_generic_parameters() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_aliases['Concrete'] = 'Box'
	tc.fn_ret_types['Box.read'] = tc.parse_type('u64')
	tc.fn_param_types['Box.read'] = []Type{}
	assert tc.resolve_generic_struct_method('Box[u8]', 'read') == none
	assert tc.resolve_generic_struct_method('Concrete', 'read') == none
}
