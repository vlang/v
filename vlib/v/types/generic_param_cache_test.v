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
