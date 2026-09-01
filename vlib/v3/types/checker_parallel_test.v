module types

import os
import strings
import v3.flat
import v3.parser
import v3.pref

fn test_fast_file_index_collects_translated_module_attribute() {
	old_no_file_idx := os.getenv_opt('V3_NO_FILE_IDX')
	os.unsetenv('V3_NO_FILE_IDX')
	defer {
		if value := old_no_file_idx {
			os.setenv('V3_NO_FILE_IDX', value, true)
		}
	}
	path := os.join_path(os.vtmp_dir(), 'v3_translated_file_index_${os.getpid()}.v')
	os.write_file(path, '@[translated]\nmodule main\n\nfn main() {}\n') or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	assert file_index_usable(a)

	mut tc := TypeChecker.new(a)
	tc.collect(a)
	assert tc.translated_files[path]
}

fn test_checker_flag_include_dir_consumes_only_the_operand() {
	assert checker_flag_include_dir('-I @VMODROOT/include -D FEATURE') or { '' } == '@VMODROOT/include'
	assert checker_flag_include_dir('-isystem "system includes" -Wall') or { '' } == 'system includes'
	assert checker_flag_include_dir('-D FEATURE -Ijoined/include') or { '' } == 'joined/include'
}

fn test_parallel_checker_dependencies_are_private_and_merged() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	master_dependency, _ := tc.intern_symbol('main.master_dependency')
	worker_dependency, _ := tc.intern_symbol('main.worker_dependency')
	tc.direct_dependencies_by_fn[10] = [master_dependency]
	tc.visible_mutation_cache.rebind_results[1] = true

	mut worker := tc.fork_for_parallel_check()
	assert voidptr(worker.visible_mutation_cache) != voidptr(tc.visible_mutation_cache)
	assert 1 !in worker.visible_mutation_cache.rebind_results
	assert worker.direct_dependencies_by_fn.len == 0
	worker.direct_dependencies_by_fn[10] = [worker_dependency]
	worker.direct_dependencies_by_fn[20] = [master_dependency]
	assert tc.direct_dependencies_by_fn[10] == [master_dependency]
	assert 20 !in tc.direct_dependencies_by_fn

	tc.merge_parallel_check_worker(worker)
	assert tc.direct_dependencies_by_fn[10] == [master_dependency, worker_dependency]
	assert tc.direct_dependencies_by_fn[20] == [master_dependency]
	worker.free_parallel_check_worker_cache()

	mut transform_worker := tc.fork_for_parallel_transform(&a)
	assert isnil(transform_worker.visible_mutation_cache)
	assert transform_worker.direct_dependencies_by_fn.len == 0
	transform_worker.share_direct_dependencies_from(&tc)
	assert transform_worker.direct_dependencies_by_fn[10] == [master_dependency, worker_dependency]
	assert transform_worker.symbol_name(master_dependency) == 'main.master_dependency'
	transform_worker.free_parallel_transform_caches()
}

fn test_nested_parallel_checker_merge_keeps_out_of_range_caches_sparse() {
	mut a := flat.FlatAst.new()
	for _ in 0 .. 4 {
		a.add(.ident)
	}
	mut tc := TypeChecker.new(&a)
	tc.extend_node_caches(a.nodes.len)
	tc.parallel_check_sparse = true
	mut worker := tc.fork_for_parallel_check()
	worker.sparse_resolved_call_names[2] = 'main.answer'
	worker.sparse_resolved_fn_values[2] = 'main.callback'
	worker.sparse_statement_nodes[2] = true
	worker.sparse_expr_type_values[2] = Type(bool_)

	tc.merge_parallel_check_worker_scoped(worker, true)
	assert tc.sparse_resolved_call_names[2] == 'main.answer'
	assert tc.sparse_resolved_fn_values[2] == 'main.callback'
	assert tc.sparse_statement_nodes[2]
	if typ := tc.sparse_expr_type_values[2] {
		assert typ is Primitive
	} else {
		assert false
	}
	assert !tc.resolved_call_set[2]
	assert !tc.resolved_fn_value_set[2]
	assert !tc.statement_nodes[2]
	assert !tc.expr_type_set[2]

	tc.parallel_check_sparse = false
	tc.merge_own_sparse_caches()
	assert tc.resolved_call_names[2] == 'main.answer'
	assert tc.resolved_fn_value_names[2] == 'main.callback'
	assert tc.statement_nodes[2]
	assert tc.expr_type_values[2] is Primitive
	worker.free_parallel_check_worker_cache()
}

fn test_scoped_checker_merge_deep_clones_diagnostic_details() {
	$if prealloc {
		a := flat.FlatAst.new()
		mut tc := TypeChecker.new(&a)
		scope := unsafe { prealloc_scope_begin() }
		mut worker := tc.fork_for_parallel_check()
		worker.notices << TypeError{
			msg:     'scoped notice'.clone()
			details: ['scoped detail'.clone()]
		}
		unsafe { prealloc_scope_leave(scope) }
		tc.merge_parallel_check_worker_scoped(worker, true)
		unsafe { prealloc_scope_free_after(scope) }

		assert tc.notices.len == 1
		assert tc.notices[0].msg == 'scoped notice'
		assert tc.notices[0].details == ['scoped detail']
	}
}

fn test_direct_parent_index_preserves_first_parent_and_falls_back_for_new_nodes() {
	mut a := flat.FlatAst.new()
	child := a.add_val(.ident, 'child')
	first_children := a.begin_children()
	a.add_child(child)
	first_parent := a.add_node(flat.Node{
		kind:           .paren
		children_start: first_children
		children_count: 1
	})
	second_children := a.begin_children()
	a.add_child(child)
	a.add_node(flat.Node{
		kind:           .expr_stmt
		children_start: second_children
		children_count: 1
	})

	mut tc := TypeChecker.new(&a)
	tc.build_direct_parent_index(&a)
	assert tc.direct_parent_id(child) == first_parent
	assert tc.direct_parent_id(first_parent) == flat.empty_node
	tc.invalidate_direct_parent_index()
	assert tc.reuse_direct_parent_index_for_unchanged_ast(&a)
	assert tc.direct_parent_id(child) == first_parent

	appended_child := a.add_val(.ident, 'appended')
	appended_children := a.begin_children()
	a.add_child(appended_child)
	appended_parent := a.add_node(flat.Node{
		kind:           .paren
		children_start: appended_children
		children_count: 1
	})
	assert !tc.reuse_direct_parent_index_for_unchanged_ast(&a)
	assert tc.direct_parent_id(appended_child) == appended_parent

	tc.refresh_rewritten_parent_index(&a)
	assert tc.direct_parent_ids.len < a.nodes.len
	assert tc.rewritten_parent_ids.len == a.nodes.len
	assert !tc.direct_parent_index_trusted
	assert tc.direct_parent_id(child) == first_parent
	assert tc.direct_parent_id(appended_child) == appended_parent
}

fn test_rewritten_parent_index_falls_back_from_a_stale_shared_edge() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.build_direct_parent_index(&a)

	shared_child := a.add_val(.ident, 'shared')
	replacement := a.add_val(.ident, 'replacement')
	first_children := a.begin_children()
	a.add_child(shared_child)
	first_parent := a.add_node(flat.Node{
		kind:           .paren
		children_start: first_children
		children_count: 1
	})
	second_children := a.begin_children()
	a.add_child(shared_child)
	second_parent := a.add_node(flat.Node{
		kind:           .expr_stmt
		children_start: second_children
		children_count: 1
	})

	tc.refresh_rewritten_parent_index(&a)
	assert tc.direct_parent_id(shared_child) == first_parent

	a.children[first_children] = replacement
	assert tc.direct_parent_id(shared_child) == second_parent
}

fn test_generated_fn_params_update_method_suffix_index() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	params := [Type(int_)]
	tc.register_generated_fn_param_types('widgets.Box.open', params)

	assert tc.fn_param_types_for_name('Box.open') == params
	assert tc.fn_param_types_for_name('open') == params

	tc.register_generated_fn_param_types('other.Door.open', [Type(string_)])
	assert tc.fn_param_types_for_name('open').len == 0
	assert tc.fn_param_types_for_name('Box.open') == params

	tc.fn_param_types.delete('other.Door.open')
	tc.rebuild_fn_param_suffix_index()
	assert tc.fn_param_types_for_name('open') == params
}

fn test_generic_receiver_pattern_index_survives_rebuild_and_worker_fork() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	structured_key := 'Box[[]T].flatten'
	tc.fn_ret_types[structured_key] = Type(int_)
	tc.register_generated_fn_param_types(structured_key, []Type{})
	for i in 0 .. 256 {
		tc.fn_ret_types['irrelevant_${i}'] = Type(int_)
	}

	matched := tc.generic_receiver_method_pattern_match('Box', ['[]int'], 'flatten') or {
		assert false
		return
	}
	assert matched.key == structured_key
	assert matched.params == ['T']
	assert matched.args == ['int']

	tc.rebuild_fn_param_suffix_index()
	assert tc.generic_receiver_method_index['flatten'] == [structured_key]
	worker := tc.fork_for_parallel_check()
	assert worker.generic_receiver_method_index['flatten'] == [structured_key]
}

fn test_enclosing_generic_param_uses_the_owning_top_level_declaration() {
	mut a := flat.FlatAst.new()
	generic_child := a.add_val(.ident, 'T')
	generic_children := a.begin_children()
	a.add_child(generic_child)
	mut generic_fn := flat.Node{
		kind:           .fn_decl
		children_start: generic_children
		children_count: 1
	}
	generic_fn.set_generic_params(['T'])
	generic_fn_id := a.add_node(generic_fn)

	unrelated_child := a.add_val(.ident, 'T')
	unrelated_children := a.begin_children()
	a.add_child(unrelated_child)
	unrelated_fn_id := a.add_node(flat.Node{
		kind:           .fn_decl
		children_start: unrelated_children
		children_count: 1
	})

	mut tc := TypeChecker.new(&a)
	tc.top_level_idx = [int(generic_fn_id), int(unrelated_fn_id)]
	tc.build_enclosing_generic_param_index(&a)
	assert tc.node_has_enclosing_generic_param(generic_child, 'T')
	assert !tc.node_has_enclosing_generic_param(unrelated_child, 'T')
}

fn test_parallel_checker_preserves_all_dependency_edges() {
	$if !windows {
		old_vjobs := os.getenv_opt('VJOBS')
		os.setenv('VJOBS', '4', true)
		defer {
			if value := old_vjobs {
				os.setenv('VJOBS', value, true)
			} else {
				os.unsetenv('VJOBS')
			}
		}
		mut source := strings.new_builder(64_000)
		source.writeln('module main')
		for i in 0 .. 320 {
			source.writeln('fn dependency_${i}() int { return ${i} }')
			source.writeln('fn caller_${i}(flag bool) int { value := if flag { dependency_${i}() } else { ${i} }; return value }')
		}
		source.writeln('fn main() { println(caller_0(true)) }')
		path := os.join_path(os.vtmp_dir(), 'v3_parallel_checker_dependencies_${os.getpid()}.v')
		os.write_file(path, source.str()) or { panic(err) }
		defer {
			os.rm(path) or {}
		}
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.building_v_fast = true
		tc.enable_scoped_parallel_workers()
		tc.collect(a)
		assert tc.check_semantics_opt(true)
		assert tc.errors.len == 0, tc.errors.str()

		mut caller_ids := map[string]int{}
		for i, node in a.nodes {
			if node.kind == .fn_decl && node.value.starts_with('caller_') {
				caller_ids[node.value] = i
			}
		}
		for i in 0 .. 320 {
			dependencies := tc.direct_dependencies(caller_ids['caller_${i}'])
			assert dependencies.any(it == 'dependency_${i}' || it.ends_with('.dependency_${i}')), 'caller_${i}: ${dependencies}'
		}
	}
}

fn assert_preflight_error_keeps_function_semantics(name string, source string, initial_error string, collection_error bool) {
	for want_parallel in [false, true] {
		path := os.join_path(os.vtmp_dir(),
			'v3_preflight_continuation_${name}_${want_parallel}_${os.getpid()}.v')
		os.write_file(path, source) or { panic(err) }
		mut p := parser.Parser.new(pref.new_preferences())
		mut a := p.parse_file(path)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := TypeChecker.new(a)
		tc.collect(a)
		if collection_error {
			assert tc.errors.any(it.msg.contains(initial_error)), tc.errors.str()
		}
		tc.diagnose_unknown_calls = true
		tc.check_semantics_opt(want_parallel)
		assert tc.errors.any(it.msg.contains(initial_error)), tc.errors.str()
		assert tc.errors.filter(it.msg.contains(initial_error)).len == 1, tc.errors.str()
		assert tc.errors.any(it.msg.contains('unknown function') && it.msg.contains('unknown_call')), tc.errors.str()

		os.rm(path) or {}
	}
}

fn test_preflight_errors_do_not_skip_function_semantics() {
	assert_preflight_error_keeps_function_semantics('collection_error',
		'type Recursive = []Recursive\n\nfn main() {\n\tunknown_call()\n}\n',
		'recursive declarations of aliases', true)
	assert_preflight_error_keeps_function_semantics('for_in_const_conflict',
		'const item = 1\n\nfn report_other_error() {\n\tunknown_call()\n}\n\nfn main() {\n\tfor item in [1, 2] {}\n}\n',
		'duplicate of a const name `item`', false)
}
