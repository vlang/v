module types

import os
import strings
import v.flat
import v.parser
import v.pref

fn test_parallel_fn_prep_recognizes_source_embedded_veb_context() {
	mut a := flat.FlatAst.new()
	a.add_val(.file, 'context.v')
	a.add_val(.module_decl, 'veb')
	context_field := a.add_node(flat.Node{
		kind:  .field_decl
		value: 'Context'
	})
	context_children := a.begin_children()
	a.add_child(context_field)
	a.add_node(flat.Node{
		kind:           .struct_decl
		value:          'RegressionContext'
		children_start: context_children
		children_count: 1
	})
	mut tc := TypeChecker.new(&a)
	for i in 0 .. a.nodes.len {
		tc.top_level_idx << i
	}
	tc.build_type_declaration_index(&a)
	tc.cur_file = 'context.v'
	tc.cur_module = 'veb'
	assert tc.source_type_embeds_veb_context('RegressionContext')

	a.add_node(flat.Node{
		kind:  .type_decl
		value: 'AliasContext'
		typ:   'RegressionContext'
	})
	tc.top_level_idx << a.nodes.len - 1
	tc.build_type_declaration_index(&a)
	assert tc.source_type_embeds_veb_context('AliasContext')
}

fn test_first_type_declaration_index_preserves_conflict_context_and_order() {
	mut a := flat.FlatAst.new()
	a.nodes = [
		flat.Node{ kind: .file, value: 'dep.v' },
		flat.Node{ kind: .module_decl, value: 'dep' },
		flat.Node{ kind: .interface_decl, value: 'Item' },
		flat.Node{ kind: .struct_decl, value: 'dep.Item' },
		flat.Node{ kind: .file, value: 'main.v' },
		flat.Node{ kind: .enum_decl, value: 'Item' },
		flat.Node{ kind: .type_decl, value: 'Item' },
		flat.Node{ kind: .module_decl, value: 'builtin' },
		flat.Node{ kind: .struct_decl, value: 'Item' },
	]
	mut tc := TypeChecker.new(&a)
	for i in 0 .. a.nodes.len {
		tc.top_level_idx << i
	}
	for indexed in [false, true] {
		if indexed {
			tc.build_type_declaration_index(&a)
		}
		tc.cur_module = 'dep'
		assert tc.type_declaration_before(2, 'Item') == none
		assert tc.type_declaration_before(3, 'dep.Item')? == flat.NodeId(2)
		tc.cur_module = 'main'
		assert tc.type_declaration_before(5, 'Item') == none
		assert tc.type_declaration_before(6, 'Item')? == flat.NodeId(5)
		tc.cur_module = 'builtin'
		assert tc.type_declaration_before(8, 'Item')? == flat.NodeId(5)
		assert tc.type_declaration_before(8, 'Missing') == none
	}
	worker := tc.fork_for_parallel_check()
	assert worker.type_declaration_before(8, 'Item')? == flat.NodeId(5)
	worker.free_parallel_check_worker_cache()
}

fn test_static_method_name_index_preserves_aliases_and_late_signatures() {
	path := os.join_path(os.vtmp_dir(), 'v3_static_method_index_${os.getpid()}.v')
	os.write_file(path, 'module main\nstruct Widget {}\ntype Alias = Widget\nfn Widget.make() Widget { return Widget{} }\n') or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	assert tc.static_associated_method_names['make']
	key := flat.encode_static_type_method_name('Widget', 'make')
	assert tc.static_assoc_fn_key_for_base('Widget', 'make')? == key
	assert tc.static_assoc_fn_key_for_base('Alias', 'make')? == key
	assert tc.static_assoc_fn_key_for_base('Widget', 'missing') == none
	worker := tc.fork_for_parallel_check()
	assert worker.static_assoc_fn_key_for_base('Alias', 'make')? == key
	assert worker.static_assoc_fn_key_for_base('Widget', 'missing') == none
	worker.free_parallel_check_worker_cache()

	late_key := flat.encode_static_type_method_name('Widget', 'late')
	tc.fn_ret_types[late_key] = Type(int_)
	tc.fn_param_types[late_key] = []Type{}
	assert tc.static_assoc_fn_key_for_base('Widget', 'late')? == late_key
}

fn test_type_promotion_cache_preserves_misses_and_distinct_live_payloads() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	initial_count := tc.type_count()
	mut cache := CheckTypePromotionCache{}
	mut originals := []Type{}
	for i in 0 .. 600 {
		typ := Type(Struct{ name: 'Item${i}' })
		originals << typ
		assert tc.cached_check_type_promotion(typ, mut cache, false, false) == none
		promoted := tc.cached_check_type_promotion(typ, mut cache, true, false)?
		assert promoted.name() == 'Item${i}'
		assert tc.cached_check_type_promotion(typ, mut cache, false, false)? == promoted
	}
	// More live payloads than slots exercises eviction without losing identity.
	// The interner is now frozen, as it is during parallel clone promotion.
	for i, typ in originals {
		assert tc.cached_check_type_promotion(typ, mut cache, false, true)?.name() == 'Item${i}'
	}
	assert tc.type_count() == initial_count + 600
}

fn test_check_heap_partitions_match_linear_load_selection() {
	for count in [0, 1, 2, 33, 256, 1500] {
		mut items := []CheckWorkItem{}
		mut total := i64(0)
		for i in 0 .. count {
			cost := (i * 31) % 97
			items << CheckWorkItem{ fn_idx: i, cost: cost, rank: i64(i % 13) }
			total += i64(cost) + 1
		}
		for n in [1, 2, 3, 17, 136] {
			mut expected := [][]CheckWorkItem{len: n}
			mut loads := []i64{len: n}
			if n > 1 {
				loads[0] = -total * check_master_bias_pct / i64(100 * n)
			}
			mut sorted := items.clone()
			sorted.sort(a.rank > b.rank)
			for item in sorted {
				mut best := 0
				for bucket in 1 .. n {
					if loads[bucket] < loads[best] {
						best = bucket
					}
				}
				expected[best] << item
				loads[best] += i64(item.cost) + 1
			}
			for mut bucket in expected {
				bucket.sort(a.fn_idx < b.fn_idx)
			}
			assert split_check_items(items, n) == expected
		}
	}
}

fn test_visibility_index_preserves_file_context_and_builtin_aliases() {
	mut a := flat.FlatAst.new()
	a.add_val(.file, 'one.v')
	a.add_val(.module_decl, 'alpha')
	a.add_node(flat.Node{ kind: .fn_decl, value: 'entry', op: .arrow })
	a.add_val(.file, 'one.v')
	a.add_val(.fn_decl, 'trailing')
	a.add_val(.file, 'two.v')
	a.add_val(.module_decl, 'builtin')
	a.add_node(flat.Node{ kind: .fn_decl, value: 'helper', op: .arrow })
	mut tc := TypeChecker.new(&a)
	for i in 0 .. a.nodes.len {
		tc.top_level_idx << i
	}
	tc.cur_module = 'original'
	tc.cur_file = 'original.v'
	tc.collect_declaration_visibility()
	assert tc.declaration_visibility['alpha.entry'].is_pub
	assert tc.declaration_visibility['alpha.trailing'].module_name == 'alpha'
	assert tc.declaration_visibility['helper'].is_pub
	assert tc.declaration_visibility['builtin.helper'].is_pub
	assert tc.cur_module == 'original'
	assert tc.cur_file == 'original.v'
	assert tc.file_modules.len == 0
}

fn test_checker_type_promotion_survives_batch_arena_release() {
	$if prealloc {
		a := flat.FlatAst.new()
		tc := TypeChecker.new(&a)
		scope := unsafe { prealloc_scope_begin() }
		borrowed := Type(FnType{
			params:      [Type(Struct{ name: 'ScopedItem'.clone() })]
			return_type: Type(Array{ elem_type: Type(string_) })
		})
		unsafe { prealloc_scope_leave(scope) }
		first := tc.promote_check_type(borrowed)
		second := tc.promote_check_type(borrowed)
		if first is FnType && second is FnType {
			assert !unsafe { prealloc_scope_owns(scope, first.params.data) }
			assert first.params.data == second.params.data
		} else {
			assert false
		}
		unsafe { prealloc_scope_free_after(scope) }
		assert first.name() == 'fn(ScopedItem) []string'
		assert second.name() == first.name()
	}
}

fn test_parent_index_ranges_preserve_cross_range_edges_and_function_costs() {
	mut a := flat.FlatAst.new()
	a.add(.ident)
	for kind, children in {
		flat.NodeKind.paren:        [flat.NodeId(0), flat.NodeId(5)]
		flat.NodeKind.fn_decl:      [flat.NodeId(1)]
		flat.NodeKind.expr_stmt:    [flat.NodeId(0)]
		flat.NodeKind.comptime_for: [flat.NodeId(5)]
	} {
		start := a.begin_children()
		for child in children {
			a.add_child(child)
		}
		a.add_node(flat.Node{ kind: kind, children_start: start, children_count: children.len })
	}
	a.add(.ident)
	start := a.begin_children()
	a.add_child(flat.NodeId(5))
	a.add_node(flat.Node{ kind: .paren, children_start: start, children_count: 1 })
	a.add(.for_in_stmt)
	a.add_val(.struct_decl, 'Box@local@1')
	a.add(.goto_stmt)
	a.add(.fn_decl)
	mut serial := TypeChecker.new(&a)
	serial.building_v_fast = true
	serial.build_direct_parent_index(&a)
	mut split := TypeChecker.new(&a)
	split.building_v_fast = true
	split.init_direct_parent_index(&a)
	// Visit the later range first to exercise source-order parent selection.
	later := split.fill_direct_parent_edges_range(&a, 3, a.nodes.len)
	earlier := split.fill_direct_parent_edges_range(&a, 0, 3)
	split.merge_direct_parent_chunk(earlier)
	split.merge_direct_parent_chunk(later)
	assert split.direct_parent_ids == serial.direct_parent_ids
	assert split.direct_parent_ids[5] == flat.NodeId(1)
	assert split.value_used_nodes == serial.value_used_nodes
	assert split.fn_check_costs == serial.fn_check_costs
	assert split.preflight_node_ids == serial.preflight_node_ids
	assert split.synthetic_top_level_type_ids == serial.synthetic_top_level_type_ids
	assert split.has_goto_nodes == serial.has_goto_nodes
}

fn test_preflight_index_falls_back_after_ast_growth_or_invalidation() {
	mut a := flat.FlatAst.new()
	a.add(.ident)
	loop := a.add(.for_in_stmt)
	comptime_loop := a.add(.comptime_for)
	mut tc := TypeChecker.new(&a)
	tc.build_direct_parent_index(&a)
	assert tc.preflight_nodes(.for_in_stmt) == [i32(loop)]
	assert tc.preflight_nodes(.comptime_for) == [i32(comptime_loop)]
	added := a.add(.comptime_for)
	assert tc.preflight_nodes(.comptime_for) == [i32(comptime_loop), i32(added)]
	tc.build_direct_parent_index(&a)
	a.nodes[int(loop)].kind = .comptime_for
	tc.direct_parent_index_trusted = false
	assert tc.preflight_nodes(.for_in_stmt).len == 0
	assert tc.preflight_nodes(.comptime_for) == [i32(loop), i32(comptime_loop), i32(added)]
}

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

fn test_parent_metadata_replay_matches_full_scan() {
	path := os.join_path(os.vtmp_dir(), 'v3_parent_metadata_${os.getpid()}.v')
	os.write_file(path, '@[translated]\nmodule main\nimport strings\n#flag -I @DIR/v3_parent_headers -D FEATURE\n#flag -isystem "@DIR/v3_parent_system" -Wall\n#flag -I @DIR/v3_parent_headers\n@[inline]\nfn make_builder() { mut b := strings.new_builder(10) }\n') or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut serial := TypeChecker.new(a)
	serial.init_direct_parent_index(a)
	serial.collect_direct_parent_metadata(a)
	mut split := TypeChecker.new(a)
	split.init_direct_parent_index(a)
	for part in 0 .. 3 {
		chunk := split.fill_direct_parent_edges_range(a, a.nodes.len * part / 3, a.nodes.len * (part + 1) / 3)
		for idx in chunk.metadata_node_ids {
			split.collect_direct_parent_node_metadata(a, idx, a.nodes[idx])
		}
	}
	assert split.declaration_attributes == serial.declaration_attributes
	assert split.translated_files == serial.translated_files
	assert split.translated_files[path]
	assert split.strings_builder_candidates == serial.strings_builder_candidates
	assert split.strings_builder_candidates.len == 1
	// The ordinary serial entry must preserve real directive operands/deduplication
	// and leave builder candidates attached to their enclosing function.
	mut indexed := TypeChecker.new(a)
	indexed.build_direct_parent_index(a)
	assert indexed.declaration_attributes == serial.declaration_attributes
	assert indexed.translated_files == serial.translated_files
	assert indexed.strings_builder_candidates == serial.strings_builder_candidates
	assert indexed.insert_include_dirs_by_file == serial.insert_include_dirs_by_file
	assert indexed.insert_include_dirs_by_file[path] == [
		os.real_path(os.join_path(os.dir(path), 'v3_parent_headers')),
		os.real_path(os.join_path(os.dir(path), 'v3_parent_system')),
	]
	mut fn_index := -1
	for idx, node in a.nodes {
		if node.kind == .fn_decl && node.value == 'make_builder' {
			fn_index = idx
			break
		}
	}
	assert fn_index >= 0
	indexed.build_file_declaration_indexes(a)
	assert indexed.strings_builder_bindings[strings_builder_binding_key(fn_index, 'b')]
	assert indexed.strings_builder_bindings.len == 1
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
	assert !tc.statement_nodes[2]
	assert !tc.expr_type_set[2]

	tc.parallel_check_sparse = false
	tc.merge_own_sparse_caches()
	assert tc.resolved_call_names[2].value == 'main.answer'
	assert tc.resolved_fn_value_name(2)? == 'main.callback'
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

fn test_parallel_interface_indexes_outlive_worker_scopes() {
	if checker_serial_only() {
		return
	}
	mut source := strings.new_builder(16_000)
	source.writeln('module main')
	mut expected := map[string][]string{}
	// Exceed the parallel interface-index threshold with distinct method
	// requirements, so each index has its own concrete implementers.
	for i in 0 .. 16 {
		iface := 'Contract${i}'
		source.writeln('interface ${iface} { value_${i}() int }')
		mut names := []string{}
		for j in 0 .. 4 {
			name := 'Item${i}_${j}'
			names << name
			source.writeln('struct ${name} {}')
			source.writeln('fn (item ${name}) value_${i}() int { return ${j} }')
		}
		expected[iface] = names
		// A same-named method with the wrong return type must stay excluded.
		source.writeln('struct Rejected${i} {}')
		source.writeln('fn (item Rejected${i}) value_${i}() string { return "wrong" }')
	}
	path := os.join_path(os.vtmp_dir(), 'v3_parallel_interface_scopes_${os.getpid()}.v')
	os.write_file(path, source.str()) or { panic(err) }
	defer { os.rm(path) or {} }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	assert tc.errors.len == 0, tc.errors.str()
	tc.prepare_interface_requirement_indexes()
	mut names := tc.interface_names.keys()
	names.sort()
	assert names.len == expected.len
	mut expected_ids := map[string]map[string]int{}
	for iface in names {
		assert tc.interface_impl_names(iface) == expected[iface], iface
		expected_ids[iface] = tc.interface_type_ids(iface)
	}
	expected_signature := tc.interface_impl_set_signature()
	a.ensure_workers(2)
	defer { a.close_workers() }
	mut retained := map[string][]string{}
	for _ in 0 .. 4 {
		// This call releases each worker's scratch arena before returning. Later
		// rounds allocate another set of indexes while the first snapshot stays live.
		assert tc.prepare_interface_impl_indexes_parallel(names)
		for iface in names {
			prepared := tc.pre_transform_interface_impl_names(iface) or {
				panic('missing prepared interface index for ${iface}')
			}
			assert prepared == expected[iface], '${iface}: ${prepared}'
			if iface !in retained {
				retained[iface] = prepared
			}
			assert retained[iface] == expected[iface], iface
		}
		tc.freeze_pre_transform_interface_impl_names()
		assert tc.interface_impl_set_signature() == expected_signature
		for iface in names {
			assert tc.interface_type_ids(iface) == expected_ids[iface], iface
		}
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

fn test_generated_parent_lookup_revalidates_cached_edges() {
	mut a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.build_direct_parent_index(&a)
	tc.invalidate_direct_parent_index()
	child := a.add_val(.ident, 'value')
	assert tc.direct_parent_id(child) == flat.empty_node
	start := a.begin_children()
	a.add_child(child)
	first := a.add_node(flat.Node{ kind: .paren, children_start: start, children_count: 1 })
	assert tc.direct_parent_id(child) == first
	assert tc.direct_parent_id(child) == first
	second_start := a.begin_children()
	a.add_child(child)
	second := a.add_node(flat.Node{ kind: .paren, children_start: second_start, children_count: 1 })
	a.nodes[int(first)].children_count = 0
	assert tc.direct_parent_id(child) == second
	a.nodes[int(second)].children_count = 0
	assert tc.direct_parent_id(child) == flat.empty_node
	assert tc.direct_parent_id(flat.empty_node) == flat.empty_node
}

fn test_generated_parent_fallback_does_not_allocate_per_ast_node() {
	$if gcboehm ? {
		mut a := flat.FlatAst.new()
		for _ in 0 .. 8192 {
			a.add_node(flat.Node{ kind: .int_literal, value: '0', typ: 'int' })
		}
		mut tc := TypeChecker.new(&a)
		tc.build_direct_parent_index(&a)
		generated := a.add_node(flat.Node{ kind: .int_literal, value: '1', typ: 'int' })
		before := gc_heap_usage().total_bytes
		for _ in 0 .. 16 {
			assert tc.direct_parent_id(generated) == flat.empty_node
			assert tc.enclosing_infix_type_for_or_expr(generated) == none
			assert !tc.expr_is_standalone_statement(generated)
		}
		allocated := gc_heap_usage().total_bytes - before
		assert allocated < 1024 * 1024, 'parent fallbacks allocated ${allocated} bytes'
		first_children := a.begin_children()
		a.add_child(generated)
		a.add_child(flat.NodeId(0))
		first := a.add_node(flat.Node{
			kind:           .infix
			op:             .plus
			typ:            'int'
			children_start: first_children
			children_count: 2
		})
		second_children := a.begin_children()
		a.add_child(generated)
		second := a.add_node(flat.Node{
			kind:           .paren
			children_start: second_children
			children_count: 1
		})
		assert tc.direct_parent_id(generated) == first
		infix_type := tc.enclosing_infix_type_for_or_expr(generated) or {
			assert false, 'missing generated infix parent type'
			return
		}
		assert infix_type == Type(int_)
		a.nodes[int(first)].children_count = 0
		assert tc.direct_parent_id(generated) == second
		assert tc.enclosing_infix_type_for_or_expr(generated) == none
		statement_children := a.begin_children()
		a.add_child(second)
		a.add_node(flat.Node{
			kind:           .expr_stmt
			children_start: statement_children
			children_count: 1
		})
		assert tc.expr_is_standalone_statement(generated)
	}
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
	tc.top_level_idx = [i32(generic_fn_id), i32(unrelated_fn_id)]
	tc.build_enclosing_generic_param_index(&a)
	assert tc.node_has_enclosing_generic_param(generic_child, 'T')
	assert !tc.node_has_enclosing_generic_param(unrelated_child, 'T')
}

fn test_parallel_checker_preserves_all_dependency_edges() {
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

fn assert_preflight_error_keeps_function_semantics(name string, source string, initial_error string, collection_error bool) {
	for want_parallel in [false, true] {
		path := os.join_path(os.vtmp_dir(), 'v3_preflight_continuation_${name}_${want_parallel}_${os.getpid()}.v')
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
	assert_preflight_error_keeps_function_semantics('collection_error', 'type Recursive = []Recursive\n\nfn main() {\n\tunknown_call()\n}\n', 'recursive declarations of aliases', true)
	assert_preflight_error_keeps_function_semantics('for_in_const_conflict', 'const item = 1\n\nfn report_other_error() {\n\tunknown_call()\n}\n\nfn main() {\n\tfor item in [1, 2] {}\n}\n', 'duplicate of a const name `item`', false)
}

fn test_disabled_resolution_views_reuse_warm_composite_type_parse_cache() {
	$if gcboehm ? {
		for fast_recent in [false, true] {
			a := flat.FlatAst.new()
			mut tc := TypeChecker.new(&a)
			tc.type_cache.parse_enabled = true
			tc.fast_parse_recent = fast_recent
			tc.disable_resolution_type_view_cache()
			tc.cur_file = 'items.v'
			tc.cur_module = 'items'
			tc.fn_context.generic_params = ['T']
			tc.resolution_type_mode = true
			tc.structs['items.Item'] = []StructField{}
			assert tc.parse_resolution_type('[]Item').name() == '[]items.Item'
			before := gc_heap_usage().total_bytes
			for _ in 0 .. 128 {
				assert tc.parse_resolution_type('[]Item').name() == '[]items.Item'
			}
			allocated := gc_heap_usage().total_bytes - before
			assert allocated < 1024 * 1024, 'warm resolution calls allocated ${allocated} bytes'
			assert isnil(tc.resolution_type_views)
		}
	}
}

fn test_disabled_resolution_views_keep_file_context_and_recursive_aliases() {
	a := flat.FlatAst.new()
	mut tc := TypeChecker.new(&a)
	tc.type_cache.parse_enabled = true
	tc.disable_resolution_type_view_cache()
	tc.cur_module = 'main'
	tc.cur_file = 'one.v'
	tc.structs['one.Item'] = []StructField{}
	tc.structs['two.Item'] = []StructField{}
	tc.register_file_import('dep', 'one')
	assert tc.parse_resolution_type('[]dep.Item').name() == '[]one.Item'
	tc.cur_file = 'two.v'
	tc.register_file_import('dep', 'two')
	assert tc.parse_resolution_type('[]dep.Item').name() == '[]two.Item'
	tc.fn_context.generic_params = ['Item']
	tc.resolution_type_mode = true
	assert tc.parse_resolution_type('[]dep.Item').name() == '[]two.Item'
	tc.cur_file = 'one.v'
	assert tc.parse_resolution_type('[]dep.Item').name() == '[]one.Item'
	assert tc.cur_module == 'main'
	assert tc.fn_context.generic_params == ['Item']
	assert tc.resolution_type_mode

	tc.type_aliases['Handlers'] = 'map[string]fn (Handlers) int'
	warm := tc.parse_resolution_type('Handlers')
	assert warm is Alias
	assert (warm as Alias).base_type is Map
	// A recursive edge must remain symbolic even after the complete alias was cached.
	tc.type_cache.alias_parse_stack << 'Handlers'
	recursive := tc.parse_resolution_type('Handlers')
	tc.type_cache.alias_parse_stack.delete_last()
	assert recursive is Alias
	assert (recursive as Alias).base_type is Unknown
	assert tc.parse_resolution_type('Handlers') == Type(warm)
	assert isnil(tc.resolution_type_views)
}
