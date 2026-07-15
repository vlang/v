module markused

import v3.flat
import v3.gen.c.naming
import v3.types

const trace_markused = false
const min_eager_markused_bodies = 4096

// mark_used updates mark used state for markused.
pub fn mark_used(a &flat.FlatAst, tc &types.TypeChecker) map[string]bool {
	used, _ := mark_used_with_test_files(a, tc, map[string]bool{}, map[string]bool{}, false)
	return used
}

pub fn mark_used_for_tests(a &flat.FlatAst, tc &types.TypeChecker, test_files []string) map[string]bool {
	used, _ := mark_used_for_tests_with_generic_usage(a, tc, test_files)
	return used
}

// mark_used_with_generic_usage also reports whether reachable code uses a generic
// function, struct, or sum type and therefore requires monomorphization.
pub fn mark_used_with_generic_usage(a &flat.FlatAst, tc &types.TypeChecker) (map[string]bool, bool) {
	return mark_used_with_test_files(a, tc, map[string]bool{}, map[string]bool{}, false)
}

// mark_used_for_tests_with_generic_usage is the test-file-rooted variant of
// mark_used_with_generic_usage.
pub fn mark_used_for_tests_with_generic_usage(a &flat.FlatAst, tc &types.TypeChecker, test_files []string) (map[string]bool, bool) {
	mut file_map := map[string]bool{}
	for file in test_files {
		file_map[file] = true
	}
	return mark_used_with_test_files(a, tc, file_map, map[string]bool{}, false)
}

// mark_used_for_cache roots every concrete function in modules being built for the object cache.
pub fn mark_used_for_cache(a &flat.FlatAst, tc &types.TypeChecker, test_files []string, source_modules map[string]bool) map[string]bool {
	used, _ := mark_used_for_cache_with_generic_usage(a, tc, test_files, source_modules)
	return used
}

// mark_used_for_cache_with_generic_usage roots cached module functions and reports whether
// their reachable code requires monomorphization.
pub fn mark_used_for_cache_with_generic_usage(a &flat.FlatAst, tc &types.TypeChecker, test_files []string, source_modules map[string]bool) (map[string]bool, bool) {
	mut file_map := map[string]bool{}
	for file in test_files {
		file_map[file] = true
	}
	return mark_used_with_test_files(a, tc, file_map, source_modules, true)
}

fn mark_used_with_test_files(a &flat.FlatAst, tc &types.TypeChecker, test_files map[string]bool, cache_modules map[string]bool, cache_mode bool) (map[string]bool, bool) {
	mut cur_module := ''
	mut imports := map[string]string{}
	mut fn_decls := map[string]FnDeclInfo{}
	mut fn_decl_lists := map[string][]FnDeclInfo{}
	mut struct_decls := map[string]StructDeclInfo{}
	mut const_decls := map[string]ConstDeclInfo{}
	mut fn_name_suffixes := map[string]bool{}
	mut const_name_suffixes := map[string]bool{}

	// Reverse index: short name (after last '.') -> list of full qualified names
	mut suffix_map := map[string][]string{}

	// Every fn_decl/c_fn_decl node (and its module), in AST order: the worklist
	// for the parallel body-call precollection below.
	mut body_ids := []int{cap: 8192}
	mut body_modules := []string{cap: 8192}
	mut cache_roots := []string{}

	mut fn_count := 0
	mut fn_with_dot := 0
	mut contains2_total := 0
	for node_idx in tc.top_level_idx {
		node := a.nodes[node_idx]
		if node.kind == .file {
			cur_module = ''
			continue
		}
		if node.kind == .module_decl {
			cur_module = node.value
			continue
		}
		if node.kind == .import_decl {
			imports[node.typ] = node.value
			continue
		}
		if node.kind == .struct_decl {
			full_name := qualify_fn(cur_module, node.value)
			info := StructDeclInfo{
				node_id: flat.NodeId(node_idx)
				module:  cur_module
			}
			struct_decls[full_name] = info
			if node.value !in struct_decls {
				struct_decls[node.value] = info
			}
			continue
		}
		if node.kind == .const_decl || node.kind == .global_decl {
			for i in 0 .. node.children_count {
				field_id := a.child(&node, i)
				field := a.node(field_id)
				if field.kind != .const_field || field.children_count == 0 {
					continue
				}
				info := ConstDeclInfo{
					expr_id: a.child(field, 0)
					module:  cur_module
				}
				const_decls[field.value] = info
				add_candidate_suffix(mut const_name_suffixes, field.value)
				full_name := qualify_fn(cur_module, field.value)
				if full_name != field.value {
					const_decls[full_name] = info
					add_candidate_suffix(mut const_name_suffixes, full_name)
				}
			}
			continue
		}
		if node.kind == .fn_decl || node.kind == .c_fn_decl {
			body_ids << node_idx
			body_modules << cur_module
			has_dot := node.value.contains('.')
			can_suffix_match := !markused_fn_decl_is_generic_template(node, a)
			if trace_markused {
				fn_count++
				if has_dot {
					fn_with_dot++
					if fn_with_dot <= 5 {
						eprintln('  fn with dot: "${node.value}"')
					}
				}
			}
			info := FnDeclInfo{
				node_id: flat.NodeId(node_idx)
				module:  cur_module
			}
			add_fn_decl_info(mut fn_decls, mut fn_decl_lists, node.value, info)
			if can_suffix_match {
				add_candidate_suffix(mut fn_name_suffixes, node.value)
			}
			lowered_name := markused_c_name(node.value)
			if lowered_name != node.value {
				add_fn_decl_info(mut fn_decls, mut fn_decl_lists, lowered_name, info)
				if can_suffix_match {
					add_candidate_suffix(mut fn_name_suffixes, lowered_name)
				}
			}
			qname := qualify_fn(cur_module, node.value)
			if cache_mode && node.kind == .fn_decl && node.generic_params.len == 0
				&& cache_modules[cur_module] {
				cache_roots << qname
			}
			if qname != node.value {
				add_fn_decl_info(mut fn_decls, mut fn_decl_lists, qname, info)
				if can_suffix_match {
					add_candidate_suffix(mut fn_name_suffixes, qname)
				}
				lowered_qname := markused_c_name(qname)
				if lowered_qname != qname {
					add_fn_decl_info(mut fn_decls, mut fn_decl_lists, lowered_qname, info)
					if can_suffix_match {
						add_candidate_suffix(mut fn_name_suffixes, lowered_qname)
					}
				}
			}
			// Build suffix_map entries
			if has_dot && can_suffix_match {
				if trace_markused {
					contains2_total++
				}
				short := node.value.all_after_last('.')
				add_suffix_candidate(mut suffix_map, short, node.value)
				if qname != node.value {
					add_suffix_candidate(mut suffix_map, short, qname)
				}
			}
			if qname != node.value && qname.contains('.') && can_suffix_match {
				short := qname.all_after_last('.')
				add_suffix_candidate(mut suffix_map, short, qname)
			}
		}
	}

	// BFS from main
	mut used := map[string]bool{}
	mut queue := []string{}
	reachable_modules := markused_reachable_modules(a, tc)
	queue << 'main'
	used['main'] = true
	enqueue_main_module_roots(fn_decls, mut used, mut queue)
	enqueue_auto_roots(fn_decls, reachable_modules, mut used, mut queue)
	enqueue_export_roots(a, mut used, mut queue)
	enqueue_veb_handler_roots(a, tc, mut used, mut queue)
	enqueue_test_file_roots(a, test_files, mut used, mut queue)
	queue << 'time.Time.new'
	used['time.Time.new'] = true
	used['Time.new'] = true
	queue << 'gen_expr_lvalue'
	used['gen_expr_lvalue'] = true
	queue << 'c.gen_expr_lvalue'
	used['c.gen_expr_lvalue'] = true
	queue << 'gen_assign'
	used['gen_assign'] = true
	queue << 'c.gen_assign'
	used['c.gen_assign'] = true
	for seed in ['__new_array', 'new_array_from_c_array', 'array.get', 'array.set', 'array.push',
		'array.push_many', 'array.insert', 'array.insert_many', 'array.prepend', 'array.reverse',
		'array.slice', 'array.slice_ni', 'string.substr_ni', 'array.pop_left', 'array.clone',
		'array.delete', 'array.ensure_cap', 'string.==', 'string.<', 'string.free',
		'string.all_before', 'string.all_before_last', 'string.all_after', 'string.all_after_last',
		'string.substr', 'string__substr', 'u8.vstring', 'u8.vstring_with_len', 'u8.vbytes',
		'charptr.vstring', 'charptr.vstring_with_len', 'byteptr.vstring', 'byteptr.vstring_with_len',
		'byteptr.vbytes', 'voidptr.vbytes', '[]rune.string', 'map.set', 'map.exists', 'map.get',
		'map.get_check', 'map.get_and_set', 'map.delete', 'map.clone', 'map.clear', 'map.keys',
		'map.values', 'map.reserve', 'map_map_eq', 'memdup', 'strings.Builder.write_ptr',
		'strings.Builder.write_runes', 'strings.Builder.free', 'strconv.format_int',
		'strconv.format_uint', 'bool.str', 'int.str', 'u64.str', 'f32.str', 'f64.str', 'rune.str',
		'string.+', 'ptr_str', 'strconv__f32_to_str_l', 'strconv__f64_to_str_l',
		'os.join_path_single', 'panic', 'u8.is_letter', 'u8.is_capital', 'string.is_capital',
		'string.to_lower_ascii', 'rune.to_lower', 'Array_u8__bytestr', 'Array_u8__hex',
		'data_to_hex_string', 'map_hash_string', 'map_hash_int_1', 'map_hash_int_2', 'map_hash_int_4',
		'map_hash_int_8', 'map_eq_string', 'map_eq_int_1', 'map_eq_int_2', 'map_eq_int_4',
		'map_eq_int_8', 'map_clone_string', 'map_clone_int_1', 'map_clone_int_2', 'map_clone_int_4',
		'map_clone_int_8', 'map_free_string', 'map_free_nop', '[]string.join', 'Array_string__join',
		'embed_file.Decoder.decompress', 'exit', 'v_exit'] {
		queue << seed
		used[seed] = true
	}
	queue << 'array.delete_last'
	used['array.delete_last'] = true
	for type_name in tc.ownership_drop_type_names() {
		method := '${type_name}.drop'
		enqueue(method, mut used, mut queue)
		lowered := markused_c_name(method)
		if lowered != method {
			enqueue(lowered, mut used, mut queue)
		}
	}
	for seed in ['i8.str', 'i16.str', 'i32.str', 'i64.str'] {
		queue << seed
		used[seed] = true
	}
	for name in cache_roots {
		enqueue(name, mut used, mut queue)
	}

	if trace_markused {
		eprintln('markused: fn_count:')
		eprintln(fn_count.str())
		eprintln('fn_with_dot:')
		eprintln(fn_with_dot.str())
		eprintln('contains2_total:')
		eprintln(contains2_total.str())
		eprintln('markused: main in fn_decls: ${'main' in fn_decls}')
		eprintln('markused: fn_decls count: ${fn_decls.len}')
		eprintln('markused: suffix_map count: ${suffix_map.len}')
		mut total_suffix_entries := 0
		for _, vals in suffix_map {
			total_suffix_entries += vals.len
		}
		eprintln('total suffix_map entries (sum of array lens):')
		eprintln(total_suffix_entries.str())
	}

	mut suffix_hits := 0
	// mut suffix_misses := 0
	mut in_cg := 0
	mut not_in_cg := 0
	mut total_callees := 0
	collector := CallCollector{
		a:                       a
		tc:                      tc
		fn_decls:                fn_decls
		fn_suffixes:             fn_name_suffixes
		struct_decls:            struct_decls
		const_decls:             const_decls
		const_suffixes:          const_name_suffixes
		selective_alias_targets: markused_selective_alias_targets(tc)
		iface_param_gate:        markused_interface_param_gate(tc)
	}
	// Precollect every body's call/initializer-ref lists up front (across
	// threads when available): the BFS below then only does the cheap
	// name-resolution work. Collecting inline used to serialize ~80% of
	// markused's time inside the BFS loop.
	mut body_index := map[int]int{}
	mut body_results := []BodyCalls{}
	if body_ids.len >= min_eager_markused_bodies {
		for i, id in body_ids {
			body_index[id] = i
		}
		body_results = precollect_body_calls(collector, body_ids, body_modules, imports)
	}
	has_entry_main := markused_has_entry_main_indexed(a, tc)
	enqueue_detected_runtime_helpers(a, tc, mut used, mut queue)
	enqueue_function_value_selectors(a, collector, fn_decls, has_entry_main, mut used, mut queue)
	// Methods used as values (`recv.method` passed as a callback) are reachable only
	// through a wrapper cgen generates later. The checker records them per enclosing
	// function in `method_values_by_fn`; they are seeded inside the BFS below (only when
	// that function is reached), so an unreachable function's method value never forces an
	// otherwise-unused specialization to be transformed/emitted.
	mut uses_generics := enqueue_initializer_calls(a, collector, imports, fn_decls, mut used, mut
		queue, false)
	uses_generics = enqueue_top_level_calls(a, collector, fn_decls, has_entry_main, mut used, mut
		queue, uses_generics)
	// Interface dispatch reachability: calling an interface method `Foo.m` may
	// dispatch to any concrete `T.m` for a type `T` that implements `Foo`. Those
	// concrete methods are only referenced from the generated dispatch switch, so
	// without this they would be pruned and produce undefined-symbol errors.
	mut iface_impls := map[string][]string{}
	mut checked_iface_impls := map[string]bool{}
	mut processed_nodes := []bool{len: a.nodes.len}
	mut processed_initializer_refs := map[string]bool{}
	mut calls := []string{cap: 128}
	mut initializer_calls := []string{cap: 32}
	mut initializer_refs := []string{cap: 32}
	// Global per-callee dedup for the body-independent parts of callee
	// processing. The same callee name (`println`, `array.get`, ...) appears in
	// thousands of bodies; its decl-alias expansion depends only on `fn_decls`
	// (fixed during the BFS) and the interface-dispatch tail only on
	// (module, callee), so each needs to run once. enqueue() is idempotent, so
	// skipping a repeat performs no fewer markings.
	mut safe_alias_done := map[string]bool{}
	mut iface_tail_done := map[string]bool{}
	mut qi := 0
	for qi < queue.len {
		name := queue[qi]
		qi++
		prev_len := queue.len
		fn_infos := fn_decl_infos_for_queue_name(name, fn_decl_lists, a)
		if fn_infos.len == 0 {
			not_in_cg++
			if trace_markused && qi <= 10 {
				eprintln('BFS qi=${qi.str()} name="${name}" in_cg=false')
			}
			continue
		}
		for fn_info in fn_infos {
			if trace_markused && qi <= 10 {
				eprintln('BFS qi=${qi.str()} name="${name}" in_cg=true')
			}
			in_cg++
			node_key := int(fn_info.node_id)
			if node_key < 0 || node_key >= processed_nodes.len {
				continue
			}
			if processed_nodes[node_key] {
				continue
			}
			processed_nodes[node_key] = true
			// This function is reachable, so any methods it uses as *values* (recorded by
			// the checker per enclosing function) are reachable too -- mark them so they
			// survive pruning (cgen emits a wrapper that calls them).
			if mvs := tc.method_values_by_fn[node_key] {
				for mkey in mvs {
					enqueue(mkey, mut used, mut queue)
					lowered_mv := markused_c_name(mkey)
					if lowered_mv != mkey {
						enqueue(lowered_mv, mut used, mut queue)
					}
					mv_short := mkey.all_after_last('.')
					if cands := suffix_map[mv_short] {
						for cand in cands {
							if cand == mkey || cand.ends_with('.${mkey}') {
								enqueue(cand, mut used, mut queue)
							}
						}
					}
				}
			}
			calls.clear()
			initializer_refs.clear()
			if body_i := body_index[node_key] {
				body := body_results[body_i]
				calls << body.calls
				initializer_refs << body.refs
				uses_generics = uses_generics || body.uses_generics
			} else {
				// Not part of the precollected decl set (should not happen; the BFS
				// resolves names through the same decl scan) — collect inline.
				node := a.node(fn_info.node_id)
				body := collector.collect_body(node, fn_info.module, imports)
				calls << body.calls
				initializer_refs << body.refs
				uses_generics = uses_generics || body.uses_generics
			}
			mut call_set := map[string]bool{}
			for call in calls {
				call_set[call] = true
			}
			for initializer_ref in initializer_refs {
				enqueue_initializer_ref_calls(a, collector, imports, fn_decls, initializer_ref, mut
					processed_initializer_refs, mut initializer_calls, mut used, mut queue)
			}
			total_callees += calls.len
			for callee in calls {
				if !valid_symbol_name(callee) {
					continue
				}
				if callee == 'string__plus' {
					enqueue(callee, mut used, mut queue)
					continue
				}
				mut found_direct := false
				if callee_info := fn_decls[callee] {
					found_direct = true
					if enqueue(callee, mut used, mut queue) {
						if trace_markused && qi == 1 {
							eprintln('main: all_fns hit: "${callee}"')
						}
					}
					if !safe_alias_done[callee] {
						safe_alias_done[callee] = true
						add_safe_decl_alias(callee, callee_info, a, mut used, mut queue)
					}
				} else if callee in tc.fn_ret_types {
					found_direct = true
					if enqueue(callee, mut used, mut queue) {
						if trace_markused && qi == 1 {
							eprintln('main: resolved hit: "${callee}"')
						}
					}
				} else if markused_is_interface_dispatch_call(callee, fn_info.module, tc) {
					found_direct = true
					enqueue(callee, mut used, mut queue)
					lowered := markused_c_name(callee)
					if lowered != callee {
						enqueue(lowered, mut used, mut queue)
					}
				}
				if !found_direct {
					short := callee.all_after_last('.')
					qcallee := qualify_fn(fn_info.module, callee)
					if qcallee == callee || !call_set[qcallee] {
						if suffix_candidates := suffix_map[short] {
							for candidate in suffix_candidates {
								if candidate in fn_decls || candidate in tc.fn_ret_types {
									if enqueue(candidate, mut used, mut queue) {
										suffix_hits++
									}
								}
							}
						}
					}
				}
				iface_tail_key := '${fn_info.module}\x01${callee}'
				if !iface_tail_done[iface_tail_key] {
					iface_tail_done[iface_tail_key] = true
					mut iface_callee := callee
					if normalized := interface_dispatch_dotted_name(callee, tc) {
						iface_callee = normalized
					}
					if iface_callee.contains('.') {
						recv := iface_callee.all_before_last('.')
						method := iface_callee.all_after_last('.')
						ensure_iface_impls(recv, fn_info.module, tc, mut iface_impls, mut
							checked_iface_impls)
						if iface_name := interface_name_for_receiver(recv, fn_info.module, tc) {
							// Keep the dispatch stub (`Iface__method`) the transform will
							// call; the call site may name the interface through an alias
							// (`Expr.name` for `type Expr = Node`), which cgen's used-fn
							// filter does not resolve back to the interface key.
							enqueue('${iface_name}.${method}', mut used, mut queue)
						}
						if impls := iface_impls[recv] {
							for impl in impls {
								impl_method := tc.concrete_method_signature_key(impl, method) or {
									'${impl}.${method}'
								}
								enqueue(impl_method, mut used, mut queue)
								short_impl := '${impl_method.all_before_last('.').all_after_last('.')}.${method}'
								if short_impl != impl_method {
									enqueue(short_impl, mut used, mut queue)
								}
							}
						}
					}
				}
			}
		}
		new_added := queue.len - prev_len
		if trace_markused && qi <= 10 {
			eprintln('  -> added ${new_added.str()} new entries, queue now ${queue.len.str()}')
		}
	}
	if !uses_generics {
		uses_generics = collector.emitted_type_declarations_use_generics(imports)
	}
	if trace_markused {
		eprintln('total_callees:')
		eprintln(total_callees.str())
		eprintln('markused: in_cg:')
		eprintln(in_cg.str())
		eprintln('not_in_cg:')
		eprintln(not_in_cg.str())
		eprintln('queue.len:')
		eprintln(queue.len.str())
		eprintln('markused: suffix_hits:')
		eprintln(suffix_hits.str())
		eprintln('markused: total used: ${used.len}')
	}
	return used, uses_generics
}

fn enqueue_used_interface_dispatch_implementers(tc &types.TypeChecker, mut used map[string]bool, mut queue []string) bool {
	mut added := false
	for iface_name in tc.interface_names.keys() {
		methods := tc.interface_abstract_method_names(iface_name)
		if methods.len == 0 {
			continue
		}
		impls := if markused_is_ierror_interface_name(iface_name) {
			tc.ierror_impl_names()
		} else {
			tc.interface_impl_names(iface_name)
		}
		if impls.len == 0 {
			continue
		}
		for method in methods {
			dispatch_key := '${iface_name}.${method}'
			dispatch_c_key := markused_c_name(dispatch_key)
			short_dispatch_key := '${iface_name.all_after_last('.')}.${method}'
			if dispatch_key !in used && dispatch_c_key !in used && short_dispatch_key !in used {
				continue
			}
			for impl in impls {
				impl_method := tc.concrete_method_signature_key(impl, method) or {
					'${impl}.${method}'
				}
				if enqueue(impl_method, mut used, mut queue) {
					added = true
				}
				lowered := markused_c_name(impl_method)
				if lowered != impl_method && enqueue(lowered, mut used, mut queue) {
					added = true
				}
				short_impl := '${impl_method.all_before_last('.').all_after_last('.')}.${method}'
				if short_impl != impl_method && enqueue(short_impl, mut used, mut queue) {
					added = true
				}
			}
		}
	}
	return added
}

fn markused_is_interface_dispatch_call(name string, cur_module string, tc &types.TypeChecker) bool {
	if !name.contains('.') {
		return false
	}
	recv := name.all_before_last('.')
	method := name.all_after_last('.')
	iface_name := interface_name_for_receiver(recv, cur_module, tc) or { return false }
	return tc.interface_method_signature_key(iface_name, method) != none
}

// ensure_iface_impls supports ensure iface impls handling for markused.
fn ensure_iface_impls(recv string, cur_module string, tc &types.TypeChecker, mut iface_impls map[string][]string, mut checked map[string]bool) {
	if recv.len == 0 {
		return
	}
	iface_name := interface_name_for_receiver(recv, cur_module, tc) or { return }
	if iface_name in checked {
		return
	}
	checked[iface_name] = true
	if recv != iface_name {
		checked[recv] = true
	}
	mut impls := []string{}
	if markused_is_ierror_interface_name(iface_name) {
		impls = tc.ierror_impl_names()
	} else {
		// Structs plus alias implementers, from the same list cgen assigns
		// dispatch ids over.
		impls = tc.interface_impl_names(iface_name)
	}
	iface_impls[recv] = impls
	if iface_name != recv {
		iface_impls[iface_name] = impls
		checked[iface_name] = true
	}
}

fn interface_name_for_receiver(recv string, cur_module string, tc &types.TypeChecker) ?string {
	if recv in tc.interface_names {
		return recv
	}
	if target := tc.type_aliases[recv] {
		if target in tc.interface_names {
			return target
		}
		qtarget := tc.qualify_name(target)
		if qtarget in tc.interface_names {
			return qtarget
		}
	}
	if recv.contains('.') {
		return none
	}
	if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
		qname := '${cur_module}.${recv}'
		if qname in tc.interface_names {
			return qname
		}
		if target := tc.type_aliases[qname] {
			if target in tc.interface_names {
				return target
			}
			qtarget := tc.qualify_name(target)
			if qtarget in tc.interface_names {
				return qtarget
			}
		}
	}
	return none
}

fn interface_dispatch_dotted_name(name string, tc &types.TypeChecker) ?string {
	if name.contains('.') {
		return name
	}
	if !name.contains('__') {
		return none
	}
	recv_c := name.all_before_last('__')
	method := name.all_after_last('__')
	if recv_c.len == 0 || method.len == 0 {
		return none
	}
	recv := recv_c.replace('__', '.')
	if recv in tc.interface_names {
		return '${recv}.${method}'
	}
	if target := tc.type_aliases[recv] {
		if target in tc.interface_names {
			return '${target}.${method}'
		}
		qtarget := tc.qualify_name(target)
		if qtarget in tc.interface_names {
			return '${qtarget}.${method}'
		}
	}
	return none
}

fn markused_is_ierror_interface_name(name string) bool {
	return name == 'IError' || name == 'builtin.IError'
}

// add_suffix_candidate updates add suffix candidate state for markused.
fn add_suffix_candidate(mut suffix_map map[string][]string, short string, name string) {
	if !valid_symbol_name(short) || !valid_symbol_name(name) {
		return
	}
	mut candidates := suffix_map[short] or { []string{} }
	candidates << name
	suffix_map[short] = candidates
}

fn add_candidate_suffix(mut suffixes map[string]bool, name string) {
	if !valid_symbol_name(name) {
		return
	}
	suffixes[name.all_after_last('.')] = true
}

fn markused_fn_decl_is_generic_template(node flat.Node, a &flat.FlatAst) bool {
	if node.generic_params.len > 0 || node.value.contains('[') || node.typ.contains('generic') {
		return true
	}
	for i in 0 .. node.children_count {
		child := a.child_node(&node, i)
		if child.kind == .param && child.typ.contains('generic') {
			return true
		}
	}
	return false
}

fn add_fn_decl_info(mut fn_decls map[string]FnDeclInfo, mut fn_decl_lists map[string][]FnDeclInfo, name string, info FnDeclInfo) {
	fn_decls[name] = info
	mut infos := fn_decl_lists[name] or { []FnDeclInfo{} }
	infos << info
	fn_decl_lists[name] = infos
}

fn fn_decl_infos_for_queue_name(name string, fn_decl_lists map[string][]FnDeclInfo, a &flat.FlatAst) []FnDeclInfo {
	infos := fn_decl_lists[name] or {
		return fn_decl_infos_for_generic_specialization_name(name, fn_decl_lists, a)
	}
	if infos.len <= 1 {
		return infos
	}
	mut exact_infos := []FnDeclInfo{}
	for info in infos {
		node := a.node(info.node_id)
		if fn_decl_key_is_exact_for_info(name, node.value, info.module) {
			exact_infos << info
		}
	}
	return exact_infos
}

fn fn_decl_infos_for_generic_specialization_name(name string, fn_decl_lists map[string][]FnDeclInfo, a &flat.FlatAst) []FnDeclInfo {
	for candidate in markused_generic_specialization_decl_keys(name) {
		infos := fn_decl_lists[candidate] or { continue }
		mut generic_infos := []FnDeclInfo{}
		for info in infos {
			node := a.node(info.node_id)
			if markused_fn_decl_is_generic_template(node, a) {
				generic_infos << info
			}
		}
		if generic_infos.len > 0 {
			return generic_infos
		}
		return infos
	}
	return []FnDeclInfo{}
}

fn markused_generic_specialization_decl_keys(name string) []string {
	mut keys := []string{}
	if name.contains('__') {
		parts := name.split('__')
		for i in 0 .. parts.len - 1 {
			markused_add_generic_specialization_keys(parts, i, mut keys)
		}
		return keys
	}
	markused_add_generic_specialization_key(name, mut keys)
	return keys
}

fn markused_add_generic_specialization_keys(parts []string, index int, mut keys []string) {
	segment := parts[index]
	if !segment.contains('_') {
		return
	}
	mut base := segment
	for base.contains('_') {
		base = base.all_before_last('_')
		if base.len == 0 {
			return
		}
		mut candidate_parts := parts.clone()
		candidate_parts[index] = base
		markused_push_unique_key(candidate_parts.join('__'), mut keys)
	}
}

fn markused_add_generic_specialization_key(name string, mut keys []string) {
	if !name.contains('_') {
		return
	}
	mut base := name
	for base.contains('_') {
		base = base.all_before_last('_')
		if base.len == 0 {
			return
		}
		markused_push_unique_key(base, mut keys)
	}
}

fn markused_push_unique_key(key string, mut keys []string) {
	if key.len == 0 || key in keys {
		return
	}
	keys << key
}

fn fn_decl_key_is_exact_for_info(name string, decl_name string, module_name string) bool {
	qname := qualify_fn(module_name, decl_name)
	if name == qname {
		return true
	}
	lowered := markused_c_name(qname)
	return lowered != qname && name == lowered
}

fn add_safe_decl_alias(callee string, callee_info FnDeclInfo, a &flat.FlatAst, mut used map[string]bool, mut queue []string) {
	alias := a.node(callee_info.node_id).value
	if fn_decl_key_is_exact_for_info(callee, alias, callee_info.module) {
		return
	}
	alias_lowered := markused_c_name(alias)
	if (callee == alias || callee == alias_lowered)
		&& markused_is_unqualified_receiver_method_name(alias) {
		return
	}
	mut aliases := []string{cap: 3}
	aliases << alias
	qalias := qualify_fn(callee_info.module, alias)
	if qalias != alias {
		aliases << qalias
	}
	lowered := markused_c_name(qalias)
	if lowered != qalias {
		aliases << lowered
	}
	for candidate in aliases {
		if candidate != callee {
			enqueue(candidate, mut used, mut queue)
		}
	}
}

fn markused_is_unqualified_receiver_method_name(name string) bool {
	if !name.contains('.') || name.all_before('.').contains('.') {
		return false
	}
	receiver := name.all_before('.')
	return receiver.len > 0 && receiver[0] >= `A` && receiver[0] <= `Z`
}

// valid_symbol_name supports valid symbol name handling for markused.
fn valid_symbol_name(name string) bool {
	return name.len > 0 && name.len < 512
}

// markused_clone_bool_map returns a value clone even when the source is passed
// from a `mut map` parameter. v3 self-host cgen otherwise infers a `map*` local
// for `param.clone()` in a few recursive markused scanners.
fn markused_clone_bool_map(src map[string]bool) map[string]bool {
	return src.clone()
}

fn markused_clone_string_map(src map[string]string) map[string]string {
	return src.clone()
}

// enqueue_initializer_calls supports enqueue initializer calls handling for markused.
fn enqueue_initializer_calls(a &flat.FlatAst, collector CallCollector, imports map[string]string, fn_decls map[string]FnDeclInfo, mut used map[string]bool, mut queue []string, initial_uses_generics bool) bool {
	mut cur_module := ''
	mut calls := []string{cap: 32}
	mut uses_generics := initial_uses_generics
	for node_idx in collector.tc.top_level_idx {
		node := a.nodes[node_idx]
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.const_decl, .global_decl {
				for i in 0 .. node.children_count {
					field := a.child_node(&node, i)
					if field.children_count == 0 {
						continue
					}
					calls.clear()
					if uses_generics {
						collector.collect_calls(field, cur_module, imports, '', '', mut calls)
					} else {
						uses_generics = collector.collect_calls_with_generic_usage(field,
							cur_module, imports, '', '', mut calls)
					}
					for callee in calls {
						enqueue_initializer_callee(callee, fn_decls, a, mut used, mut queue)
					}
				}
			}
			else {}
		}
	}
	return uses_generics
}

fn enqueue_initializer_ref_calls(a &flat.FlatAst, collector CallCollector, imports map[string]string, fn_decls map[string]FnDeclInfo, initializer_ref string, mut processed map[string]bool, mut calls []string, mut used map[string]bool, mut queue []string) {
	if initializer_ref in processed {
		return
	}
	info := collector.const_decls[initializer_ref] or { return }
	processed[initializer_ref] = true
	calls.clear()
	collector.collect_calls(a.node(info.expr_id), info.module, imports, '', '', mut calls)
	for callee in calls {
		enqueue_initializer_callee(callee, fn_decls, a, mut used, mut queue)
	}
}

fn enqueue_initializer_callee(callee string, fn_decls map[string]FnDeclInfo, a &flat.FlatAst, mut used map[string]bool, mut queue []string) {
	if !valid_symbol_name(callee) {
		return
	}
	if callee_info := fn_decls[callee] {
		enqueue(callee, mut used, mut queue)
		add_safe_decl_alias(callee, callee_info, a, mut used, mut queue)
	} else {
		enqueue(callee, mut used, mut queue)
	}
}

fn markused_module_has_reachable_initializer(module_name string, reachable_modules map[string]bool) bool {
	if module_name.len == 0 {
		return '' in reachable_modules || 'main' in reachable_modules
	}
	return module_name in reachable_modules
}

fn markused_reachable_modules(a &flat.FlatAst, tc &types.TypeChecker) map[string]bool {
	mut module_imports := map[string][]string{}
	mut roots := []string{}
	mut has_user_root := false
	for file_idx in tc.top_level_idx {
		file_node := a.nodes[file_idx]
		if file_node.kind != .file {
			continue
		}
		module_name := markused_top_level_file_module_name(a, file_node)
		if file_idx >= a.user_code_start
			&& (!markused_file_is_vlib(file_node.value) || !has_user_root) {
			has_user_root = true
			roots << module_name
			if module_name.len == 0 {
				roots << 'main'
			}
		}
		mut imports := module_imports[module_name] or { []string{} }
		if file_idx >= a.user_code_start || !markused_file_is_test(file_node.value) {
			for i in 0 .. file_node.children_count {
				child := a.child_node(&file_node, i)
				if child.kind == .import_decl && child.value.len > 0 {
					imports << child.value
				}
			}
		}
		module_imports[module_name] = imports
	}
	roots << ''
	roots << 'main'
	roots << 'builtin'
	mut reachable := map[string]bool{}
	mut queue := []string{}
	for root in roots {
		if root in reachable {
			continue
		}
		reachable[root] = true
		queue << root
	}
	for qi := 0; qi < queue.len; qi++ {
		module_name := queue[qi]
		for imported in module_imports[module_name] or { []string{} } {
			if imported in reachable {
				continue
			}
			reachable[imported] = true
			queue << imported
		}
	}
	return reachable
}

fn markused_file_is_vlib(file string) bool {
	return file.contains('/vlib/') || file.starts_with('vlib/') || file.contains('\\vlib\\')
		|| file.starts_with('vlib\\')
}

fn markused_file_is_test(file string) bool {
	return file.ends_with('_test.v') || file.ends_with('_test.c.v') || file.ends_with('_test.js.v')
}

fn enqueue_top_level_calls(a &flat.FlatAst, collector CallCollector, fn_decls map[string]FnDeclInfo, has_entry_main bool, mut used map[string]bool, mut queue []string, initial_uses_generics bool) bool {
	if has_entry_main {
		return initial_uses_generics
	}
	mut calls := []string{cap: 32}
	mut uses_generics := initial_uses_generics
	for file_idx, file_node in a.nodes {
		if !markused_should_scan_top_level_file(a, file_idx, file_node) {
			continue
		}
		module_name := markused_top_level_file_module_name(a, file_node)
		file_imports := markused_top_level_file_imports(a, file_node)
		mut local_values := map[string]bool{}
		mut local_types := map[string]string{}
		for i in 0 .. file_node.children_count {
			child_id := a.child(&file_node, i)
			if int(child_id) < a.user_code_start {
				continue
			}
			child := a.node(child_id)
			if !markused_is_top_level_stmt(child) {
				continue
			}
			calls.clear()
			collector.collect_top_level_stmt_calls(child_id, module_name, file_imports, mut
				local_values, mut local_types, mut calls)
			if !uses_generics
				&& collector.node_tree_uses_generics(child_id, module_name, file_imports) {
				uses_generics = true
			}
			for callee in calls {
				if callee_info := fn_decls[callee] {
					enqueue(callee, mut used, mut queue)
					add_safe_decl_alias(callee, callee_info, a, mut used, mut queue)
				} else {
					enqueue(callee, mut used, mut queue)
				}
			}
		}
	}
	return uses_generics
}

fn markused_has_entry_main_indexed(a &flat.FlatAst, tc &types.TypeChecker) bool {
	mut cur_module := ''
	for node_idx in tc.top_level_idx {
		node := a.nodes[node_idx]
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				if node.value == 'main' && (cur_module.len == 0 || cur_module == 'main') {
					return true
				}
			}
			else {}
		}
	}
	return false
}

fn markused_has_entry_main(a &flat.FlatAst) bool {
	mut cur_module := ''
	for node in a.nodes {
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				if node.value == 'main' && (cur_module.len == 0 || cur_module == 'main') {
					return true
				}
			}
			else {}
		}
	}
	return false
}

fn markused_should_scan_top_level_file(a &flat.FlatAst, file_idx int, file_node flat.Node) bool {
	if file_idx < a.user_code_start || file_node.kind != .file || file_node.children_count == 0 {
		return false
	}
	module_name := markused_top_level_file_module_name(a, file_node)
	return module_name.len == 0 || module_name == 'main'
}

fn markused_top_level_file_module_name(a &flat.FlatAst, file_node flat.Node) string {
	for i in 0 .. file_node.children_count {
		child := a.child_node(&file_node, i)
		if child.kind == .module_decl {
			return child.value
		}
	}
	return ''
}

fn markused_top_level_file_imports(a &flat.FlatAst, file_node flat.Node) map[string]string {
	mut imports := map[string]string{}
	for i in 0 .. file_node.children_count {
		child := a.child_node(&file_node, i)
		if child.kind == .import_decl {
			imports[child.typ] = child.value
		}
	}
	return imports
}

fn markused_is_top_level_stmt(node flat.Node) bool {
	return match node.kind {
		.expr_stmt, .assign, .decl_assign, .global_decl, .selector_assign, .index_assign,
		.for_stmt, .for_in_stmt, .if_expr, .match_stmt, .assert_stmt, .defer_stmt, .block {
			true
		}
		else {
			false
		}
	}
}

// enqueue supports enqueue handling for markused.
fn enqueue(name string, mut used map[string]bool, mut queue []string) bool {
	if !valid_symbol_name(name) {
		return false
	}
	if name in used {
		return false
	}
	used[name] = true
	queue << name
	return true
}

// FnDeclInfo stores fn decl info metadata used by markused.
struct FnDeclInfo {
	node_id flat.NodeId
	module  string
}

// StructDeclInfo stores struct decl info metadata used by markused.
struct StructDeclInfo {
	node_id flat.NodeId
	module  string
}

// ConstDeclInfo stores const decl info metadata used by markused.
struct ConstDeclInfo {
	expr_id flat.NodeId
	module  string
}

// CallCollector represents call collector data used by markused.
struct CallCollector {
	a              &flat.FlatAst      = unsafe { nil }
	tc             &types.TypeChecker = unsafe { nil }
	fn_decls       map[string]FnDeclInfo
	fn_suffixes    map[string]bool
	struct_decls   map[string]StructDeclInfo
	const_decls    map[string]ConstDeclInfo
	const_suffixes map[string]bool
	// Unqualified selective-import symbol -> all known alias targets. This is
	// global because generic detection only needs to know whether any target
	// requires monomorphization.
	selective_alias_targets map[string][]string
	// Gate set for collect_interface_boxed_generic_methods: every fn_param_types
	// key with at least one interface-typed parameter, plus its post-'.' and
	// post-'__' short spellings (see markused_interface_param_gate). Lets the
	// per-call-node scan bail out with a couple of map probes instead of building
	// signature-candidate spellings (c_name/qualify allocations) for every call.
	iface_param_gate map[string]bool
}

// markused_interface_param_gate builds CallCollector.iface_param_gate. Any raw
// callee spelling whose expanded signature candidates could hit an
// interface-param signature is covered by the key itself, its post-dot short
// name, or its post-'__' short name (candidate expansion only prepends module
// qualifiers or applies c_name, both of which preserve the final segment).
fn markused_interface_param_gate(tc &types.TypeChecker) map[string]bool {
	mut gate := map[string]bool{}
	for name, params in tc.fn_param_types {
		mut has_iface := false
		for p in params {
			if types.unwrap_pointer(p) is types.Interface {
				has_iface = true
				break
			}
		}
		if !has_iface {
			continue
		}
		gate[name] = true
		short_dot := name.all_after_last('.')
		if short_dot != name {
			gate[short_dot] = true
		}
		if name.contains('__') {
			short_us := name.all_after_last('__')
			if short_us != name {
				gate[short_us] = true
			}
		}
	}
	return gate
}

fn markused_selective_alias_targets(tc &types.TypeChecker) map[string][]string {
	mut result := map[string][]string{}
	for key, candidates in tc.file_selective_imports {
		name := key.all_after_last('\n')
		for candidate in candidates {
			target := tc.type_aliases[candidate] or { continue }
			mut targets := result[name]
			if target !in targets {
				targets << target
				result[name] = targets
			}
		}
	}
	return result
}

// may_target_interface_params reports whether `name` (a raw callee spelling)
// could expand to a signature in iface_param_gate.
fn (c &CallCollector) may_target_interface_params(name string) bool {
	if name.len == 0 {
		return false
	}
	if name in c.iface_param_gate {
		return true
	}
	short := name.all_after_last('.')
	return short != name && short in c.iface_param_gate
}

// enqueue_auto_roots supports enqueue auto roots handling for markused.
fn enqueue_auto_roots(fn_decls map[string]FnDeclInfo, reachable_modules map[string]bool, mut used map[string]bool, mut queue []string) {
	for name, info in fn_decls {
		if !is_auto_root_fn(name) {
			continue
		}
		if !markused_module_has_reachable_initializer(info.module, reachable_modules) {
			continue
		}
		enqueue(name, mut used, mut queue)
		short_name := name.all_after_last('.')
		if short_name != name {
			enqueue(short_name, mut used, mut queue)
		}
		qualified_name := qualify_fn(info.module, short_name)
		if qualified_name != name {
			enqueue(qualified_name, mut used, mut queue)
		}
	}
}

fn enqueue_export_roots(a &flat.FlatAst, mut used map[string]bool, mut queue []string) {
	if a.export_fn_names.len == 0 {
		return
	}
	mut cur_module := ''
	for node in a.nodes {
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				qname := qualify_fn(cur_module, node.value)
				if qname in a.export_fn_names {
					enqueue(qname, mut used, mut queue)
				}
			}
			else {}
		}
	}
}

fn enqueue_veb_handler_roots(a &flat.FlatAst, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	mut cur_module := ''
	for node in a.nodes {
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.fn_decl {
				if markused_fn_needs_implicit_veb_ctx(a, tc, cur_module, node) {
					enqueue(qualify_fn(cur_module, node.value), mut used, mut queue)
				}
			}
			else {}
		}
	}
}

fn markused_fn_needs_implicit_veb_ctx(a &flat.FlatAst, tc &types.TypeChecker, cur_module string, node flat.Node) bool {
	return markused_fn_returns_veb_result(tc, node) && markused_fn_has_receiver_param(a, node)
		&& !markused_fn_receiver_type_is_context(a, node) && !markused_fn_has_param(a, node, 'ctx')
		&& markused_type_name_known_in_module(tc, cur_module, 'Context')
}

fn markused_fn_returns_veb_result(tc &types.TypeChecker, node flat.Node) bool {
	if node.typ == 'veb.Result' {
		return true
	}
	ret := tc.parse_type(node.typ)
	return ret.name() == 'veb.Result'
}

fn markused_fn_has_receiver_param(a &flat.FlatAst, node flat.Node) bool {
	if !node.value.contains('.') || node.children_count == 0 {
		return false
	}
	first := a.child_node(&node, 0)
	if first.kind != .param || first.typ.len == 0 {
		return false
	}
	receiver := node.value.all_before_last('.').all_after_last('.')
	param_type := first.typ.trim_left('&').all_after_last('.')
	return receiver == param_type
}

fn markused_fn_receiver_type_is_context(a &flat.FlatAst, node flat.Node) bool {
	if !markused_fn_has_receiver_param(a, node) {
		return false
	}
	first := a.child_node(&node, 0)
	return first.typ.trim_left('&').all_after_last('.') == 'Context'
}

fn markused_fn_has_param(a &flat.FlatAst, node flat.Node, name string) bool {
	for i in 0 .. node.children_count {
		param := a.child_node(&node, i)
		if param.kind == .param && param.value == name {
			return true
		}
	}
	return false
}

fn markused_type_name_known_in_module(tc &types.TypeChecker, module_name string, typ string) bool {
	qtyp := qualify_fn(module_name, typ)
	return qtyp in tc.type_aliases || qtyp in tc.structs || qtyp in tc.interface_names
		|| qtyp in tc.enum_names || qtyp in tc.sum_types
}

fn enqueue_test_file_roots(a &flat.FlatAst, test_files map[string]bool, mut used map[string]bool, mut queue []string) {
	if test_files.len == 0 {
		return
	}
	for file_idx, file_node in a.nodes {
		if !is_user_test_file_node(a, file_idx, file_node, test_files) {
			continue
		}
		module_name := test_file_module_name(a, file_node)
		mut decl_ids := []flat.NodeId{}
		markused_collect_test_harness_decl_ids(a, file_node, mut decl_ids)
		for child_id in decl_ids {
			child := a.node(child_id)
			if is_test_harness_root_name(child.value) {
				enqueue(qualify_fn(module_name, child.value), mut used, mut queue)
			}
		}
	}
}

fn markused_collect_test_harness_decl_ids(a &flat.FlatAst, node flat.Node, mut ids []flat.NodeId) {
	if node.kind != .file && node.kind != .block {
		return
	}
	for i in 0 .. node.children_count {
		child_id := a.child(&node, i)
		if int(child_id) < a.user_code_start {
			continue
		}
		child := a.node(child_id)
		if child.kind == .fn_decl {
			ids << child_id
		} else if child.kind == .block {
			markused_collect_test_harness_decl_ids(a, child, mut ids)
		}
	}
}

fn is_user_test_file_node(a &flat.FlatAst, file_idx int, file_node flat.Node, test_files map[string]bool) bool {
	if file_idx < a.user_code_start || file_node.kind != .file || file_node.children_count == 0 {
		return false
	}
	return test_files[file_node.value]
}

fn test_file_module_name(a &flat.FlatAst, file_node flat.Node) string {
	for i in 0 .. file_node.children_count {
		child := a.child_node(&file_node, i)
		if child.kind == .module_decl {
			return child.value
		}
	}
	return ''
}

fn is_test_harness_root_name(name string) bool {
	return name.starts_with('test_')
		|| name in ['testsuite_begin', 'testsuite_end', 'before_each', 'after_each']
}

// enqueue_main_module_roots supports enqueue main module roots handling for markused.
fn enqueue_main_module_roots(fn_decls map[string]FnDeclInfo, mut used map[string]bool, mut queue []string) {
	for name, info in fn_decls {
		if info.module != 'main' || name != 'main' {
			continue
		}
		enqueue(name, mut used, mut queue)
	}
}

// is_auto_root_fn reports whether is auto root fn applies in markused.
fn is_auto_root_fn(name string) bool {
	short_name := name.all_after_last('.')
	return short_name == 'init'
}

// enqueue_detected_runtime_helpers supports enqueue detected runtime helpers handling for markused.
fn enqueue_detected_runtime_helpers(a &flat.FlatAst, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	mut needs_optional_helpers := false
	mut needs_string_interp_helpers := false
	mut needs_string_plus_helper := false
	mut needs_string_membership_helpers := false
	mut needs_new_map := false
	mut needs_map_iteration_snapshot := false
	mut needs_channel_helpers := false
	mut needs_channel_select_helpers := false
	mut needs_channel_str_helpers := false
	mut needs_f32_eq_epsilon := false
	mut needs_ierror_equality_dispatch := false
	mut needs_shared_runtime := false
	mut channel_stringify_cache := map[string]int{}
	mut ierror_equality_cache := map[string]int{}
	mut cur_module := ''
	mut imports := map[string]string{}
	for node in a.nodes {
		if node.typ.len > 0 {
			if !needs_channel_helpers && markused_type_text_is_channel(node.typ) {
				needs_channel_helpers = true
			}
			if !needs_shared_runtime && markused_type_text_needs_shared_runtime(node.typ) {
				needs_shared_runtime = true
			}
		}
		match node.kind {
			.file {
				cur_module = ''
				imports = map[string]string{}
			}
			.module_decl {
				cur_module = node.value
			}
			.import_decl {
				imports[node.typ] = node.value
			}
			.fn_decl {
				if !needs_optional_helpers && type_string_needs_optional_helpers(node.typ) {
					needs_optional_helpers = true
				}
			}
			.param, .field_decl, .field_init, .const_field {
				if !needs_optional_helpers && type_string_needs_optional_helpers(node.typ) {
					needs_optional_helpers = true
				}
			}
			.none_expr {
				needs_optional_helpers = true
			}
			.or_expr {
				needs_optional_helpers = true
				if node.children_count > 0 {
					expr_id := a.child(&node, 0)
					expr_type := tc.expr_type(expr_id) or { tc.resolve_type(expr_id) }
					if type_needs_zero_map(expr_type) {
						needs_new_map = true
					}
				}
			}
			.call {
				if node.children_count > 0 {
					fn_node := a.child_node(&node, 0)
					if !needs_channel_str_helpers && fn_node.kind == .selector
						&& fn_node.value == 'str' && fn_node.children_count > 0
						&& markused_expr_stringifies_channel(a, tc, a.child(fn_node, 0), cur_module, mut channel_stringify_cache) {
						needs_channel_str_helpers = true
					}
					if fn_node.kind == .ident
						&& (fn_node.value == 'error' || fn_node.value == 'error_with_code') {
						needs_optional_helpers = true
					}
					if fn_node.kind == .ident && fn_node.value == 'flag_default_value' {
						enqueue('escape_default_string', mut used, mut queue)
					}
					if fn_node.kind == .selector
						&& fn_node.value in ['trim_space', 'trim_space_left', 'trim_space_right', 'to_upper', 'to_upper_ascii', 'to_lower', 'to_lower_ascii'] {
						enqueue('string.${fn_node.value}', mut used, mut queue)
					}
					if markused_call_lowers_to_join_path_single(a, fn_node, imports) {
						enqueue('join_path_single', mut used, mut queue)
						enqueue('os.join_path_single', mut used, mut queue)
					}
					if fn_node.kind == .selector && fn_node.value == 'runes_iterator' {
						enqueue('RunesIterator.next', mut used, mut queue)
					}
					if fn_node.kind == .ident
						&& fn_node.value in ['print', 'println', 'eprint', 'eprintln']
						&& node.children_count >= 2 {
						if !needs_channel_str_helpers
							&& markused_expr_stringifies_channel(a, tc, a.child(&node, 1), cur_module, mut channel_stringify_cache) {
							needs_channel_str_helpers = true
						}
						enqueue_stringified_custom_str_method(a.child(&node, 1), cur_module, tc, mut
							used, mut queue)
					}
				}
			}
			.select_stmt {
				needs_channel_helpers = true
				needs_channel_select_helpers = true
			}
			.struct_init {
				if node.value.starts_with('chan ') {
					needs_channel_helpers = true
				}
			}
			.lock_expr {
				needs_shared_runtime = true
			}
			.infix {
				if node.op == .arrow {
					needs_channel_helpers = true
				}
				if node.op in [.eq, .ne] {
					if !needs_f32_eq_epsilon && markused_infix_needs_f32_eq_epsilon(a, tc, node) {
						needs_f32_eq_epsilon = true
					}
					if !needs_ierror_equality_dispatch && node.children_count >= 2 {
						lhs_type := tc.resolve_type(a.child(&node, 0))
						rhs_type := tc.resolve_type(a.child(&node, 1))
						needs_ierror_equality_dispatch =
							markused_type_equality_uses_ierror(lhs_type, tc, mut ierror_equality_cache)
							|| markused_type_equality_uses_ierror(rhs_type, tc, mut ierror_equality_cache)
					}
				}
			}
			.prefix {
				if node.op == .arrow {
					needs_channel_helpers = true
				}
			}
			.string_interp {
				needs_string_interp_helpers = true
				needs_string_plus_helper = true
				for i in 0 .. node.children_count {
					part_id := a.child(&node, i)
					if !needs_channel_str_helpers
						&& markused_expr_stringifies_channel(a, tc, part_id, cur_module, mut channel_stringify_cache) {
						needs_channel_str_helpers = true
					}
					enqueue_stringified_custom_str_method(part_id, cur_module, tc, mut used, mut
						queue)
				}
			}
			.assign {
				if node.op == .plus_assign && node.children_count == 2 {
					lhs_id := a.child(&node, 0)
					rhs_id := a.child(&node, 1)
					rhs := a.node(rhs_id)
					lhs_type := markused_membership_container_type(tc, tc.resolve_type(lhs_id))
					rhs_type := markused_membership_container_type(tc, tc.resolve_type(rhs_id))
					if lhs_type == 'string' || rhs_type == 'string'
						|| rhs.kind in [.string_literal, .string_interp] {
						needs_string_plus_helper = true
					}
				}
			}
			.in_expr {
				if node.children_count >= 2 {
					lhs_id := a.child(&node, 0)
					if !needs_ierror_equality_dispatch {
						needs_ierror_equality_dispatch = markused_type_equality_uses_ierror(tc.resolve_type(lhs_id),
							tc, mut ierror_equality_cache)
					}
					if !needs_string_membership_helpers {
						rhs_id := a.child(&node, 1)
						rhs_type := markused_membership_container_type(tc, tc.resolve_type(rhs_id))
						if rhs_type == 'string' {
							needs_string_membership_helpers = true
						}
					}
				}
			}
			.map_init {
				needs_new_map = true
			}
			.for_in_stmt {
				if node.value.int() == 3 && node.children_count > 2 {
					container_id := a.child(&node, 2)
					container_type := tc.resolve_type(container_id)
					if types.unwrap_pointer(container_type) is types.Map {
						needs_map_iteration_snapshot = true
					}
					if info := tc.iterator_for_in_next_call_info(container_type) {
						enqueue(info.name, mut used, mut queue)
						needs_optional_helpers = true
					}
				}
			}
			.enum_decl {
				// A `[flag]` enum's synthesized `<Enum>__autostr` helper (emitted
				// unconditionally in cgen) builds its `Enum{.a | .b}` string with
				// `string__plus`. markused never walks that generated body, so without
				// seeding the helper here it can be pruned when the program has no other
				// string concatenation, leaving the autostr calling an undefined
				// `string__plus`.
				if node.typ == 'flag' {
					needs_string_plus_helper = true
				}
			}
			else {}
		}
	}
	if needs_optional_helpers {
		for helper in ['IError.str', 'error', 'error_with_code'] {
			enqueue(helper, mut used, mut queue)
		}
	}
	if needs_string_interp_helpers {
		for helper in ['strings.new_builder', 'strings.Builder.write_string', 'strings.Builder.str',
			'string_plus_many'] {
			enqueue(helper, mut used, mut queue)
		}
	}
	if needs_string_plus_helper {
		enqueue('string__plus', mut used, mut queue)
	}
	if needs_string_membership_helpers {
		for helper in ['string__contains', 'string__contains_u8'] {
			enqueue(helper, mut used, mut queue)
		}
	}
	if needs_new_map {
		enqueue('new_map', mut used, mut queue)
	}
	if needs_map_iteration_snapshot {
		for helper in ['map.clone', 'map__clone', 'map.free', 'map__free'] {
			enqueue(helper, mut used, mut queue)
		}
	}
	if needs_channel_helpers {
		for helper in ['sync.new_channel_st', 'sync.Channel.push', 'sync.Channel.pop',
			'sync.Channel.close', 'sync.Channel.len', 'sync.Channel.closed', 'new_channel_st',
			'Channel.push', 'Channel.pop', 'Channel.close', 'Channel.len', 'Channel.closed'] {
			enqueue(helper, mut used, mut queue)
		}
	}
	if needs_channel_select_helpers {
		for helper in ['sync.channel_select', 'sync.channel_select_lang', 'channel_select',
			'channel_select_lang', 'rand.init', 'array.free', 'array__free', 'time.sleep',
			'time__sleep'] {
			enqueue(helper, mut used, mut queue)
		}
	}
	if needs_channel_str_helpers {
		for helper in ['string__plus', 'int.str', 'int__str'] {
			enqueue(helper, mut used, mut queue)
		}
	}
	if needs_f32_eq_epsilon {
		enqueue('f32.eq_epsilon', mut used, mut queue)
		enqueue('f32__eq_epsilon', mut used, mut queue)
	}
	if needs_ierror_equality_dispatch {
		enqueue_ierror_equality_dispatch_helpers(tc, mut used, mut queue)
	}
	if needs_shared_runtime {
		for helper in ['sync.cpanic', 'sync.cpanic_errno', 'sync.should_be_zero', 'sync.RwMutex.init',
			'sync.RwMutex.lazy_init', 'sync.RwMutex.lock', 'sync.RwMutex.unlock',
			'sync.RwMutex.rlock', 'sync.RwMutex.runlock', 'cpanic', 'cpanic_errno', 'should_be_zero',
			'RwMutex.init', 'RwMutex.lazy_init', 'RwMutex.lock', 'RwMutex.unlock', 'RwMutex.rlock',
			'RwMutex.runlock'] {
			enqueue(helper, mut used, mut queue)
		}
	}
}

fn enqueue_ierror_equality_dispatch_helpers(tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	for iface_name in tc.interface_names.keys() {
		if !markused_is_ierror_interface_name(iface_name) {
			continue
		}
		for method in ['msg', 'code'] {
			dispatch_key := '${iface_name}.${method}'
			enqueue(dispatch_key, mut used, mut queue)
			enqueue(markused_c_name(dispatch_key), mut used, mut queue)
		}
	}
	enqueue_used_interface_dispatch_implementers(tc, mut used, mut queue)
}

fn markused_type_equality_uses_ierror(typ types.Type, tc &types.TypeChecker, mut cache map[string]int) bool {
	key := typ.name()
	if cached := cache[key] {
		return cached == 1
	}
	cache[key] = 2
	result := markused_type_equality_uses_ierror_uncached(typ, tc, mut cache)
	cache[key] = if result { 1 } else { -1 }
	return result
}

fn markused_type_equality_uses_ierror_uncached(typ types.Type, tc &types.TypeChecker, mut cache map[string]int) bool {
	match typ {
		types.Alias {
			return markused_type_equality_uses_ierror(typ.base_type, tc, mut cache)
		}
		types.OptionType {
			return markused_type_equality_uses_ierror(typ.base_type, tc, mut cache)
		}
		types.ResultType {
			return markused_type_equality_uses_ierror(typ.base_type, tc, mut cache)
		}
		types.Array {
			return markused_type_equality_uses_ierror(typ.elem_type, tc, mut cache)
		}
		types.ArrayFixed {
			return markused_type_equality_uses_ierror(typ.elem_type, tc, mut cache)
		}
		types.Map {
			return markused_type_equality_uses_ierror(typ.key_type, tc, mut cache)
				|| markused_type_equality_uses_ierror(typ.value_type, tc, mut cache)
		}
		types.Struct {
			for field in markused_struct_fields(typ.name, tc) {
				if markused_type_equality_uses_ierror(field.typ, tc, mut cache) {
					return true
				}
			}
		}
		types.SumType {
			for variant in markused_sum_variants(typ.name, tc) {
				if markused_type_equality_uses_ierror(tc.parse_type(variant), tc, mut cache) {
					return true
				}
			}
		}
		types.Interface {
			if markused_is_ierror_interface_name(typ.name) {
				return true
			}
			for concrete in tc.interface_impl_names(typ.name) {
				if markused_type_equality_uses_ierror(tc.parse_type(concrete), tc, mut cache) {
					return true
				}
			}
		}
		types.MultiReturn {
			for item in typ.types {
				if markused_type_equality_uses_ierror(item, tc, mut cache) {
					return true
				}
			}
		}
		else {}
	}

	return false
}

fn markused_type_text_needs_shared_runtime(typ string) bool {
	clean := typ.trim_space()
	return clean.starts_with('shared ') || clean.contains(' shared ')
}

fn markused_infix_needs_f32_eq_epsilon(a &flat.FlatAst, tc &types.TypeChecker, node flat.Node) bool {
	if node.children_count < 2 {
		return false
	}
	return markused_type_is_f32(tc.resolve_type(a.child(&node, 0)))
		|| markused_type_is_f32(tc.resolve_type(a.child(&node, 1)))
}

fn markused_type_is_f32(typ types.Type) bool {
	if typ is types.Alias {
		return markused_type_is_f32(typ.base_type)
	}
	return typ.name() == 'f32'
}

fn markused_expr_stringifies_channel(a &flat.FlatAst, tc &types.TypeChecker, id flat.NodeId, cur_module string, mut cache map[string]int) bool {
	typ := tc.expr_type(id) or { tc.resolve_type(id) }
	return markused_type_stringifies_channel(typ, cur_module, tc, mut cache)
}

fn markused_type_stringifies_channel(typ types.Type, cur_module string, tc &types.TypeChecker, mut cache map[string]int) bool {
	key := '${cur_module}\x01${typ.name()}'
	if cached := cache[key] {
		return cached == 1
	}
	cache[key] = 2
	result := markused_type_stringifies_channel_uncached(typ, cur_module, tc, mut cache)
	cache[key] = if result { 1 } else { -1 }
	return result
}

fn markused_type_stringifies_channel_uncached(typ types.Type, cur_module string, tc &types.TypeChecker, mut cache map[string]int) bool {
	match typ {
		types.Channel {
			return true
		}
		types.Alias {
			if markused_type_has_custom_str(typ.name, cur_module, tc) {
				return false
			}
			return markused_type_stringifies_channel(typ.base_type, cur_module, tc, mut cache)
		}
		types.Pointer {
			return markused_type_stringifies_channel(typ.base_type, cur_module, tc, mut cache)
		}
		types.OptionType {
			return markused_type_stringifies_channel(typ.base_type, cur_module, tc, mut cache)
		}
		types.ResultType {
			return markused_type_stringifies_channel(typ.base_type, cur_module, tc, mut cache)
		}
		types.Array {
			return markused_type_stringifies_channel(typ.elem_type, cur_module, tc, mut cache)
		}
		types.ArrayFixed {
			return markused_type_stringifies_channel(typ.elem_type, cur_module, tc, mut cache)
		}
		types.Map {
			return markused_type_stringifies_channel(typ.key_type, cur_module, tc, mut cache)
				|| markused_type_stringifies_channel(typ.value_type, cur_module, tc, mut cache)
		}
		types.Struct {
			if markused_type_has_custom_str(typ.name, cur_module, tc) {
				return false
			}
			if typ.name.contains('chan ') {
				return true
			}
			for field in markused_struct_fields(typ.name, tc) {
				if markused_type_stringifies_channel(field.typ, cur_module, tc, mut cache) {
					return true
				}
			}
		}
		types.SumType {
			if markused_type_has_custom_str(typ.name, cur_module, tc) {
				return false
			}
			for variant in markused_sum_variants(typ.name, tc) {
				if markused_type_stringifies_channel(tc.parse_type(variant), cur_module, tc, mut
					cache)
				{
					return true
				}
			}
		}
		types.Interface {
			if 'str' in tc.interface_abstract_method_names(typ.name) {
				return false
			}
			for concrete in tc.interface_impl_names(typ.name) {
				if markused_type_stringifies_channel(tc.parse_type(concrete), cur_module, tc, mut
					cache)
				{
					return true
				}
			}
		}
		types.MultiReturn {
			for item in typ.types {
				if markused_type_stringifies_channel(item, cur_module, tc, mut cache) {
					return true
				}
			}
		}
		else {}
	}

	return false
}

fn markused_type_has_custom_str(name string, cur_module string, tc &types.TypeChecker) bool {
	for candidate in stringification_type_candidates(name, cur_module) {
		if '${candidate}.str' in tc.fn_ret_types
			|| '${markused_c_name(candidate)}__str' in tc.fn_ret_types {
			return true
		}
	}
	for candidate in generic_stringification_type_candidates(name, cur_module, tc) {
		if '${candidate}.str' in tc.fn_ret_types
			|| '${markused_c_name(candidate)}__str' in tc.fn_ret_types {
			return true
		}
	}
	return false
}

fn markused_struct_fields(name string, tc &types.TypeChecker) []types.StructField {
	mut candidates := [name]
	base_name := types.generic_base_name(name)
	if base_name != name {
		candidates << base_name
	}
	qualified := tc.qualify_name(name)
	if qualified !in candidates {
		candidates << qualified
	}
	if name.contains('.') {
		short_name := name.all_after_last('.')
		if short_name !in candidates {
			candidates << short_name
		}
	}
	for candidate in candidates {
		if fields := tc.structs[candidate] {
			return fields
		}
	}
	return []types.StructField{}
}

fn markused_sum_variants(name string, tc &types.TypeChecker) []string {
	mut candidates := [name]
	qualified := tc.qualify_name(name)
	if qualified !in candidates {
		candidates << qualified
	}
	if name.contains('.') {
		short_name := name.all_after_last('.')
		if short_name !in candidates {
			candidates << short_name
		}
	}
	for candidate in candidates {
		if variants := tc.sum_types[candidate] {
			return variants
		}
	}
	return []string{}
}

fn markused_type_text_is_channel(raw string) bool {
	mut clean := raw.trim_space()
	for {
		if clean.starts_with('&') {
			clean = clean[1..].trim_space()
			continue
		}
		if clean.starts_with('mut ') {
			clean = clean[4..].trim_space()
			continue
		}
		break
	}
	return clean.starts_with('chan ') || clean == 'chan'
}

fn markused_call_lowers_to_join_path_single(a &flat.FlatAst, fn_node flat.Node, imports map[string]string) bool {
	if fn_node.kind == .ident {
		return fn_node.value == 'join_path'
	}
	if fn_node.kind != .selector || fn_node.value != 'join_path' || fn_node.children_count == 0 {
		return false
	}
	base_id := a.child(&fn_node, 0)
	if int(base_id) < 0 {
		return false
	}
	base := a.node(base_id)
	if base.kind != .ident {
		return false
	}
	return base.value == 'os' || imports[base.value] == 'os'
}

fn markused_type_name_lowers_to_map_str(type_name string, tc &types.TypeChecker) bool {
	if markused_clean_map_type(type_name).starts_with('map[') {
		return true
	}
	return markused_type_lowers_to_map_str(tc.parse_type(type_name))
}

fn markused_type_lowers_to_map_str(typ0 types.Type) bool {
	mut typ := typ0
	for _ in 0 .. 8 {
		if typ is types.Alias {
			typ = typ.base_type
			continue
		}
		if typ is types.Pointer {
			typ = typ.base_type
			continue
		}
		break
	}
	return typ is types.Map || markused_clean_map_type(typ.name()).starts_with('map[')
}

// enqueue_stringified_custom_str_method supports enqueue_stringified_custom_str_method handling.
fn enqueue_stringified_custom_str_method(expr_id flat.NodeId, cur_module string, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	mut typ := tc.expr_type(expr_id) or { tc.resolve_type(expr_id) }
	for _ in 0 .. 8 {
		if typ is types.Alias {
			typ = typ.base_type
			continue
		}
		if typ is types.OptionType {
			typ = typ.base_type
			continue
		}
		if typ is types.ResultType {
			typ = typ.base_type
			continue
		}
		break
	}
	type_name := typ.name()
	match typ {
		types.Primitive, types.Rune, types.Char, types.ISize, types.USize, types.String {
			enqueue_stringified_primitive_helpers(type_name, mut used, mut queue)
		}
		types.Enum {
			enqueue_enum_str_method(typ.name, cur_module, tc, mut used, mut queue)
		}
		types.Struct {
			enqueue_structlike_str_method(typ.name, cur_module, tc, mut used, mut queue)
		}
		types.SumType {
			enqueue_structlike_str_method(typ.name, cur_module, tc, mut used, mut queue)
		}
		types.Interface {
			enqueue_interface_str_methods(typ.name, tc, mut used, mut queue)
		}
		else {}
	}
}

fn enqueue_interface_str_methods(iface_name string, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	enqueue('${iface_name}.str', mut used, mut queue)
	for impl in tc.interface_impl_names(iface_name) {
		method := '${impl}.str'
		enqueue(method, mut used, mut queue)
		lowered := markused_c_name(method)
		if lowered != method {
			enqueue(lowered, mut used, mut queue)
		}
		short := '${impl.all_after_last('.')}.str'
		if short != method {
			enqueue(short, mut used, mut queue)
		}
	}
}

fn enqueue_stringified_primitive_helpers(type_name string, mut used map[string]bool, mut queue []string) {
	match type_name {
		'bool' {
			enqueue('bool.str', mut used, mut queue)
		}
		'rune', 'char' {
			enqueue('rune.str', mut used, mut queue)
		}
		'int', 'i8', 'i16', 'i32', 'i64' {
			enqueue('${type_name}.str', mut used, mut queue)
			enqueue(markused_c_name('${type_name}.str'), mut used, mut queue)
			enqueue('strconv__format_int', mut used, mut queue)
		}
		'isize' {
			enqueue('strconv__format_int', mut used, mut queue)
		}
		'u8', 'byte', 'u16', 'u32', 'usize' {
			enqueue('strconv__format_uint', mut used, mut queue)
		}
		'u64' {
			enqueue('u64.str', mut used, mut queue)
			enqueue(markused_c_name('u64.str'), mut used, mut queue)
			enqueue('strconv__format_uint', mut used, mut queue)
		}
		'f32' {
			enqueue('f32.str', mut used, mut queue)
			enqueue(markused_c_name('f32.str'), mut used, mut queue)
			enqueue('strconv__f32_to_str_l', mut used, mut queue)
		}
		'f64' {
			enqueue('strconv__f64_to_str_l', mut used, mut queue)
		}
		else {}
	}
}

// enqueue_enum_str_method supports enqueue enum str method handling for markused.
fn enqueue_enum_str_method(type_name string, cur_module string, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	for candidate in stringification_type_candidates(type_name, cur_module) {
		method := '${candidate}.str'
		if method in tc.fn_ret_types {
			enqueue(method, mut used, mut queue)
			lowered := markused_c_name(method)
			if lowered != method {
				enqueue(lowered, mut used, mut queue)
			}
		}
	}
}

// enqueue_structlike_str_method supports enqueue structlike str method handling for markused.
fn enqueue_structlike_str_method(type_name string, cur_module string, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	for candidate in stringification_type_candidates(type_name, cur_module) {
		enqueue_structlike_str_candidate(candidate, tc, mut used, mut queue)
	}
	for candidate in generic_stringification_type_candidates(type_name, cur_module, tc) {
		enqueue_structlike_str_candidate(candidate, tc, mut used, mut queue)
	}
}

fn enqueue_structlike_str_candidate(candidate string, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	lowered := '${markused_c_name(candidate)}__str'
	if lowered in tc.fn_ret_types {
		enqueue(lowered, mut used, mut queue)
	}
	method := '${candidate}.str'
	if method in tc.fn_ret_types {
		enqueue(method, mut used, mut queue)
	}
}

// stringification_type_candidates supports stringification type candidates handling for markused.
fn stringification_type_candidates(type_name string, cur_module string) []string {
	if type_name.len == 0 {
		return []string{}
	}
	mut candidates := []string{cap: 2}
	candidates << type_name
	if !type_name.contains('.') && cur_module.len > 0 && cur_module != 'main'
		&& cur_module != 'builtin' {
		candidates << '${cur_module}.${type_name}'
	}
	return candidates
}

fn generic_stringification_type_candidates(type_name string, cur_module string, tc &types.TypeChecker) []string {
	base, args, ok := markused_generic_app_parts(type_name)
	if !ok || args.len == 0 {
		return []string{}
	}
	mut candidates := []string{}
	for base_candidate in stringification_type_candidates(base, cur_module) {
		params := generic_stringification_params(base_candidate, tc) or { continue }
		if params.len != args.len {
			continue
		}
		candidates << '${base_candidate}[${params.join(', ')}]'
	}
	return candidates
}

fn generic_stringification_params(base_candidate string, tc &types.TypeChecker) ?[]string {
	if params := tc.struct_generic_params[base_candidate] {
		return params
	}
	if params := tc.sum_generic_params[base_candidate] {
		return params
	}
	return none
}

fn markused_generic_app_parts(typ string) (string, []string, bool) {
	clean := typ.trim_space()
	if clean.starts_with('fn(') || clean.starts_with('fn (') {
		return '', []string{}, false
	}
	bracket := clean.index_u8(`[`)
	if bracket <= 0 {
		return '', []string{}, false
	}
	bracket_end := markused_generic_matching_bracket(clean, bracket)
	if bracket_end <= bracket || bracket_end >= clean.len {
		return '', []string{}, false
	}
	return clean[..bracket], markused_split_generic_args(clean[bracket + 1..bracket_end]), true
}

fn markused_generic_matching_bracket(s string, start int) int {
	mut depth := 0
	for i in start .. s.len {
		if s[i] == `[` {
			depth++
		} else if s[i] == `]` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return s.len
}

fn markused_split_generic_args(s string) []string {
	mut args := []string{}
	mut start := 0
	mut bracket_depth := 0
	mut paren_depth := 0
	for i in 0 .. s.len {
		match s[i] {
			`[` {
				bracket_depth++
			}
			`]` {
				bracket_depth--
			}
			`(` {
				paren_depth++
			}
			`)` {
				paren_depth--
			}
			`,` {
				if bracket_depth == 0 && paren_depth == 0 {
					args << s[start..i].trim_space()
					start = i + 1
				}
			}
			else {}
		}
	}
	if start <= s.len {
		args << s[start..].trim_space()
	}
	return args
}

fn (c &CallCollector) node_tree_uses_generics(root flat.NodeId, cur_module string, imports map[string]string) bool {
	mut stack := [root]
	for stack.len > 0 {
		id := stack.pop()
		if int(id) < 0 || int(id) >= c.a.nodes.len {
			continue
		}
		node := &c.a.nodes[int(id)]
		if c.node_uses_generics(node, cur_module, imports) {
			return true
		}
		if node.kind == .call {
			if name := c.tc.resolved_call_name(id) {
				if c.generic_fn_name_is_known(name, cur_module) {
					return true
				}
			}
		}
		for i in 0 .. node.children_count {
			child_id := c.a.child(node, i)
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
	return false
}

fn (c &CallCollector) node_uses_generics(node &flat.Node, cur_module string, imports map[string]string) bool {
	if c.type_text_uses_generics(node.typ, cur_module, imports) {
		return true
	}
	return match node.kind {
		.struct_init, .array_init, .cast_expr, .as_expr, .sizeof_expr, .typeof_expr, .is_expr {
			c.type_text_uses_generics(node.value, cur_module, imports)
		}
		else {
			false
		}
	}
}

fn (c &CallCollector) type_text_uses_generics(typ string, cur_module string, imports map[string]string) bool {
	if typ.len == 0 {
		return false
	}
	return c.type_text_uses_generics_depth(typ.trim_space(), cur_module, imports, 0)
}

fn (c &CallCollector) type_text_uses_generics_depth(typ string, cur_module string, imports map[string]string, depth int) bool {
	if typ.len == 0 || depth > 8 {
		return false
	}
	for i, ch in typ {
		if ch != `[` || i == 0 {
			continue
		}
		mut start := i
		for start > 0 && markused_generic_name_byte(typ[start - 1]) {
			start--
		}
		if start < i && c.generic_type_base_is_known(typ[start..i], cur_module, imports) {
			return true
		}
	}
	mut may_name_alias := false
	for ch in typ {
		if ch >= `A` && ch <= `Z` {
			may_name_alias = true
			break
		}
	}
	if !may_name_alias {
		return false
	}
	clean := typ.trim_left('&?!').trim_space()
	if target := c.generic_alias_target(clean, cur_module, imports) {
		return c.type_text_uses_generics_depth(target.trim_space(), cur_module, imports, depth + 1)
	}
	if c.selective_alias_uses_generics(clean, cur_module, imports, depth + 1) {
		return true
	}
	// Wrapped aliases are not whole type strings, so inspect each named component.
	mut start := 0
	for start < clean.len {
		for start < clean.len && !markused_type_name_start_byte(clean[start]) {
			start++
		}
		if start >= clean.len {
			break
		}
		mut end := start + 1
		mut candidate_may_name_alias := clean[start] >= `A` && clean[start] <= `Z`
		for end < clean.len && markused_generic_name_byte(clean[end]) {
			if clean[end] >= `A` && clean[end] <= `Z` {
				candidate_may_name_alias = true
			}
			end++
		}
		if candidate_may_name_alias {
			candidate := clean[start..end]
			if target := c.generic_alias_target(candidate, cur_module, imports) {
				alias_target := target.trim_space()
				if c.type_text_uses_generics_depth(alias_target, cur_module, imports, depth + 1) {
					return true
				}
			} else if c.selective_alias_uses_generics(candidate, cur_module, imports, depth + 1) {
				return true
			}
			if c.struct_fields_use_generics(candidate, cur_module, imports, depth + 1) {
				return true
			}
		}
		start = end
	}
	return false
}

fn (c &CallCollector) struct_fields_use_generics(name string, cur_module string, imports map[string]string, depth int) bool {
	info := c.struct_decl_info_with_imports(name, cur_module, imports) or { return false }
	decl := c.a.node(info.node_id)
	qualified_name := qualify_fn(info.module, decl.value)
	fields := c.tc.structs[qualified_name] or { return false }
	field_module := if info.module.len > 0 { info.module } else { cur_module }
	for field in fields {
		if c.type_text_uses_generics_depth(field.typ.name(), field_module, imports, depth) {
			return true
		}
	}
	return false
}

fn (c &CallCollector) emitted_type_declarations_use_generics(imports map[string]string) bool {
	for name, fields in c.tc.structs {
		if name in c.tc.struct_generic_params {
			continue
		}
		for field in fields {
			if c.parsed_type_uses_concrete_generics(field.typ, 0) {
				return true
			}
		}
	}
	for name, variants in c.tc.sum_types {
		if name in c.tc.sum_generic_params {
			continue
		}
		for variant in variants {
			if c.parsed_type_uses_concrete_generics(c.tc.parse_type(variant), 0) {
				return true
			}
		}
	}
	for _, fields in c.tc.interface_fields {
		for field in fields {
			if c.parsed_type_uses_concrete_generics(field.typ, 0) {
				return true
			}
		}
	}
	mut cur_module := ''
	for node_idx in c.tc.top_level_idx {
		node := c.a.nodes[node_idx]
		match node.kind {
			.file {
				cur_module = ''
			}
			.module_decl {
				cur_module = node.value
			}
			.const_decl, .global_decl {
				for i in 0 .. node.children_count {
					if c.node_tree_uses_generics(c.a.child(&node, i), cur_module, imports) {
						return true
					}
				}
			}
			else {}
		}
	}
	return false
}

fn (c &CallCollector) parsed_type_uses_concrete_generics(typ types.Type, depth int) bool {
	if depth > 16 {
		return false
	}
	match typ {
		types.Array {
			return c.parsed_type_uses_concrete_generics(typ.elem_type, depth + 1)
		}
		types.ArrayFixed {
			return c.parsed_type_uses_concrete_generics(typ.elem_type, depth + 1)
		}
		types.Channel {
			return c.parsed_type_uses_concrete_generics(typ.elem_type, depth + 1)
		}
		types.Map {
			return c.parsed_type_uses_concrete_generics(typ.key_type, depth + 1)
				|| c.parsed_type_uses_concrete_generics(typ.value_type, depth + 1)
		}
		types.Pointer {
			return c.parsed_type_uses_concrete_generics(typ.base_type, depth + 1)
		}
		types.FnType {
			for param in typ.params {
				if c.parsed_type_uses_concrete_generics(param, depth + 1) {
					return true
				}
			}
			return c.parsed_type_uses_concrete_generics(typ.return_type, depth + 1)
		}
		types.OptionType {
			return c.parsed_type_uses_concrete_generics(typ.base_type, depth + 1)
		}
		types.ResultType {
			return c.parsed_type_uses_concrete_generics(typ.base_type, depth + 1)
		}
		types.Alias {
			return c.parsed_type_uses_concrete_generics(typ.base_type, depth + 1)
		}
		types.Struct {
			return c.named_type_uses_concrete_generics(typ.name)
		}
		types.Interface {
			return c.named_type_uses_concrete_generics(typ.name)
		}
		types.SumType {
			return c.named_type_uses_concrete_generics(typ.name)
		}
		types.MultiReturn {
			for item in typ.types {
				if c.parsed_type_uses_concrete_generics(item, depth + 1) {
					return true
				}
			}
		}
		else {}
	}

	return false
}

fn (c &CallCollector) named_type_uses_concrete_generics(name string) bool {
	base_name := types.generic_base_name(name)
	return base_name != name
		&& (base_name in c.tc.struct_generic_params || base_name in c.tc.sum_generic_params)
}

fn (c &CallCollector) struct_decl_info_with_imports(name string, cur_module string, imports map[string]string) ?StructDeclInfo {
	if info := c.struct_decl_info(name, cur_module) {
		return info
	}
	if !name.contains('.') {
		return none
	}
	alias := name.all_before('.')
	if module_name := imports[alias] {
		resolved_name := module_name + name[alias.len..]
		return c.struct_decl_info(resolved_name, module_name)
	}
	return none
}

fn markused_type_name_start_byte(ch u8) bool {
	return (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`) || ch == `_`
}

fn markused_generic_name_byte(ch u8) bool {
	return (ch >= `a` && ch <= `z`) || (ch >= `A` && ch <= `Z`)
		|| (ch >= `0` && ch <= `9`) || ch in [`_`, `.`]
}

fn (c &CallCollector) generic_type_base_is_known(base string, cur_module string, imports map[string]string) bool {
	if base in c.tc.struct_generic_params || base in c.tc.sum_generic_params {
		return true
	}
	if !base.contains('.') {
		qbase := qualify_fn(cur_module, base)
		return qbase in c.tc.struct_generic_params || qbase in c.tc.sum_generic_params
	}
	alias := base.all_before('.')
	if module_name := imports[alias] {
		resolved := module_name + base[alias.len..]
		return resolved in c.tc.struct_generic_params || resolved in c.tc.sum_generic_params
	}
	return false
}

fn (c &CallCollector) generic_alias_target(name string, cur_module string, imports map[string]string) ?string {
	if target := c.tc.type_aliases[name] {
		return target
	}
	if !name.contains('.') {
		qname := qualify_fn(cur_module, name)
		if target := c.tc.type_aliases[qname] {
			return target
		}
		return none
	}
	alias := name.all_before('.')
	if module_name := imports[alias] {
		resolved := module_name + name[alias.len..]
		if target := c.tc.type_aliases[resolved] {
			return target
		}
	}
	return none
}

fn (c &CallCollector) selective_alias_uses_generics(name string, cur_module string, imports map[string]string, depth int) bool {
	if c.selective_alias_targets.len == 0 || name.contains('.') {
		return false
	}
	targets := c.selective_alias_targets[name] or { return false }
	for target in targets {
		if c.type_text_uses_generics_depth(target.trim_space(), cur_module, imports, depth) {
			return true
		}
	}
	return false
}

fn (c &CallCollector) generic_fn_name_is_known(name string, cur_module string) bool {
	if name in c.tc.fn_generic_params {
		return true
	}
	qname := qualify_fn(cur_module, name)
	return qname != name && qname in c.tc.fn_generic_params
}

// enqueue_function_value_selectors supports enqueue function value selectors handling for markused.
fn enqueue_function_value_selectors(a &flat.FlatAst, collector CallCollector, fn_decls map[string]FnDeclInfo, has_entry_main bool, mut used map[string]bool, mut queue []string) {
	if has_entry_main {
		enqueue_function_value_selectors_with_entry_main(a, collector, fn_decls, mut used, mut
			queue)
		return
	}
	ignored_top_level_nodes := markused_ignored_top_level_nodes(a)
	ignored_fn_decl_nodes := markused_ignored_fn_decl_nodes(a)
	shadowed_value_idents := markused_shadowed_value_idents(a)
	for node_idx, node in a.nodes {
		if node_idx < ignored_fn_decl_nodes.len && ignored_fn_decl_nodes[node_idx] {
			continue
		}
		if node_idx < ignored_top_level_nodes.len && ignored_top_level_nodes[node_idx] {
			continue
		}
		if node.kind == .ident && node.value.len > 0 {
			node_id := flat.NodeId(node_idx)
			is_shadowed := node_idx < shadowed_value_idents.len && shadowed_value_idents[node_idx]
			if is_shadowed {
				continue
			}
			if resolved := collector.tc.resolved_fn_value_name(node_id) {
				enqueue(resolved, mut used, mut queue)
				continue
			}
			if node.value in fn_decls && collector.node_is_fn_value(node_id) {
				enqueue(node.value, mut used, mut queue)
			}
			continue
		}
		if node.kind == .selector && node.children_count > 0 && node.value.len > 0 {
			base_id := a.child(&node, 0)
			if int(base_id) >= 0 && int(base_id) < shadowed_value_idents.len
				&& shadowed_value_idents[int(base_id)] {
				continue
			}
			base := a.node(base_id)
			if base.kind == .ident && base.value.len > 0 {
				name := '${base.value}.${node.value}'
				if name in fn_decls {
					enqueue(name, mut used, mut queue)
				}
			}
		}
	}
}

fn enqueue_function_value_selectors_with_entry_main(a &flat.FlatAst, collector CallCollector, fn_decls map[string]FnDeclInfo, mut used map[string]bool, mut queue []string) {
	for file_idx, file_node in a.nodes {
		if file_node.kind != .file {
			continue
		}
		for i in 0 .. file_node.children_count {
			child_id := a.child(&file_node, i)
			child := a.node(child_id)
			if child.kind == .fn_decl {
				continue
			}
			if file_idx >= a.user_code_start && int(child_id) >= a.user_code_start
				&& markused_is_top_level_stmt(child) {
				continue
			}
			enqueue_function_value_selectors_in_node(a, collector, fn_decls, child_id, mut used, mut
				queue)
		}
	}
}

fn enqueue_function_value_selectors_in_node(a &flat.FlatAst, collector CallCollector, fn_decls map[string]FnDeclInfo, id flat.NodeId, mut used map[string]bool, mut queue []string) {
	if int(id) < 0 || int(id) >= a.nodes.len {
		return
	}
	node := a.node(id)
	if node.kind == .fn_decl {
		return
	}
	if node.kind == .ident && node.value.len > 0 {
		if resolved := collector.tc.resolved_fn_value_name(id) {
			enqueue(resolved, mut used, mut queue)
			return
		}
		if node.value in fn_decls && collector.node_is_fn_value(id) {
			enqueue(node.value, mut used, mut queue)
		}
		return
	}
	if node.kind == .selector && node.children_count > 0 && node.value.len > 0 {
		base_id := a.child(node, 0)
		base := a.node(base_id)
		if base.kind == .ident && base.value.len > 0 {
			name := '${base.value}.${node.value}'
			if name in fn_decls {
				enqueue(name, mut used, mut queue)
			}
		}
	}
	for i in 0 .. node.children_count {
		enqueue_function_value_selectors_in_node(a, collector, fn_decls, a.child(node, i), mut
			used, mut queue)
	}
}

fn markused_ignored_fn_decl_nodes(a &flat.FlatAst) []bool {
	mut ignored := []bool{len: a.nodes.len}
	for node_idx, node in a.nodes {
		if node.kind == .fn_decl {
			markused_mark_node_subtree(a, flat.NodeId(node_idx), mut ignored)
		}
	}
	return ignored
}

fn markused_ignored_top_level_nodes(a &flat.FlatAst) []bool {
	if !markused_has_entry_main(a) {
		return []bool{}
	}
	mut ignored := []bool{len: a.nodes.len}
	for file_idx, file_node in a.nodes {
		if !markused_should_scan_top_level_file(a, file_idx, file_node) {
			continue
		}
		for i in 0 .. file_node.children_count {
			child_id := a.child(&file_node, i)
			if int(child_id) < a.user_code_start {
				continue
			}
			child := a.node(child_id)
			if !markused_is_top_level_stmt(child) {
				continue
			}
			markused_mark_node_subtree(a, child_id, mut ignored)
		}
	}
	return ignored
}

fn markused_shadowed_value_idents(a &flat.FlatAst) []bool {
	mut shadowed := []bool{len: a.nodes.len}
	for file_idx, file_node in a.nodes {
		if !markused_should_scan_top_level_file(a, file_idx, file_node) {
			continue
		}
		mut local_values := map[string]bool{}
		for i in 0 .. file_node.children_count {
			child_id := a.child(&file_node, i)
			if int(child_id) < a.user_code_start {
				continue
			}
			child := a.node(child_id)
			if !markused_is_top_level_stmt(child) {
				continue
			}
			if local_values.len > 0 {
				markused_mark_shadowed_idents(a, child, local_values, mut shadowed)
			}
			markused_mark_top_level_lhs_idents(a, child, mut shadowed)
			markused_add_top_level_lhs_names(a, child, mut local_values)
		}
	}
	return shadowed
}

fn markused_mark_shadowed_idents(a &flat.FlatAst, node &flat.Node, local_values map[string]bool, mut shadowed []bool) {
	if local_values.len == 0 {
		return
	}
	mut stack := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := a.child(node, i)
		if int(child_id) >= 0 {
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		idx := int(id)
		if idx < 0 || idx >= shadowed.len {
			continue
		}
		child := a.node(id)
		if child.kind == .ident && child.value in local_values {
			shadowed[idx] = true
		}
		for i in 0 .. child.children_count {
			next_id := a.child(child, i)
			if int(next_id) >= 0 {
				stack << next_id
			}
		}
	}
}

fn markused_mark_top_level_lhs_idents(a &flat.FlatAst, node &flat.Node, mut shadowed []bool) {
	if node.kind != .decl_assign {
		return
	}
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := a.child(node, i)
		idx := int(lhs_id)
		if idx >= 0 && idx < shadowed.len {
			lhs := a.node(lhs_id)
			if lhs.kind == .ident {
				shadowed[idx] = true
			}
		}
		i += 2
	}
}

fn markused_add_top_level_lhs_names(a &flat.FlatAst, node &flat.Node, mut local_values map[string]bool) {
	match node.kind {
		.decl_assign, .assign {
			mut i := 0
			for i + 1 < node.children_count {
				lhs_id := a.child(node, i)
				if int(lhs_id) >= 0 {
					lhs := a.node(lhs_id)
					if lhs.kind == .ident && lhs.value.len > 0 {
						local_values[lhs.value] = true
					}
				}
				i += 2
			}
		}
		.global_decl {
			for i in 0 .. node.children_count {
				field := a.child_node(node, i)
				if field.value.len > 0 {
					local_values[field.value] = true
				}
			}
		}
		else {}
	}
}

fn markused_mark_node_subtree(a &flat.FlatAst, root flat.NodeId, mut marked []bool) {
	mut stack := [root]
	for stack.len > 0 {
		id := stack.pop()
		idx := int(id)
		if idx < 0 || idx >= marked.len || marked[idx] {
			continue
		}
		marked[idx] = true
		node := a.node(id)
		for i in 0 .. node.children_count {
			child_id := a.child(node, i)
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
}

// type_string_needs_optional_helpers returns type string needs optional helpers data for markused.
fn type_string_needs_optional_helpers(typ string) bool {
	return typ.len > 0 && (typ[0] == `?` || typ[0] == `!`)
}

// type_needs_zero_map returns type needs zero map data for markused.
fn type_needs_zero_map(typ types.Type) bool {
	mut clean := typ
	for _ in 0 .. 8 {
		if clean is types.Alias {
			clean = clean.base_type
			continue
		}
		if clean is types.OptionType {
			clean = clean.base_type
			continue
		}
		if clean is types.ResultType {
			clean = clean.base_type
			continue
		}
		break
	}
	return clean is types.Map
}

// markused_membership_container_type
// supports helper handling in markused.
fn markused_membership_container_type(tc &types.TypeChecker, typ types.Type) string {
	mut clean := typ.name().trim_space()
	for {
		if clean.starts_with('shared ') {
			clean = clean[7..].trim_space()
			continue
		}
		if clean.starts_with('&') {
			clean = clean[1..].trim_space()
			continue
		}
		if clean in tc.type_aliases {
			alias := tc.type_aliases[clean].trim_space()
			if alias == clean {
				break
			}
			clean = alias
			continue
		}
		break
	}
	return clean
}

// qualify_fn supports qualify fn handling for markused.
fn qualify_fn(mod string, name string) string {
	if mod.len == 0 || mod == 'main' || mod == 'builtin' {
		return name
	}
	return '${mod}.${name}'
}

// receiver_info supports receiver info handling for markused.
fn receiver_info(a &flat.FlatAst, node &flat.Node) (string, string) {
	mut receiver_struct := ''
	if node.value.contains('.') {
		receiver_struct = node.value.all_before_last('.')
	}
	for pi in 0 .. node.children_count {
		pc := a.child_node(node, pi)
		if pc.kind == .param {
			if receiver_struct.len > 0 {
				return pc.value, receiver_struct
			}
			clean_type := pc.typ.trim_left('&')
			if pc.value.len > 0 && clean_type.len > 0 && clean_type[0] >= `A`
				&& clean_type[0] <= `Z` {
				return pc.value, clean_type
			}
		}
	}
	if receiver_struct.len == 0 {
		return '', ''
	}
	return '', receiver_struct
}

fn (c &CallCollector) collect_interface_boxed_generic_methods(call &flat.Node, resolved_call string, cur_module string, imports map[string]string, local_values map[string]bool, mut calls []string) {
	if call.children_count < 2 {
		return
	}
	// Cheap pre-check on the raw callee spellings before building the full
	// candidate list: this pass only matters for the few signatures that take
	// an interface-typed parameter.
	if !c.call_may_target_interface_params(call, resolved_call) {
		return
	}
	signatures := c.call_signature_candidates(call, resolved_call, cur_module, imports,
		local_values)
	if signatures.len == 0 {
		return
	}
	callee := c.a.child_node(call, 0)
	param_offset := c.call_receiver_param_offset(callee, imports, local_values)
	for signature in signatures {
		params := c.tc.fn_param_types[signature] or { continue }
		for arg_i := 1; arg_i < call.children_count; arg_i++ {
			param_i := arg_i - 1 + param_offset
			if param_i < 0 || param_i >= params.len {
				continue
			}
			expected := types.unwrap_pointer(params[param_i])
			if expected !is types.Interface {
				continue
			}
			arg_id := markused_generic_call_arg_value(c.a, c.a.child(call, arg_i))
			actual := types.unwrap_pointer(c.tc.resolve_type(arg_id))
			actual_name := resolve_type_name(actual)
			c.add_interface_boxed_generic_methods(expected.name(), actual_name, mut calls)
		}
	}
}

// call_may_target_interface_params mirrors call_signature_candidates' raw name
// extraction (resolved call name, ident callee, selector method name) against
// the precomputed interface-param gate.
fn (c &CallCollector) call_may_target_interface_params(call &flat.Node, resolved_call string) bool {
	if c.may_target_interface_params(resolved_call) {
		return true
	}
	if call.children_count == 0 {
		return false
	}
	callee := c.a.child_node(call, 0)
	if callee.kind in [.ident, .selector] {
		return c.may_target_interface_params(callee.value)
	}
	return false
}

fn (c &CallCollector) call_signature_candidates(call &flat.Node, resolved_call string, cur_module string, imports map[string]string, local_values map[string]bool) []string {
	mut names := []string{}
	markused_add_call_signature_candidate(resolved_call, cur_module, mut names)
	if call.children_count == 0 {
		return names
	}
	callee := c.a.child_node(call, 0)
	if callee.kind == .ident && callee.value.len > 0 && callee.value !in local_values {
		markused_add_call_signature_candidate(callee.value, cur_module, mut names)
		return names
	}
	if callee.kind == .selector && callee.value.len > 0 && callee.children_count > 0 {
		base := c.a.child_node(callee, 0)
		if base.kind == .ident && base.value.len > 0 {
			if imported := imports[base.value] {
				markused_add_call_signature_candidate('${imported}.${callee.value}', cur_module, mut
					names)
			}
		}
	}
	return names
}

fn markused_add_call_signature_candidate(name string, cur_module string, mut names []string) {
	if name.len == 0 {
		return
	}
	markused_push_unique_key(name, mut names)
	lowered := markused_c_name(name)
	if lowered != name {
		markused_push_unique_key(lowered, mut names)
	}
	qname := qualify_fn(cur_module, name)
	if qname != name {
		markused_push_unique_key(qname, mut names)
		lowered_qname := markused_c_name(qname)
		if lowered_qname != qname {
			markused_push_unique_key(lowered_qname, mut names)
		}
	}
}

fn (c &CallCollector) call_receiver_param_offset(callee &flat.Node, imports map[string]string, local_values map[string]bool) int {
	if callee.kind != .selector || callee.children_count == 0 {
		return 0
	}
	base := c.a.child_node(callee, 0)
	if base.kind != .ident || base.value.len == 0 {
		return 1
	}
	if base.value in imports && base.value !in local_values {
		return 0
	}
	if base.value in local_values {
		return 1
	}
	if base.value[0] >= `A` && base.value[0] <= `Z` {
		return 0
	}
	return 0
}

fn (c &CallCollector) add_interface_boxed_generic_methods(iface_name string, actual_name string, mut calls []string) {
	if iface_name.len == 0 || actual_name.len == 0 {
		return
	}
	_, _, is_generic := markused_generic_app_parts(actual_name)
	if !is_generic {
		return
	}
	if !c.tc.named_type_implements_interface(actual_name, iface_name) {
		return
	}
	for method in c.tc.interface_abstract_method_names(iface_name) {
		info := c.tc.resolve_generic_struct_method(actual_name, method) or { continue }
		c.add_typed_receiver_method_name(info.name, mut calls)
		c.add_typed_receiver_method_name('${actual_name}.${method}', mut calls)
	}
}

// BodyCalls holds the precollected call and initializer-ref names of one
// fn_decl body (see precollect_body_calls).
struct BodyCalls {
mut:
	calls         []string
	refs          []string
	uses_generics bool
}

// collect_body runs the full per-body analysis (local values, calls,
// initializer refs) for one fn_decl node. It only reads shared state — the
// checker memoizes exclusively into its private type_cache — so it can run on
// worker threads against a forked TypeChecker.
fn (c &CallCollector) collect_body(node &flat.Node, cur_module string, imports map[string]string) BodyCalls {
	receiver_name, receiver_struct := receiver_info(c.a, node)
	local_values, local_types := c.local_value_info(node, cur_module, imports)
	needs_visibility := c.local_values_need_visibility(local_values, cur_module, imports)
	visible_local_idents := if needs_visibility {
		markused_visible_local_idents(c.a, node, local_values)
	} else {
		map[int]bool{}
	}
	mut result := BodyCalls{
		calls:         []string{cap: 32}
		refs:          []string{cap: 8}
		uses_generics: c.generic_fn_name_is_known(node.value, cur_module)
			|| c.type_text_uses_generics(node.value, cur_module, imports)
			|| c.node_uses_generics(node, cur_module, imports)
	}
	result.uses_generics = c.collect_calls_with_locals_and_generics(node, cur_module, imports,
		receiver_name, receiver_struct, local_values, local_types, visible_local_idents, true,
		result.uses_generics, mut result.calls)
	c.collect_initializer_refs_with_locals(node, cur_module, imports, local_values,
		visible_local_idents, mut result.refs)
	return result
}

// collect_bodies_range fills results[start..end] for the given body list; the
// per-worker unit of the parallel precollection (also the serial fallback).
fn (c &CallCollector) collect_bodies_range(body_ids []int, body_modules []string, imports map[string]string, start int, end int, mut results []BodyCalls) {
	for i in start .. end {
		node := c.a.node(flat.NodeId(body_ids[i]))
		results[i] = c.collect_body(node, body_modules[i], imports)
	}
}

// fork_with_tc returns a copy of this collector bound to a worker's forked
// TypeChecker. The lookup maps are shared read-only.
fn (c &CallCollector) fork_with_tc(wtc &types.TypeChecker) CallCollector {
	return CallCollector{
		a:                       c.a
		tc:                      wtc
		fn_decls:                c.fn_decls
		fn_suffixes:             c.fn_suffixes
		struct_decls:            c.struct_decls
		const_decls:             c.const_decls
		const_suffixes:          c.const_suffixes
		selective_alias_targets: c.selective_alias_targets
		iface_param_gate:        c.iface_param_gate
	}
}

// collect_calls updates collect calls state for markused.
fn (c &CallCollector) collect_calls(node &flat.Node, cur_module string, imports map[string]string, receiver_name string, receiver_struct string, mut calls []string) {
	local_values, local_types := c.local_value_info(node, cur_module, imports)
	visible_local_idents := if c.local_values_need_visibility(local_values, cur_module, imports) {
		markused_visible_local_idents(c.a, node, local_values)
	} else {
		map[int]bool{}
	}
	c.collect_calls_with_locals(node, cur_module, imports, receiver_name, receiver_struct,
		local_values, local_types, visible_local_idents, mut calls)
}

fn (c &CallCollector) collect_calls_with_generic_usage(node &flat.Node, cur_module string, imports map[string]string, receiver_name string, receiver_struct string, mut calls []string) bool {
	local_values, local_types := c.local_value_info(node, cur_module, imports)
	visible_local_idents := if c.local_values_need_visibility(local_values, cur_module, imports) {
		markused_visible_local_idents(c.a, node, local_values)
	} else {
		map[int]bool{}
	}
	uses_generics := c.node_uses_generics(node, cur_module, imports)
	return c.collect_calls_with_locals_and_generics(node, cur_module, imports, receiver_name,
		receiver_struct, local_values, local_types, visible_local_idents, true, uses_generics, mut
		calls)
}

// collect_calls_with_locals is collect_calls with the per-body local-value
// analysis precomputed by the caller, so the BFS can share one analysis between
// collect_calls and collect_initializer_refs instead of walking each function
// subtree twice more.
fn (c &CallCollector) collect_calls_with_locals(node &flat.Node, cur_module string, imports map[string]string, receiver_name string, receiver_struct string, local_values map[string]bool, local_types map[string]string, visible_local_idents map[int]bool, mut calls []string) {
	uses_generics := false
	_ = c.collect_calls_with_locals_and_generics(node, cur_module, imports, receiver_name,
		receiver_struct, local_values, local_types, visible_local_idents, false, uses_generics, mut
		calls)
}

fn (c &CallCollector) collect_calls_with_locals_and_generics(node &flat.Node, cur_module string, imports map[string]string, receiver_name string, receiver_struct string, local_values map[string]bool, local_types map[string]string, visible_local_idents map[int]bool, detect_generics bool, initial_uses_generics bool, mut calls []string) bool {
	mut uses_generics := initial_uses_generics
	if cur_module == 'ast' && node.value == 'TypeSymbol.find_method_with_generic_parent' {
		c.add_typed_receiver_method_name('ast.Table.find_structured_receiver_method', mut calls)
	}
	mut stack := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := c.a.child(node, i)
		if int(child_id) >= 0 {
			stack << child_id
		}
	}
	for stack.len > 0 {
		child_id := stack.pop()
		child := &c.a.nodes[int(child_id)]
		if detect_generics && !uses_generics && c.node_uses_generics(child, cur_module, imports) {
			uses_generics = true
		}
		match child.kind {
			.ident {
				name_is_local := markused_ident_is_visible_local(child_id, child.value,
					local_values, visible_local_idents)
				c.collect_fn_value_ident(child_id, child.value, cur_module, imports, name_is_local, mut
					calls)
			}
			.selector {
				if !c.selector_base_is_local(child, local_values) {
					c.collect_fn_value_selector(child_id, child, cur_module, imports, mut calls)
				} else if resolved := c.tc.resolved_fn_value_name(child_id) {
					calls << resolved
				}
			}
			.call {
				mut resolved_call := ''
				if resolved := c.tc.resolved_call_name(child_id) {
					resolved_call = resolved
				}
				if detect_generics && !uses_generics
					&& c.generic_fn_name_is_known(resolved_call, cur_module) {
					uses_generics = true
				}
				c.collect_interface_boxed_generic_methods(child, resolved_call, cur_module,
					imports, local_values, mut calls)
				c.collect_lowered_join_path_single(child, resolved_call, mut calls)
				if child.children_count > 0 {
					callee_id := c.a.child(child, 0)
					if int(callee_id) >= 0 {
						callee := c.a.nodes[int(callee_id)]
						if callee.kind == .ident && callee.value.len > 0 {
							if resolved_call.len > 0 {
								calls << resolved_call
							}
							if callee.value in ['print', 'println', 'eprint', 'eprintln']
								&& child.children_count >= 2 {
								arg_id := c.a.child(child, 1)
								if c.expr_lowers_to_map_str(arg_id, local_values, local_types) {
									calls << 'string__plus'
								}
							}
							if callee.value !in local_values {
								qcallee := qualify_fn(cur_module, callee.value)
								if qcallee != callee.value && c.is_known_fn_name(qcallee) {
									calls << qcallee
								} else {
									calls << callee.value
									if qcallee != callee.value {
										calls << qcallee
									}
								}
							}
						} else if callee.kind == .selector && callee.value.len > 0 {
							mut has_exact_selector_call := false
							if callee.children_count > 0 {
								base_id := c.a.child(&callee, 0)
								if int(base_id) >= 0 {
									base := c.a.nodes[int(base_id)]
									base_is_local_value := base.kind == .ident
										&& base.value in local_values
									if callee.value == 'str'
										&& c.expr_lowers_to_map_str(base_id, local_values, local_types) {
										calls << 'string__plus'
									}
									if base_is_local_value {
										has_exact_selector_call = c.collect_typed_receiver_method(base_id,
											callee.value, cur_module, imports, local_values,
											local_types, mut calls)
									}
									if !has_exact_selector_call && !base_is_local_value {
										has_exact_selector_call = c.collect_checker_selected_call(resolved_call, mut
											calls)
									}
									if !has_exact_selector_call {
										has_exact_selector_call = c.collect_typed_receiver_method(base_id,
											callee.value, cur_module, imports, local_values,
											local_types, mut calls)
									}
									if !has_exact_selector_call && base.kind == .ident
										&& base.value in imports && base.value !in local_values {
										calls << imports[base.value] + '.' + callee.value
										has_exact_selector_call = true
									}
									if !has_exact_selector_call && resolved_call.len > 0 {
										calls << resolved_call
										has_exact_selector_call = true
									}
									if !has_exact_selector_call && base.kind == .ident
										&& base.value in local_values && !(receiver_name.len > 0
										&& base.value == receiver_name) {
										has_exact_selector_call = true
									}
									if !has_exact_selector_call {
										if base.kind == .ident && base.value.len > 0 {
											if receiver_name.len > 0 && base.value == receiver_name {
												calls << receiver_struct + '.' + callee.value
												qrecv := qualify_fn(cur_module, receiver_struct +
													'.' + callee.value)
												if qrecv != receiver_struct + '.' + callee.value {
													calls << qrecv
												}
											}
											mod_name := if base.value in imports {
												imports[base.value]
											} else {
												base.value
											}
											calls << mod_name + '.' + callee.value
											calls << qualify_fn(cur_module, base.value + '.' +
												callee.value)
											if base.value.len > 0 && base.value[0] >= `A`
												&& base.value[0] <= `Z` {
												named_type := c.tc.parse_type(base.value)
												named_type_name := resolve_type_name(named_type)
												if named_type_name.len > 0 {
													calls << named_type_name + '.' + callee.value
												}
											}
										} else if base.kind == .selector && base.children_count > 0 {
											inner_id := c.a.child(&base, 0)
											if int(inner_id) >= 0 {
												inner := c.a.nodes[int(inner_id)]
												if inner.kind == .ident && inner.value.len > 0 {
													mod_name := if inner.value in imports {
														imports[inner.value]
													} else {
														inner.value
													}
													calls << mod_name + '.' + base.value + '.' +
														callee.value
												}
											}
										}
									}
								}
							}
							if !has_exact_selector_call {
								calls << callee.value
							}
						} else if resolved_call.len > 0 {
							calls << resolved_call
						}
					}
				} else if resolved_call.len > 0 {
					calls << resolved_call
				}
				for ci in 1 .. child.children_count {
					arg_id := c.a.child(child, ci)
					if int(arg_id) >= 0 {
						arg := c.a.nodes[int(arg_id)]
						if arg.kind == .ident && arg.value.len > 0 {
							arg_is_local := markused_ident_is_visible_local(arg_id, arg.value,
								local_values, visible_local_idents)
							c.collect_fn_value_ident(arg_id, arg.value, cur_module, imports,
								arg_is_local, mut calls)
						}
					}
				}
				c.collect_generic_alias_operator_usage(child, resolved_call, cur_module, imports,
					local_values, local_types, mut calls)
			}
			.prefix {
				if child.op == .amp && child.children_count > 0 {
					inner_id := c.a.child(child, 0)
					if int(inner_id) >= 0 {
						inner := c.a.nodes[int(inner_id)]
						if inner.kind == .struct_init {
							calls << 'memdup'
						}
					}
				}
			}
			.string_interp {
				calls << 'string_plus_many'
			}
			.assign, .selector_assign, .index_assign {
				c.collect_assign_operator_call(child, cur_module, local_types, mut calls)
			}
			.infix {
				if child.op == .plus {
					calls << 'string__plus'
				}
				if child.children_count >= 2 {
					lhs_id := c.a.child(child, 0)
					rhs_id := c.a.child(child, 1)
					if child.op in [.dot, .amp] {
						lhs_name := c.qualified_expr_name(lhs_id)
						if lhs_name.len > 0 {
							rhs := c.a.nodes[int(rhs_id)]
							if rhs.kind == .call && rhs.children_count > 0 {
								fn_node := c.a.child_node(&rhs, 0)
								if fn_node.kind == .ident && fn_node.value.len > 0 {
									mod_name := if lhs_name in imports {
										imports[lhs_name]
									} else {
										lhs_name
									}
									calls << mod_name + '.' + fn_node.value
									calls << qualify_fn(cur_module, lhs_name + '.' + fn_node.value)
								}
							}
						}
					}
					if int(lhs_id) >= 0 {
						c.collect_struct_operator_call(lhs_id, child.op, cur_module, local_types, mut
							calls)
					}
				}
			}
			.or_expr {
				if child.children_count > 0 {
					expr_id := c.a.child(child, 0)
					c.collect_zero_struct_default_calls(c.node_type(expr_id), cur_module, imports, mut
						calls)
				}
			}
			.struct_init {
				c.collect_struct_default_calls(child, cur_module, imports, mut calls)
			}
			else {}
		}

		if child.children_count > 0 {
			mut j := int(child.children_count) - 1
			for j >= 0 {
				if child.kind == .call && j == 0 {
					if callee_expr_id := c.call_callee_expr_to_collect(child) {
						stack << callee_expr_id
					}
					j--
					continue
				}
				if child.kind == .decl_assign && j % 2 == 0 {
					j--
					continue
				}
				next_id := c.a.child(child, j)
				if int(next_id) >= 0 {
					stack << next_id
				}
				j--
			}
		}
	}
	return uses_generics
}

fn (c &CallCollector) call_callee_expr_to_collect(node &flat.Node) ?flat.NodeId {
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	callee_id := c.a.child(node, 0)
	if int(callee_id) < 0 {
		return none
	}
	callee := c.a.node(callee_id)
	if callee.kind == .selector {
		if callee.children_count == 0 {
			return none
		}
		base_id := c.a.child(callee, 0)
		if int(base_id) >= 0 && c.expr_contains_call(base_id) {
			return base_id
		}
		return none
	}
	if callee.kind == .ident {
		return none
	}
	if c.expr_contains_call(callee_id) {
		return callee_id
	}
	return none
}

fn (c &CallCollector) expr_contains_call(id flat.NodeId) bool {
	if int(id) < 0 {
		return false
	}
	mut stack := [id]
	for stack.len > 0 {
		cur_id := stack.pop()
		node := c.a.node(cur_id)
		if node.kind == .call {
			return true
		}
		for i in 0 .. node.children_count {
			child_id := c.a.child(node, i)
			if int(child_id) >= 0 {
				stack << child_id
			}
		}
	}
	return false
}

fn (c &CallCollector) collect_initializer_refs(node &flat.Node, cur_module string, imports map[string]string, mut refs []string) {
	local_values, _ := c.local_value_info(node, cur_module, imports)
	visible_local_idents := if c.local_values_need_visibility(local_values, cur_module, imports) {
		markused_visible_local_idents(c.a, node, local_values)
	} else {
		map[int]bool{}
	}
	c.collect_initializer_refs_with_locals(node, cur_module, imports, local_values,
		visible_local_idents, mut refs)
}

// collect_initializer_refs_with_locals is collect_initializer_refs with the
// per-body local-value analysis precomputed by the caller (see
// collect_calls_with_locals).
fn (c &CallCollector) collect_initializer_refs_with_locals(node &flat.Node, cur_module string, imports map[string]string, local_values map[string]bool, visible_local_idents map[int]bool, mut refs []string) {
	mut stack := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := c.a.child(node, i)
		if int(child_id) >= 0 {
			stack << child_id
		}
	}
	for stack.len > 0 {
		child_id := stack.pop()
		child := &c.a.nodes[int(child_id)]
		match child.kind {
			.ident {
				if !markused_ident_is_visible_local(child_id, child.value, local_values,
					visible_local_idents) {
					c.add_initializer_ref_candidates(child.value, cur_module, imports, mut refs)
				}
			}
			.selector {
				if !c.selector_base_is_local(child, local_values) && child.children_count > 0 {
					base := c.a.child_node(child, 0)
					if base.kind == .ident && base.value.len > 0 && child.value.len > 0 {
						c.add_initializer_ref_candidates('${base.value}.${child.value}',
							cur_module, imports, mut refs)
					}
				}
			}
			else {}
		}

		if child.children_count > 0 {
			mut j := int(child.children_count) - 1
			for j >= 0 {
				if child.kind == .call && j == 0 {
					j--
					continue
				}
				if child.kind == .decl_assign && j % 2 == 0 {
					j--
					continue
				}
				next_id := c.a.child(child, j)
				if int(next_id) >= 0 {
					stack << next_id
				}
				j--
			}
		}
	}
}

fn (c &CallCollector) add_initializer_ref_candidates(name string, cur_module string, imports map[string]string, mut refs []string) {
	if name.len == 0 {
		return
	}
	for candidate in c.value_name_candidates(name, cur_module, imports) {
		if candidate in c.const_decls {
			markused_push_receiver_candidate(mut refs, candidate)
		}
	}
}

fn (c &CallCollector) local_value_info(node &flat.Node, cur_module string, imports map[string]string) (map[string]bool, map[string]string) {
	mut names := map[string]bool{}
	mut type_names := map[string]string{}
	param_types := c.local_fn_param_type_names(node, cur_module)
	mut stack := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := c.a.child(node, i)
		if int(child_id) >= 0 {
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		child := c.a.node(id)
		if child.kind == .param && child.value.len > 0 {
			names[child.value] = true
			if child.typ.len > 0 {
				type_names[child.value] = param_types[child.value] or {
					markused_resolve_imported_type_name(child.typ, imports)
				}
			}
		} else if child.kind == .decl_assign {
			mut i := 0
			for i < child.children_count {
				lhs_id := c.a.child(child, i)
				if int(lhs_id) >= 0 {
					lhs := c.a.node(lhs_id)
					if lhs.kind == .ident && lhs.value.len > 0 {
						names[lhs.value] = true
						if i + 1 < child.children_count {
							rhs_id := c.a.child(child, i + 1)
							if int(rhs_id) >= 0 {
								type_name := if child.children_count == 2 && child.typ.len > 0 {
									c.local_decl_type_name(child.typ, rhs_id, cur_module, imports,
										type_names)
								} else {
									c.top_level_decl_rhs_type_name(rhs_id, cur_module, imports,
										names, type_names)
								}
								if type_name.len > 0 {
									type_names[lhs.value] = type_name
								}
							}
						}
					}
				}
				i += 2
			}
		}
		for i in 0 .. child.children_count {
			next_id := c.a.child(child, i)
			if int(next_id) >= 0 {
				stack << next_id
			}
		}
	}
	return names, type_names
}

fn (c &CallCollector) local_fn_param_type_names(node &flat.Node, cur_module string) map[string]string {
	mut result := map[string]string{}
	mut param_names := []string{}
	for i in 0 .. node.children_count {
		child := c.a.child_node(node, i)
		if child.kind == .param && child.value.len > 0 {
			param_names << child.value
		}
	}
	if param_names.len == 0 {
		return result
	}
	for candidate in markused_fn_signature_name_candidates(node.value, cur_module) {
		params := c.tc.fn_param_types[candidate] or { continue }
		if params.len != param_names.len {
			continue
		}
		for i, name in param_names {
			result[name] = params[i].name()
		}
		return result
	}
	return result
}

fn (c &CallCollector) local_decl_type_name(declared string, rhs_id flat.NodeId, cur_module string, imports map[string]string, local_types map[string]string) string {
	declared_type := c.tc.parse_type(declared)
	if declared_type is types.Alias {
		return declared_type.name
	}
	rhs_name := c.top_level_decl_rhs_type_name(rhs_id, cur_module, imports, map[string]bool{},
		local_types)
	if rhs_name.len == 0 {
		return declared
	}
	rhs_type := c.tc.parse_type(rhs_name)
	if rhs_type is types.Alias && markused_alias_base_matches_type(c.tc, rhs_type, declared_type) {
		return rhs_type.name
	}
	return declared
}

fn markused_fn_signature_name_candidates(name string, cur_module string) []string {
	dotted_name := qualify_fn(cur_module, name)
	mut names := []string{}
	if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
		names << dotted_name
		names << markused_c_name(dotted_name)
		names << name
		names << markused_c_name(name)
	} else {
		names << name
		names << markused_c_name(name)
		names << dotted_name
		names << markused_c_name(dotted_name)
	}
	mut deduped := []string{cap: names.len}
	for item in names {
		if item.len > 0 && item !in deduped {
			deduped << item
		}
	}
	return deduped
}

fn (c &CallCollector) local_values_need_visibility(local_values map[string]bool, cur_module string, imports map[string]string) bool {
	for name, _ in local_values {
		if c.name_may_reference_fn(name, cur_module, imports) {
			return true
		}
	}
	return false
}

fn markused_visible_local_idents(a &flat.FlatAst, root &flat.Node, local_values map[string]bool) map[int]bool {
	mut visible_ids := map[int]bool{}
	if local_values.len == 0 {
		return visible_ids
	}
	mut locals := map[string]int{}
	mut local_stack := []string{}
	if root.kind == .fn_decl {
		for i in 0 .. root.children_count {
			child := a.child_node(root, i)
			if child.kind == .param && child.value.len > 0 {
				markused_push_visible_local(child.value, mut locals, mut local_stack)
			}
		}
	}
	markused_collect_visible_local_idents(a, root, local_values, mut locals, mut local_stack, mut
		visible_ids)
	return visible_ids
}

fn markused_ident_is_visible_local(id flat.NodeId, name string, local_values map[string]bool, visible_local_idents map[int]bool) bool {
	return name.len > 0 && name in local_values && int(id) in visible_local_idents
}

fn markused_push_visible_local(name string, mut locals map[string]int, mut local_stack []string) {
	if name.len == 0 {
		return
	}
	locals[name] = (locals[name] or { 0 }) + 1
	local_stack << name
}

fn markused_restore_visible_local_scope(mark int, mut locals map[string]int, mut local_stack []string) {
	for local_stack.len > mark {
		name := local_stack.pop()
		count := locals[name] or { 0 }
		if count <= 1 {
			locals.delete(name)
		} else {
			locals[name] = count - 1
		}
	}
}

fn markused_local_visible(name string, local_values map[string]bool, locals map[string]int) bool {
	return name.len > 0 && name in local_values && (locals[name] or { 0 }) > 0
}

fn markused_collect_visible_local_idents(a &flat.FlatAst, node &flat.Node, local_values map[string]bool, mut locals map[string]int, mut local_stack []string, mut visible_ids map[int]bool) {
	for i in 0 .. node.children_count {
		child_id := a.child(node, i)
		if int(child_id) < 0 {
			continue
		}
		child := a.node(child_id)
		match child.kind {
			.fn_decl, .c_fn_decl, .fn_literal {
				continue
			}
			.block, .if_expr, .match_stmt, .for_stmt, .for_in_stmt {
				scope_mark := local_stack.len
				markused_collect_visible_local_idents(a, child, local_values, mut locals, mut
					local_stack, mut visible_ids)
				markused_restore_visible_local_scope(scope_mark, mut locals, mut local_stack)
			}
			.decl_assign {
				mut i2 := 1
				for i2 < child.children_count {
					rhs_id := a.child(child, i2)
					if int(rhs_id) >= 0 {
						markused_mark_visible_local_ident(a, rhs_id, local_values, locals, mut
							visible_ids)
						rhs := a.node(rhs_id)
						markused_collect_visible_local_idents(a, rhs, local_values, mut locals, mut
							local_stack, mut visible_ids)
					}
					i2 += 2
				}
				i2 = 0
				for i2 + 1 < child.children_count {
					lhs_id := a.child(child, i2)
					lhs := a.node(lhs_id)
					if lhs.kind == .ident && markused_local_visible(lhs.value, local_values, locals) {
						visible_ids[int(lhs_id)] = true
					}
					if lhs.kind == .ident && lhs.value.len > 0 {
						markused_push_visible_local(lhs.value, mut locals, mut local_stack)
					}
					i2 += 2
				}
			}
			else {
				if child.kind == .ident && markused_local_visible(child.value, local_values, locals) {
					visible_ids[int(child_id)] = true
				}
				markused_collect_visible_local_idents(a, child, local_values, mut locals, mut
					local_stack, mut visible_ids)
			}
		}
	}
}

fn markused_mark_visible_local_ident(a &flat.FlatAst, id flat.NodeId, local_values map[string]bool, locals map[string]int, mut visible_ids map[int]bool) {
	node := a.node(id)
	if node.kind == .ident && markused_local_visible(node.value, local_values, locals) {
		visible_ids[int(id)] = true
	}
}

fn (c &CallCollector) top_level_decl_rhs_type_name(rhs_id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string) string {
	rhs := c.a.node(rhs_id)
	if rhs.kind == .struct_init && rhs.value.len > 0 {
		return c.struct_lookup_name(markused_resolve_imported_type_name(rhs.value, imports),
			cur_module)
	}
	type_name := c.top_level_expr_type_name(rhs_id, cur_module, imports, local_values, local_types,
		false)
	if type_name.len > 0 {
		struct_type := c.struct_lookup_name(type_name, cur_module)
		if struct_type.len > 0 {
			return struct_type
		}
		return type_name
	}
	return ''
}

fn (c &CallCollector) collect_top_level_stmt_calls(id flat.NodeId, cur_module string, imports map[string]string, mut local_values map[string]bool, mut local_types map[string]string, mut calls []string) {
	if int(id) < 0 {
		return
	}
	node := c.a.node(id)
	match node.kind {
		.decl_assign, .assign {
			c.collect_top_level_assign_calls(node, cur_module, imports, mut local_values, mut
				local_types, mut calls)
		}
		.global_decl {
			c.collect_top_level_global_decl_calls(node, cur_module, imports, mut local_values, mut
				local_types, mut calls)
		}
		.block, .for_stmt, .for_in_stmt {
			mut nested_values := markused_clone_bool_map(local_values)
			mut nested_types := markused_clone_string_map(local_types)
			c.collect_top_level_child_calls(node, cur_module, imports, mut nested_values, mut
				nested_types, mut calls)
		}
		.if_expr, .match_stmt {
			c.collect_top_level_branch_calls(node, cur_module, imports, local_values, local_types, mut
				calls)
		}
		else {
			c.collect_top_level_expr_calls(id, cur_module, imports, local_values, local_types, mut
				calls)
		}
	}
}

fn (c &CallCollector) collect_top_level_child_calls(node &flat.Node, cur_module string, imports map[string]string, mut local_values map[string]bool, mut local_types map[string]string, mut calls []string) {
	for i in 0 .. node.children_count {
		child_id := c.a.child(node, i)
		if int(child_id) >= 0 {
			c.collect_top_level_stmt_calls(child_id, cur_module, imports, mut local_values, mut
				local_types, mut calls)
		}
	}
}

fn (c &CallCollector) collect_top_level_branch_calls(node &flat.Node, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string, mut calls []string) {
	for i in 0 .. node.children_count {
		child_id := c.a.child(node, i)
		if int(child_id) < 0 {
			continue
		}
		mut branch_values := local_values.clone()
		mut branch_types := local_types.clone()
		c.collect_top_level_stmt_calls(child_id, cur_module, imports, mut branch_values, mut
			branch_types, mut calls)
	}
}

fn (c &CallCollector) collect_top_level_assign_calls(node &flat.Node, cur_module string, imports map[string]string, mut local_values map[string]bool, mut local_types map[string]string, mut calls []string) {
	if node.kind !in [.decl_assign, .assign] {
		return
	}
	mut i := 0
	pre_values := markused_clone_bool_map(local_values)
	pre_types := markused_clone_string_map(local_types)
	for i + 1 < node.children_count {
		rhs_id := c.a.child(node, i + 1)
		if int(rhs_id) >= 0 {
			mut rhs_values := markused_clone_bool_map(pre_values)
			mut rhs_types := markused_clone_string_map(pre_types)
			c.collect_top_level_stmt_calls(rhs_id, cur_module, imports, mut rhs_values, mut
				rhs_types, mut calls)
		}
		i += 2
	}
	i = 0
	for i + 1 < node.children_count {
		lhs_id := c.a.child(node, i)
		rhs_id := c.a.child(node, i + 1)
		if int(lhs_id) >= 0 && int(rhs_id) >= 0 {
			lhs := c.a.node(lhs_id)
			if lhs.kind == .ident && lhs.value.len > 0 {
				local_values[lhs.value] = true
				type_name := c.top_level_decl_rhs_type_name(rhs_id, cur_module, imports,
					pre_values, pre_types)
				if type_name.len > 0 {
					local_types[lhs.value] = type_name
				}
			}
		}
		i += 2
	}
}

fn (c &CallCollector) collect_top_level_global_decl_calls(node &flat.Node, cur_module string, imports map[string]string, mut local_values map[string]bool, mut local_types map[string]string, mut calls []string) {
	if node.kind != .global_decl {
		return
	}
	pre_values := markused_clone_bool_map(local_values)
	pre_types := markused_clone_string_map(local_types)
	for i in 0 .. node.children_count {
		field := c.a.child_node(node, i)
		if field.children_count == 0 {
			continue
		}
		expr_id := c.a.child(field, 0)
		if int(expr_id) >= 0 {
			mut rhs_values := markused_clone_bool_map(pre_values)
			mut rhs_types := markused_clone_string_map(pre_types)
			c.collect_top_level_stmt_calls(expr_id, cur_module, imports, mut rhs_values, mut
				rhs_types, mut calls)
		}
	}
	for i in 0 .. node.children_count {
		field := c.a.child_node(node, i)
		if field.value.len == 0 {
			continue
		}
		local_values[field.value] = true
		if field.children_count > 0 {
			expr_id := c.a.child(field, 0)
			type_name := c.top_level_decl_rhs_type_name(expr_id, cur_module, imports, pre_values,
				pre_types)
			if type_name.len > 0 {
				local_types[field.value] = type_name
			}
		}
	}
}

fn (c &CallCollector) collect_top_level_expr_calls(id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string, mut calls []string) {
	if int(id) < 0 {
		return
	}
	mut stack := [id]
	for stack.len > 0 {
		child_id := stack.pop()
		child := c.a.node(child_id)
		match child.kind {
			.ident {
				if child.value !in local_values {
					c.collect_fn_value_ident(child_id, child.value, cur_module, imports, false, mut
						calls)
				}
			}
			.selector {
				if !c.top_level_selector_base_is_local(child, local_values) {
					c.collect_fn_value_selector(child_id, child, cur_module, imports, mut calls)
				}
			}
			.call {
				c.collect_top_level_call(child_id, child, cur_module, imports, local_values,
					local_types, mut calls)
			}
			.prefix {
				if child.op == .amp && child.children_count > 0 {
					inner_id := c.a.child(child, 0)
					if int(inner_id) >= 0 {
						inner := c.a.node(inner_id)
						if inner.kind == .struct_init {
							calls << 'memdup'
						}
					}
				}
			}
			.string_interp {
				calls << 'string_plus_many'
			}
			.assign, .selector_assign, .index_assign {
				c.collect_assign_operator_call(child, cur_module, local_types, mut calls)
			}
			.infix {
				if child.op == .plus {
					calls << 'string__plus'
				}
				if child.children_count >= 2 {
					lhs_id := c.a.child(child, 0)
					rhs_id := c.a.child(child, 1)
					if child.op in [.dot, .amp] {
						lhs_name := c.qualified_expr_name(lhs_id)
						if lhs_name.len > 0 {
							rhs := c.a.node(rhs_id)
							if rhs.kind == .call && rhs.children_count > 0 {
								fn_node := c.a.child_node(rhs, 0)
								if fn_node.kind == .ident && fn_node.value.len > 0 {
									mod_name := if lhs_name in imports {
										imports[lhs_name]
									} else {
										lhs_name
									}
									calls << mod_name + '.' + fn_node.value
									calls << qualify_fn(cur_module, lhs_name + '.' + fn_node.value)
								}
							}
						}
					}
					if int(lhs_id) >= 0 {
						c.collect_struct_operator_call(lhs_id, child.op, cur_module, local_types, mut
							calls)
					}
				}
			}
			.or_expr {
				if child.children_count > 0 {
					expr_id := c.a.child(child, 0)
					c.collect_zero_struct_default_calls(c.node_type(expr_id), cur_module, imports, mut
						calls)
				}
			}
			.struct_init {
				c.collect_struct_default_calls(child, cur_module, imports, mut calls)
			}
			else {}
		}

		if child.children_count > 0 {
			mut j := int(child.children_count) - 1
			for j >= 0 {
				next_id := c.a.child(child, j)
				if int(next_id) >= 0 {
					stack << next_id
				}
				j--
			}
		}
	}
}

fn (c &CallCollector) top_level_selector_base_is_local(node flat.Node, local_values map[string]bool) bool {
	return c.selector_base_is_local(&node, local_values)
}

fn (c &CallCollector) selector_base_is_local(node &flat.Node, local_values map[string]bool) bool {
	if node.children_count == 0 {
		return false
	}
	base_id := c.a.child(node, 0)
	if int(base_id) < 0 {
		return false
	}
	base := c.a.node(base_id)
	return base.kind == .ident && base.value in local_values
}

fn (c &CallCollector) expr_lowers_to_map_str(id flat.NodeId, local_values map[string]bool, local_types map[string]string) bool {
	if int(id) < 0 {
		return false
	}
	if markused_type_lowers_to_map_str(c.node_type(id)) {
		return true
	}
	expr := c.a.node(id)
	if expr.kind == .ident && expr.value.len > 0 && expr.value in local_values {
		if type_name := local_types[expr.value] {
			return markused_type_name_lowers_to_map_str(type_name, c.tc)
		}
	}
	return false
}

fn (c &CallCollector) top_level_expr_lowers_to_map_str(id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string) bool {
	if int(id) < 0 {
		return false
	}
	if markused_type_lowers_to_map_str(c.node_type(id)) {
		return true
	}
	type_name := c.top_level_expr_type_name(id, cur_module, imports, local_values, local_types,
		false)
	return type_name.len > 0 && markused_type_name_lowers_to_map_str(type_name, c.tc)
}

fn (c &CallCollector) collect_top_level_call(call_id flat.NodeId, call &flat.Node, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string, mut calls []string) {
	mut resolved_call := ''
	if resolved := c.tc.resolved_call_name(call_id) {
		resolved_call = resolved
	}
	c.collect_lowered_join_path_single(call, resolved_call, mut calls)
	if call.children_count == 0 {
		if resolved_call.len > 0 {
			calls << resolved_call
		}
		return
	}
	callee_id := c.a.child(call, 0)
	if int(callee_id) < 0 {
		return
	}
	callee := c.a.node(callee_id)
	if callee.kind == .ident && callee.value.len > 0 {
		if resolved_call.len > 0 {
			calls << resolved_call
		}
		if callee.value in ['print', 'println', 'eprint', 'eprintln'] && call.children_count >= 2 {
			arg_id := c.a.child(call, 1)
			if c.top_level_expr_lowers_to_map_str(arg_id, cur_module, imports, local_values,
				local_types)
			{
				calls << 'string__plus'
			}
		}
		if callee.value !in local_values {
			calls << callee.value
			qcallee := qualify_fn(cur_module, callee.value)
			if qcallee != callee.value {
				calls << qcallee
			}
		}
	} else if callee.kind == .selector && callee.value.len > 0 {
		c.collect_top_level_selector_call(callee, callee.value, resolved_call, cur_module, imports,
			local_values, local_types, mut calls)
	} else if resolved_call.len > 0 {
		calls << resolved_call
	}
	c.collect_generic_alias_operator_usage(call, resolved_call, cur_module, imports, local_values,
		local_types, mut calls)
	for ci in 1 .. call.children_count {
		arg_id := c.a.child(call, ci)
		if int(arg_id) >= 0 {
			arg := c.a.node(arg_id)
			if arg.kind == .ident && arg.value.len > 0 {
				if arg.value !in local_values {
					c.collect_fn_value_ident(arg_id, arg.value, cur_module, imports, false, mut
						calls)
				}
			}
		}
	}
}

fn (c &CallCollector) collect_generic_alias_operator_usage(call &flat.Node, resolved_call string, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string, mut calls []string) {
	if call.children_count == 0 {
		return
	}
	for candidate in c.generic_alias_operator_call_candidates(call, resolved_call, cur_module,
		imports, local_values) {
		info := c.fn_decls[candidate] or { continue }
		fn_node := c.a.node(info.node_id)
		generic_params := c.generic_fn_param_names(candidate, fn_node)
		param_texts := c.generic_fn_param_type_texts(candidate, fn_node)
		inferred := c.infer_alias_generic_args(call, param_texts, generic_params, cur_module,
			imports, local_values, local_types)
		if inferred.len == 0 {
			continue
		}
		specialized_locals := c.generic_alias_local_type_names(fn_node, info.module, imports,
			inferred)
		c.collect_generic_body_alias_operator_calls(fn_node, info.module, specialized_locals, mut
			calls)
	}
}

fn (c &CallCollector) generic_alias_operator_call_candidates(call &flat.Node, resolved_call string, cur_module string, imports map[string]string, local_values map[string]bool) []string {
	mut names := []string{}
	c.add_generic_alias_candidate(resolved_call, mut names)
	mut callee := c.a.child_node(call, 0)
	if callee.kind == .index && callee.children_count > 0 && callee.value != 'range' {
		callee = c.a.child_node(callee, 0)
	}
	if callee.kind == .ident && callee.value.len > 0 && callee.value !in local_values {
		c.add_generic_alias_candidate(callee.value, mut names)
		c.add_generic_alias_candidate(qualify_fn(cur_module, callee.value), mut names)
	} else if callee.kind == .selector && callee.value.len > 0 && callee.children_count > 0 {
		base := c.a.child_node(callee, 0)
		if base.kind == .ident && base.value.len > 0 && base.value !in local_values {
			if imported := imports[base.value] {
				c.add_generic_alias_candidate('${imported}.${callee.value}', mut names)
			}
			c.add_generic_alias_candidate('${base.value}.${callee.value}', mut names)
			c.add_generic_alias_candidate(qualify_fn(cur_module, '${base.value}.${callee.value}'), mut
				names)
		}
	}
	return names
}

fn (c &CallCollector) add_generic_alias_candidate(name string, mut names []string) {
	if name.len == 0 {
		return
	}
	if c.generic_candidate_is_generic(name) {
		if name !in names {
			names << name
		}
		return
	}
	if marker := name.index('_T_') {
		base := name[..marker]
		if c.generic_candidate_is_generic(base) && base !in names {
			names << base
		}
	}
}

fn (c &CallCollector) generic_candidate_is_generic(name string) bool {
	if name in c.tc.fn_generic_params {
		return true
	}
	if info := c.fn_decls[name] {
		node := c.a.node(info.node_id)
		return node.kind == .fn_decl && node.generic_params.len > 0
	}
	return false
}

fn (c &CallCollector) generic_fn_param_names(name string, fn_node &flat.Node) []string {
	if params := c.tc.fn_generic_params[name] {
		return params
	}
	return fn_node.generic_params.clone()
}

fn (c &CallCollector) generic_fn_param_type_texts(name string, fn_node &flat.Node) []string {
	if param_texts := c.tc.fn_param_type_texts[name] {
		return param_texts
	}
	mut result := []string{}
	for i in 0 .. fn_node.children_count {
		child := c.a.child_node(fn_node, i)
		if child.kind == .param {
			result << child.typ
		}
	}
	return result
}

fn (c &CallCollector) infer_alias_generic_args(call &flat.Node, param_texts []string, generic_params []string, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string) map[string]string {
	mut inferred := map[string]string{}
	if call.children_count == 0 || param_texts.len == 0 || generic_params.len == 0 {
		return inferred
	}
	mut param_idx := 0
	callee := c.a.child_node(call, 0)
	if callee.kind == .selector && callee.children_count > 0 {
		if actual := c.alias_aware_expr_type(c.a.child(callee, 0), cur_module, imports,
			local_values, local_types)
		{
			markused_infer_alias_generic_type(param_texts[0], actual, generic_params, mut inferred)
			param_idx = 1
		}
	}
	for arg_i in 1 .. call.children_count {
		if param_idx >= param_texts.len {
			break
		}
		arg_id := markused_generic_call_arg_value(c.a, c.a.child(call, arg_i))
		if actual := c.alias_aware_expr_type(arg_id, cur_module, imports, local_values, local_types) {
			markused_infer_alias_generic_type(param_texts[param_idx], actual, generic_params, mut
				inferred)
		}
		param_idx++
	}
	return inferred
}

fn (c &CallCollector) alias_aware_expr_type(id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string) ?types.Type {
	if int(id) < 0 {
		return none
	}
	if typ := c.syntax_alias_expr_type(id, cur_module, imports) {
		return typ
	}
	type_name := c.top_level_expr_type_name(id, cur_module, imports, local_values, local_types,
		false)
	if type_name.len > 0 {
		typ := c.tc.parse_type(type_name)
		if typ !is types.Unknown && typ !is types.Void {
			return typ
		}
	}
	typ := c.node_type(id)
	if typ !is types.Unknown && typ !is types.Void {
		return typ
	}
	return none
}

fn (c &CallCollector) syntax_alias_expr_type(id flat.NodeId, cur_module string, imports map[string]string) ?types.Type {
	if int(id) < 0 {
		return none
	}
	node := c.a.node(id)
	match node.kind {
		.array_literal {
			for i in 0 .. node.children_count {
				elem_id := c.a.child(node, i)
				elem_type := c.syntax_alias_expr_type(elem_id, cur_module, imports) or { continue }
				return types.Type(types.Array{
					elem_type: elem_type
				})
			}
		}
		.call {
			if node.children_count > 0 {
				callee := c.a.child_node(node, 0)
				if alias_type := c.alias_type_from_callee(callee, cur_module, imports) {
					return alias_type
				}
			}
		}
		.cast_expr {
			if node.value.len > 0 {
				if alias_type := c.alias_type_from_name(node.value, cur_module, imports) {
					return alias_type
				}
			}
		}
		else {}
	}

	return none
}

fn (c &CallCollector) alias_type_from_callee(callee &flat.Node, cur_module string, imports map[string]string) ?types.Type {
	if callee.kind == .ident && callee.value.len > 0 {
		return c.alias_type_from_name(callee.value, cur_module, imports)
	}
	if callee.kind == .selector && callee.value.len > 0 && callee.children_count > 0 {
		base := c.a.child_node(callee, 0)
		if base.kind == .ident && base.value.len > 0 {
			if imported := imports[base.value] {
				if alias_type := c.alias_type_from_name('${imported}.${callee.value}', cur_module,
					imports)
				{
					return alias_type
				}
			}
			return c.alias_type_from_name('${base.value}.${callee.value}', cur_module, imports)
		}
	}
	return none
}

fn (c &CallCollector) alias_type_from_name(name string, cur_module string, imports map[string]string) ?types.Type {
	resolved := markused_resolve_imported_type_name(name, imports)
	for candidate in markused_alias_type_candidates(resolved, name, cur_module) {
		if candidate in c.tc.type_aliases {
			return c.tc.parse_type(candidate)
		}
	}
	return none
}

fn markused_alias_type_candidates(resolved string, name string, cur_module string) []string {
	mut candidates := []string{cap: 4}
	candidates << resolved
	candidates << name
	if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
		candidates << '${cur_module}.${resolved}'
		candidates << '${cur_module}.${name}'
	}
	mut deduped := []string{cap: candidates.len}
	for candidate in candidates {
		if candidate.len > 0 && candidate !in deduped {
			deduped << candidate
		}
	}
	return deduped
}

fn markused_generic_call_arg_value(a &flat.FlatAst, id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := a.node(id)
	if node.kind == .field_init && node.children_count > 0 {
		return a.child(node, 0)
	}
	return id
}

fn markused_infer_alias_generic_type(param_text string, actual types.Type, generic_params []string, mut inferred map[string]string) {
	clean := param_text.trim_space()
	if clean.len == 0 {
		return
	}
	if clean.starts_with('mut ') {
		markused_infer_alias_generic_type(clean[4..], actual, generic_params, mut inferred)
		return
	}
	if clean.starts_with('&') {
		if actual is types.Pointer {
			markused_infer_alias_generic_type(clean[1..], actual.base_type, generic_params, mut
				inferred)
		} else {
			markused_infer_alias_generic_type(clean[1..], actual, generic_params, mut inferred)
		}
		return
	}
	if clean.starts_with('[]') {
		actual_clean := types.unwrap_pointer(actual)
		if actual_clean is types.Array {
			markused_infer_alias_generic_type(clean[2..], actual_clean.elem_type, generic_params, mut
				inferred)
		} else if actual_clean is types.ArrayFixed {
			markused_infer_alias_generic_type(clean[2..], actual_clean.elem_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('...') {
		markused_infer_alias_generic_type(clean[3..], actual, generic_params, mut inferred)
		return
	}
	if clean.starts_with('?') {
		if actual is types.OptionType {
			markused_infer_alias_generic_type(clean[1..], actual.base_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('!') {
		if actual is types.ResultType {
			markused_infer_alias_generic_type(clean[1..], actual.base_type, generic_params, mut
				inferred)
		}
		return
	}
	if clean.starts_with('map[') {
		bracket_end := markused_matching_bracket(clean, 3)
		actual_clean := types.unwrap_pointer(actual)
		if bracket_end < clean.len && actual_clean is types.Map {
			markused_infer_alias_generic_type(clean[4..bracket_end], actual_clean.key_type,
				generic_params, mut inferred)
			markused_infer_alias_generic_type(clean[bracket_end + 1..], actual_clean.value_type,
				generic_params, mut inferred)
		}
		return
	}
	if clean in generic_params {
		type_name := resolve_type_name(actual)
		if type_name.len > 0 {
			inferred[clean] = type_name
		}
	}
}

fn (c &CallCollector) generic_alias_local_type_names(fn_node &flat.Node, cur_module string, imports map[string]string, inferred map[string]string) map[string]string {
	mut result := map[string]string{}
	mut stack := []flat.NodeId{cap: int(fn_node.children_count)}
	for i in 0 .. fn_node.children_count {
		child_id := c.a.child(fn_node, i)
		if int(child_id) >= 0 {
			child := c.a.node(child_id)
			if child.kind == .param && child.value.len > 0 && child.typ.len > 0 {
				result[child.value] = markused_resolve_imported_type_name(markused_substitute_alias_generics(child.typ,
					inferred), imports)
			}
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		child := c.a.node(id)
		if child.kind == .param && child.value.len > 0 && child.typ.len > 0 {
			result[child.value] = markused_resolve_imported_type_name(markused_substitute_alias_generics(child.typ,
				inferred), imports)
		} else if child.kind == .decl_assign {
			mut i := 0
			for i + 1 < child.children_count {
				lhs_id := c.a.child(child, i)
				rhs_id := c.a.child(child, i + 1)
				if int(lhs_id) >= 0 && int(rhs_id) >= 0 {
					lhs := c.a.node(lhs_id)
					if lhs.kind == .ident && lhs.value.len > 0 {
						type_name := if child.children_count == 2 && child.typ.len > 0 {
							c.local_decl_type_name(markused_substitute_alias_generics(child.typ,
								inferred), rhs_id, cur_module, imports, result)
						} else {
							c.top_level_decl_rhs_type_name(rhs_id, cur_module, imports,
								map[string]bool{}, result)
						}
						if type_name.len > 0 {
							result[lhs.value] = type_name
						}
					}
				}
				i += 2
			}
		}
		for i in 0 .. child.children_count {
			next_id := c.a.child(child, i)
			if int(next_id) >= 0 {
				stack << next_id
			}
		}
	}
	return result
}

fn markused_substitute_alias_generics(type_text string, inferred map[string]string) string {
	clean := type_text.trim_space()
	if replacement := inferred[clean] {
		return replacement
	}
	if clean.starts_with('&') {
		return '&' + markused_substitute_alias_generics(clean[1..], inferred)
	}
	if clean.starts_with('mut ') {
		return 'mut ' + markused_substitute_alias_generics(clean[4..], inferred)
	}
	if clean.starts_with('[]') {
		return '[]' + markused_substitute_alias_generics(clean[2..], inferred)
	}
	if clean.starts_with('...') {
		return '...' + markused_substitute_alias_generics(clean[3..], inferred)
	}
	if clean.starts_with('?') {
		return '?' + markused_substitute_alias_generics(clean[1..], inferred)
	}
	if clean.starts_with('!') {
		return '!' + markused_substitute_alias_generics(clean[1..], inferred)
	}
	if clean.starts_with('map[') {
		bracket_end := markused_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := markused_substitute_alias_generics(clean[4..bracket_end], inferred)
			value := markused_substitute_alias_generics(clean[bracket_end + 1..], inferred)
			return 'map[${key}]${value}'
		}
	}
	return clean
}

fn (c &CallCollector) collect_generic_body_alias_operator_calls(fn_node &flat.Node, cur_module string, local_types map[string]string, mut calls []string) {
	mut stack := []flat.NodeId{cap: int(fn_node.children_count)}
	for i in 0 .. fn_node.children_count {
		child_id := c.a.child(fn_node, i)
		if int(child_id) >= 0 {
			stack << child_id
		}
	}
	for stack.len > 0 {
		child_id := stack.pop()
		child := c.a.node(child_id)
		match child.kind {
			.assign, .selector_assign, .index_assign {
				c.collect_assign_operator_call(child, cur_module, local_types, mut calls)
			}
			.infix {
				if child.children_count >= 2 {
					lhs_id := c.a.child(child, 0)
					if int(lhs_id) >= 0 {
						c.collect_struct_operator_call(lhs_id, child.op, cur_module, local_types, mut
							calls)
					}
				}
			}
			else {}
		}

		for i in 0 .. child.children_count {
			next_id := c.a.child(child, i)
			if int(next_id) >= 0 {
				stack << next_id
			}
		}
	}
}

fn (c &CallCollector) collect_top_level_selector_call(callee &flat.Node, method string, resolved_call string, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string, mut calls []string) {
	mut has_exact_selector_call := false
	if callee.children_count > 0 {
		base_id := c.a.child(callee, 0)
		if int(base_id) >= 0 {
			base := c.a.node(base_id)
			base_is_local_value := base.kind == .ident && base.value in local_values
			if method == 'str'
				&& c.top_level_expr_lowers_to_map_str(base_id, cur_module, imports, local_values, local_types) {
				calls << 'string__plus'
			}
			if base_is_local_value {
				has_exact_selector_call = c.collect_top_level_typed_receiver_method(base_id,
					method, cur_module, imports, local_values, local_types, mut calls)
			}
			if !has_exact_selector_call && !base_is_local_value {
				has_exact_selector_call = c.collect_checker_selected_call(resolved_call, mut calls)
			}
			if !has_exact_selector_call {
				has_exact_selector_call = c.collect_top_level_typed_receiver_method(base_id,
					method, cur_module, imports, local_values, local_types, mut calls)
			}
			if !has_exact_selector_call && base.kind == .ident && base.value in imports
				&& base.value !in local_values {
				calls << imports[base.value] + '.' + method
				has_exact_selector_call = true
			}
			if !has_exact_selector_call && resolved_call.len > 0 {
				calls << resolved_call
				has_exact_selector_call = true
			}
			if !has_exact_selector_call {
				c.collect_top_level_selector_fallback(base, method, cur_module, imports,
					local_values, mut calls)
			}
		}
	}
}

fn (c &CallCollector) collect_checker_selected_call(resolved_call string, mut calls []string) bool {
	if resolved_call.len == 0 || markused_is_builtin_collection_resolved_call(resolved_call)
		|| !c.is_known_fn_name(resolved_call) {
		return false
	}
	c.add_typed_receiver_method_name(resolved_call, mut calls)
	return true
}

fn markused_is_builtin_collection_resolved_call(name string) bool {
	return name.len == 0 || markused_is_raw_collection_method_name(name, 'array.')
		|| name == 'array_clone' || markused_is_runtime_collection_helper_name(name)
		|| markused_is_raw_collection_method_name(name, 'map.')
}

fn markused_is_raw_collection_method_name(name string, prefix string) bool {
	if !name.starts_with(prefix) {
		return false
	}
	rest := name[prefix.len..]
	return rest.len > 0 && !rest.contains('.')
}

fn markused_is_runtime_collection_helper_name(name string) bool {
	return name in ['array__clone', 'array__reverse', 'array__prepend', 'array__insert',
		'array__push_many', 'array__needs_unique_shift', 'map__delete', 'map__move', 'map__reserve',
		'map__keys', 'map__values', 'map__clear', 'map__free', 'map__get', 'map__get_check',
		'map__exists', 'map__set']
}

fn (c &CallCollector) collect_top_level_selector_fallback(base &flat.Node, method string, cur_module string, imports map[string]string, local_values map[string]bool, mut calls []string) {
	if base.kind == .ident && base.value.len > 0 {
		if base.value in local_values {
			return
		}
		if base.value in imports {
			return
		}
		mod_name := base.value
		calls << mod_name + '.' + method
		calls << qualify_fn(cur_module, base.value + '.' + method)
		if base.value.len > 0 && base.value[0] >= `A` && base.value[0] <= `Z` {
			named_type := c.tc.parse_type(base.value)
			named_type_name := resolve_type_name(named_type)
			if named_type_name.len > 0 {
				calls << named_type_name + '.' + method
			}
		}
	} else if base.kind == .selector && base.children_count > 0 {
		inner_id := c.a.child(base, 0)
		if int(inner_id) >= 0 {
			inner := c.a.node(inner_id)
			if inner.kind == .ident && inner.value.len > 0 {
				mod_name := if inner.value in imports {
					imports[inner.value]
				} else {
					inner.value
				}
				calls << mod_name + '.' + base.value + '.' + method
			}
		}
	}
}

fn (c &CallCollector) collect_lowered_join_path_single(call &flat.Node, resolved_call string, mut calls []string) {
	mut call_names := []string{}
	if resolved_call.len > 0 {
		call_names << resolved_call
	}
	if call.children_count > 0 {
		callee_id := c.a.child(call, 0)
		if int(callee_id) >= 0 {
			callee := c.a.node(callee_id)
			if callee.kind == .ident && callee.value.len > 0 {
				call_names << callee.value
			} else if callee.kind == .selector && callee.value.len > 0 && callee.children_count > 0 {
				base_id := c.a.child(callee, 0)
				if int(base_id) >= 0 {
					base := c.a.node(base_id)
					if base.kind == .ident && base.value.len > 0 {
						call_names << '${base.value}.${callee.value}'
					}
				}
			}
		}
	}
	if !call_names.any(it == 'join_path' || it == 'os.join_path') {
		return
	}
	if call.children_count <= 2 {
		return
	}
	for i in 1 .. call.children_count {
		arg := c.a.child_node(call, i)
		if arg.kind == .prefix && arg.value == '...' {
			return
		}
	}
	calls << 'join_path_single'
	calls << 'os.join_path_single'
}

fn (c &CallCollector) collect_top_level_typed_receiver_method(base_id flat.NodeId, method string, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string, mut calls []string) bool {
	type_name := c.top_level_receiver_type_name(base_id, cur_module, imports, local_values,
		local_types)
	if type_name.len == 0 {
		return false
	}
	method_name := c.typed_receiver_method_name(type_name, method, cur_module) or { return false }
	c.add_typed_receiver_method_name(method_name, mut calls)
	return true
}

fn (c &CallCollector) top_level_receiver_type_name(base_id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string) string {
	base := c.a.node(base_id)
	type_name := resolve_type_name(c.node_type(base_id))
	if type_name.len > 0 {
		struct_type := c.struct_lookup_name(type_name, cur_module)
		if struct_type.len > 0 {
			return struct_type
		}
		return type_name
	}
	if base.kind == .ident && base.value.len > 0 {
		if local_type := local_types[base.value] {
			return local_type
		}
		if base.value in local_values {
			return ''
		}
		if base.value in imports {
			return ''
		}
	}
	if base.kind == .or_expr {
		or_type_name := c.top_level_or_expr_base_type_name(base_id, cur_module, imports,
			local_values, local_types)
		if or_type_name.len > 0 {
			return or_type_name
		}
	}
	if base.kind == .ident && base.value.len > 0 {
		return c.value_type_name(base.value, cur_module, imports)
	}
	if base.kind == .selector {
		name := c.qualified_expr_name(base_id)
		if name.len > 0 {
			return c.value_type_name(name, cur_module, imports)
		}
	}
	return ''
}

fn (c &CallCollector) top_level_expr_type_name(id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string, unwrap_optional_result bool) string {
	node := c.a.node(id)
	if node.kind == .ident {
		if local_type := local_types[node.value] {
			return local_type
		}
	}
	if node.kind == .index {
		if elem_type := c.top_level_index_elem_type_name(id, cur_module, imports, local_values,
			local_types)
		{
			return elem_type
		}
	}
	typ := c.node_type(id)
	if type_name := markused_type_name(typ, unwrap_optional_result) {
		return type_name
	}
	match node.kind {
		.call {
			return c.top_level_call_return_type_name(id, cur_module, imports, local_values,
				local_types, unwrap_optional_result)
		}
		.or_expr {
			return c.top_level_or_expr_base_type_name(id, cur_module, imports, local_values,
				local_types)
		}
		.ident {
			return local_types[node.value] or { '' }
		}
		else {}
	}

	return ''
}

fn (c &CallCollector) top_level_index_elem_type_name(id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string) ?string {
	node := c.a.node(id)
	if node.children_count == 0 {
		return none
	}
	base_type_name := c.top_level_expr_type_name(c.a.child(node, 0), cur_module, imports,
		local_values, local_types, false)
	if base_type_name.len == 0 {
		return none
	}
	base_type := types.unwrap_pointer(c.tc.parse_type(base_type_name))
	if base_type is types.Array {
		return base_type.elem_type.name()
	}
	if base_type is types.ArrayFixed {
		return base_type.elem_type.name()
	}
	if base_type is types.Map {
		return base_type.value_type.name()
	}
	return none
}

fn (c &CallCollector) top_level_call_return_type_name(call_id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string, unwrap_optional_result bool) string {
	call := c.a.node(call_id)
	if call.kind != .call || call.children_count == 0 {
		return ''
	}
	callee_id := c.a.child(call, 0)
	if int(callee_id) < 0 {
		return ''
	}
	callee := c.a.node(callee_id)
	if callee.kind == .selector && callee.value.len > 0 && callee.children_count > 0 {
		base_id := c.a.child(callee, 0)
		base := c.a.node(base_id)
		type_name := c.top_level_receiver_type_name(base_id, cur_module, imports, local_values,
			local_types)
		if type_name.len > 0 {
			if method_name := c.typed_receiver_method_name(type_name, callee.value, cur_module) {
				return c.fn_return_type_name(method_name, unwrap_optional_result)
			}
		}
		if base.kind == .ident && base.value in imports {
			return c.fn_return_type_name('${imports[base.value]}.${callee.value}',
				unwrap_optional_result)
		}
	}
	if callee.kind == .ident && callee.value.len > 0 {
		name := qualify_fn(cur_module, callee.value)
		type_name := c.fn_return_type_name(name, unwrap_optional_result)
		if type_name.len > 0 {
			return type_name
		}
		return c.fn_return_type_name(callee.value, unwrap_optional_result)
	}
	return ''
}

fn (c &CallCollector) top_level_or_expr_base_type_name(id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string) string {
	node := c.a.node(id)
	if node.kind != .or_expr || node.children_count == 0 {
		return ''
	}
	return c.top_level_expr_type_name(c.a.child(node, 0), cur_module, imports, local_values,
		local_types, true)
}

fn (c &CallCollector) fn_return_type_name(name string, unwrap_optional_result bool) string {
	if typ := c.tc.fn_ret_types[name] {
		return markused_type_name_or_empty(typ, unwrap_optional_result)
	}
	lowered := markused_c_name(name)
	if lowered != name {
		if typ := c.tc.fn_ret_types[lowered] {
			return markused_type_name_or_empty(typ, unwrap_optional_result)
		}
	}
	return ''
}

// collect_struct_operator_call updates collect struct operator call state for markused.
fn (c &CallCollector) collect_struct_operator_call(lhs_id flat.NodeId, op flat.Op, cur_module string, local_types map[string]string, mut calls []string) {
	lhs_type := c.operator_lhs_type(lhs_id, local_types)
	for receiver in c.struct_operator_receivers_for_call(lhs_type, cur_module) {
		method_name := c.struct_operator_call_name(receiver, op) or { continue }
		c.add_operator_call_name(method_name, mut calls)
		return
	}
}

// collect_assign_operator_call adds operator overloads used through assignment operators.
fn (c &CallCollector) collect_assign_operator_call(node &flat.Node, cur_module string, local_types map[string]string, mut calls []string) {
	op := markused_assign_operator_symbol(node.op) or { return }
	mut i := 0
	for i + 1 < node.children_count {
		lhs_id := c.a.child(node, i)
		if int(lhs_id) >= 0 {
			c.collect_struct_operator_call(lhs_id, op, cur_module, local_types, mut calls)
		}
		i += 2
	}
}

fn (c &CallCollector) operator_lhs_type(lhs_id flat.NodeId, local_types map[string]string) types.Type {
	if int(lhs_id) >= 0 {
		lhs := c.a.node(lhs_id)
		if lhs.kind == .ident {
			if local_type := local_types[lhs.value] {
				typ := c.tc.parse_type(local_type)
				if typ !is types.Unknown && typ !is types.Void {
					return typ
				}
			}
		}
	}
	return c.node_type(lhs_id)
}

fn (c &CallCollector) struct_operator_receivers_for_call(lhs_type types.Type, cur_module string) []string {
	clean := types.unwrap_pointer(lhs_type)
	lhs_name := resolve_type_name(clean)
	mut receivers := []string{}
	if clean is types.Alias {
		markused_add_operator_receiver_candidates(lhs_name, cur_module, mut receivers)
		return receivers
	}
	struct_type := c.struct_lookup_name(lhs_name, cur_module)
	if struct_type.len > 0 {
		receivers << struct_type
	}
	return receivers
}

fn markused_add_operator_receiver_candidates(type_name string, cur_module string, mut receivers []string) {
	if type_name.len == 0 {
		return
	}
	receivers << type_name
	if !type_name.contains('.') && cur_module.len > 0 && cur_module != 'main'
		&& cur_module != 'builtin' {
		receivers << '${cur_module}.${type_name}'
	}
}

fn markused_alias_base_matches_type(tc &types.TypeChecker, alias_type types.Alias, typ types.Type) bool {
	base := types.unwrap_pointer(alias_type.base_type)
	clean := types.unwrap_pointer(typ)
	if base.name() == clean.name()
		|| base.name().all_after_last('.') == clean.name().all_after_last('.') {
		return true
	}
	return tc.c_type(base) == tc.c_type(clean)
}

// struct_operator_call_name supports struct operator call name handling for CallCollector.
fn (c &CallCollector) struct_operator_call_name(struct_type string, op flat.Op) ?string {
	if op_name := markused_struct_operator_symbol(op) {
		if method_name := c.struct_operator_fn_name(struct_type, op_name) {
			return method_name
		}
	}
	match op {
		.gt, .ge, .le {
			if method_name := c.struct_operator_fn_name(struct_type, '<') {
				return method_name
			}
		}
		.ne {
			if method_name := c.struct_operator_fn_name(struct_type, '==') {
				return method_name
			}
		}
		else {}
	}

	return none
}

// markused_assign_operator_symbol maps assignment operators to their binary operator.
fn markused_assign_operator_symbol(op flat.Op) ?flat.Op {
	match op {
		.plus_assign { return .plus }
		.minus_assign { return .minus }
		.mul_assign { return .mul }
		.div_assign { return .div }
		.mod_assign { return .mod }
		else {}
	}

	return none
}

// markused_struct_operator_symbol supports markused struct operator symbol handling for markused.
fn markused_struct_operator_symbol(op flat.Op) ?string {
	match op {
		.plus { return '+' }
		.minus { return '-' }
		.mul { return '*' }
		.div { return '/' }
		.mod { return '%' }
		.eq { return '==' }
		.ne { return '!=' }
		.lt { return '<' }
		.gt { return '>' }
		.le { return '<=' }
		.ge { return '>=' }
		else {}
	}

	return none
}

// struct_operator_fn_name supports struct operator fn name handling for CallCollector.
fn (c &CallCollector) struct_operator_fn_name(struct_type string, op_name string) ?string {
	method_name := '${struct_type}.${op_name}'
	if c.is_known_fn_name(method_name) {
		return method_name
	}
	cmethod_name := markused_c_name(method_name)
	if c.is_known_fn_name(cmethod_name) {
		return cmethod_name
	}
	return none
}

// is_known_fn_name reports whether is known fn name applies in markused.
fn (c &CallCollector) is_known_fn_name(name string) bool {
	return name in c.fn_decls || name in c.tc.fn_ret_types || name in c.tc.fn_param_types
}

// struct_lookup_name supports struct lookup name handling for CallCollector.
fn (c &CallCollector) struct_lookup_name(type_name string, cur_module string) string {
	if type_name.len == 0 {
		return ''
	}
	if type_name.contains('.') {
		if type_name in c.struct_decls {
			return type_name
		}
		short_type := type_name.all_after_last('.')
		type_mod := type_name.all_before_last('.')
		if info := c.struct_decls[short_type] {
			if info.module == type_mod {
				return short_type
			}
		}
		if info := c.struct_decls[type_name] {
			if info.module == type_mod {
				return type_name
			}
		}
		return ''
	}
	if info := c.struct_decls[type_name] {
		if info.module.len == 0 || info.module == cur_module {
			return type_name
		}
	}
	if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
		qtype := '${cur_module}.${type_name}'
		if qtype in c.struct_decls {
			return qtype
		}
	}
	return ''
}

// add_operator_call_name updates add operator call name state for CallCollector.
fn (c &CallCollector) add_operator_call_name(method_name string, mut calls []string) {
	calls << method_name
	lowered := markused_c_name(method_name)
	if lowered != method_name {
		calls << lowered
	}
}

// qualified_expr_name supports qualified expr name handling for CallCollector.
fn (c &CallCollector) qualified_expr_name(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := c.a.nodes[int(id)]
	match node.kind {
		.ident {
			return node.value
		}
		.selector {
			if node.children_count == 0 {
				return node.value
			}
			base := c.qualified_expr_name(c.a.child(&node, 0))
			if base.len == 0 {
				return node.value
			}
			return base + '.' + node.value
		}
		else {
			return ''
		}
	}
}

// collect_fn_value_ident updates collect fn value ident state for markused.
fn (c &CallCollector) collect_fn_value_ident(id flat.NodeId, name string, cur_module string, imports map[string]string, is_local_value bool, mut calls []string) {
	if is_local_value {
		return
	}
	if resolved := c.tc.resolved_fn_value_name(id) {
		calls << resolved
		return
	}
	if name.len == 0 || !c.name_may_reference_fn(name, cur_module, imports) {
		return
	}
	has_fn_decl := c.name_has_fn_decl(name, cur_module, imports)
	if !has_fn_decl && !c.node_is_fn_value(id) {
		return
	}
	c.add_fn_value_candidates(name, cur_module, imports, mut calls)
	c.add_const_alias_candidates(name, cur_module, imports, mut calls)
}

// collect_fn_value_selector updates collect fn value selector state for markused.
fn (c &CallCollector) collect_fn_value_selector(id flat.NodeId, node &flat.Node, cur_module string, imports map[string]string, mut calls []string) {
	if resolved := c.tc.resolved_fn_value_name(id) {
		calls << resolved
		return
	}
	if node.children_count == 0 {
		return
	}
	base := c.a.child_node(node, 0)
	if base.kind != .ident || base.value.len == 0 || node.value.len == 0 {
		return
	}
	name := '${base.value}.${node.value}'
	if !c.name_may_reference_fn(name, cur_module, imports) {
		return
	}
	has_fn_decl := c.name_has_fn_decl(name, cur_module, imports)
	if !has_fn_decl && !c.node_is_fn_value(id) {
		return
	}
	c.add_fn_value_candidates(name, cur_module, imports, mut calls)
	c.add_const_alias_candidates(name, cur_module, imports, mut calls)
	if base.value in imports {
		mod_name := imports[base.value]
		resolved_name := '${mod_name}.${node.value}'
		c.add_fn_value_candidates(resolved_name, cur_module, imports, mut calls)
		c.add_const_alias_candidates(resolved_name, cur_module, imports, mut calls)
	}
}

// name_may_reference_fn returns name may reference fn data for CallCollector.
fn (c &CallCollector) name_may_reference_fn(name string, cur_module string, imports map[string]string) bool {
	return c.name_has_candidate_decl(name, cur_module, imports, true)
}

// name_has_fn_decl returns name has fn decl data for CallCollector.
fn (c &CallCollector) name_has_fn_decl(name string, cur_module string, imports map[string]string) bool {
	return c.name_has_candidate_decl(name, cur_module, imports, false)
}

// name_has_candidate_decl returns name has candidate decl data for CallCollector.
fn (c &CallCollector) name_has_candidate_decl(name string, cur_module string, imports map[string]string, include_consts bool) bool {
	if !name.contains('.') && !c.fn_suffixes[name] && !(include_consts && c.const_suffixes[name]) {
		return false
	}
	if c.candidate_matches_decl(name, include_consts) {
		return true
	}
	qname := qualify_fn(cur_module, name)
	if qname != name && c.candidate_matches_decl(qname, include_consts) {
		return true
	}
	if name.contains('.') {
		base := name.all_before_last('.')
		member := name.all_after_last('.')
		if base in imports {
			imported_name := imports[base] + '.' + member
			if c.candidate_matches_decl(imported_name, include_consts) {
				return true
			}
		}
	}
	return false
}

// candidate_matches_decl supports candidate matches decl handling for CallCollector.
fn (c &CallCollector) candidate_matches_decl(candidate string, include_consts bool) bool {
	if candidate in c.fn_decls {
		return true
	}
	return include_consts && candidate in c.const_decls
}

// node_is_fn_value supports node is fn value handling for CallCollector.
fn (c &CallCollector) node_is_fn_value(id flat.NodeId) bool {
	expr_type := c.node_type(id)
	return expr_type is types.FnType
}

// add_fn_value_candidates updates add fn value candidates state for CallCollector.
fn (c &CallCollector) add_fn_value_candidates(name string, cur_module string, imports map[string]string, mut calls []string) {
	if name.len == 0 {
		return
	}
	calls << name
	qname := qualify_fn(cur_module, name)
	if qname != name {
		calls << qname
	}
	if name.contains('.') {
		base := name.all_before_last('.')
		member := name.all_after_last('.')
		if base in imports {
			calls << imports[base] + '.' + member
		}
	}
}

// add_const_alias_candidates converts add const alias candidates data for markused.
fn (c &CallCollector) add_const_alias_candidates(name string, cur_module string, imports map[string]string, mut calls []string) {
	c.add_const_alias_candidate(name, imports, mut calls)
	qname := qualify_fn(cur_module, name)
	if qname != name {
		c.add_const_alias_candidate(qname, imports, mut calls)
	}
	if name.contains('.') {
		base := name.all_before_last('.')
		member := name.all_after_last('.')
		if base in imports {
			c.add_const_alias_candidate(imports[base] + '.' + member, imports, mut calls)
		}
	}
}

// add_const_alias_candidate converts add const alias candidate data for markused.
fn (c &CallCollector) add_const_alias_candidate(const_name string, imports map[string]string, mut calls []string) {
	info := c.const_decls[const_name] or { return }
	expr := c.a.node(info.expr_id)
	match expr.kind {
		.ident {
			c.add_fn_value_candidates(expr.value, info.module, imports, mut calls)
		}
		.selector {
			if expr.children_count > 0 {
				base := c.a.child_node(expr, 0)
				if base.kind == .ident && base.value.len > 0 && expr.value.len > 0 {
					c.add_fn_value_candidates('${base.value}.${expr.value}', info.module, imports, mut
						calls)
					if base.value in imports {
						c.add_fn_value_candidates('${imports[base.value]}.${expr.value}',
							info.module, imports, mut calls)
					}
				}
			}
		}
		else {}
	}
}

// node_type supports node type handling for CallCollector.
fn (c &CallCollector) node_type(id flat.NodeId) types.Type {
	if t := c.tc.expr_type(id) {
		return t
	}
	return c.tc.resolve_type(id)
}

fn (c &CallCollector) collect_typed_receiver_method(base_id flat.NodeId, method string, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string, mut calls []string) bool {
	type_name := c.receiver_type_name(base_id, cur_module, imports, local_values, local_types)
	if type_name.len == 0 {
		return false
	}
	method_name := c.typed_receiver_method_name(type_name, method, cur_module) or { return false }
	c.add_typed_receiver_method_name(method_name, mut calls)
	return true
}

fn (c &CallCollector) receiver_type_name(base_id flat.NodeId, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string) string {
	base := c.a.node(base_id)
	base_type := c.node_type(base_id)
	type_name := resolve_type_name(base_type)
	if type_name.len > 0 {
		return type_name
	}
	if base.kind == .index {
		if elem_type := c.top_level_index_elem_type_name(base_id, cur_module, imports,
			local_values, local_types)
		{
			return elem_type
		}
	}
	if base.kind == .ident && base.value.len > 0 {
		if local_type := local_types[base.value] {
			return local_type
		}
	}
	if base.kind == .ident && base.value.len > 0 {
		if base.value in local_values {
			return ''
		}
		return c.value_type_name(base.value, cur_module, imports)
	}
	if base.kind == .selector {
		name := c.qualified_expr_name(base_id)
		if name.len > 0 {
			return c.value_type_name(name, cur_module, imports)
		}
	}
	return ''
}

fn (c &CallCollector) typed_receiver_method_name(type_name string, method string, cur_module string) ?string {
	clean_type_name := markused_clean_receiver_type_name(type_name)
	if clean_type_name.len == 0 || method.len == 0 {
		return none
	}
	mut candidates := []string{cap: 4}
	if clean_type_name.starts_with('map[') {
		candidates << markused_map_receiver_method_candidates(clean_type_name, method, cur_module)
	} else if clean_type_name.starts_with('[]') {
		candidates << markused_array_receiver_method_candidates(clean_type_name, method, cur_module)
	} else if clean_type_name.contains('.') {
		candidates << '${clean_type_name}.${method}'
		unqualified_type := markused_unqualified_receiver_type_name(clean_type_name)
		if unqualified_type != clean_type_name {
			candidates << '${unqualified_type}.${method}'
		}
	} else {
		if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
			candidates << '${cur_module}.${clean_type_name}.${method}'
		}
		candidates << '${clean_type_name}.${method}'
	}
	for candidate in candidates {
		if c.is_known_fn_name(candidate) {
			return candidate
		}
		lowered := markused_c_name(candidate)
		if lowered != candidate && c.is_known_fn_name(lowered) {
			return lowered
		}
	}
	return none
}

fn markused_clean_receiver_type_name(type_name string) string {
	mut clean := type_name.trim_space()
	for {
		if clean.starts_with('&') {
			clean = clean[1..].trim_space()
			continue
		}
		if clean.starts_with('mut ') {
			clean = clean[4..].trim_space()
			continue
		}
		if clean.starts_with('shared ') {
			clean = clean[7..].trim_space()
			continue
		}
		break
	}
	return clean
}

fn markused_can_prefix_collection_receiver(cur_module string) bool {
	return cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin'
}

fn markused_push_receiver_candidate(mut candidates []string, candidate string) {
	if candidate.len > 0 && candidate !in candidates {
		candidates << candidate
	}
}

fn markused_array_receiver_method_candidates(receiver_type string, method string, cur_module string) []string {
	mut clean_type := receiver_type.trim_space()
	for {
		if clean_type.starts_with('&') {
			clean_type = clean_type[1..].trim_space()
			continue
		}
		break
	}
	mut candidates := []string{}
	markused_push_receiver_candidate(mut candidates, '${clean_type}.${method}')
	if !clean_type.starts_with('[]') || clean_type.len <= 2 {
		return candidates
	}
	elem_type := clean_type[2..]
	short_elem := if elem_type.contains('.') { elem_type.all_after_last('.') } else { elem_type }
	markused_push_receiver_candidate(mut candidates, '[]${short_elem}.${method}')
	if elem_type.contains('.') {
		markused_push_receiver_candidate(mut candidates,
			'${elem_type.all_before_last('.')}.[]${short_elem}.${method}')
	} else if markused_can_prefix_collection_receiver(cur_module) {
		markused_push_receiver_candidate(mut candidates, '${cur_module}.[]${short_elem}.${method}')
	}
	return candidates
}

fn markused_map_receiver_method_candidates(receiver_type string, method string, cur_module string) []string {
	clean_type := markused_clean_map_type(receiver_type)
	key_type := markused_map_key_type(clean_type)
	value_type := markused_map_value_type(clean_type)
	mut candidates := []string{}
	candidates << '${clean_type}.${method}'
	candidates << 'map.${method}'
	if key_type.len == 0 || value_type.len == 0 {
		markused_push_receiver_candidate(mut candidates, '${clean_type}.${method}')
		return candidates
	}
	key_types := markused_receiver_type_text_variants(key_type)
	value_types := markused_receiver_type_text_variants(value_type)
	mut map_types := []string{}
	for key in key_types {
		for value in value_types {
			markused_push_receiver_candidate(mut map_types, 'map[${key}]${value}')
		}
	}
	for map_type in map_types {
		markused_push_receiver_candidate(mut candidates, '${map_type}.${method}')
	}
	mut module_names := []string{}
	if markused_can_prefix_collection_receiver(cur_module) {
		markused_push_receiver_candidate(mut module_names, cur_module)
	}
	for mod_name in markused_receiver_type_text_module_names(key_type) {
		markused_push_receiver_candidate(mut module_names, mod_name)
	}
	for mod_name in markused_receiver_type_text_module_names(value_type) {
		markused_push_receiver_candidate(mut module_names, mod_name)
	}
	for mod_name in module_names {
		for map_type in map_types {
			markused_push_receiver_candidate(mut candidates, '${mod_name}.${map_type}.${method}')
		}
	}
	return candidates
}

fn markused_receiver_type_text_variants(type_text string) []string {
	clean := type_text.trim_space()
	mut names := []string{}
	markused_push_receiver_candidate(mut names, clean)
	markused_push_receiver_candidate(mut names, markused_receiver_type_text_short_spelling(clean))
	if markused_type_text_is_fixed_array(clean) {
		source := markused_receiver_type_text_source_fixed_spelling(clean)
		markused_push_receiver_candidate(mut names, source)
		markused_push_receiver_candidate(mut names,
			markused_receiver_type_text_short_spelling(source))
	}
	return names
}

fn markused_receiver_type_text_source_fixed_spelling(type_text string) string {
	clean := type_text.trim_space()
	if clean.len == 0 || clean.starts_with('[') || !markused_type_text_is_fixed_array(clean) {
		return clean
	}
	elem, dims := markused_postfix_fixed_array_parts(clean)
	if elem.len == 0 || dims.len == 0 {
		return clean
	}
	mut source := elem
	for i := dims.len; i > 0; i-- {
		source = '[${dims[i - 1]}]${source}'
	}
	return source
}

fn markused_postfix_fixed_array_parts(type_text string) (string, []string) {
	clean := type_text.trim_space()
	mut end := clean.len
	mut dims := []string{}
	for end > 0 && clean[end - 1] == `]` {
		start := markused_trailing_matching_bracket_start(clean, end)
		if start < 0 {
			break
		}
		dims << clean[start + 1..end - 1].trim_space()
		end = start
	}
	return clean[..end], dims
}

fn markused_trailing_matching_bracket_start(s string, end int) int {
	mut depth := 0
	for i := end - 1; i >= 0; i-- {
		if s[i] == `]` {
			depth++
		} else if s[i] == `[` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return -1
}

fn markused_receiver_type_text_short_spelling(type_text string) string {
	clean := type_text.trim_space()
	if clean.starts_with('[]') {
		return '[]' + markused_receiver_type_text_short_spelling(clean[2..])
	}
	if clean.starts_with('[') {
		bracket_end := markused_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return clean[..bracket_end + 1] +
				markused_receiver_type_text_short_spelling(clean[bracket_end + 1..])
		}
	}
	if clean.starts_with('map[') {
		bracket_end := markused_matching_bracket(clean, 3)
		if bracket_end < clean.len {
			key := markused_receiver_type_text_short_spelling(clean[4..bracket_end])
			value := markused_receiver_type_text_short_spelling(clean[bracket_end + 1..])
			return 'map[${key}]${value}'
		}
	}
	if clean.contains('.') {
		return clean.all_after_last('.')
	}
	return clean
}

fn markused_receiver_type_text_module_names(type_text string) []string {
	clean := type_text.trim_space()
	mut names := []string{}
	if clean.starts_with('[]') {
		return markused_receiver_type_text_module_names(clean[2..])
	}
	if clean.starts_with('[') {
		bracket_end := markused_matching_bracket(clean, 0)
		if bracket_end < clean.len {
			return markused_receiver_type_text_module_names(clean[bracket_end + 1..])
		}
	}
	if clean.starts_with('map[') {
		key_type := markused_map_key_type(clean)
		value_type := markused_map_value_type(clean)
		for name in markused_receiver_type_text_module_names(key_type) {
			markused_push_receiver_candidate(mut names, name)
		}
		for name in markused_receiver_type_text_module_names(value_type) {
			markused_push_receiver_candidate(mut names, name)
		}
		return names
	}
	if markused_type_text_is_fixed_array(clean) {
		return markused_receiver_type_text_module_names(markused_fixed_array_elem_type(clean))
	}
	if clean.contains('.') {
		markused_push_receiver_candidate(mut names, clean.all_before_last('.'))
	}
	return names
}

fn markused_clean_map_type(receiver_type string) string {
	mut clean := receiver_type.trim_space()
	for {
		if clean.starts_with('shared ') {
			clean = clean[7..].trim_space()
			continue
		}
		if clean.starts_with('&') {
			clean = clean[1..].trim_space()
			continue
		}
		break
	}
	return clean
}

fn markused_type_text_is_fixed_array(type_text string) bool {
	clean := type_text.trim_space()
	if clean.len == 0 || clean.starts_with('[]') || clean.starts_with('map[') {
		return false
	}
	if clean.starts_with('[') {
		bracket_end := markused_matching_bracket(clean, 0)
		return bracket_end < clean.len
	}
	if !clean.contains('[') || !clean.ends_with(']') {
		return false
	}
	len_text := markused_fixed_array_len_text(clean)
	return len_text.len > 0 && !len_text.contains(',')
}

fn markused_fixed_array_len_text(type_text string) string {
	return type_text.all_after('[').all_before(']').trim_space()
}

fn markused_fixed_array_elem_type(type_text string) string {
	if type_text.starts_with('[') {
		bracket_end := markused_matching_bracket(type_text, 0)
		if bracket_end < type_text.len {
			return type_text[bracket_end + 1..]
		}
		return ''
	}
	return type_text.all_before('[')
}

fn markused_map_key_type(type_str string) string {
	if !type_str.starts_with('map[') {
		return ''
	}
	bracket_end := markused_matching_bracket(type_str, 3)
	if bracket_end > 4 {
		return type_str[4..bracket_end]
	}
	return ''
}

fn markused_map_value_type(type_str string) string {
	if !type_str.starts_with('map[') {
		return ''
	}
	bracket_end := markused_matching_bracket(type_str, 3)
	if bracket_end + 1 < type_str.len {
		return type_str[bracket_end + 1..]
	}
	return ''
}

fn markused_matching_bracket(s string, start int) int {
	mut depth := 0
	for i in start .. s.len {
		if s[i] == `[` {
			depth++
		} else if s[i] == `]` {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return s.len
}

fn markused_unqualified_receiver_type_name(type_name string) string {
	if !type_name.starts_with('map[') {
		return type_name.all_after_last('.')
	}
	key_start := 'map['.len
	key_end := type_name.index_u8(`]`)
	if key_end < key_start {
		return type_name
	}
	key := type_name[key_start..key_end].all_after_last('.')
	value := type_name[key_end + 1..].all_after_last('.')
	return 'map[${key}]${value}'
}

fn (c &CallCollector) add_typed_receiver_method_name(method_name string, mut calls []string) {
	calls << method_name
	lowered := markused_c_name(method_name)
	if lowered != method_name {
		calls << lowered
	}
}

fn (c &CallCollector) value_type_name(name string, cur_module string, imports map[string]string) string {
	for candidate in c.value_name_candidates(name, cur_module, imports) {
		if typ := c.tc.file_scope.lookup(candidate) {
			type_name := resolve_type_name(typ)
			if type_name.len > 0 {
				return type_name
			}
		}
		if typ := c.tc.const_types[candidate] {
			type_name := resolve_type_name(typ)
			if type_name.len > 0 {
				return type_name
			}
		}
		if info := c.const_decls[candidate] {
			type_name := c.const_initializer_type_name(info)
			if type_name.len > 0 {
				return type_name
			}
		}
	}
	return ''
}

fn (c &CallCollector) const_initializer_type_name(info ConstDeclInfo) string {
	expr := c.a.node(info.expr_id)
	if expr.kind == .struct_init && expr.value.len > 0 {
		return c.struct_lookup_name(expr.value, info.module)
	}
	type_name := resolve_type_name(c.node_type(info.expr_id))
	if type_name.len > 0 {
		return type_name
	}
	return ''
}

fn (c &CallCollector) value_name_candidates(name string, cur_module string, imports map[string]string) []string {
	mut candidates := []string{cap: 3}
	qname := qualify_fn(cur_module, name)
	candidates << qname
	if qname != name {
		candidates << name
	}
	if name.contains('.') {
		base := name.all_before_last('.')
		member := name.all_after_last('.')
		if base in imports {
			candidates << '${imports[base]}.${member}'
		}
	}
	return candidates
}

fn markused_resolve_imported_type_name(name string, imports map[string]string) string {
	if !name.contains('.') {
		return name
	}
	base := name.all_before_last('.')
	member := name.all_after_last('.')
	if base in imports {
		return '${imports[base]}.${member}'
	}
	return name
}

// collect_zero_struct_default_calls updates collect zero struct default calls state for markused.
fn (c &CallCollector) collect_zero_struct_default_calls(typ types.Type, cur_module string, imports map[string]string, mut calls []string) {
	type_name := zero_value_struct_type_name(typ)
	if type_name.len == 0 {
		return
	}
	c.collect_struct_default_calls_for_type(type_name, cur_module, imports, mut calls)
}

// zero_value_struct_type_name supports zero value struct type name handling for markused.
fn zero_value_struct_type_name(typ types.Type) string {
	mut clean := typ
	for _ in 0 .. 8 {
		if clean is types.Alias {
			clean = clean.base_type
			continue
		}
		if clean is types.OptionType {
			clean = clean.base_type
			continue
		}
		if clean is types.ResultType {
			clean = clean.base_type
			continue
		}
		break
	}
	if clean is types.Struct {
		return clean.name
	}
	return ''
}

// collect_struct_default_calls updates collect struct default calls state for markused.
fn (c &CallCollector) collect_struct_default_calls(init &flat.Node, cur_module string, imports map[string]string, mut calls []string) {
	info := c.struct_decl_info(init.value, cur_module) or { return }
	mut set_fields := map[string]bool{}
	for i in 0 .. init.children_count {
		field := c.a.child_node(init, i)
		if field.kind == .field_init {
			set_fields[field.value] = true
		}
	}
	c.collect_struct_default_calls_from_info(info, set_fields, imports, mut calls)
}

// collect_struct_default_calls_for_type supports collect_struct_default_calls_for_type handling.
fn (c &CallCollector) collect_struct_default_calls_for_type(type_name string, cur_module string, imports map[string]string, mut calls []string) {
	info := c.struct_decl_info(type_name, cur_module) or { return }
	c.collect_struct_default_calls_from_info(info, map[string]bool{}, imports, mut calls)
}

// struct_decl_info supports struct decl info handling for CallCollector.
fn (c &CallCollector) struct_decl_info(type_name string, cur_module string) ?StructDeclInfo {
	struct_name := c.struct_lookup_name(type_name, cur_module)
	if struct_name.len == 0 {
		return none
	}
	return c.struct_decls[struct_name] or { none }
}

// collect_struct_default_calls_from_info supports collect_struct_default_calls_from_info handling.
fn (c &CallCollector) collect_struct_default_calls_from_info(info StructDeclInfo, provided map[string]bool, imports map[string]string, mut calls []string) {
	node := c.a.node(info.node_id)
	for i in 0 .. node.children_count {
		field := c.a.child_node(node, i)
		if field.kind != .field_decl || field.children_count == 0 || field.value in provided {
			continue
		}
		c.collect_calls(field, info.module, imports, '', '', mut calls)
	}
}

// resolve_type_name resolves resolve type name information for markused.
fn resolve_type_name(t types.Type) string {
	if t is types.Alias {
		return t.name
	} else if t is types.Struct {
		return t.name
	} else if t is types.Interface {
		return t.name
	} else if t is types.SumType {
		return t.name
	} else if t is types.Enum {
		return t.name
	} else if t is types.String {
		return 'string'
	} else if t is types.Array {
		return '[]${markused_nested_type_name(t.elem_type)}'
	} else if t is types.ArrayFixed {
		mut len_text := t.len.str()
		if t.len_expr.len > 0 {
			len_text = t.len_expr
		}
		return '${markused_nested_type_name(t.elem_type)}[${len_text}]'
	} else if t is types.Map {
		return 'map[${markused_nested_type_name(t.key_type)}]${markused_nested_type_name(t.value_type)}'
	} else if t is types.Pointer {
		return resolve_type_name(t.base_type)
	} else if t is types.Primitive {
		props := int(t.props)
		sz := int(t.size)
		if props & 1 > 0 {
			return 'bool'
		}
		if props & 4 > 0 {
			if props & 8 > 0 {
				return match sz {
					8 { 'u8' }
					16 { 'u16' }
					32 { 'u32' }
					64 { 'u64' }
					else { 'int' }
				}
			}
			return match sz {
				0 { 'int' }
				8 { 'i8' }
				16 { 'i16' }
				32 { 'i32' }
				64 { 'i64' }
				else { 'int' }
			}
		}
		if props & 2 > 0 {
			return match sz {
				32 { 'f32' }
				64 { 'f64' }
				else { 'f64' }
			}
		}
		return 'int'
	} else if t is types.ISize {
		return 'isize'
	} else if t is types.USize {
		return 'usize'
	} else if t is types.Rune {
		return 'rune'
	}
	return ''
}

fn markused_nested_type_name(t types.Type) string {
	return t.name()
}

fn markused_type_name(t types.Type, unwrap_optional_result bool) ?string {
	if unwrap_optional_result {
		if t is types.OptionType {
			name := resolve_type_name(t.base_type)
			if name.len > 0 {
				return name
			}
		}
		if t is types.ResultType {
			name := resolve_type_name(t.base_type)
			if name.len > 0 {
				return name
			}
		}
	}
	name := resolve_type_name(t)
	if name.len == 0 {
		return none
	}
	return name
}

fn markused_type_name_or_empty(t types.Type, unwrap_optional_result bool) string {
	return markused_type_name(t, unwrap_optional_result) or { '' }
}

// markused_c_name returns the C identifier used for a V symbol or type name.
fn markused_c_name(name string) string {
	if name.starts_with('C.') {
		return name[2..]
	}
	if name == 'malloc' {
		return 'v_malloc'
	}
	if name == 'exit' {
		return 'v_exit'
	}
	if naming.is_plain_identifier(name) {
		return name
	}
	return naming.sanitize(name)
}
