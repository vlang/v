module markused

import v3.flat
import v3.types

const trace_markused = false

// mark_used updates mark used state for markused.
pub fn mark_used(a &flat.FlatAst, tc &types.TypeChecker) map[string]bool {
	return mark_used_with_test_files(a, tc, map[string]bool{})
}

pub fn mark_used_for_tests(a &flat.FlatAst, tc &types.TypeChecker, test_files []string) map[string]bool {
	mut file_map := map[string]bool{}
	for file in test_files {
		file_map[file] = true
	}
	return mark_used_with_test_files(a, tc, file_map)
}

fn mark_used_with_test_files(a &flat.FlatAst, tc &types.TypeChecker, test_files map[string]bool) map[string]bool {
	mut cur_module := ''
	mut imports := map[string]string{}
	mut fn_decls := map[string]FnDeclInfo{}
	mut fn_decl_lists := map[string][]FnDeclInfo{}
	mut struct_decls := map[string]StructDeclInfo{}
	mut const_decls := map[string]ConstDeclInfo{}

	// Reverse index: short name (after last '.') -> list of full qualified names
	mut suffix_map := map[string][]string{}

	mut fn_count := 0
	mut fn_with_dot := 0
	mut contains2_total := 0
	for node_idx, node in a.nodes {
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
		if node.kind == .const_decl {
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
				full_name := qualify_fn(cur_module, field.value)
				if full_name != field.value {
					const_decls[full_name] = info
				}
			}
			continue
		}
		if node.kind == .fn_decl {
			has_dot := node.value.contains('.')
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
			lowered_name := markused_c_name(node.value)
			if lowered_name != node.value {
				add_fn_decl_info(mut fn_decls, mut fn_decl_lists, lowered_name, info)
			}
			qname := qualify_fn(cur_module, node.value)
			if qname != node.value {
				add_fn_decl_info(mut fn_decls, mut fn_decl_lists, qname, info)
				lowered_qname := markused_c_name(qname)
				if lowered_qname != qname {
					add_fn_decl_info(mut fn_decls, mut fn_decl_lists, lowered_qname, info)
				}
			}
			// Build suffix_map entries
			if has_dot {
				if trace_markused {
					contains2_total++
				}
				short := node.value.all_after_last('.')
				add_suffix_candidate(mut suffix_map, short, node.value)
				if qname != node.value {
					add_suffix_candidate(mut suffix_map, short, qname)
				}
			}
			if qname != node.value && qname.contains('.') {
				short := qname.all_after_last('.')
				add_suffix_candidate(mut suffix_map, short, qname)
			}
		}
	}

	// BFS from main
	mut used := map[string]bool{}
	mut queue := []string{}
	queue << 'main'
	used['main'] = true
	enqueue_main_module_roots(fn_decls, mut used, mut queue)
	enqueue_auto_roots(fn_decls, mut used, mut queue)
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
		'array.slice', 'array.pop_left', 'array.clone', 'array.delete', 'array.ensure_cap',
		'string.==', 'string.<', 'string.free', 'string.all_before', 'string.all_before_last',
		'string.all_after', 'string.all_after_last', 'string.substr', 'string__substr', 'u8.vstring',
		'u8.vstring_with_len', 'charptr.vstring', 'charptr.vstring_with_len', 'byteptr.vstring',
		'byteptr.vstring_with_len', 'byteptr.vbytes', 'voidptr.vbytes', '[]rune.string', 'map.set',
		'map.exists', 'map.get', 'map.get_check', 'map.get_and_set', 'map.delete', 'map.clone',
		'map.clear', 'map.keys', 'map.values', 'map.reserve', 'map_map_eq', 'memdup',
		'strings.Builder.write_ptr', 'strings.Builder.write_runes', 'strings.Builder.free',
		'strconv.format_int', 'strconv.format_uint', 'bool.str', 'int.str', 'u64.str', 'f64.str',
		'rune.str', 'string.+', 'ptr_str', 'strconv__f32_to_str_l', 'strconv__f64_to_str_l',
		'sync.new_channel_st', 'sync.Channel.push', 'sync.Channel.pop', 'sync.Channel.close',
		'sync.Channel.len', 'sync.Channel.closed', 'new_channel_st', 'Channel.push', 'Channel.pop',
		'Channel.close', 'Channel.len', 'Channel.closed', 'os.join_path_single', 'panic',
		'u8.is_letter', 'u8.is_capital', 'string.is_capital', 'string.to_lower_ascii',
		'rune.to_lower', 'Array_u8__bytestr', 'Array_u8__hex', 'data_to_hex_string',
		'map_hash_string', 'map_hash_int_1', 'map_hash_int_2', 'map_hash_int_4', 'map_hash_int_8',
		'map_eq_string', 'map_eq_int_1', 'map_eq_int_2', 'map_eq_int_4', 'map_eq_int_8',
		'map_clone_string', 'map_clone_int_1', 'map_clone_int_2', 'map_clone_int_4',
		'map_clone_int_8', 'map_free_string', 'map_free_nop', '[]string.join', 'Array_string__join',
		'exit', 'v_exit'] {
		queue << seed
		used[seed] = true
	}
	queue << 'array.delete_last'
	used['array.delete_last'] = true
	for seed in ['i8.str', 'i16.str', 'i32.str', 'i64.str'] {
		queue << seed
		used[seed] = true
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
		a:            a
		tc:           tc
		fn_decls:     fn_decls
		struct_decls: struct_decls
		const_decls:  const_decls
	}
	enqueue_detected_runtime_helpers(a, tc, mut used, mut queue)
	enqueue_function_value_selectors(a, collector, fn_decls, mut used, mut queue)
	// Methods used as values (`recv.method` passed as a callback) are reachable only
	// through a wrapper cgen generates later. The checker records them per enclosing
	// function in `method_values_by_fn`; they are seeded inside the BFS below (only when
	// that function is reached), so an unreachable function's method value never forces an
	// otherwise-unused specialization to be transformed/emitted.
	enqueue_initializer_calls(a, collector, imports, fn_decls, mut used, mut queue)
	enqueue_top_level_calls(a, collector, fn_decls, mut used, mut queue)
	// Interface dispatch reachability: calling an interface method `Foo.m` may
	// dispatch to any concrete `T.m` for a type `T` that implements `Foo`. Those
	// concrete methods are only referenced from the generated dispatch switch, so
	// without this they would be pruned and produce undefined-symbol errors.
	mut iface_impls := map[string][]string{}
	mut checked_iface_impls := map[string]bool{}
	mut processed_nodes := []bool{len: a.nodes.len}
	mut calls := []string{cap: 128}
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
			node := a.node(fn_info.node_id)
			receiver_name, receiver_struct := receiver_info(a, node)
			collector.collect_calls(node, fn_info.module, imports, receiver_name, receiver_struct, mut
				calls)
			total_callees += calls.len
			for callee in calls {
				if !valid_symbol_name(callee) {
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
					add_safe_decl_alias(callee, callee_info, a, mut used)
				} else if callee in tc.fn_ret_types {
					found_direct = true
					if enqueue(callee, mut used, mut queue) {
						if trace_markused && qi == 1 {
							eprintln('main: resolved hit: "${callee}"')
						}
					}
				}
				if !found_direct || !callee.contains('.') {
					short := callee.all_after_last('.')
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
				if callee.contains('.') {
					recv := callee.all_before_last('.')
					method := callee.all_after_last('.')
					ensure_iface_impls(recv, fn_info.module, tc, mut iface_impls, mut
						checked_iface_impls)
					if impls := iface_impls[recv] {
						for impl in impls {
							impl_method := '${impl}.${method}'
							enqueue(impl_method, mut used, mut queue)
							short_impl := '${impl.all_after_last('.')}.${method}'
							if short_impl != impl_method {
								enqueue(short_impl, mut used, mut queue)
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
	return used
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
	for struct_name, _ in tc.structs {
		if tc.named_type_implements_interface(struct_name, iface_name) {
			impls << struct_name
		}
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
	if recv.contains('.') {
		return none
	}
	if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
		qname := '${cur_module}.${recv}'
		if qname in tc.interface_names {
			return qname
		}
	}
	return none
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

fn add_fn_decl_info(mut fn_decls map[string]FnDeclInfo, mut fn_decl_lists map[string][]FnDeclInfo, name string, info FnDeclInfo) {
	fn_decls[name] = info
	mut infos := fn_decl_lists[name] or { []FnDeclInfo{} }
	infos << info
	fn_decl_lists[name] = infos
}

fn fn_decl_infos_for_queue_name(name string, fn_decl_lists map[string][]FnDeclInfo, a &flat.FlatAst) []FnDeclInfo {
	infos := fn_decl_lists[name] or { return []FnDeclInfo{} }
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

fn fn_decl_key_is_exact_for_info(name string, decl_name string, module_name string) bool {
	qname := qualify_fn(module_name, decl_name)
	if name == qname {
		return true
	}
	lowered := markused_c_name(qname)
	return lowered != qname && name == lowered
}

fn add_safe_decl_alias(callee string, callee_info FnDeclInfo, a &flat.FlatAst, mut used map[string]bool) {
	alias := a.node(callee_info.node_id).value
	if alias == callee || alias in used {
		return
	}
	if fn_decl_key_is_exact_for_info(callee, alias, callee_info.module) {
		return
	}
	used[alias] = true
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
fn enqueue_initializer_calls(a &flat.FlatAst, collector CallCollector, imports map[string]string, fn_decls map[string]FnDeclInfo, mut used map[string]bool, mut queue []string) {
	mut cur_module := ''
	mut calls := []string{cap: 32}
	for node in a.nodes {
		match node.kind {
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
					collector.collect_calls(field, cur_module, imports, '', '', mut calls)
					for callee in calls {
						if callee_info := fn_decls[callee] {
							enqueue(callee, mut used, mut queue)
							add_safe_decl_alias(callee, callee_info, a, mut used)
						} else {
							enqueue(callee, mut used, mut queue)
						}
					}
				}
			}
			else {}
		}
	}
}

fn enqueue_top_level_calls(a &flat.FlatAst, collector CallCollector, fn_decls map[string]FnDeclInfo, mut used map[string]bool, mut queue []string) {
	if markused_has_entry_main(a) {
		return
	}
	mut calls := []string{cap: 32}
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
			for callee in calls {
				if callee_info := fn_decls[callee] {
					enqueue(callee, mut used, mut queue)
					add_safe_decl_alias(callee, callee_info, a, mut used)
				} else {
					enqueue(callee, mut used, mut queue)
				}
			}
		}
	}
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
	a            &flat.FlatAst      = unsafe { nil }
	tc           &types.TypeChecker = unsafe { nil }
	fn_decls     map[string]FnDeclInfo
	struct_decls map[string]StructDeclInfo
	const_decls  map[string]ConstDeclInfo
}

// enqueue_auto_roots supports enqueue auto roots handling for markused.
fn enqueue_auto_roots(fn_decls map[string]FnDeclInfo, mut used map[string]bool, mut queue []string) {
	for name, info in fn_decls {
		if !is_auto_root_fn(name) {
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
	mut cur_module := ''
	mut imports := map[string]string{}
	for node in a.nodes {
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
				ret_type := tc.parse_type(node.typ)
				if ret_type is types.OptionType || ret_type is types.ResultType {
					needs_optional_helpers = true
				}
			}
			.param, .field_decl, .field_init, .const_field {
				if type_string_needs_optional_helpers(node.typ) {
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
						enqueue_stringified_custom_str_method(a.child(&node, 1), cur_module, tc, mut
							used, mut queue)
					}
				}
			}
			.string_interp {
				needs_string_interp_helpers = true
				needs_string_plus_helper = true
				for i in 0 .. node.children_count {
					enqueue_stringified_custom_str_method(a.child(&node, i), cur_module, tc, mut
						used, mut queue)
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
					rhs_id := a.child(&node, 1)
					rhs_type := markused_membership_container_type(tc, tc.resolve_type(rhs_id))
					if rhs_type == 'string' {
						needs_string_membership_helpers = true
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
		else {}
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
		lowered := '${markused_c_name(candidate)}__str'
		if lowered in tc.fn_ret_types {
			enqueue(lowered, mut used, mut queue)
		}
		method := '${candidate}.str'
		if method in tc.fn_ret_types {
			enqueue(method, mut used, mut queue)
		}
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

// enqueue_function_value_selectors supports enqueue function value selectors handling for markused.
fn enqueue_function_value_selectors(a &flat.FlatAst, collector CallCollector, fn_decls map[string]FnDeclInfo, mut used map[string]bool, mut queue []string) {
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

// collect_calls updates collect calls state for markused.
fn (c &CallCollector) collect_calls(node &flat.Node, cur_module string, imports map[string]string, receiver_name string, receiver_struct string, mut calls []string) {
	local_values := c.local_value_names(node)
	local_types := c.local_value_type_names(node, cur_module, imports)
	visible_local_idents := markused_visible_local_idents(c.a, node, local_values)
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
				if child.children_count > 0 {
					callee_id := c.a.child(child, 0)
					if int(callee_id) >= 0 {
						callee := c.a.nodes[int(callee_id)]
						if callee.kind == .ident && callee.value.len > 0 {
							if resolved_call.len > 0 {
								calls << resolved_call
							}
							if callee.value !in local_values {
								calls << callee.value
								qcallee := qualify_fn(cur_module, callee.value)
								if qcallee != callee.value {
									calls << qcallee
								}
							}
						} else if callee.kind == .selector && callee.value.len > 0 {
							mut has_exact_selector_call := false
							if callee.children_count > 0 {
								base_id := c.a.child(&callee, 0)
								if int(base_id) >= 0 {
									base := c.a.nodes[int(base_id)]
									has_exact_selector_call = c.collect_checker_selected_call(resolved_call, mut
										calls)
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
						c.collect_struct_operator_call(lhs_id, child.op, cur_module, mut calls)
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

fn (c &CallCollector) local_value_names(node &flat.Node) map[string]bool {
	return markused_local_value_names(c.a, node)
}

fn (c &CallCollector) local_value_type_names(node &flat.Node, cur_module string, imports map[string]string) map[string]string {
	mut result := map[string]string{}
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
		if child.kind == .param && child.value.len > 0 && child.typ.len > 0 {
			result[child.value] = markused_resolve_imported_type_name(child.typ, imports)
		} else if child.kind == .decl_assign {
			mut i := 0
			for i + 1 < child.children_count {
				lhs_id := c.a.child(child, i)
				rhs_id := c.a.child(child, i + 1)
				if int(lhs_id) >= 0 && int(rhs_id) >= 0 {
					lhs := c.a.node(lhs_id)
					if lhs.kind == .ident && lhs.value.len > 0 {
						type_name := if child.children_count == 2 && child.typ.len > 0 {
							child.typ
						} else {
							c.top_level_decl_rhs_type_name(rhs_id, cur_module, imports)
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

fn markused_local_value_names(a &flat.FlatAst, node &flat.Node) map[string]bool {
	mut names := map[string]bool{}
	mut stack := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := a.child(node, i)
		if int(child_id) >= 0 {
			stack << child_id
		}
	}
	for stack.len > 0 {
		id := stack.pop()
		child := a.node(id)
		if child.kind == .param && child.value.len > 0 {
			names[child.value] = true
		} else if child.kind == .decl_assign {
			mut i := 0
			for i < child.children_count {
				lhs_id := a.child(child, i)
				if int(lhs_id) >= 0 {
					lhs := a.node(lhs_id)
					if lhs.kind == .ident && lhs.value.len > 0 {
						names[lhs.value] = true
					}
				}
				i += 2
			}
		}
		for i in 0 .. child.children_count {
			next_id := a.child(child, i)
			if int(next_id) >= 0 {
				stack << next_id
			}
		}
	}
	return names
}

fn markused_visible_local_idents(a &flat.FlatAst, root &flat.Node, local_values map[string]bool) map[int]bool {
	mut visible_ids := map[int]bool{}
	if local_values.len == 0 {
		return visible_ids
	}
	mut locals := map[string]bool{}
	if root.kind == .fn_decl {
		for i in 0 .. root.children_count {
			child := a.child_node(root, i)
			if child.kind == .param && child.value.len > 0 {
				locals[child.value] = true
			}
		}
	}
	markused_collect_visible_local_idents(a, root, local_values, mut locals, mut visible_ids)
	return visible_ids
}

fn markused_ident_is_visible_local(id flat.NodeId, name string, local_values map[string]bool, visible_local_idents map[int]bool) bool {
	return name.len > 0 && name in local_values && int(id) in visible_local_idents
}

fn markused_collect_visible_local_idents(a &flat.FlatAst, node &flat.Node, local_values map[string]bool, mut locals map[string]bool, mut visible_ids map[int]bool) {
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
				mut scoped := markused_clone_bool_map(locals)
				markused_collect_visible_local_idents(a, child, local_values, mut scoped, mut
					visible_ids)
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
							visible_ids)
					}
					i2 += 2
				}
				i2 = 0
				for i2 + 1 < child.children_count {
					lhs_id := a.child(child, i2)
					lhs := a.node(lhs_id)
					if lhs.kind == .ident && lhs.value in local_values && lhs.value in locals {
						visible_ids[int(lhs_id)] = true
					}
					if lhs.kind == .ident && lhs.value.len > 0 {
						locals[lhs.value] = true
					}
					i2 += 2
				}
			}
			else {
				if child.kind == .ident && child.value in local_values && child.value in locals {
					visible_ids[int(child_id)] = true
				}
				markused_collect_visible_local_idents(a, child, local_values, mut locals, mut
					visible_ids)
			}
		}
	}
}

fn markused_mark_visible_local_ident(a &flat.FlatAst, id flat.NodeId, local_values map[string]bool, locals map[string]bool, mut visible_ids map[int]bool) {
	node := a.node(id)
	if node.kind == .ident && node.value in local_values && node.value in locals {
		visible_ids[int(id)] = true
	}
}

fn (c &CallCollector) top_level_decl_rhs_type_name(rhs_id flat.NodeId, cur_module string, imports map[string]string) string {
	rhs := c.a.node(rhs_id)
	if rhs.kind == .struct_init && rhs.value.len > 0 {
		return c.struct_lookup_name(markused_resolve_imported_type_name(rhs.value, imports),
			cur_module)
	}
	type_name := resolve_type_name(c.node_type(rhs_id))
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
				type_name := c.top_level_decl_rhs_type_name(rhs_id, cur_module, imports)
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
			type_name := c.top_level_decl_rhs_type_name(expr_id, cur_module, imports)
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
						c.collect_struct_operator_call(lhs_id, child.op, cur_module, mut calls)
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

fn (c &CallCollector) collect_top_level_call(call_id flat.NodeId, call &flat.Node, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string, mut calls []string) {
	mut resolved_call := ''
	if resolved := c.tc.resolved_call_name(call_id) {
		resolved_call = resolved
	}
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

fn (c &CallCollector) collect_top_level_selector_call(callee &flat.Node, method string, resolved_call string, cur_module string, imports map[string]string, local_values map[string]bool, local_types map[string]string, mut calls []string) {
	mut has_exact_selector_call := false
	if callee.children_count > 0 {
		base_id := c.a.child(callee, 0)
		if int(base_id) >= 0 {
			base := c.a.node(base_id)
			has_exact_selector_call = c.collect_checker_selected_call(resolved_call, mut calls)
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

// collect_struct_operator_call updates collect struct operator call state for markused.
fn (c &CallCollector) collect_struct_operator_call(lhs_id flat.NodeId, op flat.Op, cur_module string, mut calls []string) {
	lhs_type := c.node_type(lhs_id)
	lhs_name := resolve_type_name(lhs_type)
	struct_type := c.struct_lookup_name(lhs_name, cur_module)
	if struct_type.len == 0 {
		return
	}
	method_name := c.struct_operator_call_name(struct_type, op) or { return }
	c.add_operator_call_name(method_name, mut calls)
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
	if type_name.len == 0 || method.len == 0 {
		return none
	}
	mut candidates := []string{cap: 4}
	if type_name.starts_with('map[') {
		candidates << markused_map_receiver_method_candidates(type_name, method, cur_module)
	} else if type_name.starts_with('[]') {
		candidates << markused_array_receiver_method_candidates(type_name, method, cur_module)
	} else if type_name.contains('.') {
		candidates << '${type_name}.${method}'
		unqualified_type := markused_unqualified_receiver_type_name(type_name)
		if unqualified_type != type_name {
			candidates << '${unqualified_type}.${method}'
		}
	} else {
		if cur_module.len > 0 && cur_module != 'main' && cur_module != 'builtin' {
			candidates << '${cur_module}.${type_name}.${method}'
		}
		candidates << '${type_name}.${method}'
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

// markused_c_name converts markused c name data for markused.
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
	if markused_c_name_is_plain(name) {
		return name
	}
	return name.replace('[]', 'Array_').replace('.-', '__minus').replace('.+', '__plus').replace('.==',
		'__eq').replace('.!=', '__ne').replace('.<=', '__le').replace('.>=', '__ge').replace('.<',
		'__lt').replace('.>', '__gt').replace('&', 'ptr').replace('[', '_').replace(']', '').replace(',',
		'_').replace(' ', '_').replace('.', '__')
}

// markused_c_name_is_plain converts markused c name is plain data for markused.
fn markused_c_name_is_plain(name string) bool {
	for i in 0 .. name.len {
		c := name[i]
		if (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || (c >= `0` && c <= `9`) || c == `_` {
			continue
		}
		return false
	}
	return true
}
