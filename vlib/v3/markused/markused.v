module markused

import v3.flat
import v3.types

const trace_markused = false

// mark_used updates mark used state for markused.
pub fn mark_used(a &flat.FlatAst, tc &types.TypeChecker) map[string]bool {
	mut cur_module := ''
	mut imports := map[string]string{}
	mut fn_decls := map[string]FnDeclInfo{}
	mut struct_decls := map[string]StructDeclInfo{}
	mut const_decls := map[string]ConstDeclInfo{}

	// Reverse index: short name (after last '.') -> list of full qualified names
	mut suffix_map := map[string][]string{}

	mut fn_count := 0
	mut fn_with_dot := 0
	mut contains2_total := 0
	for node_idx, node in a.nodes {
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
			fn_decls[node.value] = info
			lowered_name := markused_c_name(node.value)
			if lowered_name != node.value {
				fn_decls[lowered_name] = info
			}
			qname := qualify_fn(cur_module, node.value)
			if qname != node.value {
				fn_decls[qname] = info
				lowered_qname := markused_c_name(qname)
				if lowered_qname != qname {
					fn_decls[lowered_qname] = info
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
		'array.push_many', 'array.slice', 'array.clone', 'array.delete', 'array.ensure_cap',
		'string.==', 'string.<', 'string.free', 'string.all_before', 'string.all_before_last',
		'string.all_after', 'string.all_after_last', 'u8.vstring', '[]rune.string', 'map.set',
		'map.exists', 'map.get', 'map.get_check', 'map.get_and_set', 'map.delete', 'map.clone',
		'map.clear', 'memdup', 'strings.Builder.write_ptr', 'strings.Builder.write_runes',
		'strings.Builder.free', 'strconv.format_int', 'strconv.format_uint', 'bool.str', 'ptr_str',
		'strconv__f32_to_str_l', 'strconv__f64_to_str_l', 'sync.new_channel_st', 'sync.Channel.push',
		'sync.Channel.pop', 'sync.Channel.close', 'sync.Channel.len', 'sync.Channel.closed',
		'new_channel_st', 'Channel.push', 'Channel.pop', 'Channel.close', 'Channel.len',
		'Channel.closed', 'panic', 'u8.is_letter', 'u8.is_capital', 'string.is_capital',
		'string.to_lower_ascii', 'rune.to_lower', 'data_to_hex_string', 'map_hash_string',
		'map_hash_int_1', 'map_hash_int_2', 'map_hash_int_4', 'map_hash_int_8', 'map_eq_string',
		'map_eq_int_1', 'map_eq_int_2', 'map_eq_int_4', 'map_eq_int_8', 'map_clone_string',
		'map_clone_int_1', 'map_clone_int_2', 'map_clone_int_4', 'map_clone_int_8', 'map_free_string',
		'map_free_nop', '[]string.join', 'Array_string__join', 'exit', 'v_exit'] {
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
	enqueue_function_value_selectors(a, fn_decls, mut used, mut queue)
	// Methods used as values (`recv.method` passed as a callback) are reachable only
	// through a wrapper cgen generates later. The checker recorded them with full type
	// info; seed them (and their suffix-resolved forms) so they survive pruning and get
	// transformed/emitted like any other reachable function.
	for mkey, _ in tc.method_value_targets {
		enqueue(mkey, mut used, mut queue)
		lowered := markused_c_name(mkey)
		if lowered != mkey {
			enqueue(lowered, mut used, mut queue)
		}
		short := mkey.all_after_last('.')
		if candidates := suffix_map[short] {
			for cand in candidates {
				if cand == mkey || cand.ends_with('.${mkey}') {
					enqueue(cand, mut used, mut queue)
				}
			}
		}
	}
	enqueue_initializer_calls(a, collector, imports, fn_decls, mut used, mut queue)
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
		fn_info := fn_decls[name] or {
			not_in_cg++
			if trace_markused && qi <= 10 {
				eprintln('BFS qi=${qi.str()} name="${name}" in_cg=false')
			}
			continue
		}
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
		// the checker per enclosing function) are reachable too — mark them so they
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
				alias := a.node(callee_info.node_id).value
				if alias != callee && alias !in used {
					used[alias] = true
				}
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
				ensure_iface_impls(recv, tc, mut iface_impls, mut checked_iface_impls)
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
fn ensure_iface_impls(recv string, tc &types.TypeChecker, mut iface_impls map[string][]string, mut checked map[string]bool) {
	if recv.len == 0 || recv in checked {
		return
	}
	checked[recv] = true
	mut iface_name := ''
	if recv in tc.interface_names {
		iface_name = recv
	} else {
		for name, _ in tc.interface_names {
			if name.all_after_last('.') == recv {
				iface_name = name
				break
			}
		}
	}
	if iface_name.len == 0 {
		return
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

// add_suffix_candidate updates add suffix candidate state for markused.
fn add_suffix_candidate(mut suffix_map map[string][]string, short string, name string) {
	if !valid_symbol_name(short) || !valid_symbol_name(name) {
		return
	}
	mut candidates := suffix_map[short] or { []string{} }
	candidates << name
	suffix_map[short] = candidates
}

// valid_symbol_name supports valid symbol name handling for markused.
fn valid_symbol_name(name string) bool {
	return name.len > 0 && name.len < 512
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
							alias := a.node(callee_info.node_id).value
							if alias != callee && alias !in used {
								used[alias] = true
							}
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
	return short_name == 'init' || short_name == 'testsuite_begin' || short_name == 'testsuite_end'
		|| short_name.starts_with('test_')
}

// enqueue_detected_runtime_helpers supports enqueue detected runtime helpers handling for markused.
fn enqueue_detected_runtime_helpers(a &flat.FlatAst, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	mut needs_optional_helpers := false
	mut needs_string_interp_helpers := false
	mut needs_string_plus_helper := false
	mut needs_string_membership_helpers := false
	mut needs_new_map := false
	mut cur_module := ''
	for node in a.nodes {
		match node.kind {
			.module_decl {
				cur_module = node.value
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
	match typ {
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
fn enqueue_function_value_selectors(a &flat.FlatAst, fn_decls map[string]FnDeclInfo, mut used map[string]bool, mut queue []string) {
	for node in a.nodes {
		if node.kind == .selector && node.children_count > 0 && node.value.len > 0 {
			base := a.child_node(&node, 0)
			if base.kind == .ident && base.value.len > 0 {
				name := '${base.value}.${node.value}'
				if name in fn_decls {
					enqueue(name, mut used, mut queue)
				}
			}
		} else if node.kind == .call && node.children_count > 0 {
			callee := a.child_node(&node, 0)
			if callee.kind == .ident && callee.value in fn_decls {
				enqueue(callee.value, mut used, mut queue)
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
				c.collect_fn_value_ident(child_id, child.value, cur_module, imports, mut calls)
			}
			.selector {
				c.collect_fn_value_selector(child_id, child, cur_module, imports, mut calls)
			}
			.call {
				if resolved := c.tc.resolved_call_name(child_id) {
					calls << resolved
				}
				if child.children_count > 0 {
					callee_id := c.a.child(child, 0)
					if int(callee_id) >= 0 {
						callee := c.a.nodes[int(callee_id)]
						if callee.kind == .ident && callee.value.len > 0 {
							calls << callee.value
							qcallee := qualify_fn(cur_module, callee.value)
							if qcallee != callee.value {
								calls << qcallee
							}
						} else if callee.kind == .selector && callee.value.len > 0 {
							if callee.children_count > 0 {
								base_id := c.a.child(&callee, 0)
								if int(base_id) >= 0 {
									base := c.a.nodes[int(base_id)]
									if base.kind == .ident && base.value.len > 0 {
										if receiver_name.len > 0 && base.value == receiver_name {
											calls << receiver_struct + '.' + callee.value
											qrecv := qualify_fn(cur_module, receiver_struct + '.' +
												callee.value)
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
									base_type := c.node_type(base_id)
									mut type_name := resolve_type_name(base_type)
									if type_name.len == 0 && base.kind == .ident {
										type_name = c.value_type_name(base.value, cur_module)
									}
									if type_name.len > 0 {
										calls << type_name + '.' + callee.value
									}
								}
							}
							calls << callee.value
						}
					}
				}
				for ci in 1 .. child.children_count {
					arg_id := c.a.child(child, ci)
					if int(arg_id) >= 0 {
						arg := c.a.nodes[int(arg_id)]
						if arg.kind == .ident && arg.value.len > 0 {
							calls << arg.value
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
				next_id := c.a.child(child, j)
				if int(next_id) >= 0 {
					stack << next_id
				}
				j--
			}
		}
	}
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
fn (c &CallCollector) collect_fn_value_ident(id flat.NodeId, name string, cur_module string, imports map[string]string, mut calls []string) {
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

fn (c &CallCollector) value_type_name(name string, cur_module string) string {
	for candidate in [qualify_fn(cur_module, name), name] {
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
	}
	return ''
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
		return 'Array'
	} else if t is types.Map {
		return 'map'
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
