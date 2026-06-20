module markused

import v3.flat
import v3.types

const trace_markused = false

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
			mod := if node.value.contains('.') { node.value.all_after_last('.') } else { node.value }
			imports[node.typ] = mod
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
		'map.clear', 'strings.Builder.write_ptr', 'strings.Builder.write_runes',
		'strings.Builder.free', 'strconv.format_int', 'strconv.format_uint', 'sync.new_channel_st',
		'sync.Channel.push', 'sync.Channel.pop', 'sync.Channel.close', 'new_channel_st',
		'Channel.push', 'Channel.pop', 'Channel.close'] {
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
	enqueue_initializer_calls(a, collector, imports, fn_decls, mut used, mut queue)
	mut processed_nodes := map[int]bool{}
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
		if node_key in processed_nodes {
			continue
		}
		processed_nodes[node_key] = true
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

fn add_suffix_candidate(mut suffix_map map[string][]string, short string, name string) {
	if !valid_symbol_name(short) || !valid_symbol_name(name) {
		return
	}
	mut candidates := suffix_map[short] or { []string{} }
	candidates << name
	suffix_map[short] = candidates
}

fn valid_symbol_name(name string) bool {
	return name.len > 0 && name.len < 512
}

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

struct FnDeclInfo {
	node_id flat.NodeId
	module  string
}

struct StructDeclInfo {
	node_id flat.NodeId
	module  string
}

struct ConstDeclInfo {
	expr_id flat.NodeId
	module  string
}

struct CallCollector {
	a            &flat.FlatAst      = unsafe { nil }
	tc           &types.TypeChecker = unsafe { nil }
	fn_decls     map[string]FnDeclInfo
	struct_decls map[string]StructDeclInfo
	const_decls  map[string]ConstDeclInfo
}

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

fn enqueue_main_module_roots(fn_decls map[string]FnDeclInfo, mut used map[string]bool, mut queue []string) {
	for name, info in fn_decls {
		if info.module != 'main' || name.contains('.') {
			continue
		}
		enqueue(name, mut used, mut queue)
	}
}

fn is_auto_root_fn(name string) bool {
	short_name := name.all_after_last('.')
	return short_name == 'init' || short_name == 'testsuite_begin' || short_name == 'testsuite_end'
		|| short_name.starts_with('test_')
}

fn enqueue_detected_runtime_helpers(a &flat.FlatAst, tc &types.TypeChecker, mut used map[string]bool, mut queue []string) {
	mut needs_optional_helpers := false
	mut needs_string_interp_helpers := false
	for node in a.nodes {
		match node.kind {
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
			.none_expr, .or_expr {
				needs_optional_helpers = true
			}
			.call {
				if node.children_count > 0 {
					fn_node := a.child_node(&node, 0)
					if fn_node.kind == .ident
						&& (fn_node.value == 'error' || fn_node.value == 'error_with_code') {
						needs_optional_helpers = true
					}
				}
			}
			.string_interp {
				needs_string_interp_helpers = true
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
}

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

fn type_string_needs_optional_helpers(typ string) bool {
	return typ.len > 0 && (typ[0] == `?` || typ[0] == `!`)
}

fn qualify_fn(mod string, name string) string {
	if mod.len == 0 || mod == 'main' || mod == 'builtin' {
		return name
	}
	return '${mod}.${name}'
}

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
									type_name := resolve_type_name(base_type)
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
						lhs_type := c.node_type(lhs_id)
						lhs_name := resolve_type_name(lhs_type)
						if lhs_name.len > 0 {
							op_name := match child.op {
								.minus { '-' }
								.plus { '+' }
								.eq { '==' }
								.ne { '!=' }
								.lt { '<' }
								.gt { '>' }
								.le { '<=' }
								.ge { '>=' }
								else { '' }
							}

							if op_name.len > 0 {
								calls << lhs_name + '.' + op_name
							}
						}
					}
				}
			}
			.struct_init {
				c.collect_struct_default_calls(child, imports, mut calls)
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

fn (c &CallCollector) collect_fn_value_ident(id flat.NodeId, name string, cur_module string, imports map[string]string, mut calls []string) {
	if name.len == 0 || !c.name_may_reference_fn(name, cur_module, imports) {
		return
	}
	if !c.node_is_fn_value(id) && !c.name_has_fn_decl(name, cur_module, imports) {
		return
	}
	c.add_fn_value_candidates(name, cur_module, imports, mut calls)
	c.add_const_alias_candidates(name, cur_module, imports, mut calls)
}

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
	if !c.node_is_fn_value(id) && !c.name_has_fn_decl(name, cur_module, imports) {
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

fn (c &CallCollector) name_may_reference_fn(name string, cur_module string, imports map[string]string) bool {
	for candidate in c.const_ref_candidates(name, cur_module, imports) {
		if candidate in c.fn_decls || candidate in c.const_decls {
			return true
		}
	}
	return false
}

fn (c &CallCollector) name_has_fn_decl(name string, cur_module string, imports map[string]string) bool {
	for candidate in c.const_ref_candidates(name, cur_module, imports) {
		if candidate in c.fn_decls {
			return true
		}
	}
	return false
}

fn (c &CallCollector) node_is_fn_value(id flat.NodeId) bool {
	expr_type := c.node_type(id)
	return expr_type is types.FnType
}

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

fn (c &CallCollector) add_const_alias_candidates(name string, cur_module string, imports map[string]string, mut calls []string) {
	for const_name in c.const_ref_candidates(name, cur_module, imports) {
		info := c.const_decls[const_name] or { continue }
		expr := c.a.node(info.expr_id)
		match expr.kind {
			.ident {
				c.add_fn_value_candidates(expr.value, info.module, imports, mut calls)
			}
			.selector {
				if expr.children_count > 0 {
					base := c.a.child_node(expr, 0)
					if base.kind == .ident && base.value.len > 0 && expr.value.len > 0 {
						c.add_fn_value_candidates('${base.value}.${expr.value}', info.module,
							imports, mut calls)
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
}

fn (c &CallCollector) const_ref_candidates(name string, cur_module string, imports map[string]string) []string {
	mut candidates := []string{}
	candidates << name
	qname := qualify_fn(cur_module, name)
	if qname != name {
		candidates << qname
	}
	if name.contains('.') {
		base := name.all_before_last('.')
		member := name.all_after_last('.')
		if base in imports {
			candidates << imports[base] + '.' + member
		}
	}
	return candidates
}

fn (c &CallCollector) node_type(id flat.NodeId) types.Type {
	if t := c.tc.expr_type(id) {
		return t
	}
	return c.tc.resolve_type(id)
}

fn (c &CallCollector) collect_struct_default_calls(init &flat.Node, imports map[string]string, mut calls []string) {
	info := c.struct_decls[init.value] or { return }
	mut set_fields := map[string]bool{}
	for i in 0 .. init.children_count {
		field := c.a.child_node(init, i)
		if field.kind == .field_init {
			set_fields[field.value] = true
		}
	}
	node := c.a.node(info.node_id)
	for i in 0 .. node.children_count {
		field := c.a.child_node(node, i)
		if field.kind != .field_decl || field.children_count == 0 || field.value in set_fields {
			continue
		}
		c.collect_calls(field, info.module, imports, '', '', mut calls)
	}
}

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

fn markused_c_name(name string) string {
	if name.starts_with('C.') {
		return name[2..]
	}
	if name == 'malloc' {
		return 'v_malloc'
	}
	return name.replace('[]', 'Array_').replace('.-', '__minus').replace('.+', '__plus').replace('.==',
		'__eq').replace('.!=', '__ne').replace('.<=', '__le').replace('.>=', '__ge').replace('.<',
		'__lt').replace('.>', '__gt').replace('&', 'ptr').replace('[', '_').replace(']', '').replace(',',
		'_').replace(' ', '_').replace('.', '__')
}
