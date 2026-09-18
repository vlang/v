		if p.prefs.is_fmt && p.tok == .semicolon {
			p.next()
			continue
		}
		start_offset := p.s.offset
		param_ids << p.parse_param_group(false)
		if p.s.offset == start_offset && p.tok != .rpar && p.tok != .eof {
			p.next()
		}
	}
	param_list_end := p.tok_pos
	p.check(.rpar)

	mut ret_type := 'void'
	ret_type_start := p.tok_pos
	if p.tok != .lcbr && p.tok != .semicolon && p.tok != .eof {
		ret_type = p.parse_type_name()
		if p.tok == .question {
			ret_type += '?'
			p.next()
		} else if p.tok == .not {
			p.record_diagnostic_span('wrong syntax, it must be !${ret_type}, not ${ret_type}!', ret_type_start, p.tok_pos)
			p.next()
		}
	}

	clean_type := method_receiver_type_name(receiver_type)
	name := '${clean_type}.${op_name}'

	mut body_ids := []flat.NodeId{}
	if p.tok == .lcbr {
		prev_fn := p.cur_fn
		prev_fn_offset := p.cur_fn_offset
		prev_struct := p.cur_struct
		prev_method_is_static := p.cur_method_is_static
		outer_defer_depth := p.defer_depth
		outer_defer_result_allowed := p.defer_result_allowed
		outer_nested_block_depth := p.nested_block_depth
		p.cur_fn = name
		// The declaration records `name_pos` as its position, the same as
		// `fn_decl_body` does, so a skipped body of it is keyed on it too.
		p.cur_fn_offset = name_pos
		p.cur_struct = method_receiver_type_name(receiver_type).all_after_last('.')
		p.cur_method_is_static = false
		p.defer_depth = 0
		p.defer_result_allowed = false
		p.nested_block_depth = 0
		p.push_local_type_scope(name)
		p.begin_comptime_value_scope()
		p.begin_local_binding_scope()
		// Seed the operator's parameters as local bindings before the body is parsed, so a
		// `$tmpl()` in a short-circuit/subexpression template that calls a function-valued
		// operator parameter (`@{render(row)}`) captures it into the inlined IIFE —
		// collect_template_free_idents only captures a bare callee that is_local_binding().
		// Mirrors fn_decl_body's parameter seeding.
		for pid in param_ids {
			pnode := p.a.nodes[int(pid)]
			if pnode.kind == .param {
				p.declare_local_binding(pnode.value)
			}
		}
		if disable_body {
			p.mark_disabled_fn(name)
			p.skip_block()
		} else {
			body_start := p.tok_pos
			p.check(.lcbr)
			p.predeclare_local_type_names_in_block(body_start)
			for p.tok != .rcbr && p.tok != .eof {
				id := p.stmt()
				// Lower a `$tmpl()` / `$veb.html()` used in an operator overload body, so
				// its `.veb_template` placeholder does not leak past the parser (no later
				// phase handles it), matching normal function/block parsing.
				if expansion := p.expand_veb_template_stmt(id) {
					body_ids << expansion
					continue
				}
				if int(id) >= 0 {
					body_ids << id
				}
			}
			p.check(.rcbr)
		}
		p.end_local_binding_scope()
		p.end_comptime_value_scope()
		p.pop_local_type_scope()
		p.cur_fn = prev_fn
		p.cur_fn_offset = prev_fn_offset
		p.cur_struct = prev_struct
		p.cur_method_is_static = prev_method_is_static
		p.defer_depth = outer_defer_depth
		p.defer_result_allowed = outer_defer_result_allowed
		p.nested_block_depth = outer_nested_block_depth
	}

	mut all_ids := []flat.NodeId{cap: param_ids.len + body_ids.len}
	for id in param_ids {
		all_ids << id
	}
	for id in body_ids {
		all_ids << id
	}
	start := p.add_children(all_ids)
	id := p.add_node(flat.Node{
		kind:           .fn_decl
		op:             if is_pub { .arrow } else { .none }
		value:          name
		typ:            ret_type
		pos:            token.new_pos(p.cur_file_id, name_pos)
		children_start: start
		children_count: flat.child_count(all_ids.len)
	})
	p.record_formatter_param_list_end(id, param_list_end)
	p.register_pending_export(name)
	return id
}

fn (mut p Parser) fn_decl_body(name string, receiver_name string, receiver_type string, receiver_is_mut bool, is_method bool, interop_prefix string, name_pos int) flat.NodeId {
	is_c_decl := interop_prefix.len > 0
	is_static_type_method := is_method && receiver_name.len == 0 && !is_c_decl
	is_pub := p.pending_decl_pub
	p.pending_decl_pub = false
	// Capture & clear here so it applies only to this function (not nested closures
	// or a following declaration), and is cleared even on the extern/no-body path.
	disable_body := p.disable_fn_body
	p.disable_fn_body = false
	// generic params — skip
	mut generic_params := []string{}
	if p.tok == .lsbr {
		generic_params = p.parse_generic_param_names()
	}

	// params
	p.check(.lpar)
	mut param_ids := []flat.NodeId{}
	if is_method && receiver_name.len > 0 {
		param_ids << p.add_node(flat.Node{
			kind:   .param
			value:  receiver_name
			typ:    receiver_type
			is_mut: receiver_is_mut
