module types

import v.flat

// A source keeps its declaration identity while aliases cross nested scopes.
// Only a source whose exact binding is still live can transfer at a return.
struct SumReturnLocal {
	name         string
	type_name    u32
	decl         flat.NodeId
	depth        int
	owner        string
	owner_decl   flat.NodeId
	is_parameter bool
}

struct SumBorrowAlias {
	decl    flat.NodeId
	sources []string
}

struct SumBorrowExit {
	aliases map[string]SumBorrowAlias
	label   string
}

struct SumBorrowFlow {
mut:
	aliases       map[string]SumBorrowAlias
	falls_through bool = true
	breaks        []SumBorrowExit
	continues     []SumBorrowExit
}

fn (tc &TypeChecker) sum_escape_has_pointer_variant(typ Type) bool {
	sum := unalias_type(typ)
	if sum !is SumType { return false }
	for variant in tc.sum_types.type_ids(sum.name()) or { []u32{} } {
		if unalias_type(tc.parse_canonical_type(variant)) is Pointer { return true }
	}
	return false
}

fn sum_escape_success_type(typ Type) Type {
	clean := unalias_type(typ)
	if clean is ResultType { return *clean.base_type }
	if clean is OptionType { return *clean.base_type }
	return *clean
}

fn (mut tc TypeChecker) ownership_prescan_sum_local_returns(fn_name string, fn_node flat.Node) {
	mut aliases := map[string][]SumReturnLocal{}
	mut locals := map[string]SumReturnLocal{}
	// Parameters supply receiver/call metadata but never become local allocation
	// sources. A reference to a value parameter still belongs to the caller.
	for i in 0 .. fn_node.children_count {
		child_id := tc.a.child(&fn_node, i)
		child := tc.a.node(child_id)
		if child.kind == .param && child.value.len > 0 {
			locals[child.value] = SumReturnLocal{
				name:         child.value
				type_name:    child.typ
				decl:         child_id
				depth:        -1
				is_parameter: true
			}
		}
	}
	for i in 0 .. fn_node.children_count {
		child := tc.a.child(&fn_node, i)
		if tc.a.node(child).kind != .param {
			tc.ownership_scan_sum_local_returns(fn_name, child, mut aliases, mut locals, 0)
		}
	}
}

// These queries read declaration metadata and the prescan's lexical map. They
// must not resolve live expressions before normal checking has seeded scopes.
fn (tc &TypeChecker) sum_escape_local_types(locals map[string]SumReturnLocal) map[string]Type {
	mut result := map[string]Type{}
	for name, local in locals {
		if local.type_name != 0 { result[name] = *tc.parse_type(local.type_name) }
	}
	return result
}

fn (mut tc TypeChecker) sum_escape_prescan_expr_type(id flat.NodeId, locals map[string]SumReturnLocal) Type {
	if !tc.valid_node_id(id) { return Type(Unknown{}) }
	node := tc.a.node(id)
	match node.kind {
		.ident {
			if local := locals[node.value] { return *tc.parse_type(local.type_name) }
		}
		.cast_expr, .struct_init, .assoc {
			if node.value.len > 0 { return *tc.parse_type(flat.source_type_id(node.value)) }
		}
		.paren, .expr_stmt {
			if node.children_count > 0 {
				return tc.sum_escape_prescan_expr_type(tc.a.child(node, 0), locals)
			}
		}
		.or_expr {
			if node.children_count > 0 {
				return sum_escape_success_type(tc.sum_escape_prescan_expr_type(tc.a.child(node, 0), locals))
			}
		}
		.prefix {
			if node.children_count > 0 {
				inner := tc.sum_escape_prescan_expr_type(tc.a.child(node, 0), locals)
				if node.op == .amp && inner !is Unknown {
					return Type(Pointer{ base_type: tc.intern_type_reference(inner) })
				}
				clean := unalias_type(inner)
				if node.op == .mul && clean is Pointer { return *clean.base_type }
			}
		}
		.call {
			if sum_name := tc.sum_constructor_call_name(*node) {
				return Type(SumType{ name_id: sum_name })
			}
			local_types := tc.sum_escape_local_types(locals)
			if info := tc.ownership_prescan_call_info(*node, local_types) {
				return info.return_type
			}
		}
		.block, .match_branch {
			if node.children_count > 0 {
				return tc.sum_escape_prescan_expr_type(tc.a.child(node, node.children_count - 1), locals)
			}
		}
		.if_expr, .match_stmt {
			for i in 1 .. node.children_count {
				branch := tc.sum_escape_prescan_expr_type(tc.a.child(node, i), locals)
				if branch !is Unknown && branch !is Void { return branch }
			}
		}
		.int_literal { return Type(int_) }
		.float_literal { return Type(f64_) }
		.bool_literal { return Type(bool_) }
		.string_literal, .string_interp { return Type(String{}) }
		else {}
	}
	if recorded := tc.expr_type(id) { return recorded }
	if node.typ != 0 { return *tc.parse_type(node.typ) }
	return Type(Unknown{})
}

fn (mut tc TypeChecker) sum_return_expr_sources(id flat.NodeId, aliases map[string][]SumReturnLocal, locals map[string]SumReturnLocal) []SumReturnLocal {
	return tc.sum_return_expr_sources_seen(id, aliases, locals, []flat.NodeId{})
}

fn (mut tc TypeChecker) sum_return_expr_sources_seen(id flat.NodeId, aliases map[string][]SumReturnLocal, locals map[string]SumReturnLocal, visited []flat.NodeId) []SumReturnLocal {
	if !tc.valid_node_id(id) || id in visited { return []SumReturnLocal{} }
	mut ancestors := visited.clone()
	ancestors << id
	node := tc.a.node(id)
	match node.kind {
		.ident { return (aliases[node.value] or { []SumReturnLocal{} }).clone() }
		.paren, .expr_stmt, .cast_expr {
			if node.children_count > 0 {
				return tc.sum_return_expr_sources_seen(tc.a.child(node, 0), aliases, locals, ancestors)
			}
		}
		.prefix {
			if node.op == .amp && node.children_count == 1 {
				value := tc.a.child_node(node, 0)
				if value.kind == .ident {
					if source := locals[value.value] {
						if !source.is_parameter && unalias_type(tc.parse_type(source.type_name)) !is Pointer {
							return [source]
						}
					}
				}
			}
		}
		.call {
			if node.children_count == 2 {
				if _ := tc.sum_constructor_call_name(*node) {
					return tc.sum_return_expr_sources_seen(tc.call_arg_value(tc.a.child(node, 1)), aliases, locals, ancestors)
				}
			}
			mut sources := []SumReturnLocal{}
			local_types := tc.sum_escape_local_types(locals)
			info := tc.ownership_prescan_call_info(*node, local_types) or { return sources }
			projection := OwnershipCallProjection{ call_id: id }
			for argument in tc.ownership_call_result_source_args_with_info(projection, *node, info) {
				for source in tc.sum_return_expr_sources_seen(argument, aliases, locals, ancestors) {
					if source !in sources { sources << source }
				}
			}
			return sources
		}
		.if_expr, .match_stmt, .or_expr {
			mut sources := []SumReturnLocal{}
			start := if node.kind == .or_expr { 0 } else { 1 }
			for i in start .. node.children_count {
				for source in tc.sum_return_expr_sources_seen(tc.a.child(node, i), aliases, locals, ancestors) {
					if source !in sources { sources << source }
				}
			}
			return sources
		}
		.block, .match_branch {
			if node.children_count > 0 {
				return tc.sum_return_expr_sources_seen(tc.a.child(node, node.children_count - 1), aliases, locals, ancestors)
			}
		}
		else {}
	}
	return []SumReturnLocal{}
}

fn (mut tc TypeChecker) ownership_scan_sum_local_returns(fn_name string, id flat.NodeId, mut aliases map[string][]SumReturnLocal, mut locals map[string]SumReturnLocal, depth int) {
	if !tc.valid_node_id(id) { return }
	node := tc.a.node(id)
	if node.kind in [.fn_decl, .fn_literal, .lambda_expr, .defer_stmt] { return }
	if node.kind in [.block, .match_branch] {
		mut inner_aliases := aliases.clone()
		mut inner_locals := locals.clone()
		for i in 0 .. node.children_count {
			tc.ownership_scan_sum_local_returns(fn_name, tc.a.child(node, i), mut inner_aliases, mut inner_locals, depth + 1)
		}
		for name, source in locals {
			if inner_source := inner_locals[name] {
				if inner_source.decl == source.decl {
					aliases[name] = inner_aliases[name] or { []SumReturnLocal{} }
				}
			}
		}
		return
	}
	if node.kind in [.decl_assign, .assign] && node.children_count == 2 {
		lhs := tc.a.child_node(node, 0)
		if lhs.kind == .ident {
			rhs_id := tc.a.child(node, 1)
			mut sources := tc.sum_return_expr_sources(rhs_id, aliases, locals)
			if node.kind == .decl_assign {
				typ := tc.sum_escape_prescan_expr_type(rhs_id, locals)
				// A local pointer is borrowed here, even when its initializer allocated.
				if typ !is Unknown && typ !is Void {
					locals[lhs.value] = SumReturnLocal{ name: lhs.value, type_name: typ.source_type_id(), decl: id, depth: depth }
				} else {
					locals.delete(lhs.value)
				}
			}
			if target := locals[lhs.value] {
				if tc.sum_escape_has_pointer_variant(tc.parse_type(target.type_name)) {
					rhs := tc.a.node(rhs_id)
					for i, source in sources {
						if target.depth < source.depth {
							tc.ownership_note_sum_heap_source(fn_name, source)
							mut st := tc.ownership_state()
							key := '${fn_name}\x01${int(id)}'
							mut candidates := st.sum_outer_store_local_sources[key] or { []OwnershipDropEntry{} }
							candidates << OwnershipDropEntry{ name: source.name, type_name: source.type_name }
							st.sum_outer_store_local_sources[key] = candidates
						}
						moves_owner := rhs.kind == .ident && source.owner == rhs.value
						if (source.owner.len == 0 && target.depth < source.depth) || moves_owner {
							updated := SumReturnLocal{ ...source, owner: lhs.value, owner_decl: target.decl }
							sources[i] = updated
							for name, previous in locals {
								if previous.decl == source.decl { locals[name] = updated }
							}
							for name, previous in aliases {
								mut replacements := previous.clone()
								for j, old in previous {
									if old.decl == source.decl { replacements[j] = updated }
								}
								aliases[name] = replacements
							}
						}
					}
				}
			}
			aliases[lhs.value] = sources
		}
		return
	}
	if node.kind == .return_stmt && node.children_count == 1 {
		ret := unalias_type(tc.fn_ret_types[fn_name] or { return })
		if !tc.sum_escape_has_pointer_variant(ret) { return }
		if tc.ownership_sum_return_branch_has_local(tc.a.child(node, 0), aliases, locals) {
			tc.record_error(.return_mismatch, 'cannot return a sum pointer to a local declared inside an if or match expression or an `or` block; declare the local before the return expression', id)
			return
		}
		mut entries := []OwnershipDropEntry{}
		for source in tc.sum_return_expr_sources(tc.a.child(node, 0), aliases, locals) {
			tc.ownership_note_sum_heap_source(fn_name, source)
			if source.owner.len > 0 {
				if owner := locals[source.owner] {
					if owner.decl == source.owner_decl {
						entries << OwnershipDropEntry{ name: owner.name, type_name: owner.type_name }
					}
				}
				continue
			}
			if current := locals[source.name] {
				if current.decl == source.decl {
					entries << OwnershipDropEntry{ name: source.name, type_name: source.type_name }
				}
			}
		}
		if entries.len > 0 {
			mut st := tc.ownership_state()
			st.sum_return_local_sources['${fn_name}\x01${int(id)}'] = entries
			st.mark_fn_return_owned(fn_name)
			tc.ownership_add_fn_return_slot(fn_name, 0)
		}
		return
	}
	if node.kind in [.for_stmt, .for_in_stmt] {
		// Flat loop bodies contain a sequence of statements after the header.
		// Their declarations must remain visible to later stores in that body.
		mut loop_aliases := aliases.clone()
		mut loop_locals := locals.clone()
		body_start := if node.kind == .for_in_stmt { node.value.int() } else { 3 }
		if node.kind == .for_stmt && node.children_count > 0 {
			tc.ownership_scan_sum_local_returns(fn_name, tc.a.child(node, 0), mut loop_aliases, mut loop_locals, depth + 1)
		}
		for i in body_start .. node.children_count {
			tc.ownership_scan_sum_local_returns(fn_name, tc.a.child(node, i), mut loop_aliases, mut loop_locals, depth + 1)
		}
		if node.kind == .for_stmt && node.children_count > 2 {
			tc.ownership_scan_sum_local_returns(fn_name, tc.a.child(node, 2), mut loop_aliases, mut loop_locals, depth + 1)
		}
		for name, original in locals {
			current := loop_locals[name] or { continue }
			if current.decl != original.decl { continue }
			mut values := (aliases[name] or { []SumReturnLocal{} }).clone()
			for source in loop_aliases[name] or { []SumReturnLocal{} } {
				if source !in values { values << source }
			}
			aliases[name] = values
		}
		return
	}
	if node.kind in [.if_expr, .match_stmt] {
		// Branches may leave different pointer alternatives. Retain all possible
		// sources; C generation checks the actual tag and pointer before transfer.
		for i in 0 .. node.children_count {
			mut branch_aliases := aliases.clone()
			mut branch_locals := locals.clone()
			tc.ownership_scan_sum_local_returns(fn_name, tc.a.child(node, i), mut branch_aliases, mut branch_locals, depth)
			for name, _ in aliases {
				for source in branch_aliases[name] or { []SumReturnLocal{} } {
					mut values := (aliases[name] or { []SumReturnLocal{} }).clone()
					if source !in values {
						values << source
						aliases[name] = values
					}
				}
			}
		}
		return
	}
	for i in 0 .. node.children_count {
		tc.ownership_scan_sum_local_returns(fn_name, tc.a.child(node, i), mut aliases, mut locals, depth)
	}
}

// ownership_sum_return_sources returns live local allocations whose cleanup may
// transfer to the active pointer variant at this particular return.
pub fn (tc &TypeChecker) ownership_sum_return_sources(fn_name string, id flat.NodeId) []OwnershipDropEntry {
	if isnil(tc.ownership) { return []OwnershipDropEntry{} }
	return (tc.ownership.sum_return_local_sources['${fn_name}\x01${int(id)}'] or { []OwnershipDropEntry{} }).clone()
}

fn (mut tc TypeChecker) sum_aggregate_expr_local_sources(id flat.NodeId, aliases map[string][]SumReturnLocal, locals map[string]SumReturnLocal) []SumReturnLocal {
	if !tc.valid_node_id(id) { return []SumReturnLocal{} }
	node := tc.a.node(id)
	if node.kind != .field_init && tc.sum_escape_has_pointer_variant(tc.sum_escape_prescan_expr_type(id, locals)) {
		return tc.sum_return_expr_sources(id, aliases, locals)
	}
	mut sources := []SumReturnLocal{}
	if node.kind in [.struct_init, .assoc, .array_literal, .array_init, .map_init, .field_init,
		.paren, .expr_stmt, .or_expr] {
		for i in 0 .. node.children_count {
			for source in tc.sum_aggregate_expr_local_sources(tc.a.child(node, i), aliases, locals) {
				if source !in sources { sources << source }
			}
		}
	}
	return sources
}

fn (mut tc TypeChecker) ownership_sum_return_branch_has_local(id flat.NodeId, aliases map[string][]SumReturnLocal, locals map[string]SumReturnLocal) bool {
	if !tc.valid_node_id(id) { return false }
	node := tc.a.node(id)
	if node.kind in [.paren, .expr_stmt, .cast_expr] && node.children_count > 0 {
		return tc.ownership_sum_return_branch_has_local(tc.a.child(node, 0), aliases, locals)
	}
	if payload := tc.ownership_sum_constructor_payload(id) {
		return tc.ownership_sum_return_branch_has_local(payload, aliases, locals)
	}
	if node.kind in [.if_expr, .match_stmt, .or_expr] {
		start := if node.kind == .or_expr { 0 } else { 1 }
		for i in start .. node.children_count {
			if tc.ownership_sum_return_branch_has_local(tc.a.child(node, i), aliases, locals) {
				return true
			}
		}
	}
	if node.kind in [.block, .match_branch] && node.children_count > 0 {
		mut inner_aliases := aliases.clone()
		mut inner_locals := locals.clone()
		for i in 0 .. node.children_count - 1 {
			statement_id := tc.a.child(node, i)
			statement := tc.a.node(statement_id)
			if statement.kind != .decl_assign || statement.children_count != 2 { continue }
			lhs := tc.a.child_node(statement, 0)
			if lhs.kind != .ident { continue }
			rhs := tc.a.child(statement, 1)
			mut sources := tc.sum_return_expr_sources(rhs, inner_aliases, inner_locals)
			for source in tc.sum_aggregate_expr_local_sources(rhs, inner_aliases, inner_locals) {
				if source !in sources { sources << source }
			}
			inner_aliases[lhs.value] = sources
			inner_locals[lhs.value] = SumReturnLocal{
				name:      lhs.value
				type_name: tc.sum_escape_prescan_expr_type(rhs, inner_locals).source_type_id()
				decl:      statement_id
			}
		}
		tail := tc.a.child(node, node.children_count - 1)
		mut sources := tc.sum_return_expr_sources(tail, inner_aliases, inner_locals)
		for source in tc.sum_aggregate_expr_local_sources(tail, inner_aliases, inner_locals) {
			if source !in sources { sources << source }
		}
		for source in sources {
			original := locals[source.name] or { return true }
			if original.decl != source.decl { return true }
		}
		return tc.ownership_sum_return_branch_has_local(tail, inner_aliases, inner_locals)
	}
	return false
}

fn (mut tc TypeChecker) ownership_note_sum_heap_source(fn_name string, source SumReturnLocal) {
	if source.is_parameter { return }
	mut st := tc.ownership_state()
	entry := OwnershipDropEntry{ name: source.name, type_name: source.type_name }
	mut entries := (st.sum_local_heap_sources[fn_name] or { []OwnershipDropEntry{} }).clone()
	if entry !in entries {
		entries << entry
		st.sum_local_heap_sources[fn_name] = entries
	}
}

// ownership_sum_heap_sources returns locals whose addresses must have stable heap
// storage before a pointer sum can retain them across their original scope.
pub fn (tc &TypeChecker) ownership_sum_heap_sources(fn_name string) []OwnershipDropEntry {
	if isnil(tc.ownership) { return []OwnershipDropEntry{} }
	return (tc.ownership.sum_local_heap_sources[fn_name] or { []OwnershipDropEntry{} }).clone()
}

fn (mut tc TypeChecker) ownership_prepare_sum_return_transfer(return_id flat.NodeId, expr_id flat.NodeId) {
	if !tc.sum_escape_has_pointer_variant(tc.fn_context.return_type) { return }
	mut st := tc.ownership_state()
	sources := st.sum_return_local_sources['${st.cur_fn}\x01${int(return_id)}'] or { return }
	heap_sources := st.sum_local_heap_sources[st.cur_fn] or { []OwnershipDropEntry{} }
	for source in sources {
		if source in heap_sources {
			if owner := tc.cur_scope.lookup_owner(source.name) {
				actual := owner.scope.types[owner.index]
				pointer := Type(Pointer{ base_type: tc.intern_type_reference(actual) })
				if tc.type_compatible(pointer, tc.fn_context.return_type) {
					st.sum_promoted_local_bindings[owner.storage_key()] = true
					name := tc.ownership_expr_ident_name(expr_id)
					if name.len > 0 && name != source.name {
						if borrowed := tc.ownership_borrowed_alias_source(name, false) {
							if borrowed == source.name {
								tc.ownership_release_borrower(name)
								tc.ownership_mark_owned(name, tc.fn_context.return_type, expr_id)
							}
						}
					}
				}
			}
		}
		if source.name !in st.owned_vars || unalias_type(tc.parse_type(source.type_name)) !is SumType {
			continue
		}
		name := tc.ownership_expr_ident_name(expr_id)
		if name.len == 0 || name == source.name { continue }
		borrowed_from := tc.ownership_borrowed_alias_source(name, false) or { continue }
		if borrowed_from != source.name { continue }
		// The return value receives a conditional owner. The original cleanup
		// remains scheduled; codegen clears only the matched payload's flag.
		tc.ownership_release_borrower(name)
		tc.ownership_mark_owned(name, tc.fn_context.return_type, expr_id)
	}
}

// ownership_sum_transfer_source returns the exact local whose allocation a sum
// storage expression acquires, or an empty name for a borrowed expression.
pub fn (tc &TypeChecker) ownership_sum_transfer_source(id flat.NodeId) string {
	if isnil(tc.ownership) { return '' }
	return tc.ownership.sum_value_transfer_sources[int(id)] or { '' }
}

fn (tc &TypeChecker) ownership_sum_local_scope(name string) int {
	if isnil(tc.ownership) { return -1 }
	for i := tc.ownership.scope_frames.len - 1; i >= 0; i-- {
		frame := tc.ownership.scope_frames[i]
		if frame.cur_fn == tc.ownership.cur_fn && name in frame.names { return i }
	}
	return -1
}

fn (mut tc TypeChecker) ownership_sum_borrow_source_name(initial string) string {
	mut name := initial
	mut seen := map[string]bool{}
	for name.len > 0 && name != '<unknown-index-alias>' && !seen[name] {
		seen[name] = true
		if next := tc.ownership.pointer_index_aliases[name] {
			name = next
		} else if next := tc.ownership_borrowed_alias_source(name, false) {
			name = next
		} else {
			return name
		}
	}
	return ''
}

fn (mut tc TypeChecker) ownership_sum_borrow_sources(id flat.NodeId, ancestors []flat.NodeId) []string {
	return tc.ownership_sum_borrow_sources_with_aliases(id, ancestors, map[string]SumBorrowAlias{})
}

fn (mut tc TypeChecker) ownership_sum_borrow_sources_with_aliases(id flat.NodeId, ancestors []flat.NodeId, aliases map[string]SumBorrowAlias) []string {
	if !tc.valid_node_id(id) || id in ancestors { return []string{} }
	mut visited := ancestors.clone()
	visited << id
	node := tc.a.node(id)
	if node.kind == .ident && node.value in aliases { return aliases[node.value].sources.clone() }
	if node.kind in [.paren, .expr_stmt, .cast_expr, .as_expr] && node.children_count > 0 {
		return tc.ownership_sum_borrow_sources_with_aliases(tc.a.child(node, 0), visited, aliases)
	}
	mut name := ''
	if node.kind == .prefix && node.op == .amp && node.children_count == 1 {
		name = tc.ownership_expr_ident_name(tc.a.child(node, 0))
	} else if node.kind in [.ident, .selector, .index] {
		key := tc.ownership_expr_ident_name(id)
		name = tc.ownership.pointer_index_aliases[key] or {
			tc.ownership_borrowed_alias_source(key, false) or { '' }
		}
		if name.len == 0 && key in tc.ownership.owned_vars
			&& unalias_type(tc.resolve_type(id)) is Pointer {
			name = key
		}
	}
	if name.len > 0 {
		source := tc.ownership_sum_borrow_source_name(name)
		return if source.len > 0 { [source] } else { []string{} }
	}
	if payload := tc.ownership_sum_constructor_payload(id) {
		return tc.ownership_sum_borrow_sources_with_aliases(payload, visited, aliases)
	}
	mut sources := []string{}
	if node.kind == .call {
		for argument in tc.ownership_call_result_source_args(id) {
			for source in tc.ownership_sum_borrow_sources_with_aliases(argument, visited, aliases) {
				if source !in sources { sources << source }
			}
		}
	} else if node.kind in [.if_expr, .match_stmt, .or_expr] {
		start := if node.kind == .or_expr { 0 } else { 1 }
		for i in start .. node.children_count {
			for source in tc.ownership_sum_borrow_sources_with_aliases(tc.a.child(node, i), visited, aliases) {
				if source !in sources { sources << source }
			}
		}
	} else if node.kind in [.block, .match_branch] && node.children_count > 0 {
		// Branch-local borrowers have left the live scope by the time the
		// enclosing result is assigned. Retain their value provenance locally.
		flow := tc.ownership_sum_borrow_flow_after(id, ancestors, aliases, '')
		if !flow.falls_through { return []string{} }
		return tc.ownership_sum_borrow_sources_with_aliases(tc.a.child(node, node.children_count - 1), visited, flow.aliases)
	}
	return sources
}

// Recover branch-local alias values after their checker scope has closed.
// Loop exits keep their own state; only continuing paths reach later statements.
fn (mut tc TypeChecker) ownership_sum_borrow_flow_after(id flat.NodeId, ancestors []flat.NodeId, initial map[string]SumBorrowAlias, loop_label string) SumBorrowFlow {
	current := tc.ownership_unwrap_expr(id)
	mut flow := SumBorrowFlow{ aliases: initial.clone() }
	if !tc.valid_node_id(current) || current in ancestors { return flow }
	mut visited := ancestors.clone()
	visited << current
	node := tc.a.node(current)
	if node.kind in [.break_stmt, .continue_stmt] {
		flow.falls_through = false
		exit := SumBorrowExit{ aliases: initial.clone(), label: node.value }
		if node.kind == .break_stmt { flow.breaks << exit } else { flow.continues << exit }
		return flow
	}
	if node.kind == .return_stmt {
		flow.falls_through = false
		return flow
	}
	if node.kind in [.decl_assign, .assign] && node.children_count == 2 {
		rhs := tc.a.child(node, 1)
		flow = tc.ownership_sum_borrow_flow_after(rhs, visited, initial, '')
		lhs := tc.a.child_node(node, 0)
		if flow.falls_through && lhs.kind == .ident {
			previous := flow.aliases[lhs.value] or { SumBorrowAlias{} }
			flow.aliases[lhs.value] = SumBorrowAlias{
				decl:    if node.kind == .decl_assign { current } else { previous.decl }
				sources: tc.ownership_sum_borrow_sources_with_aliases(rhs, visited, flow.aliases)
			}
		}
		return flow
	}
	if node.kind in [.block, .match_branch] {
		start := if node.kind == .match_branch && node.value != 'else' {
			node.value.int()
		} else {
			0
		}
		return tc.ownership_sum_borrow_sequence_flow(*node, start, visited, initial)
	}
	if node.kind in [.if_expr, .match_stmt, .or_expr] {
		mut paths := []map[string]SumBorrowAlias{}
		mut has_else := node.kind == .if_expr && node.children_count > 2
		if node.kind == .or_expr { paths << initial }
		for i in 1 .. node.children_count {
			branch_id := tc.a.child(node, i)
			branch := tc.a.node(branch_id)
			if node.kind == .match_stmt && branch.kind != .match_branch { continue }
			if branch.kind == .match_branch && branch.value == 'else' { has_else = true }
			branch_flow := sum_borrow_scope_flow(initial, tc.ownership_sum_borrow_flow_after(branch_id, visited, initial, ''))
			if branch_flow.falls_through { paths << branch_flow.aliases }
			flow.breaks << branch_flow.breaks
			flow.continues << branch_flow.continues
		}
		if node.kind != .or_expr && !has_else
			&& (node.kind != .match_stmt || !tc.match_covers_all_variants(*node)) {
			paths << initial
		}
		flow.falls_through = paths.len > 0
		flow.aliases = sum_borrow_merge_alias_paths(initial, paths)
		return flow
	}
	if node.kind in [.for_stmt, .for_in_stmt] {
		mut entry := initial.clone()
		start := if node.kind == .for_in_stmt { node.value.int() } else { 3 }
		if node.kind == .for_in_stmt {
			for i in 0 .. int_min(2, int(node.children_count)) {
				binding := tc.a.child_node(node, i)
				if binding.kind == .ident && binding.value.len > 0 {
					entry[binding.value] = SumBorrowAlias{ decl: current }
				}
			}
		} else if node.children_count > 0 {
			init_flow := tc.ownership_sum_borrow_flow_after(tc.a.child(node, 0), visited, entry, '')
			if !init_flow.falls_through { return sum_borrow_scope_flow(initial, init_flow) }
			entry = init_flow.aliases.clone()
		}
		mut iteration := entry.clone()
		for {
			body := tc.ownership_sum_borrow_sequence_flow(*node, start, visited, iteration)
			mut backedges := []map[string]SumBorrowAlias{}
			mut exits := []map[string]SumBorrowAlias{}
			mut outer_breaks := []SumBorrowExit{}
			mut outer_continues := []SumBorrowExit{}
			if body.falls_through { backedges << body.aliases }
			for exit in body.breaks {
				if exit.label.len == 0 || exit.label == loop_label {
					exits << exit.aliases
				} else {
					outer_breaks << exit
				}
			}
			for exit in body.continues {
				if exit.label.len == 0 || exit.label == loop_label {
					backedges << exit.aliases
				} else {
					outer_continues << exit
				}
			}
			if node.kind == .for_stmt && node.children_count > 2 {
				mut posted := []map[string]SumBorrowAlias{}
				for edge in backedges {
					post := tc.ownership_sum_borrow_flow_after(tc.a.child(node, 2), visited, edge, '')
					if post.falls_through { posted << post.aliases }
					for exit in post.breaks {
						if exit.label.len == 0 || exit.label == loop_label {
							exits << exit.aliases
						} else {
							outer_breaks << exit
						}
					}
					for exit in post.continues {
						if exit.label.len == 0 || exit.label == loop_label {
							posted << exit.aliases
						} else {
							outer_continues << exit
						}
					}
				}
				backedges = posted.clone()
			}
			mut heads := [entry]
			heads << backedges
			merged := sum_borrow_merge_alias_paths(entry, heads)
			if sum_borrow_aliases_equal(merged, iteration) {
				if node.kind == .for_in_stmt || (node.children_count > 1
					&& tc.a.child_node(node, 1).kind != .empty) {
					exits << merged
				}
				flow.aliases = sum_borrow_merge_alias_paths(initial, exits)
				flow.falls_through = exits.len > 0
				flow.breaks = outer_breaks
				flow.continues = outer_continues
				return sum_borrow_scope_flow(initial, flow)
			}
			iteration = merged.clone()
		}
	}
	flow.falls_through = !tc.stmt_definitely_returns(current)
	return flow
}

fn (mut tc TypeChecker) ownership_sum_borrow_sequence_flow(node flat.Node, start int, ancestors []flat.NodeId, initial map[string]SumBorrowAlias) SumBorrowFlow {
	mut flow := SumBorrowFlow{ aliases: initial.clone() }
	mut label := ''
	for i in start .. node.children_count {
		if !flow.falls_through { break }
		statement := tc.a.child(&node, i)
		statement_id := tc.ownership_unwrap_expr(statement)
		if !tc.valid_node_id(statement_id) { continue }
		statement_node := tc.a.node(statement_id)
		if statement_node.kind == .label_stmt {
			label = statement_node.value
			continue
		}
		mut next := tc.ownership_sum_borrow_flow_after(statement, ancestors, flow.aliases, label)
		label = ''
		if statement_node.kind == .block { next = sum_borrow_scope_flow(flow.aliases, next) }
		flow.aliases = next.aliases
		flow.falls_through = next.falls_through
		flow.breaks << next.breaks
		flow.continues << next.continues
	}
	return flow
}

fn sum_borrow_scope_flow(base map[string]SumBorrowAlias, source SumBorrowFlow) SumBorrowFlow {
	mut flow := source
	flow.aliases = sum_borrow_merge_alias_paths(base, [source.aliases])
	flow.breaks = []SumBorrowExit{}
	flow.continues = []SumBorrowExit{}
	for exit in source.breaks {
		flow.breaks << SumBorrowExit{ aliases: sum_borrow_merge_alias_paths(base, [exit.aliases]), label: exit.label }
	}
	for exit in source.continues {
		flow.continues << SumBorrowExit{ aliases: sum_borrow_merge_alias_paths(base, [exit.aliases]), label: exit.label }
	}
	return flow
}

fn sum_borrow_merge_alias_paths(base map[string]SumBorrowAlias, paths []map[string]SumBorrowAlias) map[string]SumBorrowAlias {
	mut names := base.keys()
	for path in paths {
		for name in path.keys() {
			if name !in names { names << name }
		}
	}
	mut merged := map[string]SumBorrowAlias{}
	for name in names {
		original := base[name] or { SumBorrowAlias{} }
		mut sources := []string{}
		mut exists := name in base
		for path in paths {
			candidate := path[name] or { original }
			value := if candidate.decl == original.decl { candidate } else { original }
			if name in path && candidate.decl == original.decl { exists = true }
			for source in value.sources {
				if source !in sources { sources << source }
			}
		}
		if exists { merged[name] = SumBorrowAlias{ decl: original.decl, sources: sources } }
	}
	return merged
}

fn sum_borrow_aliases_equal(left map[string]SumBorrowAlias, right map[string]SumBorrowAlias) bool {
	if left.len != right.len { return false }
	for name, value in left {
		other := right[name] or { return false }
		if value.decl != other.decl || value.sources.len != other.sources.len { return false }
		for source in value.sources {
			if source !in other.sources { return false }
		}
	}
	return true
}

// Same-sum constructors preserve the payload's loans and move obligations. This
// helper runs only while checking a live expression, after its type is known.
fn (mut tc TypeChecker) ownership_sum_wrapped_value(expr flat.NodeId, typ Type) flat.NodeId {
	mut current := tc.ownership_unwrap_expr(expr)
	if !tc.sum_escape_has_pointer_variant(typ) { return current }
	mut visited := map[int]bool{}
	for tc.valid_node_id(current) && !visited[int(current)] {
		visited[int(current)] = true
		payload := tc.ownership_sum_constructor_payload(current) or { break }
		if !semantic_types_equal(unalias_type(tc.resolve_type(payload)), unalias_type(typ)) {
			break
		}
		current = payload
	}
	return current
}

fn (mut tc TypeChecker) ownership_mark_sum_borrow(target string, expr flat.NodeId, typ Type, pos flat.NodeId) bool {
	if !tc.sum_escape_has_pointer_variant(typ) { return false }
	value := tc.ownership_sum_wrapped_value(expr, typ)
	if tc.ownership_sum_name_has_owner(tc.ownership_expr_ident_name(value)) { return false }
	sources := tc.ownership_sum_borrow_sources(expr, []flat.NodeId{})
	if sources.len == 0 { return false }
	// A fresh-return summary means the result can own storage. It does not
	// exclude a borrowed alternative, including through a same-sum wrapper.
	mut owns_result := tc.ownership_sum_value_creates_owner(expr, typ)
		|| tc.ownership_expr_is_ownership_call(value)
	node := tc.a.node(value)
	if node.kind in [.if_expr, .match_stmt, .or_expr] {
		owns_result = tc.ownership_mark_from_conditional_expr(target, value, typ, pos) || owns_result
	}
	mut st := tc.ownership_state()
	if owns_result {
		// A mixed return can carry a fresh owner or borrow an argument. Keep
		// both obligations; the runtime flag chooses its actual destructor.
		tc.ownership_mark_owned(target, typ, pos)
	} else {
		st.owned_vars.delete(target)
		st.owned_var_types.delete(target)
		st.moved_vars.delete(target)
	}
	for source in sources { tc.ownership_add_borrow(source, target, pos, false) }
	return true
}

fn (mut tc TypeChecker) ownership_sum_name_has_owner(name string) bool {
	return name.len > 0 && name in tc.ownership_state().owned_vars
		&& tc.sum_escape_has_pointer_variant(tc.ownership_type_for_var(name, Type(Unknown{})))
}

fn (mut tc TypeChecker) ownership_sum_aggregate_return_sources(id flat.NodeId, ancestors []flat.NodeId) []string {
	if !tc.valid_node_id(id) || id in ancestors { return []string{} }
	mut visited := ancestors.clone()
	visited << id
	typ := tc.resolve_type(id)
	if tc.sum_escape_has_pointer_variant(typ) {
		return tc.ownership_sum_borrow_sources(id, []flat.NodeId{})
	}
	node := tc.a.node(id)
	mut sources := []string{}
	if node.kind in [.ident, .selector, .index] {
		name := tc.ownership_expr_ident_name(id)
		if name.len == 0 { return sources }
		for source, borrows in tc.ownership.borrowed_vars {
			for borrow in borrows {
				if !ownership_storage_key_is_descendant(borrow.borrower, name) { continue }
				projected := tc.returned_receiver_projection_type(typ, borrow.borrower[name.len..]) or { continue }
				if !tc.sum_escape_has_pointer_variant(projected) { continue }
				resolved := tc.ownership_sum_borrow_source_name(source)
				if resolved.len > 0 && resolved !in sources { sources << resolved }
			}
		}
		return sources
	}
	if node.kind in [.paren, .expr_stmt, .cast_expr, .as_expr, .field_init] && node.children_count > 0 {
		return tc.ownership_sum_aggregate_return_sources(tc.a.child(node, 0), visited)
	}
	if node.kind in [.block, .match_branch] && node.children_count > 0 {
		return tc.ownership_sum_aggregate_return_sources(tc.a.child(node, node.children_count - 1), visited)
	}
	if node.kind in [.struct_init, .assoc, .array_literal, .array_init, .map_init, .if_expr,
		.match_stmt, .or_expr] {
		start := if node.kind in [.if_expr, .match_stmt] { 1 } else { 0 }
		for i in start .. node.children_count {
			for source in tc.ownership_sum_aggregate_return_sources(tc.a.child(node, i), visited) {
				if source !in sources { sources << source }
			}
		}
	}
	return sources
}

// A reference to an aggregate keeps the aggregate's lifetime, rather than the
// lifetime of the temporary reference binding used to reach its storage.
fn (mut tc TypeChecker) ownership_sum_aggregate_scope(storage string) int {
	mut root := storage.all_before('.').all_before('[')
	mut seen := map[string]bool{}
	for root.len > 0 && root != '<unknown-index-alias>' && !seen[root] {
		seen[root] = true
		if source := tc.ownership.pointer_index_aliases[root] {
			root = source.all_before('.').all_before('[')
		} else if source := tc.ownership_borrowed_alias_source(root, false) {
			root = source.all_before('.').all_before('[')
		} else {
			if typ := tc.cur_scope.lookup(root) {
				if unalias_type(typ) is Pointer && root !in tc.ownership.owned_vars { return -1 }
			}
			return tc.ownership_sum_local_scope(root)
		}
	}
	return -1
}

fn (mut tc TypeChecker) ownership_reject_sum_aggregate_store(target string, rhs flat.NodeId, typ Type, pos flat.NodeId) bool {
	pointer_sum := tc.sum_escape_has_pointer_variant(typ)
	clean := unalias_type(typ)
	if pointer_sum {
		if !target.contains('.') && !target.contains('[') { return false }
	} else if clean !is Struct && clean !is Array && clean !is ArrayFixed && clean !is Map {
		return false
	}
	target_scope := tc.ownership_sum_aggregate_scope(target)
	target_root := target.all_before('.').all_before('[')
	if target_scope < 0 && tc.cur_scope.lookup_owner(target_root) == none {
		mut global_storage := false
		for name, _ in tc.ownership.owned_globals.ids {
			if ownership_storage_keys_overlap(name, target) {
				global_storage = true
				break
			}
		}
		// Internal expression temporaries do not establish a longer-lived store.
		if !global_storage { return false }
	}
	mut shorter_lifetime := tc.ownership_sum_return_branch_has_local(rhs, map[string][]SumReturnLocal{}, map[string]SumReturnLocal{})
	sources := if pointer_sum {
		tc.ownership_sum_borrow_sources(rhs, []flat.NodeId{})
	} else {
		tc.ownership_sum_aggregate_return_sources(rhs, []flat.NodeId{})
	}
	for source in sources {
		root := source.all_before('.').all_before('[')
		source_scope := tc.ownership_sum_local_scope(root)
		if source_scope >= 0 && source_scope > target_scope { shorter_lifetime = true }
	}
	if !shorter_lifetime { return false }
	tc.record_error(.assignment_mismatch, 'cannot retain a sum pointer to a local with a shorter lifetime in aggregate storage; retain it in an outer sum variable first', pos)
	return true
}

fn (mut tc TypeChecker) ownership_reject_sum_aggregate_return(return_id flat.NodeId, expr flat.NodeId) bool {
	transfers := tc.ownership_sum_return_sources(tc.ownership.cur_fn, return_id)
	for source in tc.ownership_sum_aggregate_return_sources(expr, []flat.NodeId{}) {
		root := source.all_before('.').all_before('[')
		mut transferred := false
		for entry in transfers {
			if entry.name == root {
				transferred = true
				break
			}
		}
		if transferred { continue }
		owner := tc.cur_scope.lookup_owner(root) or { continue }
		if tc.binding_owner_is_global(owner) { continue }
		typ := unalias_type(owner.scope.types[owner.index])
		if typ is Pointer && root !in tc.ownership.owned_vars { continue }
		tc.record_error(.return_mismatch, 'cannot return a sum field or element borrowing local storage; return a separately owned sum instead', expr)
		return true
	}
	return false
}

fn (mut tc TypeChecker) ownership_transfer_inner_sum_local(target string, rhs flat.NodeId, target_type Type, pos flat.NodeId) bool {
	if !tc.sum_escape_has_pointer_variant(target_type) { return false }
	mut source := tc.ownership_pointer_alias_target(rhs) or {
		return tc.ownership_reject_ambiguous_sum_escape(target, pos)
	}
	mut visited := map[string]bool{}
	for {
		if source == '<unknown-index-alias>' || visited[source] {
			return tc.ownership_reject_ambiguous_sum_escape(target, pos)
		}
		visited[source] = true
		if next := tc.ownership.pointer_index_aliases[source] {
			source = next
		} else {
			break
		}
	}
	if source.contains('.') || source.contains('[') { return false }
	source_type := tc.cur_scope.lookup(source) or { return false }
	if unalias_type(source_type) is Pointer { return false }
	source_scope := tc.ownership_sum_local_scope(source)
	target_scope := tc.ownership_sum_local_scope(target)
	if target_scope < 0 || source_scope <= target_scope { return false }
	mut st := tc.ownership_state()
	owner := tc.ownership_borrowed_alias_source(source, false) or { source }
	if owner == source {
		st.owned_vars.delete(source)
		st.owned_var_types.delete(source)
		st.moved_vars.delete(source)
		tc.ownership_clear_descendant_state(source)
		tc.ownership_mark_owned(target, target_type, pos)
		st.sum_value_transfer_sources[int(rhs)] = source
		tc.ownership_add_borrow(target, source, pos, false)
	} else {
		// Another outer sum already keeps this allocation alive. A second
		// wrapper is a loan from that owner, rather than a second destructor.
		st.owned_vars.delete(target)
		st.owned_var_types.delete(target)
		tc.ownership_add_borrow(owner, target, pos, false)
	}
	return true
}

fn (mut tc TypeChecker) ownership_reject_ambiguous_sum_escape(target string, pos flat.NodeId) bool {
	st := tc.ownership_state()
	key := '${st.cur_fn}\x01${int(pos)}'
	target_scope := tc.ownership_sum_local_scope(target)
	for source in st.sum_outer_store_local_sources[key] or { []OwnershipDropEntry{} } {
		if target_scope >= 0 && tc.ownership_sum_local_scope(source.name) > target_scope {
			tc.record_error(.assignment_mismatch, 'cannot retain a sum with an ambiguous local pointer source outside its scope; store each pointer alternative separately', pos)
			return true
		}
	}
	return false
}
