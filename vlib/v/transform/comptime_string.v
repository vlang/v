module transform

import strings
import v.flat
import v.util

// comptime_scalar_expr evaluates only literal operands and immutable lexical bindings.
// Reading a constant follows its initializer without changing the AST or executing user code.
fn (t &Transformer) comptime_scalar_expr(id flat.NodeId, depth int) ?ComptimeStringScalar {
	return t.comptime_scalar_expr_in_context(id, depth, t.cur_module, t.cur_file, true)
}

fn (t &Transformer) comptime_scalar_expr_in_context(id flat.NodeId, depth int, module_name string, file string, allow_locals bool) ?ComptimeStringScalar {
	if depth > 64 || int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.string_literal {
			if node.children_count != 0 {
				return none
			}
			return ComptimeStringScalar{'string', node.value}
		}
		.int_literal, .bool_literal {
			return ComptimeStringScalar{if node.kind == .bool_literal { 'bool' } else { 'int' }, node.value}
		}
		.ident {
			if allow_locals {
				if value := t.comptime_scalar_locals[node.value] {
					return value
				}
			}
			if allow_locals && t.var_type(node.value).len > 0 {
				return none
			}
			return t.comptime_scalar_named_const(node.value, depth, module_name, file)
		}
		.paren {
			if node.children_count == 1 {
				return t.comptime_scalar_expr_in_context(t.a.child(&node, 0), depth + 1, module_name, file, allow_locals)
			}
		}
		.selector {
			if node.value == 'len' && node.children_count == 1 {
				base := t.comptime_scalar_expr_in_context(t.a.child(&node, 0), depth + 1, module_name, file, allow_locals) or { return none }
				if base.typ == 'string' {
					return ComptimeStringScalar{'int', base.value.len.str()}
				}
			}
			if node.children_count == 1 {
				base := t.a.child_node(&node, 0)
				if base.kind == .ident && (!allow_locals || t.var_type(base.value).len == 0) {
					return t.comptime_scalar_named_const('${base.value}.${node.value}', depth, module_name, file)
				}
			}
		}
		.infix {
			if node.children_count != 2 { return none }
			left := t.comptime_scalar_expr_in_context(t.a.child(&node, 0), depth + 1, module_name, file, allow_locals)?
			right := t.comptime_scalar_expr_in_context(t.a.child(&node, 1), depth + 1, module_name, file, allow_locals)?
			if left.typ != right.typ { return none }
			if left.typ == 'string' && node.op == .plus {
				return ComptimeStringScalar{'string', left.value + right.value}
			}
			if node.op in [.eq, .ne] {
				equal := if left.typ == 'int' {
					l := util.comptime_string_bound(left.value)?
					r := util.comptime_string_bound(right.value)?
					l == r
				} else {
					left.value == right.value
				}
				return ComptimeStringScalar{'bool', (if node.op == .eq { equal } else { !equal }).str()}
			}
			if left.typ == 'bool' && node.op in [.logical_and, .logical_or] {
				value := if node.op == .logical_and {
					left.value == 'true' && right.value == 'true'
				} else {
					left.value == 'true' || right.value == 'true'
				}
				return ComptimeStringScalar{'bool', value.str()}
			}
		}
		.prefix {
			if node.op == .not && node.children_count == 1 {
				value := t.comptime_scalar_expr_in_context(t.a.child(&node, 0), depth + 1, module_name, file, allow_locals)?
				if value.typ == 'bool' {
					return ComptimeStringScalar{'bool', (value.value == 'false').str()}
				}
			}
		}
		.in_expr {
			if node.children_count != 2 { return none }
			needle := t.comptime_scalar_expr_in_context(t.a.child(&node, 0), depth + 1, module_name, file, allow_locals)?
			if needle.typ != 'string' { return none }
			right := t.a.child_node(&node, 1)
			if right.kind == .array_literal {
				mut found := false
				for child in t.a.children_of(right) {
					value := t.comptime_scalar_expr_in_context(child, depth + 1, module_name, file, allow_locals)?
					if value.typ != 'string' { return none }
					found = found || value.value == needle.value
				}
				return ComptimeStringScalar{'bool', found.str()}
			}
			value := t.comptime_scalar_expr_in_context(t.a.child(&node, 1), depth + 1, module_name, file, allow_locals)?
			if value.typ == 'string' {
				return ComptimeStringScalar{'bool', value.value.contains(needle.value).str()}
			}
		}
		.call {
			if node.children_count == 0 {
				return none
			}
			callee := t.a.child_node(&node, 0)
			if callee.kind != .selector || callee.children_count != 1 {
				return none
			}
			receiver := t.comptime_scalar_expr_in_context(t.a.child(callee, 0), depth + 1, module_name, file, allow_locals) or { return none }
			if receiver.typ != 'string' {
				return none
			}
			mut args := []string{}
			for i in 1 .. node.children_count {
				arg := t.comptime_scalar_expr_in_context(t.a.child(&node, i), depth + 1, module_name, file, allow_locals) or { return none }
				if arg.typ != 'string' {
					return none
				}
				args << arg.value
			}
			return comptime_string_scalar(receiver.value, callee.value, args)
		}
		.index {
			if node.value != 'range' || node.children_count < 2 {
				return none
			}
			base := t.comptime_scalar_expr_in_context(t.a.child(&node, 0), depth + 1, module_name, file, allow_locals) or { return none }
			low := t.comptime_scalar_expr_in_context(t.a.child(&node, 1), depth + 1, module_name, file, allow_locals) or { return none }
			if base.typ != 'string' || low.typ != 'int' {
				return none
			}
			mut high := base.value.len
			if node.children_count == 3 {
				bound := t.comptime_scalar_expr_in_context(t.a.child(&node, 2), depth + 1, module_name, file, allow_locals) or { return none }
				if bound.typ != 'int' {
					return none
				}
				high = util.comptime_string_bound(bound.value) or { return none }
			}
			start := util.comptime_string_bound(low.value) or { return none }
			if start < 0 || high < start || high > base.value.len {
				return none
			}
			return ComptimeStringScalar{'string', base.value[start..high]}
		}
		else {}
	}
	return none
}

// Constant ownership comes from its declaration, even when its initializer has no context cache entry.
fn (t &Transformer) comptime_scalar_named_const(name string, depth int, module_name string, file string) ?ComptimeStringScalar {
	if isnil(t.tc) { return none }
	key := t.const_type_key_in_context(name, module_name, file)?
	owner := t.tc.const_modules[key] or { module_name }
	base := name.all_before('.')
	global_name := if module_name !in ['', 'main', 'builtin'] {
		'${module_name}.${base}'
	} else {
		base
	}
	// Explicit imports are namespaces; other selectors can refer to owner globals.
	imported := name.contains('.') && t.file_import_module(file, base) != none
	same_owner := owner == module_name || (owner in ['', 'main'] && module_name in ['', 'main'])
	if !imported && global_name in t.globals
		&& (name.contains('.') || !same_owner) {
		return none
	}
	expr := t.tc.const_exprs[key] or { return none }
	owner_file := t.tc.const_files[key] or { file }
	return t.comptime_scalar_expr_in_context(expr, depth + 1, owner, owner_file, false)
}

fn (mut t Transformer) make_comptime_scalar_literal(value ComptimeStringScalar) flat.NodeId {
	return match value.typ {
		'string' { t.make_string_literal(value.value) }
		'bool' { t.make_bool_literal(value.value == 'true') }
		else { t.make_int_literal(value.value.int()) }
	}
}

fn (mut t Transformer) transform_comptime_scalar_decl(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count != 2 {
		return t.transform_decl_assign_stmt(id, node)
	}
	lhs := *t.a.child_node(&node, 0)
	value := t.comptime_scalar_expr(t.a.child(&node, 1), 0)
	result := t.transform_decl_assign_stmt(id, node)
	if lhs.kind == .ident {
		t.comptime_scalar_locals.delete(lhs.value)
		// Generated staging locals can be assigned later despite lacking `mut` flags.
		if node.pos.is_valid() && !lhs.is_mut && !node.is_mut {
			if scalar := value {
				// So can a source local of a generic function, whose body is not checked
				// for assignments to immutable names.
				if !t.later_stmts_assign_local(id, lhs.value) {
					t.comptime_scalar_locals[lhs.value] = scalar
				}
			}
		}
	}
	return result
}

// later_stmts_assign_local reports whether a statement that follows the declaration
// `decl_id` in its statement list assigns to the declared local `name`, increments it, or
// passes it as a mutable argument. Only those statements can see the binding: the same name
// in an earlier statement belongs to another one. Reading it, as on the right of an
// assignment, does not count: a compile-time construct may still need its value.
fn (t &Transformer) later_stmts_assign_local(decl_id flat.NodeId, name string) bool {
	ids := t.cur_stmt_list
	mut first := 0
	for i, id in ids {
		if id == decl_id {
			first = i + 1
			break
		}
	}
	for i in first .. ids.len {
		if t.node_assigns_local(ids[i], name) {
			return true
		}
	}
	return false
}

fn (t &Transformer) node_assigns_local(id flat.NodeId, name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	// A function literal has its own scope. A name declared there is another binding,
	// and a captured scalar is a copy that belongs to the closure.
	if node.kind == .fn_literal {
		return false
	}
	if node.kind == .ident && node.is_mut && node.value == name {
		return true
	}
	if node.kind == .assign {
		for i in 0 .. t.multi_assign_lhs_count(node) {
			if t.node_is_local_ident(t.multi_assign_lhs_id(node, i), name) {
				return true
			}
		}
	} else if node.kind == .postfix && (node.op == .inc || node.op == .dec) {
		if node.children_count > 0 && t.node_is_local_ident(t.a.child(&node, 0), name) {
			return true
		}
	}
	for i in 0 .. node.children_count {
		if t.node_assigns_local(t.a.child(&node, i), name) {
			return true
		}
	}
	return false
}

fn (t &Transformer) node_is_local_ident(id flat.NodeId, name string) bool {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return false
	}
	node := t.a.nodes[int(id)]
	return node.kind == .ident && node.value == name
}

// subst_comptime_scalar_locals substitutes bare value names, leaving members, quotes and type tests alone.
fn (t &Transformer) subst_comptime_scalar_locals(cond string) string {
	if cond.contains(' is ') || cond.contains(' !is ') {
		return cond
	}
	mut out := strings.new_builder(cond.len)
	mut i := 0
	for i < cond.len {
		if cond[i] in [`'`, `"`, `\``] {
			end := comptime_cond_skip_string(cond, i)
			out.write_string(cond[i..end])
			i = end
			continue
		}
		if comptime_cond_name_char(cond[i]) && !cond[i].is_digit() {
			start := i
			for i < cond.len && comptime_cond_name_char(cond[i]) {
				i++
			}
			name := cond[start..i]
			mut prev := start
			for prev > 0 && cond[prev - 1].is_space() {
				prev--
			}
			if prev == 0 || cond[prev - 1] !in [`.`, `$`] {
				if value := t.comptime_scalar_locals[name] {
					out.write_string(if value.typ == 'string' {
						comptime_cond_string_literal(value.value)
					} else {
						value.value
					})
					continue
				}
				if t.var_type(name).len == 0 {
					mut candidate_end := i
					for candidate_end + 1 < cond.len && cond[candidate_end] == `.`
						&& comptime_cond_name_char(cond[candidate_end + 1]) {
						candidate_end++
						for candidate_end < cond.len && comptime_cond_name_char(cond[candidate_end]) {
							candidate_end++
						}
					}
					mut constant_name := cond[start..candidate_end]
					mut constant_end := candidate_end
					mut replaced_constant := false
					for constant_name.contains('.') {
						if value := t.comptime_scalar_named_const(constant_name, 0, t.cur_module, t.cur_file) {
							out.write_string(if value.typ == 'string' {
								comptime_cond_string_literal(value.value)
							} else {
								value.value
							})
							i = constant_end
							replaced_constant = true
							break
						}
						constant_name = constant_name.all_before_last('.')
						constant_end = start + constant_name.len
					}
					if replaced_constant {
						continue
					}
					if value := t.comptime_scalar_named_const(name, 0, t.cur_module, t.cur_file) {
						out.write_string(if value.typ == 'string' {
							comptime_cond_string_literal(value.value)
						} else {
							value.value
						})
						continue
					}
				}
			}
			out.write_string(name)
			continue
		}
		out.write_u8(cond[i])
		i++
	}
	return out.str()
}

// comptime_condition_needs_local_value delays reflection condition evaluation until
// earlier declarations in its block have been transformed, instead of comparing their names.
fn (t &Transformer) comptime_condition_needs_local_value(cond string, node flat.Node) bool {
	for name in comptime_condition_bare_names(cond) {
		decls := t.local_decl_nodes_by_name[name] or { continue }
		for decl_id in decls {
			decl := t.a.nodes[decl_id]
			if decl.pos.id == node.pos.id && decl.pos.offset < node.pos.offset {
				return true
			}
		}
	}
	return false
}

fn comptime_condition_bare_names(cond string) []string {
	mut names := []string{}
	mut i := 0
	for i < cond.len {
		if cond[i] in [`\'`, `"`, `\``] {
			i = comptime_cond_skip_string(cond, i)
			continue
		}
		if comptime_cond_name_char(cond[i]) && !cond[i].is_digit() {
			start := i
			for i < cond.len && comptime_cond_name_char(cond[i]) {
				i++
			}
			mut prev := start
			for prev > 0 && cond[prev - 1].is_space() {
				prev--
			}
			if prev == 0 || cond[prev - 1] !in [`.`, `$`] {
				names << cond[start..i]
			}
			continue
		}
		i++
	}
	return names
}

fn (mut t Transformer) comptime_scalar_condition_value(cond string) ?bool {
	for name in comptime_condition_bare_names(cond) {
		if name !in ['true', 'false', 'in'] {
			return t.comptime_type_condition_value(cond)
		}
	}
	if value := t.eval_field_cond(cond) {
		return value
	}
	return t.comptime_type_condition_value(cond)
}

fn (t &Transformer) comptime_string_source(id flat.NodeId) ?[]string {
	if int(id) < 0 || int(id) >= t.a.nodes.len {
		return none
	}
	node := t.a.nodes[int(id)]
	if node.kind != .call || node.children_count == 0 {
		return none
	}
	callee := t.a.child_node(&node, 0)
	if callee.kind != .selector || callee.children_count != 1 {
		return none
	}
	receiver := t.comptime_scalar_expr(t.a.child(callee, 0), 0) or { return none }
	if receiver.typ != 'string' {
		return none
	}
	if callee.value == 'fields' && node.children_count == 1 {
		return receiver.value.fields()
	}
	if callee.value !in ['split', 'split_any'] || node.children_count != 2 {
		return none
	}
	arg := t.comptime_scalar_expr(t.a.child(&node, 1), 0) or { return none }
	if arg.typ != 'string' {
		return none
	}
	return if callee.value == 'split' {
		receiver.value.split(arg.value)
	} else {
		receiver.value.split_any(arg.value)
	}
}

fn (mut t Transformer) expand_comptime_for_strings(id flat.NodeId, node flat.Node, var_name string) []flat.NodeId {
	values := t.comptime_string_source(t.a.child(&node, 1)) or {
		if !t.cur_fn_is_generic {
			t.tc.record_transform_error(id, node.pos, '`\$for` string source must use `split`, `split_any` or `fields` on a compile-time-known string')
		}
		return [id]
	}
	t.ignore_comptime_for_subtree(id)
	body := *t.a.child_node(&node, 0)
	mut result := []flat.NodeId{}
	for value in values {
		// The literal binding and the entire iteration have their own lexical scope.
		binding := t.make_decl_assign_typed(var_name, t.make_string_literal(value), 'string')
		// This generated binding is immutable and belongs to the source iteration.
		t.a.nodes[int(binding)].pos = node.pos
		mut stmts := [binding]
		for stmt in t.a.children_of(&body) {
			stmts << t.clone_node_tree(stmt)
		}
		result << t.make_block(t.transform_scope_stmts(stmts))
	}
	return result
}

fn (mut t Transformer) clone_node_tree(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	mut children := []flat.NodeId{}
	for child in t.a.children_of(&node) {
		children << t.clone_node_tree(child)
	}
	start := t.a.children.len
	for child in children {
		t.a.children << child
	}
	return t.a.add_node(flat.Node{ ...node, children_start: start })
}

fn (mut t Transformer) eval_reflected_string_condition(cond string, node flat.Node) ?bool {
	if t.comptime_condition_needs_local_value(cond, node) {
		return none
	}
	return t.eval_field_cond(cond)
}

// A type guard can stay unresolved until a generic template is specialized.
// Concrete type tests do not hide invalid string operands in the same condition.
fn (mut t Transformer) comptime_condition_has_unresolved_type_test(cond string) bool {
	if 'is' !in comptime_condition_bare_names(cond) { return false }
	clean := comptime_condition_strip_outer_parens(cond.trim_space())
	for op in ['||', '&&'] {
		index := comptime_condition_top_level_index(clean, op)
		if index >= 0 {
			return t.comptime_condition_has_unresolved_type_test(clean[..index])
				|| t.comptime_condition_has_unresolved_type_test(clean[index + op.len..])
		}
	}
	for op in [' !is ', ' is '] {
		if comptime_condition_top_level_index(clean, op) >= 0 {
			return t.comptime_type_condition_value(clean) == none
		}
	}
	if clean.starts_with('!') {
		return t.comptime_condition_has_unresolved_type_test(clean[1..])
	}
	return false
}
