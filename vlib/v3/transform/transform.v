module transform

import v3.flat
import v3.types

fn arr1(a flat.NodeId) []flat.NodeId {
	mut r := []flat.NodeId{}
	r << a
	return r
}

fn arr2(a flat.NodeId, b flat.NodeId) []flat.NodeId {
	mut r := []flat.NodeId{}
	r << a
	r << b
	return r
}

fn arr3(a flat.NodeId, b flat.NodeId, c flat.NodeId) []flat.NodeId {
	mut r := []flat.NodeId{}
	r << a
	r << b
	r << c
	return r
}

pub struct SmartcastContext {
pub:
	expr_name     string // the expression being smartcast (e.g. "node")
	variant_name  string // the variant type name (e.g. "Ident")
	sum_type_name string // the parent sum type name (e.g. "Expr")
}

pub struct Transformer {
mut:
	a               &flat.FlatAst      = unsafe { nil }
	tc              &types.TypeChecker = unsafe { nil }
	structs         map[string]StructInfo
	globals         map[string]string
	sum_types       map[string][]string
	fn_ret_types    map[string]string
	enum_types      map[string][]string
	cur_file        string
	cur_module      string
	cur_fn_name     string
	cur_fn_ret_type string
	var_types       []VarTypeBinding
	temp_counter    int
	pending_stmts   []flat.NodeId
	smartcast_stack []SmartcastContext
}

pub struct StructInfo {
pub:
	name   string
	module string
	fields []FieldInfo
}

pub struct FieldInfo {
pub:
	name         string
	typ          string
	default_expr flat.NodeId
}

struct VarTypeBinding {
	name string
	typ  string
}

// --- entry point ---

pub fn transform(mut a flat.FlatAst, tc &types.TypeChecker) {
	mut t := Transformer{
		a:  a
		tc: unsafe { tc }
	}
	t.collect_types()
	t.transform_all()
}

fn (mut t Transformer) reset_var_types() {
	t.var_types.clear()
}

fn (mut t Transformer) set_var_type(name string, typ string) {
	if name.len == 0 {
		return
	}
	for i, binding in t.var_types {
		if binding.name == name {
			t.var_types[i] = VarTypeBinding{
				name: name
				typ:  typ
			}
			return
		}
	}
	t.var_types << VarTypeBinding{
		name: name
		typ:  typ
	}
}

fn (t &Transformer) var_type(name string) string {
	for binding in t.var_types {
		if binding.name == name {
			return binding.typ
		}
	}
	return ''
}

// --- type collection ---

fn (mut t Transformer) collect_types() {
	mut cur_mod := ''
	for node in t.a.nodes {
		match node.kind {
			.module_decl {
				cur_mod = node.value
			}
			.struct_decl {
				owner_type := if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
					'${cur_mod}.${node.value}'
				} else {
					node.value
				}
				mut fields := []FieldInfo{}
				for i in 0 .. node.children_count {
					f := t.a.child_node(&node, i)
					if f.kind != .field_decl {
						continue
					}
					default_expr := if f.children_count > 0 {
						t.a.child(f, 0)
					} else {
						flat.empty_node
					}
					fields << FieldInfo{
						name:         f.value
						typ:          t.normalize_field_type(f.typ, owner_type)
						default_expr: default_expr
					}
				}
				info := StructInfo{
					name:   node.value
					module: cur_mod
					fields: fields
				}
				t.structs[node.value] = info
				if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
					t.structs['${cur_mod}.${node.value}'] = info
				}
			}
			.type_decl {
				if node.children_count > 0 {
					mut variants := []string{}
					for i in 0 .. node.children_count {
						v := t.a.child_node(&node, i)
						variants << v.value
					}
					t.sum_types[node.value] = variants
					if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
						qname := '${cur_mod}.${node.value}'
						t.sum_types[qname] = variants
					}
				}
			}
			.enum_decl {
				mut field_names := []string{}
				for i in 0 .. node.children_count {
					f := t.a.child_node(&node, i)
					if f.kind == .enum_field {
						field_names << f.value
					}
				}
				t.enum_types[node.value] = field_names
				if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
					t.enum_types['${cur_mod}.${node.value}'] = field_names
				}
			}
			.global_decl {
				for i in 0 .. node.children_count {
					f := t.a.child_node(&node, i)
					t.globals[f.value] = f.typ
				}
			}
			.fn_decl {
				if node.typ.len > 0 {
					t.fn_ret_types[node.value] = node.typ
					if cur_mod.len > 0 && cur_mod != 'main' && cur_mod != 'builtin' {
						t.fn_ret_types['${cur_mod}.${node.value}'] = node.typ
					}
				}
			}
			.c_fn_decl {
				if node.typ.len > 0 {
					t.fn_ret_types[node.value] = node.typ
					if node.value.starts_with('C.') {
						t.fn_ret_types[node.value[2..]] = node.typ
					} else {
						t.fn_ret_types['C.${node.value}'] = node.typ
					}
				}
			}
			else {}
		}
	}
}

// --- main transform pass ---

fn (mut t Transformer) transform_all() {
	for i in 0 .. t.a.nodes.len {
		node := t.a.nodes[i]
		if node.kind == .file {
			t.cur_file = node.value
		}
		if node.kind == .module_decl {
			t.cur_module = node.value
		}
		if node.kind == .fn_decl {
			t.transform_fn_body(i)
		} else if node.kind == .const_decl {
			t.transform_const_decl(node)
		}
	}
}

// transform_const_decl transforms the initializer expression of each const field
// so that const-level lowering (e.g. string concatenation in the prelude's
// embedded data tables) happens in the transformer rather than the backend.
fn (mut t Transformer) transform_const_decl(node flat.Node) {
	for ci in 0 .. node.children_count {
		cf_id := t.a.child(&node, ci)
		if int(cf_id) < 0 {
			continue
		}
		cf := t.a.nodes[int(cf_id)]
		if cf.kind == .const_field && cf.children_count >= 1 {
			val_id := t.a.child(&cf, 0)
			val := t.a.nodes[int(val_id)]
			if val.kind == .infix && val.children_count >= 2 {
				new_val := t.transform_expr(val_id)
				// Overwrite the field's value slot in place (each const_field owns
				// its own single-element child range, so this is safe).
				t.a.children[cf.children_start] = new_val
			}
		}
	}
}

fn (mut t Transformer) transform_fn_body(fn_idx int) {
	fn_node := t.a.nodes[fn_idx]
	t.cur_fn_name = fn_node.value
	t.cur_fn_ret_type = t.normalize_type_alias(fn_node.typ)
	t.reset_var_types()
	t.smartcast_stack.clear()
	// Collect param types
	for i in 0 .. fn_node.children_count {
		child_id := t.a.children[fn_node.children_start + i]
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if child.kind == .param && child.value.len > 0 && child.typ.len > 0 {
			t.set_var_type(child.value, t.normalize_type_alias(child.typ))
		}
	}
	// Transform non-param children directly, avoiding a temporary body-id list
	// for every function.
	mut new_body := []flat.NodeId{cap: int(fn_node.children_count)}
	for i in 0 .. fn_node.children_count {
		child_id := t.a.children[fn_node.children_start + i]
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if child.kind != .param {
			expanded := t.transform_stmt(child_id)
			t.drain_pending(mut new_body)
			for eid in expanded {
				new_body << eid
			}
		}
	}
	t.drain_pending(mut new_body)
	// Rebuild function children: params then new body
	start := t.a.children.len
	for i in 0 .. fn_node.children_count {
		child_id := t.a.children[fn_node.children_start + i]
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if child.kind == .param {
			t.a.children << child_id
		}
	}
	for id in new_body {
		t.a.children << id
	}
	count := t.a.children.len - start
	t.a.nodes[fn_idx] = flat.Node{
		kind:           .fn_decl
		op:             fn_node.op
		children_start: start
		children_count: flat.child_count(count)
		pos:            fn_node.pos
		value:          fn_node.value
		typ:            fn_node.typ
	}
	t.smartcast_stack.clear()
}

// --- statement list driver ---

pub fn (mut t Transformer) transform_stmts(ids []flat.NodeId) []flat.NodeId {
	mut result := []flat.NodeId{cap: ids.len}
	for id in ids {
		expanded := t.transform_stmt(id)
		t.drain_pending(mut result)
		for eid in expanded {
			result << eid
		}
	}
	t.drain_pending(mut result)
	return result
}

pub fn (mut t Transformer) transform_stmt(id flat.NodeId) []flat.NodeId {
	if int(id) < 0 {
		return arr1(id)
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.return_stmt {
			return t.transform_return_stmt(id, node)
		}
		.assign, .selector_assign, .index_assign {
			return t.transform_assign_stmt(id, node)
		}
		.decl_assign {
			return t.transform_decl_assign_stmt(id, node)
		}
		.expr_stmt {
			return t.transform_expr_stmt(id, node)
		}
		.for_stmt {
			return t.transform_for_stmt(id, node)
		}
		.for_in_stmt {
			return t.transform_for_in_stmt(id, node)
		}
		.block {
			return t.transform_block_stmt(id, node)
		}
		.if_expr {
			return t.transform_if_stmt(id, node)
		}
		.match_stmt {
			return arr1(t.lower_one_match(node))
		}
		.defer_stmt {
			return t.transform_children_stmt(id, node)
		}
		.assert_stmt {
			return t.transform_children_stmt(id, node)
		}
		.select_stmt {
			return t.transform_children_stmt(id, node)
		}
		else {
			return arr1(id)
		}
	}
}

pub fn (mut t Transformer) transform_expr(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.infix {
			return t.transform_infix_expr(id, node)
		}
		.call {
			return t.transform_call_expr(id, node)
		}
		.if_expr {
			return t.transform_if_expr(id, node)
		}
		.struct_init {
			return t.transform_struct_init(id, node)
		}
		.index {
			return t.transform_index_expr(id, node)
		}
		.string_interp {
			return t.transform_string_interp(id, node)
		}
		.selector {
			return t.transform_selector_expr(id, node)
		}
		.or_expr {
			return t.transform_or_expr(id, node)
		}
		.as_expr {
			return t.transform_as_expr(id, node)
		}
		.prefix {
			return t.transform_prefix_expr(id, node)
		}
		.paren {
			return t.transform_paren_expr(id, node)
		}
		.postfix {
			return t.transform_postfix_expr(id, node)
		}
		.cast_expr {
			return t.transform_cast_expr(id, node)
		}
		.array_literal {
			return t.transform_array_literal(id, node)
		}
		.array_init {
			return t.transform_array_init_expr(id, node)
		}
		.map_init {
			return t.transform_map_init(id, node)
		}
		.in_expr {
			return t.transform_in_expr(id, node)
		}
		.is_expr {
			return t.transform_is_expr(id, node)
		}
		.match_stmt {
			return t.lower_one_match(node)
		}
		.typeof_expr {
			return t.transform_typeof_expr(id, node)
		}
		.ident {
			return t.transform_ident_expr(id, node)
		}
		.assoc {
			return t.transform_assoc_expr(id, node)
		}
		.fn_literal, .lambda_expr, .spawn_expr, .lock_expr, .dump_expr, .range, .select_stmt,
		.select_branch {
			return t.transform_children_expr(id, node)
		}
		.int_literal, .float_literal, .bool_literal, .char_literal, .string_literal, .nil_literal,
		.none_expr, .enum_val, .sizeof_expr, .offsetof_expr {
			// leaf/simple nodes - pass through unchanged
			return id
		}
		else {
			return id
		}
	}
}

pub fn (mut t Transformer) transform_lvalue(id flat.NodeId) flat.NodeId {
	if int(id) < 0 {
		return id
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return id
		}
		.selector {
			if node.children_count == 0 {
				return id
			}
			base := t.transform_lvalue(t.a.child(&node, 0))
			mut new_children := []flat.NodeId{cap: int(node.children_count)}
			new_children << base
			for i in 1 .. node.children_count {
				new_children << t.transform_expr(t.a.child(&node, i))
			}
			start := t.a.children.len
			for child in new_children {
				t.a.children << child
			}
			return t.a.add_node(flat.Node{
				kind:           .selector
				op:             node.op
				children_start: start
				children_count: flat.child_count(new_children.len)
				pos:            node.pos
				value:          node.value
				typ:            node.typ
			})
		}
		.index {
			if node.children_count == 0 {
				return id
			}
			mut new_children := []flat.NodeId{cap: int(node.children_count)}
			new_children << t.transform_expr(t.a.child(&node, 0))
			for i in 1 .. node.children_count {
				new_children << t.transform_expr(t.a.child(&node, i))
			}
			start := t.a.children.len
			for child in new_children {
				t.a.children << child
			}
			return t.a.add_node(flat.Node{
				kind:           .index
				op:             node.op
				children_start: start
				children_count: flat.child_count(new_children.len)
				pos:            node.pos
				value:          node.value
				typ:            node.typ
			})
		}
		.prefix {
			if node.op == .mul && node.children_count > 0 {
				child := t.transform_expr(t.a.child(&node, 0))
				start := t.a.children.len
				t.a.children << child
				return t.a.add_node(flat.Node{
					kind:           .prefix
					op:             node.op
					children_start: start
					children_count: 1
					pos:            node.pos
					value:          node.value
					typ:            node.typ
				})
			}
			return t.transform_expr(id)
		}
		.paren {
			if node.children_count == 0 {
				return id
			}
			child := t.transform_lvalue(t.a.child(&node, 0))
			start := t.a.children.len
			t.a.children << child
			return t.a.add_node(flat.Node{
				kind:           .paren
				op:             node.op
				children_start: start
				children_count: 1
				pos:            node.pos
				value:          node.value
				typ:            node.typ
			})
		}
		else {
			return t.transform_expr(id)
		}
	}
}

// --- stmt handlers (skeleton - identity transforms with child recursion) ---

fn (mut t Transformer) transform_return_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	if expanded := t.try_expand_return_if(id, node) {
		return expanded
	}
	if expanded := t.try_expand_return_match(id, node) {
		return expanded
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.wrap_sum_return_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind:           .return_stmt
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	return t.with_pending_before(new_id)
}

fn (mut t Transformer) transform_assign_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	if expanded := t.try_expand_multi_return_assign(node) {
		return expanded
	}
	if lowered := t.try_lower_map_index_assign(node) {
		return lowered
	}
	// string `s += x` on a plain ident -> `s = string__plus(s, x)` (only when detectable as string)
	if expanded := t.try_lower_string_compound_assign(id, node) {
		return expanded
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if i % 2 == 0 {
			new_children << t.transform_lvalue(child_id)
		} else {
			new_children << t.transform_expr(child_id)
		}
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	if node.kind == .assign && node.op == .left_shift_assign {
		t.annotate_left_shift_assign(new_id)
	}
	return t.with_pending_before(new_id)
}

fn (mut t Transformer) try_lower_string_compound_assign(_id flat.NodeId, node flat.Node) ?[]flat.NodeId {
	if node.kind != .assign || node.op != .plus_assign || node.children_count != 2 {
		return none
	}
	lhs_id := t.a.child(&node, 0)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident {
		return none
	}
	rhs_id := t.a.child(&node, 1)
	rhs := t.a.nodes[int(rhs_id)]
	is_string := t.resolve_expr_type(lhs_id) == 'string' || rhs.kind == .string_literal
		|| rhs.kind == .string_interp || t.resolve_expr_type(rhs_id) == 'string'
	if !is_string {
		return none
	}
	new_rhs := t.transform_expr(rhs_id)
	lhs_copy := t.make_ident(lhs.value)
	concat := t.make_call('string__plus', arr2(lhs_copy, new_rhs))
	new_lhs := t.make_ident(lhs.value)
	return arr1(t.make_assign(new_lhs, concat))
}

fn (mut t Transformer) transform_decl_assign_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	mut inferred_typ := ''
	if node.children_count > 2 && !isnil(t.tc) {
		rhs_id := t.a.child(&node, 1)
		if rhs_type := t.tc.expr_type(rhs_id) {
			if rhs_type is types.MultiReturn {
				for j, field_type in rhs_type.types {
					lhs_idx := if j == 0 { 0 } else { j + 1 }
					if lhs_idx >= node.children_count {
						continue
					}
					lhs := t.a.child_node(&node, lhs_idx)
					if lhs.kind == .ident && lhs.value.len > 0 && lhs.value != '_' {
						t.set_var_type(lhs.value, field_type.name())
					}
				}
			}
		}
	}
	if expanded := t.try_expand_multi_return_decl(node) {
		return expanded
	}
	// Track the variable type for the common 2-child case.
	if node.children_count == 2 {
		lhs := t.a.child_node(&node, 0)
		if lhs.kind == .ident && lhs.value.len > 0 {
			mut typ := t.infer_decl_type(node)
			if node.typ.len == 0 {
				rhs_id := t.a.child(&node, 1)
				rhs := t.a.nodes[int(rhs_id)]
				if rhs.kind == .array_literal && is_fixed_array_type(typ) {
					typ = '[]${fixed_array_elem_type(typ)}'
					t.a.nodes[int(rhs_id)].typ = typ
				}
			}
			if typ.len > 0 {
				t.set_var_type(lhs.value, typ)
				inferred_typ = typ
			}
		}
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if i == 0 || (node.children_count > 2 && i > 1) {
			new_children << t.transform_lvalue(child_id)
		} else {
			new_children << t.transform_expr(child_id)
		}
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	new_id := t.a.add_node(flat.Node{
		kind:           .decl_assign
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            if node.typ.len > 0 { node.typ } else { inferred_typ }
	})
	return t.with_pending_before(new_id)
}

fn (mut t Transformer) try_expand_multi_return_decl(node flat.Node) ?[]flat.NodeId {
	if node.kind != .decl_assign || node.children_count < 3 || isnil(t.tc) {
		return none
	}
	rhs_id := t.a.child(&node, 1)
	if rhs_type := t.tc.expr_type(rhs_id) {
		if rhs_type is types.MultiReturn {
			tmp_name := t.new_temp('multi_ret')
			mut result := []flat.NodeId{}
			new_rhs := t.transform_expr(rhs_id)
			t.drain_pending(mut result)
			result << t.make_decl_assign(tmp_name, new_rhs)
			for j, field_type in rhs_type.types {
				lhs_idx := if j == 0 { 0 } else { j + 1 }
				if lhs_idx >= node.children_count {
					continue
				}
				lhs_id := t.a.child(&node, lhs_idx)
				lhs := t.a.nodes[int(lhs_id)]
				if lhs.kind != .ident || lhs.value == '_' {
					continue
				}
				field_name := 'arg${j}'
				field_type_name := field_type.name()
				field := t.make_selector(t.make_ident(tmp_name), field_name, field_type_name)
				result << t.make_decl_assign_typed(lhs.value, field, field_type_name)
			}
			return result
		}
	}
	return none
}

fn (mut t Transformer) try_expand_multi_return_assign(node flat.Node) ?[]flat.NodeId {
	if node.kind != .assign || node.children_count < 3 || isnil(t.tc) {
		return none
	}
	rhs_id := t.a.child(&node, 1)
	if rhs_type := t.tc.expr_type(rhs_id) {
		if rhs_type is types.MultiReturn {
			tmp_name := t.new_temp('multi_ret')
			mut result := []flat.NodeId{}
			new_rhs := t.transform_expr(rhs_id)
			t.drain_pending(mut result)
			result << t.make_decl_assign(tmp_name, new_rhs)
			for j, field_type in rhs_type.types {
				lhs_idx := if j == 0 { 0 } else { j + 1 }
				if lhs_idx >= node.children_count {
					continue
				}
				lhs_id := t.a.child(&node, lhs_idx)
				lhs := t.a.nodes[int(lhs_id)]
				if lhs.kind == .ident && lhs.value == '_' {
					continue
				}
				field_name := 'arg${j}'
				field_type_name := field_type.name()
				field := t.make_selector(t.make_ident(tmp_name), field_name, field_type_name)
				result << t.make_assign(t.transform_lvalue(lhs_id), field)
			}
			return result
		}
	}
	return none
}

fn (mut t Transformer) transform_expr_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	child_id := t.a.children[node.children_start]
	child := t.a.nodes[int(child_id)]
	if child.kind == .or_expr && !t.is_map_index_or_expr(child) {
		_ = t.lower_or_expr_to_temp(child_id, child)
		mut result := []flat.NodeId{}
		t.drain_pending(mut result)
		return result
	}
	if lowered := t.try_lower_map_index_append_stmt(child_id) {
		return lowered
	}
	if lowered := t.try_lower_map_index_postfix_stmt(child_id) {
		return lowered
	}
	if lowered := t.try_lower_array_append_stmt(child_id) {
		return lowered
	}
	if lowered := t.try_lower_flag_enum_stmt(child_id) {
		return arr1(lowered)
	}
	new_child := t.transform_expr(child_id)
	start := t.a.children.len
	t.a.children << new_child
	new_id := t.a.add_node(flat.Node{
		kind:           .expr_stmt
		op:             node.op
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	return t.with_pending_before(new_id)
}

fn (mut t Transformer) transform_for_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	return t.transform_for_body(id, node)
}

fn (mut t Transformer) transform_for_in_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	return t.transform_for_in_body(id, node)
}

fn (mut t Transformer) transform_block_stmt(_id flat.NodeId, node flat.Node) []flat.NodeId {
	mut child_ids := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_ids << t.a.children[node.children_start + i]
	}
	new_children := t.transform_stmts(child_ids)
	new_block := t.make_block(new_children)
	return arr1(new_block)
}

fn (mut t Transformer) transform_if_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if expanded := t.try_expand_if_guard(id, node) {
		return expanded
	}
	new_id := t.transform_if_branches_with_smartcast(id, node)
	return arr1(new_id)
}

// Generic handler: rebuild a node with all children recursively transformed.
fn (mut t Transformer) transform_children_stmt(id flat.NodeId, node flat.Node) []flat.NodeId {
	if node.children_count == 0 {
		return arr1(id)
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if t.is_stmt_kind(child.kind) {
			expanded := t.transform_stmt(child_id)
			for eid in expanded {
				new_children << eid
			}
		} else {
			new_children << t.transform_expr(child_id)
		}
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	count := new_children.len
	new_id := t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: start
		children_count: flat.child_count(count)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
	return arr1(new_id)
}

// --- expr handlers (skeleton - identity transforms with child recursion) ---

fn (mut t Transformer) transform_children_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if int(child_id) < 0 {
			new_children << child_id
			continue
		}
		child := t.a.nodes[int(child_id)]
		if t.is_stmt_kind(child.kind) {
			expanded := t.transform_stmt(child_id)
			if expanded.len == 1 {
				new_children << expanded[0]
			} else {
				new_children << t.make_block(expanded)
			}
		} else {
			new_children << t.transform_expr(child_id)
		}
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: start
		children_count: flat.child_count(new_children.len)
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_infix_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	if node.op == .left_shift {
		lhs_id := t.a.children[node.children_start]
		rhs_id := t.a.children[node.children_start + 1]
		new_rhs := t.transform_expr(rhs_id)
		start := t.a.children.len
		t.a.children << lhs_id
		t.a.children << new_rhs
		new_id := t.a.add_node(flat.Node{
			kind:           .infix
			op:             node.op
			children_start: start
			children_count: 2
			pos:            node.pos
			value:          node.value
			typ:            node.typ
		})
		t.annotate_left_shift(new_id)
		return new_id
	}
	if str_result := t.transform_infix_string_ops(id, node) {
		return str_result
	}
	if struct_result := t.transform_infix_struct_ops(id, node) {
		return struct_result
	}
	lhs_id := t.a.children[node.children_start]
	rhs_id := t.a.children[node.children_start + 1]
	new_lhs := t.transform_expr(lhs_id)
	new_rhs := t.transform_expr(rhs_id)
	start := t.a.children.len
	t.a.children << new_lhs
	t.a.children << new_rhs
	return t.a.add_node(flat.Node{
		kind:           .infix
		op:             node.op
		children_start: start
		children_count: 2
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_call_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if lowered := t.try_lower_builtin_call(id, node) {
		return lowered
	}
	return t.transform_call_args(node)
}

fn (mut t Transformer) transform_if_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if lowered := t.try_expand_if_expr_value(id, node) {
		return lowered
	}
	return t.transform_if_branches_with_smartcast(id, node)
}

fn (mut t Transformer) transform_struct_init(id flat.NodeId, node flat.Node) flat.NodeId {
	return t.transform_struct_fields(id, node)
}

fn (mut t Transformer) transform_index_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	if lowered := t.try_lower_map_index_expr(id, node) {
		return lowered
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           .index
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_string_interp(_id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return t.make_string_literal('')
	}
	tmp_name := t.new_temp('str_intp')
	mut min_cap := 0
	mut parts := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		child := t.a.nodes[int(child_id)]
		if child.kind == .string_literal {
			min_cap += child.value.len
		} else {
			min_cap += 16
		}
		transformed := t.transform_expr(child_id)
		mut typ := t.reliable_stringify_type(child_id)
		if typ.len == 0 {
			typ = t.node_type(transformed)
		}
		if typ.len == 0 {
			typ = t.node_type(child_id)
		}
		if typ.len == 0 {
			typ = 'string'
		}
		parts << t.wrap_string_conversion(transformed, typ)
	}
	if min_cap < 16 {
		min_cap = 16
	}
	decl := t.make_decl_assign(tmp_name, t.make_call_typed('strings.new_builder',
		arr1(t.make_int_literal(min_cap)), 'strings.Builder'))
	t.a.nodes[int(decl)].typ = 'strings.Builder'
	t.set_var_type(tmp_name, 'strings.Builder')
	t.pending_stmts << decl
	for part in parts {
		call := t.make_method_call(t.make_ident(tmp_name), 'write_string', arr1(part))
		t.a.nodes[int(call)].typ = 'void'
		t.pending_stmts << t.make_expr_stmt(call)
	}
	result := t.make_method_call(t.make_ident(tmp_name), 'str', []flat.NodeId{})
	t.a.nodes[int(result)].typ = 'string'
	return result
}

fn (mut t Transformer) transform_selector_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	if fixed_len := t.transform_fixed_array_len(id, node) {
		return fixed_len
	}
	base_id := t.a.child(&node, 0)
	sc_key := t.expr_key(base_id)
	if sc_key.len > 0 {
		if sc := t.find_smartcast(sc_key) {
			base_node := t.a.nodes[int(base_id)]
			new_base := if base_node.kind == .ident {
				t.make_ident(base_node.value)
			} else {
				t.transform_expr(base_id)
			}
			qv := t.qualify_variant(sc.variant_name, sc.sum_type_name)
			field_name := t.sum_field_name(qv)
			use_arrow := t.variant_references_sum(qv, sc.sum_type_name)
			variant_typ := if use_arrow { '&${qv}' } else { qv }
			variant_sel_start := t.a.children.len
			t.a.children << new_base
			variant_sel := t.a.add_node(flat.Node{
				kind:           .selector
				op:             node.op
				value:          field_name
				typ:            variant_typ
				children_start: variant_sel_start
				children_count: 1
			})
			sel_start := t.a.children.len
			t.a.children << variant_sel
			sel_typ := if node.typ.len > 0 { node.typ } else { t.resolve_selector_type(node) }
			return t.a.add_node(flat.Node{
				kind:           .selector
				op:             if use_arrow { flat.Op.arrow } else { flat.Op.dot }
				children_start: sel_start
				children_count: 1
				pos:            node.pos
				value:          node.value
				typ:            sel_typ
			})
		}
	}
	new_base := t.transform_expr(base_id)
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	new_children << new_base
	for i in 1 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	sel_typ := if node.typ.len > 0 { node.typ } else { t.resolve_selector_type(node) }
	return t.a.add_node(flat.Node{
		kind:           .selector
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            sel_typ
	})
}

fn (mut t Transformer) transform_or_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count < 2 {
		return id
	}
	if t.is_map_index_or_expr(node) {
		return t.transform_map_index_or_expr(id, node)
	}
	return t.lower_or_expr_to_temp(id, node)
}

fn (mut t Transformer) transform_prefix_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		mut new_child := t.transform_expr(child_id)
		if node.op == .not {
			child := t.a.nodes[int(new_child)]
			if child.kind == .infix {
				new_child = t.make_paren(new_child)
			}
		}
		new_children << new_child
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           .prefix
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_paren_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	child_id := t.a.child(&node, 0)
	new_child := t.transform_expr(child_id)
	start := t.a.children.len
	t.a.children << new_child
	return t.a.add_node(flat.Node{
		kind:           .paren
		op:             node.op
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_postfix_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	child_id := t.a.child(&node, 0)
	new_child := t.transform_expr(child_id)
	start := t.a.children.len
	t.a.children << new_child
	return t.a.add_node(flat.Node{
		kind:           .postfix
		op:             node.op
		children_start: start
		children_count: 1
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_cast_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           .cast_expr
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_array_literal(id flat.NodeId, node flat.Node) flat.NodeId {
	lowered := t.lower_array_literal_to_runtime(id, node)
	if lowered != id {
		return lowered
	}
	if node.children_count == 0 {
		return id
	}
	mut new_children := []flat.NodeId{cap: int(node.children_count)}
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		new_children << t.transform_expr(child_id)
	}
	start := t.a.children.len
	for nc in new_children {
		t.a.children << nc
	}
	return t.a.add_node(flat.Node{
		kind:           .array_literal
		op:             node.op
		children_start: start
		children_count: node.children_count
		pos:            node.pos
		value:          node.value
		typ:            node.typ
	})
}

fn (mut t Transformer) transform_map_init(id flat.NodeId, node flat.Node) flat.NodeId {
	return t.transform_map_init_expr(id, node)
}

fn (mut t Transformer) transform_typeof_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	if node.children_count == 0 {
		return id
	}
	expr_id := t.a.child(&node, 0)
	expr := t.a.nodes[int(expr_id)]
	if expr.kind == .int_literal {
		return t.make_string_literal('int literal')
	}
	mut typ := t.node_type(expr_id)
	if typ.len == 0 {
		typ = t.reliable_stringify_type(expr_id)
	}
	if typ.len == 0 {
		typ = t.resolve_expr_type(expr_id)
	}
	if typ.len == 0 {
		typ = 'unknown'
	}
	return t.make_string_literal(typ)
}

fn (mut t Transformer) transform_ident_expr(id flat.NodeId, node flat.Node) flat.NodeId {
	match node.value {
		'@VMODROOT' {
			return t.make_string_literal(t.vmod_root())
		}
		else {
			if sc := t.find_smartcast(node.value) {
				qv := t.resolve_variant(sc.sum_type_name, sc.variant_name)
				field := t.sum_field_name(qv)
				base := t.make_ident(node.value)
				use_ptr := t.variant_references_sum(qv, sc.sum_type_name)
				field_typ := if use_ptr { '&${qv}' } else { qv }
				field_sel := t.make_selector_op(base, field, field_typ, .dot)
				if use_ptr {
					return t.make_prefix(.mul, field_sel)
				}
				return field_sel
			}
			return id
		}
	}
}

// --- helper methods ---

pub fn (mut t Transformer) new_temp(prefix string) string {
	name := '__${prefix}_${t.temp_counter}'
	t.temp_counter++
	return name
}

pub fn (mut t Transformer) make_ident(name string) flat.NodeId {
	return t.a.add_val(.ident, name)
}

pub fn (mut t Transformer) make_decl_assign(name string, rhs flat.NodeId) flat.NodeId {
	lhs := t.make_ident(name)
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	return t.a.add_node(flat.Node{
		kind:           .decl_assign
		children_start: start
		children_count: 2
	})
}

pub fn (mut t Transformer) make_expr_stmt(expr flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind:           .expr_stmt
		children_start: start
		children_count: 1
	})
}

pub fn (mut t Transformer) make_assign(lhs flat.NodeId, rhs flat.NodeId) flat.NodeId {
	return t.make_assign_op(lhs, rhs, .assign)
}

pub fn (mut t Transformer) make_assign_op(lhs flat.NodeId, rhs flat.NodeId, op flat.Op) flat.NodeId {
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	return t.a.add_node(flat.Node{
		kind:           .assign
		op:             op
		children_start: start
		children_count: 2
	})
}

pub fn (mut t Transformer) make_block(stmts []flat.NodeId) flat.NodeId {
	start := t.a.children.len
	for id in stmts {
		t.a.children << id
	}
	return t.a.add_node(flat.Node{
		kind:           .block
		children_start: start
		children_count: flat.child_count(stmts.len)
	})
}

pub fn (mut t Transformer) make_infix(op flat.Op, lhs flat.NodeId, rhs flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	return t.a.add_node(flat.Node{
		kind:           .infix
		op:             op
		children_start: start
		children_count: 2
	})
}

pub fn (mut t Transformer) make_prefix(op flat.Op, expr flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind:           .prefix
		op:             op
		children_start: start
		children_count: 1
	})
}

pub fn (mut t Transformer) make_paren(expr flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << expr
	return t.a.add_node(flat.Node{
		kind:           .paren
		children_start: start
		children_count: 1
	})
}

pub fn (mut t Transformer) make_if(cond flat.NodeId, then_block flat.NodeId, else_block flat.NodeId) flat.NodeId {
	start := t.a.children.len
	t.a.children << cond
	t.a.children << then_block
	if int(else_block) >= 0 {
		t.a.children << else_block
		return t.a.add_node(flat.Node{
			kind:           .if_expr
			children_start: start
			children_count: 3
		})
	}
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: start
		children_count: 2
	})
}

pub fn (mut t Transformer) push_smartcast(expr_name string, variant string, sum_type string) {
	t.smartcast_stack << SmartcastContext{
		expr_name:     expr_name
		variant_name:  variant
		sum_type_name: sum_type
	}
}

pub fn (mut t Transformer) pop_smartcast() {
	if t.smartcast_stack.len > 0 {
		t.smartcast_stack.delete_last()
	}
}

pub fn (t &Transformer) find_smartcast(expr_name string) ?SmartcastContext {
	// Search from top of stack (most recent) to bottom
	mut i := t.smartcast_stack.len - 1
	for i >= 0 {
		if t.smartcast_stack[i].expr_name == expr_name {
			return t.smartcast_stack[i]
		}
		i--
	}
	return none
}

fn (t &Transformer) expr_key(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	if node.kind == .ident {
		return node.value
	}
	if node.kind == .selector && node.children_count >= 1 {
		base_id := t.a.child(&node, 0)
		base_key := t.expr_key(base_id)
		if base_key.len > 0 {
			return '${base_key}.${node.value}'
		}
	}
	return ''
}

fn (t &Transformer) qualify_variant(variant string, sum_type_name string) string {
	if variant.contains('.') {
		return variant
	}
	if sum_type_name.contains('.') {
		mod := sum_type_name.all_before_last('.')
		return '${mod}.${variant}'
	}
	return variant
}

fn (t &Transformer) sum_field_name(variant string) string {
	if variant.starts_with('[]') {
		return '_Array_${c_name(variant[2..])}'
	}
	if variant.starts_with('map[') {
		return '_Map_${c_name(variant[4..])}'
	}
	return match variant {
		'int' { '_int' }
		'i8' { '_i8' }
		'i16' { '_i16' }
		'i64' { '_i64' }
		'u8', 'byte' { '_u8' }
		'u16' { '_u16' }
		'u32' { '_u32' }
		'u64' { '_u64' }
		'f32' { '_f32' }
		'f64' { '_f64' }
		'bool' { '_bool' }
		'string' { '_string' }
		else { c_name(variant) }
	}
}

fn (t &Transformer) variant_references_sum(variant string, sum_name string) bool {
	mut visited := map[string]bool{}
	return t.variant_refs_sum_inner(variant, sum_name, mut visited)
}

fn (t &Transformer) variant_refs_sum_inner(variant string, sum_name string, mut visited map[string]bool) bool {
	short_v := if variant.contains('.') { variant.all_after_last('.') } else { variant }
	short_s := if sum_name.contains('.') { sum_name.all_after_last('.') } else { sum_name }
	if short_v == short_s {
		return true
	}
	if variant in visited {
		return false
	}
	visited[variant] = true
	qualified := if sum_name.contains('.') && !variant.contains('.') {
		'${sum_name.all_before_last('.')}.${variant}'
	} else {
		variant
	}
	lookup := if qualified in t.structs {
		qualified
	} else if variant in t.structs {
		variant
	} else {
		short_v
	}
	if lookup in t.structs {
		for f in t.structs[lookup].fields {
			ftyp := f.typ.trim_left('[]&')
			short_f := if ftyp.contains('.') { ftyp.all_after_last('.') } else { ftyp }
			if ftyp == sum_name || short_f == short_s {
				return true
			}
			if ftyp in t.sum_types || short_f in t.sum_types {
				return true
			}
			qftyp := if sum_name.contains('.') && !ftyp.contains('.') {
				'${sum_name.all_before_last('.')}.${ftyp}'
			} else {
				ftyp
			}
			if qftyp in t.sum_types {
				return true
			}
			if ftyp in t.structs || short_f in t.structs || qftyp in t.structs {
				inner_lookup := if ftyp in t.structs {
					ftyp
				} else if short_f in t.structs {
					short_f
				} else {
					qftyp
				}
				if t.variant_refs_sum_inner(inner_lookup, sum_name, mut visited) {
					return true
				}
			}
		}
	}
	return false
}

pub fn (mut t Transformer) drain_pending(mut result []flat.NodeId) {
	for id in t.pending_stmts {
		result << id
	}
	t.pending_stmts.clear()
}

fn (mut t Transformer) with_pending_before(stmt flat.NodeId) []flat.NodeId {
	mut result := []flat.NodeId{}
	t.drain_pending(mut result)
	result << stmt
	return result
}

fn (t &Transformer) is_stmt_kind(kind flat.NodeKind) bool {
	return kind in [.expr_stmt, .assign, .decl_assign, .selector_assign, .index_assign, .return_stmt,
		.block, .for_stmt, .for_in_stmt, .break_stmt, .continue_stmt, .match_stmt, .defer_stmt,
		.assert_stmt, .goto_stmt, .label_stmt, .if_expr, .select_stmt, .select_branch, .asm_stmt]
}

// --- type resolution helpers (will move to types.v later) ---

fn (t &Transformer) infer_decl_type(node &flat.Node) string {
	if node.typ.len > 0 {
		return node.typ
	}
	if node.children_count >= 2 {
		rhs_id := t.a.child(node, 1)
		return t.node_type(rhs_id)
	}
	return ''
}

fn (t &Transformer) resolve_expr_type(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.ident {
			return t.normalize_type_alias(t.var_type(node.value))
		}
		.call {
			ret := t.get_call_return_type(id, node)
			if ret.len > 0 {
				return ret
			}
			return ''
		}
		.array_literal {
			if node.typ.len > 0 {
				return node.typ
			}
			if node.children_count > 0 {
				elem_type := t.node_type(t.a.child(&node, 0))
				if elem_type.len > 0 {
					return '[]${elem_type}'
				}
			}
			return '[]int'
		}
		.array_init {
			if node.typ.len > 0 {
				return node.typ
			}
			if is_fixed_array_type(node.value) {
				return node.value
			}
			if node.value.len > 0 {
				return '[]${node.value}'
			}
			return '[]int'
		}
		.map_init {
			return node.value
		}
		.selector {
			return t.resolve_selector_type(node)
		}
		.string_literal, .string_interp {
			return 'string'
		}
		.int_literal {
			return 'int'
		}
		.infix {
			if node.children_count >= 2 {
				if node.op in [.eq, .ne, .lt, .gt, .le, .ge, .logical_and, .logical_or] {
					return 'bool'
				}
				lhs_type := t.resolve_expr_type(t.a.child(&node, 0))
				if node.op == .plus && lhs_type == 'string' {
					return 'string'
				}
				rhs_type := t.resolve_expr_type(t.a.child(&node, 1))
				if node.op == .plus && rhs_type == 'string' {
					return 'string'
				}
			}
			return ''
		}
		.or_expr {
			if node.children_count > 0 {
				inner_type := t.resolve_expr_type(t.a.child(&node, 0))
				if inner_type.starts_with('!') {
					return inner_type[1..]
				}
				if inner_type.starts_with('?') {
					return inner_type[1..]
				}
				return inner_type
			}
			return ''
		}
		.match_stmt {
			return t.match_expr_type(node)
		}
		else {
			return ''
		}
	}
}

fn (t &Transformer) match_expr_type(node flat.Node) string {
	if node.kind != .match_stmt || node.children_count < 2 {
		return ''
	}
	for i in 1 .. node.children_count {
		branch := t.a.child_node(&node, i)
		if branch.kind != .match_branch {
			continue
		}
		body_start := if branch.value == 'else' { 0 } else { t.count_conds(*branch) }
		if branch.children_count <= body_start {
			continue
		}
		for j := branch.children_count - 1; j >= body_start; j-- {
			stmt_id := t.a.child(branch, j)
			typ := t.stmt_value_type(stmt_id)
			if typ.len > 0 {
				return typ
			}
		}
	}
	return ''
}

fn (t &Transformer) stmt_value_type(id flat.NodeId) string {
	if int(id) < 0 {
		return ''
	}
	node := t.a.nodes[int(id)]
	match node.kind {
		.expr_stmt {
			if node.children_count > 0 {
				return t.node_type(t.a.child(&node, node.children_count - 1))
			}
			return ''
		}
		.block {
			for i := node.children_count - 1; i >= 0; i-- {
				typ := t.stmt_value_type(t.a.child(&node, i))
				if typ.len > 0 {
					return typ
				}
			}
			return ''
		}
		else {
			return t.node_type(id)
		}
	}
}

// --- match lowering (existing, will move to expr.v later) ---

fn (mut t Transformer) lower_match_stmts() {
	for i, node in t.a.nodes {
		if node.kind == .match_stmt {
			if_id := t.lower_one_match(node)
			t.a.nodes[i] = t.a.nodes[int(if_id)]
		} else if node.kind == .expr_stmt && node.children_count == 1 {
			child_id := t.a.child(&node, 0)
			child := t.a.nodes[int(child_id)]
			if child.kind == .match_stmt {
				if_id := t.lower_one_match(child)
				t.a.nodes[i] = flat.Node{
					kind:           .expr_stmt
					children_start: t.a.children.len
					children_count: 1
				}
				t.a.children << if_id
			}
		}
	}
}

fn (mut t Transformer) lower_one_match(node flat.Node) flat.NodeId {
	match_expr_id := t.a.child(&node, 0)
	match_expr := t.a.nodes[int(match_expr_id)]

	needs_temp := match_expr.kind !in [.ident, .int_literal, .bool_literal, .string_literal,
		.char_literal]

	mut actual_expr_id := match_expr_id
	mut prefix_id := flat.empty_node

	if needs_temp {
		tmp_name := '__match_tmp_${int(match_expr_id)}'
		match_type := t.node_type(match_expr_id)
		tmp_ident := t.a.add_val(.ident, tmp_name)
		t.a.nodes[int(tmp_ident)].typ = match_type
		decl_start := t.a.children.len
		t.a.children << tmp_ident
		t.a.children << match_expr_id
		prefix_id = t.a.add_node(flat.Node{
			kind:           .decl_assign
			children_start: decl_start
			children_count: 2
			typ:            match_type
		})
		actual_expr_id = t.a.add_val(.ident, tmp_name)
		t.a.nodes[int(actual_expr_id)].typ = match_type
	}

	mut branches := []flat.NodeId{}
	for i in 1 .. node.children_count {
		branches << t.a.child(&node, i)
	}
	if_id := t.build_match_chain(actual_expr_id, match_expr_id, branches, 0)

	if needs_temp {
		block_start := t.a.children.len
		t.a.children << prefix_id
		t.a.children << if_id
		return t.a.add_node(flat.Node{
			kind:           .block
			children_start: block_start
			children_count: 2
		})
	}
	return if_id
}

fn (mut t Transformer) build_match_chain(match_expr_id flat.NodeId, orig_expr_id flat.NodeId, branches []flat.NodeId, idx int) flat.NodeId {
	if idx >= branches.len {
		return t.a.add(flat.NodeKind.empty)
	}
	branch := t.a.nodes[int(branches[idx])]
	is_else := branch.value == 'else'

	body_start_idx := if is_else { 0 } else { t.count_conds(branch) }
	// Push a smartcast around the body transform when this branch matches a
	// single sum-type variant, so selectors inside the body get narrowed.
	mut sc_pushed := 0
	if !is_else {
		n_conds := t.count_conds(branch)
		if n_conds == 1 {
			cond_val := t.a.nodes[int(t.a.child(&branch, 0))]
			if cond_val.kind == .ident && t.is_sum_variant(cond_val.value) {
				subj := t.expr_key(match_expr_id)
				sum_name := t.find_sum_type_for_variant(cond_val.value)
				if subj.len > 0 && sum_name.len > 0 {
					t.push_smartcast(subj, cond_val.value, sum_name)
					sc_pushed++
				}
				orig_subj := t.expr_key(orig_expr_id)
				if orig_subj.len > 0 && orig_subj != subj && sum_name.len > 0 {
					t.push_smartcast(orig_subj, cond_val.value, sum_name)
					sc_pushed++
				}
			}
		}
	}
	mut body_ids := []flat.NodeId{}
	for i in body_start_idx .. branch.children_count {
		body_ids << t.a.child(&branch, i)
	}
	new_body := t.transform_stmts(body_ids)
	for _ in 0 .. sc_pushed {
		t.pop_smartcast()
	}
	body_block := t.make_block(new_body)

	if is_else {
		return body_block
	}

	cond_id := t.build_match_cond(match_expr_id, branch)

	mut if_ids := []flat.NodeId{}
	if_ids << cond_id
	if_ids << body_block
	if idx + 1 < branches.len {
		else_part := t.build_match_chain(match_expr_id, orig_expr_id, branches, idx + 1)
		if_ids << else_part
	}

	if_start := t.a.children.len
	for id in if_ids {
		t.a.children << id
	}
	return t.a.add_node(flat.Node{
		kind:           .if_expr
		children_start: if_start
		children_count: flat.child_count(if_ids.len)
	})
}

// make_match_eq builds the equality test between a match subject and a branch
// value, lowering string comparisons to string__eq (the transformer owns string
// lowering; the backend no longer special-cases it).
fn (mut t Transformer) make_match_eq(lhs flat.NodeId, rhs flat.NodeId) flat.NodeId {
	if t.is_string_type(lhs) || t.is_string_type(rhs) {
		return t.make_call('string__eq', arr2(lhs, rhs))
	}
	start := t.a.children.len
	t.a.children << lhs
	t.a.children << rhs
	return t.a.add_node(flat.Node{
		kind:           .infix
		op:             .eq
		children_start: start
		children_count: 2
	})
}

fn (mut t Transformer) match_cond_value(match_expr_id flat.NodeId, cond_val_id flat.NodeId) flat.NodeId {
	cond_val := t.a.nodes[int(cond_val_id)]
	if cond_val.kind == .enum_val {
		return t.transform_enum_shorthand(cond_val_id, cond_val, t.node_type(match_expr_id))
	}
	return t.transform_expr(cond_val_id)
}

fn (mut t Transformer) build_match_cond(match_expr_id flat.NodeId, branch flat.Node) flat.NodeId {
	n_conds := t.count_conds(branch)
	if n_conds == 1 {
		cond_val_id := t.a.child(&branch, 0)
		cond_val := t.a.nodes[int(cond_val_id)]
		if cond_val.kind == .ident && t.is_sum_variant(cond_val.value) {
			is_start := t.a.children.len
			t.a.children << match_expr_id
			is_id := t.a.add_node(flat.Node{
				kind:           .is_expr
				value:          cond_val.value
				children_start: is_start
				children_count: 1
			})
			return t.transform_is_expr(is_id, t.a.nodes[int(is_id)])
		}
		return t.make_match_eq(match_expr_id, t.match_cond_value(match_expr_id, cond_val_id))
	}
	mut result := flat.empty_node
	for i in 0 .. n_conds {
		cond_val_id := t.a.child(&branch, i)
		cond_val := t.a.nodes[int(cond_val_id)]
		is_sum_cond := cond_val.kind == .ident && t.is_sum_variant(cond_val.value)
		cmp := if is_sum_cond {
			is_start := t.a.children.len
			t.a.children << match_expr_id
			is_id := t.a.add_node(flat.Node{
				kind:           .is_expr
				value:          cond_val.value
				children_start: is_start
				children_count: 1
			})
			t.transform_is_expr(is_id, t.a.nodes[int(is_id)])
		} else {
			t.make_match_eq(match_expr_id, t.match_cond_value(match_expr_id, cond_val_id))
		}
		if int(result) < 0 {
			result = cmp
		} else {
			or_start := t.a.children.len
			t.a.children << result
			t.a.children << cmp
			result = t.a.add_node(flat.Node{
				kind:           .infix
				op:             .logical_or
				children_start: or_start
				children_count: 2
			})
		}
	}
	return result
}

fn (t &Transformer) count_conds(branch flat.Node) int {
	if branch.value.len > 0 && branch.value != 'else' {
		return branch.value.int()
	}
	mut count := 0
	for i in 0 .. branch.children_count {
		child := t.a.child_node(&branch, i)
		if child.kind == .int_literal || child.kind == .ident || child.kind == .string_literal
			|| child.kind == .enum_val || child.kind == .bool_literal || child.kind == .char_literal {
			count++
		} else {
			break
		}
	}
	return count
}

pub fn (t &Transformer) is_sum_variant(name string) bool {
	for _, variants in t.sum_types {
		for v in variants {
			if v == name {
				return true
			}
		}
	}
	return false
}

// --- array append lowering (existing, will move to expr.v later) ---

fn (mut t Transformer) lower_array_appends() {
	for i, node in t.a.nodes {
		if node.kind == .module_decl {
			t.cur_module = node.value
			continue
		}
		if node.kind == .fn_decl {
			t.reset_var_types()
			t.annotate_fn_body(node)
			continue
		}
		if node.kind == .decl_assign && node.children_count >= 2 {
			lhs := t.a.child_node(&node, 0)
			if lhs.kind == .ident && lhs.value.len > 0 {
				typ := t.infer_decl_type(node)
				if typ.len > 0 {
					t.set_var_type(lhs.value, typ)
				}
			}
		}
		if node.kind == .expr_stmt && node.children_count == 1 {
			child_id := t.a.child(&node, 0)
			mut child := &t.a.nodes[int(child_id)]
			if child.kind == .infix && child.op == .left_shift {
				t.annotate_left_shift(child_id)
			}
		}
		if node.kind == .assign && node.op == .left_shift_assign && node.children_count >= 2 {
			lhs_id := t.a.child(&node, 0)
			lhs_type := t.lvalue_type(lhs_id)
			if lhs_type.starts_with('[]') {
				rhs_id := t.a.child(&node, 1)
				rhs_type := t.lvalue_type(rhs_id)
				elem_type := lhs_type[2..]
				rhs_node := t.a.nodes[int(rhs_id)]
				val := if array_append_rhs_is_push_many(rhs_type, elem_type, rhs_node) {
					'push_many'
				} else {
					'push'
				}
				t.a.nodes[i] = flat.Node{
					kind:           node.kind
					op:             node.op
					children_start: node.children_start
					children_count: node.children_count
					value:          val
					typ:            lhs_type[2..]
				}
			}
		}
	}
}

fn (mut t Transformer) annotate_fn_body(fn_node flat.Node) {
	for i in 0 .. fn_node.children_count {
		child_id := t.a.child(&fn_node, i)
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if child.kind == .param && child.value.len > 0 && child.typ.len > 0 {
			t.set_var_type(child.value, t.normalize_type_alias(child.typ))
		}
		if child.kind == .decl_assign && child.children_count >= 2 {
			lhs := t.a.child_node(&child, 0)
			if lhs.kind == .ident && lhs.value.len > 0 {
				typ := t.infer_decl_type(child)
				if typ.len > 0 {
					t.set_var_type(lhs.value, typ)
				}
			}
		}
		if child.kind == .expr_stmt && child.children_count == 1 {
			inner_id := t.a.child(&child, 0)
			inner := t.a.nodes[int(inner_id)]
			if inner.kind == .infix && inner.op == .left_shift {
				t.annotate_left_shift(inner_id)
			}
		}
		t.annotate_block_stmts(child_id)
	}
}

fn (mut t Transformer) annotate_block_stmts(node_id flat.NodeId) {
	if int(node_id) < 0 {
		return
	}
	node := t.a.nodes[int(node_id)]
	for i in 0 .. node.children_count {
		child_id := t.a.child(&node, i)
		if int(child_id) < 0 {
			continue
		}
		child := t.a.nodes[int(child_id)]
		if child.kind == .decl_assign && child.children_count >= 2 {
			lhs := t.a.child_node(&child, 0)
			if lhs.kind == .ident && lhs.value.len > 0 {
				typ := t.infer_decl_type(child)
				if typ.len > 0 {
					t.set_var_type(lhs.value, typ)
				}
			}
		}
		if child.kind == .expr_stmt && child.children_count == 1 {
			inner_id := t.a.child(&child, 0)
			inner := t.a.nodes[int(inner_id)]
			if inner.kind == .infix && inner.op == .left_shift {
				t.annotate_left_shift(inner_id)
			}
		}
		t.annotate_block_stmts(child_id)
	}
}

fn (mut t Transformer) annotate_left_shift(node_id flat.NodeId) {
	node := t.a.nodes[int(node_id)]
	if node.children_count < 2 {
		return
	}
	lhs_id := t.a.child(&node, 0)
	mut lhs_type := t.lvalue_type(lhs_id)
	if lhs_type == 'strings.Builder' || lhs_type == '&strings.Builder' || lhs_type == 'Builder'
		|| lhs_type == '&Builder' {
		lhs_type = '[]u8'
	}
	if !lhs_type.starts_with('[]') {
		return
	}
	rhs_id := t.a.child(&node, 1)
	rhs_type := t.lvalue_type(rhs_id)
	elem_type := lhs_type[2..]
	rhs_node := t.a.nodes[int(rhs_id)]
	if array_append_rhs_is_push_many(rhs_type, elem_type, rhs_node) {
		t.a.nodes[int(node_id)] = flat.Node{
			kind:           .infix
			op:             .left_shift
			children_start: node.children_start
			children_count: node.children_count
			value:          'push_many'
			typ:            lhs_type[2..]
		}
	} else {
		t.a.nodes[int(node_id)] = flat.Node{
			kind:           .infix
			op:             .left_shift
			children_start: node.children_start
			children_count: node.children_count
			value:          'push'
			typ:            lhs_type[2..]
		}
	}
}

fn (mut t Transformer) annotate_left_shift_assign(node_id flat.NodeId) {
	node := t.a.nodes[int(node_id)]
	if node.kind != .assign || node.op != .left_shift_assign || node.children_count < 2 {
		return
	}
	lhs_id := t.a.child(&node, 0)
	lhs := t.a.nodes[int(lhs_id)]
	if lhs.kind != .ident {
		return
	}
	lhs_type := t.lvalue_type(lhs_id)
	if !lhs_type.starts_with('[]') {
		return
	}
	rhs_id := t.a.child(&node, 1)
	rhs_type := t.lvalue_type(rhs_id)
	elem_type := lhs_type[2..]
	rhs_node := t.a.nodes[int(rhs_id)]
	val := if array_append_rhs_is_push_many(rhs_type, elem_type, rhs_node) {
		'push_many'
	} else {
		'push'
	}
	t.a.nodes[int(node_id)] = flat.Node{
		kind:           node.kind
		op:             node.op
		children_start: node.children_start
		children_count: node.children_count
		value:          val
		typ:            lhs_type[2..]
	}
}

// --- public query helpers ---

pub fn (t &Transformer) get_struct_info(name string) ?StructInfo {
	if info := t.structs[name] {
		return info
	}
	return none
}

pub fn (t &Transformer) get_global_type(name string) ?string {
	if typ := t.globals[name] {
		return typ
	}
	return none
}
