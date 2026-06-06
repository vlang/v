// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module markused

import v2.ast
import v2.token
import v2.types

// Guard functions for ARM64 backend where default-initialized sum types have NULL data pointers.
fn sumtype_payload_word_is_valid(tag_word u64, data_word u64) bool {
	if data_word == 0 {
		return false
	}
	// Native v2 backends use `(tag, data_ptr)` for sumtypes. If the first word
	// looks like a small tag, the payload must be a real pointer, not a leaked
	// enum/default value like `3`.
	if tag_word < 256 {
		return data_word >= 0x10000 && data_word < 0x0000800000000000
	}
	return true
}

fn string_ok(s string) bool {
	if s.len == 0 {
		return true
	}
	if s.len < 0 || s.len > 1024 {
		return false
	}
	ptr := unsafe { u64(s.str) }
	return ptr >= 0x10000 && ptr < 0x0000800000000000
}

fn expr_ok(expr ast.Expr) bool {
	tag_word := unsafe { (&u64(&expr))[0] }
	data_word := unsafe { (&u64(&expr))[1] }
	return sumtype_payload_word_is_valid(tag_word, data_word)
}

fn stmt_ok(stmt ast.Stmt) bool {
	tag_word := unsafe { (&u64(&stmt))[0] }
	data_word := unsafe { (&u64(&stmt))[1] }
	return sumtype_payload_word_is_valid(tag_word, data_word)
}

fn type_ok(typ types.Type) bool {
	tag_word := unsafe { (&u64(&typ))[0] }
	data_word := unsafe { (&u64(&typ))[1] }
	if data_word == 0 {
		return false
	}
	return sumtype_payload_word_is_valid(tag_word, data_word)
}

const builtin_cast_type_names = [
	'bool',
	'byte',
	'byteptr',
	'char',
	'charptr',
	'f32',
	'f64',
	'i8',
	'i16',
	'i32',
	'i64',
	'int',
	'isize',
	'rune',
	'u8',
	'u16',
	'u32',
	'u64',
	'usize',
	'voidptr',
]

struct FnInfo {
	key  string
	mod  string
	file string
	decl ast.FnDecl
	// decl_id is the FlatNodeId of the FnDecl in the source FlatAst, or
	// -1 when the FnInfo was produced by the legacy []ast.File path that
	// has no flat backing. When >= 0, walk_collected_from_flat iterates
	// the body via Cursor instead of info.decl.stmts so future PRs can
	// port walk_stmt arms to cursor input one at a time.
	decl_id ast.FlatNodeId = -1
	// has_body is true when the FnDecl has a non-empty body. In flat mode
	// the body is not decoded into info.decl.stmts (signature-only
	// decode), so we precompute the bit at collect time from the body
	// edge's child count. In legacy mode it mirrors info.decl.stmts.len > 0.
	has_body bool
}

struct Walker {
	files []ast.File
	env   &types.Environment = unsafe { nil }
	opts  MarkUsedOptions
mut:
	fns               []FnInfo
	queue             []int
	queued_fn_indices map[int]bool

	used_keys map[string]bool

	module_names                map[string]bool
	module_alias_to_real        map[string]string
	type_names                  map[string]bool
	interface_type_names        map[string]bool
	interface_method_names      map[string][]string
	interface_embedded_names    map[string][]string
	methods_by_receiver         map[string][]int
	struct_field_receivers      map[string][]string
	struct_embedded_receivers   map[string][]string
	struct_fn_fields            map[string]bool
	global_interface_names      map[string]string
	const_fn_value_aliases      map[string]string
	selective_import_fn_targets map[string]string

	lookup map[string][]int

	cur_fn_scope &types.Scope = unsafe { nil }
	cur_fn_decl  ast.FnDecl
	cur_fn_file  string
}

pub struct MarkUsedOptions {
pub:
	minimal_runtime_roots bool
}

// mark_used walks reachable function/method bodies and returns declaration keys
// for the functions that are used at least once.
pub fn mark_used(files []ast.File, env &types.Environment) map[string]bool {
	mut w := new_walker(files, env, MarkUsedOptions{})
	return w.walk()
}

pub fn mark_used_with_options(files []ast.File, env &types.Environment, opts MarkUsedOptions) map[string]bool {
	mut w := new_walker(files, env, opts)
	return w.walk()
}

// decl_key computes the stable key used by mark_used for a function declaration.
pub fn decl_key(module_name string, decl ast.FnDecl, env &types.Environment) string {
	mod_name := normalize_module_name(module_name)
	if is_method_decl(decl) {
		receiver := receiver_primary_name(mod_name, decl, env)
		return '${mod_name}|m|${receiver}|${normalize_method_name(decl.name)}'
	}
	return '${mod_name}|f|${decl.name}'
}

fn new_walker(files []ast.File, env &types.Environment, opts MarkUsedOptions) Walker {
	return Walker{
		files:                       files
		env:                         unsafe { env }
		opts:                        opts
		used_keys:                   map[string]bool{}
		queued_fn_indices:           map[int]bool{}
		module_names:                map[string]bool{}
		type_names:                  map[string]bool{}
		interface_type_names:        map[string]bool{}
		interface_method_names:      map[string][]string{}
		interface_embedded_names:    map[string][]string{}
		methods_by_receiver:         map[string][]int{}
		struct_field_receivers:      map[string][]string{}
		struct_embedded_receivers:   map[string][]string{}
		struct_fn_fields:            map[string]bool{}
		global_interface_names:      map[string]string{}
		const_fn_value_aliases:      map[string]string{}
		selective_import_fn_targets: map[string]string{}
		lookup:                      map[string][]int{}
	}
}

fn (mut w Walker) walk() map[string]bool {
	w.collect_defs()
	return w.walk_collected()
}

// walk_collected runs the body-walking phase after collect_defs (or
// collect_defs_from_flat) has populated w.fns. Split out so flat-input
// entry points can substitute their own collect step.
fn (mut w Walker) walk_collected() map[string]bool {
	if w.fns.len == 0 {
		return map[string]bool{}
	}
	if !w.seed_roots() {
		mut all := map[string]bool{}
		for info in w.fns {
			all[info.key] = true
		}
		return all
	}
	mut qi := 0
	for qi < w.queue.len {
		idx := w.queue[qi]
		qi++
		info := w.fns[idx]
		w.cur_fn_scope = w.lookup_fn_scope(info)
		w.cur_fn_decl = info.decl
		w.cur_fn_file = info.file
		w.walk_stmts(info.decl.stmts, info.mod)
		w.cur_fn_scope = unsafe { nil }
		w.cur_fn_file = ''
	}
	return w.used_keys
}

// walk_collected_from_flat mirrors walk_collected but, for FnInfos that
// originated from flat input (decl_id >= 0), iterates the body via Cursor
// instead of the pre-rehydrated info.decl.stmts slice. For now each body
// stmt is decoded back to ast.Stmt before being dispatched to walk_stmt —
// later PRs port walk_stmt arms to accept cursor input directly so the
// per-stmt rehydration can drop out. FnInfos without a flat backing
// (decl_id < 0) fall back to the legacy slice walk.
fn (mut w Walker) walk_collected_from_flat(flat &ast.FlatAst) map[string]bool {
	if w.fns.len == 0 {
		return map[string]bool{}
	}
	if !w.seed_roots_from_flat(flat) {
		mut all := map[string]bool{}
		for info in w.fns {
			all[info.key] = true
		}
		return all
	}
	mut qi := 0
	for qi < w.queue.len {
		idx := w.queue[qi]
		qi++
		info := w.fns[idx]
		w.cur_fn_scope = w.lookup_fn_scope(info)
		w.cur_fn_decl = info.decl
		w.cur_fn_file = info.file
		if info.decl_id >= 0 {
			w.walk_fn_body_cursor(flat, info.decl_id, info.mod)
		} else {
			w.walk_stmts(info.decl.stmts, info.mod)
		}
		w.cur_fn_scope = unsafe { nil }
		w.cur_fn_file = ''
	}
	return w.used_keys
}

// walk_fn_body_cursor iterates an FnDecl body (edge 3 of stmt_fn_decl) via
// CursorList and dispatches each child through walk_stmt_cursor. Arms that
// have been ported to cursor input avoid per-stmt rehydration; un-ported
// arms decode back to ast.Stmt and call walk_stmt.
fn (mut w Walker) walk_fn_body_cursor(flat &ast.FlatAst, decl_id ast.FlatNodeId, mod_name string) {
	body := ast.Cursor{
		flat: unsafe { flat }
		id:   decl_id
	}.list_at(3)
	for i in 0 .. body.len() {
		w.walk_stmt_cursor(body.at(i), mod_name)
	}
}

// walk_stmt_cursor is the cursor-input analogue of walk_stmt. Arms that have
// been ported operate directly on the FlatNode (no Stmt rehydration); the
// fallback decodes the stmt and delegates to walk_stmt so partial coverage
// stays bit-identical with the legacy walker.
//
// Ported arms: stmt_assert, stmt_assign, stmt_attributes, stmt_block,
// stmt_comptime, stmt_const_decl, stmt_defer, stmt_enum_decl, stmt_expr,
// stmt_for, stmt_for_in, stmt_global_decl, stmt_label, stmt_return,
// stmt_struct_decl, stmt_type_decl — every kind the legacy walk_stmt
// matched explicitly. Nested expressions are still routed through
// walk_expr (decoded per edge) until walk_expr itself gets a cursor-input
// port. The else arm catches stmt kinds the legacy walker ignored
// (stmt_asm/_directive/_empty/_flow_control/_fn_decl/_import/_interface_decl
// /_module) plus any future kinds; it stays in place as a safety net.
fn (mut w Walker) walk_stmt_cursor(c ast.Cursor, mod_name string) {
	if !c.is_valid() {
		return
	}
	match c.kind() {
		.stmt_assert {
			w.walk_expr_cursor_edge(c, 0, mod_name)
			w.walk_expr_cursor_edge(c, 1, mod_name)
		}
		.stmt_attributes {
			// edge 0 = aux_list of aux_attribute children; each attribute has
			// edge 0 = value expr, edge 1 = comptime_cond expr.
			attrs := c.list_at(0)
			for i in 0 .. attrs.len() {
				attr := attrs.at(i)
				if !attr.is_valid() {
					continue
				}
				w.walk_expr_cursor_edge(attr, 0, mod_name)
				w.walk_expr_cursor_edge(attr, 1, mod_name)
			}
		}
		.stmt_assign {
			ec := c.edge_count()
			lhs_len := c.extra_int()
			// Mirror the legacy ordering: first the interface-conversion
			// checks across paired (lhs[i], rhs[i]) edges, then walk all
			// lhs exprs, then all rhs exprs.
			pair_len := if lhs_len < ec - lhs_len { lhs_len } else { ec - lhs_len }
			for i in 0 .. pair_len {
				lhs_c := c.edge(i)
				rhs_c := c.edge(lhs_len + i)
				if !lhs_c.is_valid() || !rhs_c.is_valid() {
					continue
				}
				w.mark_assignment_interface_conversion_cursor(lhs_c, rhs_c, mod_name)
			}
			for i in lhs_len .. ec {
				w.mark_fn_value_expr_cursor(c.edge(i), mod_name)
			}
			for i in 0 .. ec {
				w.walk_expr_cursor_edge(c, i, mod_name)
			}
		}
		.stmt_block {
			for i in 0 .. c.edge_count() {
				w.walk_stmt_cursor(c.edge(i), mod_name)
			}
		}
		.stmt_comptime {
			w.mark_comptime_method_loop_receivers_cursor(c.edge(0), mod_name)
			w.walk_stmt_cursor(c.edge(0), mod_name)
		}
		.stmt_const_decl {
			// edge 0 = fields (aux_field_init list); each field edge 0 = value expr.
			w.walk_field_value_list(c, 0, mod_name)
		}
		.stmt_defer {
			for i in 0 .. c.edge_count() {
				w.walk_stmt_cursor(c.edge(i), mod_name)
			}
		}
		.stmt_enum_decl {
			// edge 2 = fields (aux_field_decl list); each field edge 1 = value expr.
			w.walk_field_decl_list(c, 2, mod_name, false)
		}
		.stmt_expr {
			w.walk_expr_cursor_edge(c, 0, mod_name)
		}
		.stmt_for {
			w.walk_stmt_cursor(c.edge(0), mod_name)
			w.walk_expr_cursor_edge(c, 1, mod_name)
			w.walk_stmt_cursor(c.edge(2), mod_name)
			for i in 3 .. c.edge_count() {
				w.walk_stmt_cursor(c.edge(i), mod_name)
			}
		}
		.stmt_for_in {
			w.walk_expr_cursor_edge(c, 0, mod_name)
			w.walk_expr_cursor_edge(c, 1, mod_name)
			w.walk_expr_cursor_edge(c, 2, mod_name)
		}
		.stmt_global_decl {
			// edge 1 = fields (aux_field_decl list); each field walks typ+value.
			w.walk_field_decl_list(c, 1, mod_name, true)
		}
		.stmt_label {
			w.walk_stmt_cursor(c.edge(0), mod_name)
		}
		.stmt_return {
			for i in 0 .. c.edge_count() {
				ec := c.edge(i)
				if !ec.is_valid() {
					continue
				}
				w.mark_fn_value_expr_cursor(ec, mod_name)
				w.walk_expr_cursor(ec, mod_name)
				w.mark_interface_conversion_methods_cursor(w.cur_fn_decl.typ.return_type, ec,
					mod_name)
				w.mark_result_error_return_methods_cursor(w.cur_fn_decl.typ.return_type, ec,
					mod_name)
			}
		}
		.stmt_struct_decl {
			// edge 4 = fields (aux_field_decl list); each field walks typ+value.
			w.walk_field_decl_list(c, 4, mod_name, true)
		}
		.stmt_type_decl {
			// edge 0 = base_type (expr); edge 3 = variants (aux expr list).
			w.walk_expr_cursor_edge(c, 0, mod_name)
			variants := c.list_at(3)
			for i in 0 .. variants.len() {
				vc := variants.at(i)
				if !vc.is_valid() {
					continue
				}
				w.walk_expr_cursor(vc, mod_name)
			}
		}
		// Kinds intentionally ignored by the walker (mirror of legacy
		// walk_stmt's `else {}`): stmt_asm, stmt_directive, stmt_empty,
		// stmt_flow_control (break/continue/goto — common in fn bodies,
		// hence worth avoiding a decode_stmt), stmt_fn_decl,
		// stmt_import, stmt_interface_decl, stmt_module. The legacy
		// walker walks none of these so neither do we.
		else {}
	}
}

// walk_expr_cursor_edge dispatches child edge `edge_i` of `parent` through
// walk_expr_cursor. Kept as a tiny helper so callers don't have to do the
// edge-+-validity dance inline.
fn (mut w Walker) walk_expr_cursor_edge(parent ast.Cursor, edge_i int, mod_name string) {
	w.walk_expr_cursor(parent.edge(edge_i), mod_name)
}

// walk_expr_cursor is the cursor-input analogue of walk_expr. Ported arms
// operate directly on the FlatNode; un-ported arms decode the expr and
// delegate to walk_expr so partial coverage stays bit-identical with the
// legacy walker.
//
// Ported arms (no per-node decoding):
//   * leaves: expr_basic_literal, expr_empty, expr_ident, typ_nil, typ_none
//   * single-edge recursers: expr_comptime, expr_lambda, expr_modifier,
//     expr_paren, expr_postfix, expr_prefix, expr_sql, typ_array,
//     typ_option, typ_pointer, typ_result, typ_thread
//   * two-edge recursers: expr_as_cast, expr_generic_arg_or_index,
//     expr_index, expr_range, typ_array_fixed, typ_channel, typ_map
//   * N-edge recursers: expr_keyword_operator, expr_tuple, typ_tuple
//   * nested-stmt / packed-shape arms: expr_array_init, expr_fn_literal,
//     expr_if, expr_if_guard, expr_lock, expr_map_init, expr_match,
//     expr_or, expr_select, expr_unsafe, typ_anon_struct, typ_fn,
//     typ_generic
//   * no-mark structural arms: expr_assoc, expr_generic_args
//
// The remaining un-ported arms (expr_call, expr_call_or_cast, expr_cast,
// expr_infix, expr_init, expr_selector, expr_string_inter) all invoke
// mark_* helpers that take decoded ast.Expr / ast.CallExpr / ast.InfixExpr
// today. Porting them buys little until those helpers grow cursor-input
// variants; the else fallback decodes + delegates and stays bit-identical.
fn (mut w Walker) walk_expr_cursor(c ast.Cursor, mod_name string) {
	if !c.is_valid() {
		return
	}
	match c.kind() {
		// Leaves with no children and no walker side-effects. The legacy
		// walk_expr arms for Keyword / StringLiteral / Lifetime hit its
		// else {}, so they belong here too.
		.expr_basic_literal, .expr_empty, .expr_keyword, .expr_lifetime, .expr_string, .typ_nil,
		.typ_none {}
		.expr_ident {
			name := c.name()
			w.mark_ierror_wrapper_dependencies(name, mod_name)
			if (!w.opts.minimal_runtime_roots || should_mark_ident_as_fn(name))
				&& !w.is_cast_type_name(name) {
				w.mark_fn_name(name, mod_name)
			}
		}
		// Single-edge recursers — child at edge 0.
		.expr_comptime, .expr_lambda, .expr_modifier, .expr_paren, .expr_postfix, .expr_prefix,
		.expr_sql, .typ_array, .typ_option, .typ_pointer, .typ_result, .typ_thread {
			w.walk_expr_cursor(c.edge(0), mod_name)
		}
		// Two-edge recursers.
		.expr_as_cast, .expr_generic_arg_or_index, .expr_index, .expr_range, .typ_array_fixed,
		.typ_channel, .typ_map {
			w.walk_expr_cursor(c.edge(0), mod_name)
			w.walk_expr_cursor(c.edge(1), mod_name)
		}
		// N-edge recursers — every direct edge is an Expr to walk.
		.expr_keyword_operator, .typ_tuple {
			for i in 0 .. c.edge_count() {
				w.walk_expr_cursor(c.edge(i), mod_name)
			}
		}
		.expr_tuple {
			for i in 0 .. c.edge_count() {
				value_c := c.edge(i)
				w.mark_fn_value_expr_cursor(value_c, mod_name)
				w.walk_expr_cursor(value_c, mod_name)
			}
		}
		// typ_generic packs name as edge 0 and generic params at edges 1..N;
		// the legacy walker calls walk_expr on each of them in order.
		.typ_generic {
			for i in 0 .. c.edge_count() {
				w.walk_expr_cursor(c.edge(i), mod_name)
			}
		}
		// expr_array_init: edge 0 = typ, 1 = init, 2 = cap, 3 = len,
		// 4 = update_expr, 5.. = exprs. Legacy walk order is typ, exprs,
		// init, cap, len, update_expr.
		.expr_array_init {
			ec := c.edge_count()
			w.walk_expr_cursor(c.edge(0), mod_name)
			for i in 5 .. ec {
				item_c := c.edge(i)
				w.mark_fn_value_expr_cursor(item_c, mod_name)
				w.walk_expr_cursor(item_c, mod_name)
			}
			w.mark_fn_value_expr_cursor(c.edge(1), mod_name)
			w.walk_expr_cursor(c.edge(1), mod_name)
			w.walk_expr_cursor(c.edge(2), mod_name)
			w.walk_expr_cursor(c.edge(3), mod_name)
			w.walk_expr_cursor(c.edge(4), mod_name)
		}
		// expr_map_init: edge 0 = typ, 1..1+keys_len = keys,
		// 1+keys_len.. = vals. keys_len is packed in `extra`.
		.expr_map_init {
			keys_len := c.extra_int()
			ec := c.edge_count()
			w.walk_expr_cursor(c.edge(0), mod_name)
			for i in 1 .. (1 + keys_len) {
				key_c := c.edge(i)
				w.mark_fn_value_expr_cursor(key_c, mod_name)
				w.walk_expr_cursor(key_c, mod_name)
			}
			for i in (1 + keys_len) .. ec {
				value_c := c.edge(i)
				w.mark_fn_value_expr_cursor(value_c, mod_name)
				w.walk_expr_cursor(value_c, mod_name)
			}
		}
		// expr_lock: lock_len + rlock_len are packed in `extra` (low/high u16).
		// Layout is lock_exprs, rlock_exprs, stmts.
		.expr_lock {
			packed := c.extra_int()
			lock_len := packed & 0xFFFF
			rlock_len := (packed >> 16) & 0xFFFF
			ec := c.edge_count()
			for i in 0 .. lock_len {
				w.walk_expr_cursor(c.edge(i), mod_name)
			}
			for i in lock_len .. (lock_len + rlock_len) {
				w.walk_expr_cursor(c.edge(i), mod_name)
			}
			for i in (lock_len + rlock_len) .. ec {
				w.walk_stmt_cursor(c.edge(i), mod_name)
			}
		}
		// expr_if: edge 0 = cond, edge 1 = else_expr, edges 2.. = stmts.
		// Legacy walks cond → stmts → else_expr.
		.expr_if {
			ec := c.edge_count()
			w.walk_expr_cursor(c.edge(0), mod_name)
			for i in 2 .. ec {
				w.walk_stmt_cursor(c.edge(i), mod_name)
			}
			w.walk_expr_cursor(c.edge(1), mod_name)
		}
		// expr_if_guard: edge 0 = the embedded AssignStmt.
		.expr_if_guard {
			w.walk_stmt_cursor(c.edge(0), mod_name)
		}
		// expr_or: edge 0 = expr, edges 1.. = stmts.
		.expr_or {
			w.walk_expr_cursor(c.edge(0), mod_name)
			for i in 1 .. c.edge_count() {
				w.walk_stmt_cursor(c.edge(i), mod_name)
			}
		}
		// expr_unsafe: all edges are stmts.
		.expr_unsafe {
			for i in 0 .. c.edge_count() {
				w.walk_stmt_cursor(c.edge(i), mod_name)
			}
		}
		// expr_fn_literal: edge 0 = fn_typ, edges 1..1+cap_len = captured
		// (not walked by the legacy walker), edges 1+cap_len.. = stmts.
		// cap_len is packed in `extra`.
		.expr_fn_literal {
			cap_len := c.extra_int()
			for i in (1 + cap_len) .. c.edge_count() {
				w.walk_stmt_cursor(c.edge(i), mod_name)
			}
		}
		// expr_select: edge 0 = stmt, edge 1 = next (expr), edges 2.. = stmts.
		// Legacy walks stmt → stmts → next.
		.expr_select {
			ec := c.edge_count()
			w.walk_stmt_cursor(c.edge(0), mod_name)
			for i in 2 .. ec {
				w.walk_stmt_cursor(c.edge(i), mod_name)
			}
			w.walk_expr_cursor(c.edge(1), mod_name)
		}
		// expr_match: edge 0 = scrutinee expr, edges 1.. = aux_match_branch.
		// Each match_branch has edge 0 = aux_list of cond exprs, edge 1 =
		// aux_list of stmts.
		.expr_match {
			w.walk_expr_cursor(c.edge(0), mod_name)
			for i in 1 .. c.edge_count() {
				branch := c.edge(i)
				if !branch.is_valid() {
					continue
				}
				conds := branch.list_at(0)
				for j in 0 .. conds.len() {
					w.walk_expr_cursor(conds.at(j), mod_name)
				}
				stmts := branch.list_at(1)
				for j in 0 .. stmts.len() {
					w.walk_stmt_cursor(stmts.at(j), mod_name)
				}
			}
		}
		// typ_fn: edge 0 = generic_params (not walked by legacy), edge 1 =
		// aux_list of aux_parameter, edge 2 = return_type. Each parameter
		// node has edge 0 = its type.
		.typ_fn {
			params_list := c.list_at(1)
			for i in 0 .. params_list.len() {
				param := params_list.at(i)
				if !param.is_valid() {
					continue
				}
				w.walk_expr_cursor(param.edge(0), mod_name)
			}
			w.walk_expr_cursor(c.edge(2), mod_name)
		}
		// typ_anon_struct: edge 0 = generic_params (not walked by legacy),
		// edge 1 = embedded (aux_list of exprs), edge 2 = fields
		// (aux_list of aux_field_decl).
		.typ_anon_struct {
			embedded := c.list_at(1)
			for i in 0 .. embedded.len() {
				w.walk_expr_cursor(embedded.at(i), mod_name)
			}
			w.walk_field_decl_list(c, 2, mod_name, true)
		}
		// expr_assoc: edge 0 = typ, edge 1 = expr, edges 2.. = aux_field_init
		// nodes (each with edge 0 = value expr). No mark side-effects, so the
		// arm is purely structural.
		.expr_assoc {
			w.walk_expr_cursor(c.edge(0), mod_name)
			w.walk_expr_cursor(c.edge(1), mod_name)
			for i in 2 .. c.edge_count() {
				field := c.edge(i)
				if !field.is_valid() {
					continue
				}
				value_c := field.edge(0)
				w.mark_fn_value_expr_cursor(value_c, mod_name)
				w.walk_expr_cursor(value_c, mod_name)
			}
		}
		// expr_generic_args: edge 0 = lhs, edges 1.. = args. No mark
		// side-effects — every edge is a walked expr.
		.expr_generic_args {
			for i in 0 .. c.edge_count() {
				w.walk_expr_cursor(c.edge(i), mod_name)
			}
		}
		// expr_call: edge 0 = lhs, edges 1.. = args. Cursor port uses
		// mark_call_lhs_cursor + mark_call_arg_interface_conversions_cursor —
		// both decode lhs.lhs (selector receiver fallback) and args (only
		// when the matching param is an interface) on demand. Everything
		// else is structural.
		.expr_call {
			lhs_c := c.edge(0)
			w.mark_call_lhs_cursor(lhs_c, mod_name)
			w.mark_call_arg_interface_conversions_cursor(c, mod_name)
			w.walk_expr_cursor(lhs_c, mod_name)
			for i in 1 .. c.edge_count() {
				arg_c := c.edge(i)
				w.mark_fn_value_expr_cursor(arg_c, mod_name)
				w.walk_expr_cursor(arg_c, mod_name)
			}
		}
		// expr_call_or_cast: edge 0 = lhs, edge 1 = inner. When lhs is an
		// Ident naming a cast type, behaves like a cast — call
		// mark_interface_conversion_methods and walk only the inner expr.
		// Otherwise behaves like a call (mark_call_lhs + the single-arg
		// interface conversion check).
		.expr_call_or_cast {
			lhs_c := c.edge(0)
			expr_c := c.edge(1)
			if lhs_c.is_valid() && lhs_c.kind() == .expr_ident && w.is_cast_type_name(lhs_c.name()) {
				iface_name := w.interface_name_from_cursor(lhs_c, mod_name)
				if iface_name != '' && expr_c.is_valid() {
					w.mark_interface_conversion_methods_for_name_cursor(iface_name, expr_c,
						mod_name)
				}
				w.walk_expr_cursor(expr_c, mod_name)
			} else {
				w.mark_call_lhs_cursor(lhs_c, mod_name)
				w.mark_call_or_cast_arg_interface_conversion_cursor(c, mod_name)
				w.walk_expr_cursor(lhs_c, mod_name)
				w.mark_fn_value_expr_cursor(expr_c, mod_name)
				w.walk_expr_cursor(expr_c, mod_name)
			}
		}
		// expr_cast: edge 0 = typ, edge 1 = value. Legacy walker calls
		// mark_interface_conversion_methods(typ, value) which is a no-op unless
		// typ resolves to a known interface. We peek typ via
		// interface_name_from_cursor (only matches Ident / SelectorExpr,
		// both cheap), and only when it fires do we decode the value subtree
		// to feed receiver_candidates_for_expr.
		.expr_cast {
			typ_c := c.edge(0)
			expr_c := c.edge(1)
			iface_name := w.interface_name_from_cursor(typ_c, mod_name)
			if iface_name != '' && expr_c.is_valid() {
				w.mark_interface_conversion_methods_for_name_cursor(iface_name, expr_c, mod_name)
			}
			w.walk_expr_cursor(typ_c, mod_name)
			w.walk_expr_cursor(expr_c, mod_name)
		}
		// expr_init: edge 0 = typ, edges 1.. = aux_field_init (edge 0 = value).
		// Legacy walker calls mark_interface_conversion_methods(typ, expr)
		// where the value_expr is the whole InitExpr; receiver_candidates_for_
		// cursor's expr_init branch reads pos + recurses into the typ cursor,
		// so passing this cursor directly matches the legacy semantics.
		.expr_init {
			typ_c := c.edge(0)
			iface_name := w.interface_name_from_cursor(typ_c, mod_name)
			if iface_name != '' {
				w.mark_interface_conversion_methods_for_name_cursor(iface_name, c, mod_name)
			}
			w.walk_expr_cursor(typ_c, mod_name)
			for i in 1 .. c.edge_count() {
				field := c.edge(i)
				if !field.is_valid() {
					continue
				}
				value_c := field.edge(0)
				if value_c.is_valid() && value_c.kind() == .expr_selector
					&& w.init_field_is_fn_value_cursor(typ_c, field.name(), mod_name) {
					w.mark_selector_fn_value_cursor_with_fallback(value_c, mod_name, true)
				}
				w.mark_fn_value_expr_cursor(value_c, mod_name)
				w.walk_expr_cursor(value_c, mod_name)
			}
		}
		// expr_infix: aux = op (Token int), edge 0 = lhs, edge 1 = rhs.
		// mark_infix_operator_method is a no-op for ops outside the method-
		// dispatching set, so we gate the decode on the op kind. When the op
		// does qualify, we decode just the lhs subtree to feed
		// receiver_candidates_for_expr, then walk both children via cursor.
		.expr_infix {
			op := unsafe { token.Token(int(c.aux())) }
			if infix_operator_may_use_method(op) {
				method_name := op.str()
				receivers := w.receiver_candidates_for_cursor(c.edge(0), mod_name)
				if receivers.len > 0 {
					w.mark_method_name(method_name, receivers)
				} else {
					w.mark_method_name_fallback(method_name)
				}
			}
			w.walk_expr_cursor(c.edge(0), mod_name)
			w.walk_expr_cursor(c.edge(1), mod_name)
		}
		// expr_string_inter: edge 0 = aux_list of value strings (not walked),
		// edge 1 = aux_list of inter nodes. Each inter has edge 0 = expr,
		// edge 1 = format_expr. Legacy walker calls
		// mark_string_interpolation_str_dependency(inter.expr) which needs a
		// decoded ast.Expr to feed receiver_candidates_for_expr. We decode
		// just that subtree per inter and walk both children via cursor —
		// format_expr's subtree never gets decoded.
		.expr_string_inter {
			inters := c.list_at(1)
			for i in 0 .. inters.len() {
				inter := inters.at(i)
				if !inter.is_valid() {
					continue
				}
				expr_c := inter.edge(0)
				if expr_c.is_valid() {
					w.mark_string_interpolation_str_dependency_cursor(expr_c, mod_name)
				}
				w.walk_expr_cursor(expr_c, mod_name)
				w.walk_expr_cursor(inter.edge(1), mod_name)
			}
		}
		// expr_selector: edge 0 = lhs, edge 1 = rhs (Ident). Legacy walker calls
		// mark_selector_fn_value(expr), which only marks module function values
		// that resolve to an indexed function.
		.expr_selector {
			lhs_c := c.edge(0)
			w.mark_selector_fn_value_cursor(c, mod_name)
			w.walk_expr_cursor(lhs_c, mod_name)
		}
		// Every expr/type FlatNodeKind has an explicit arm above; aux_*
		// nodes never reach walk_expr_cursor (they are addressed via list_at
		// / edge by their parents). Anything else is a schema mismatch.
		else {}
	}
}

// walk_field_value_list iterates the aux_field_init list at `parent.edge(list_edge)`
// and handles each field's value expr (aux_field_init edge 0). Used by
// stmt_const_decl, whose only walk-relevant per-field datum is the value.
fn (mut w Walker) walk_field_value_list(parent ast.Cursor, list_edge int, mod_name string) {
	fields := parent.list_at(list_edge)
	for i in 0 .. fields.len() {
		field := fields.at(i)
		if !field.is_valid() {
			continue
		}
		w.mark_fn_value_expr_cursor(field.edge(0), mod_name)
		w.walk_expr_cursor_edge(field, 0, mod_name)
	}
}

// walk_field_decl_list iterates the aux_field_decl list at `parent.edge(list_edge)`.
// Each aux_field_decl has edge 0 = typ, edge 1 = value, edge 2 = attrs. When
// `walk_typ` is true the typ edge is also walked (struct/global decls);
// otherwise only the value edge is walked (enum decls).
fn (mut w Walker) walk_field_decl_list(parent ast.Cursor, list_edge int, mod_name string, walk_typ bool) {
	fields := parent.list_at(list_edge)
	for i in 0 .. fields.len() {
		field := fields.at(i)
		if !field.is_valid() {
			continue
		}
		if walk_typ {
			w.walk_expr_cursor_edge(field, 0, mod_name)
		}
		w.walk_expr_cursor_edge(field, 1, mod_name)
	}
}

fn (mut w Walker) collect_defs() {
	for file in w.files {
		mod_name := normalize_module_name(file.mod)
		w.add_module_name(mod_name)
		w.collect_file_imports(file.name, file.imports)
		for stmt in file.stmts {
			w.collect_def_stmt(stmt, mod_name, file.name, ast.FlatNodeId(-1))
		}
	}
	w.collect_const_fn_value_aliases()
}

// collect_defs_from_flat is the flat-input analogue of collect_defs. It
// walks each file's top-level statements through Cursor and only decodes
// the decls collect_def_stmt actually consumes (struct/enum/interface/
// type/global/fn). Imports are read directly from cursor fields
// (name / extra_str alias) so the []ImportStmt slice is never allocated.
// The shared collect_def_stmt dispatch keeps the flat path bit-identical
// to the legacy path on the same input.
fn (mut w Walker) collect_defs_from_flat(flat &ast.FlatAst) {
	for fi in 0 .. flat.files.len {
		fc := flat.file_cursor(fi)
		mod_name := normalize_module_name(fc.mod())
		w.add_module_name(mod_name)
		imports_list := fc.imports()
		for i in 0 .. imports_list.len() {
			imp_cur := imports_list.at(i)
			name := imp_cur.name()
			w.add_module_name(name)
			alias := imp_cur.extra_str()
			import_mod := if imp_cur.flag(ast.flag_is_aliased) {
				name.all_after_last('.')
			} else {
				alias
			}
			for si in 0 .. imp_cur.edge_count() {
				symbol := imp_cur.edge(si)
				if symbol.is_valid() && symbol.kind() == .expr_ident {
					w.selective_import_fn_targets['${fc.name()}:${symbol.name()}'] = markused_imported_symbol_fn_name(import_mod,
						symbol.name())
				}
			}
			if alias != '' {
				w.add_module_name(alias)
				// Map alias to real module name for correct symbol resolution.
				// e.g., 'import v2.ssa.optimize as ssa_optimize' → 'ssa_optimize' → 'optimize'
				real_name := if name.contains('.') {
					name.all_after_last('.')
				} else {
					name
				}
				w.module_alias_to_real[alias] = real_name
			}
		}
		file_name := fc.name()
		stmts_list := fc.stmts()
		for i in 0 .. stmts_list.len() {
			c := stmts_list.at(i)
			match c.kind() {
				.stmt_fn_decl {
					// Signature-only decode skips read_stmt_list(body_id),
					// which dominates collect_defs time when the body is
					// large. The cursor walk reads the body directly later.
					fn_decl := flat.decode_fn_decl_signature(c.id)
					if fn_decl.name == '' {
						continue
					}
					has_body := c.list_at(3).len() > 0
					info := FnInfo{
						key:      decl_key(mod_name, fn_decl, w.env)
						mod:      mod_name
						file:     file_name
						decl:     fn_decl
						decl_id:  c.id
						has_body: has_body
					}
					w.fns << info
					idx := w.fns.len - 1
					w.index_fn(idx, info)
				}
				.stmt_struct_decl, .stmt_enum_decl, .stmt_interface_decl, .stmt_type_decl,
				.stmt_global_decl {
					stmt := flat.decode_stmt(c.id)
					w.collect_def_stmt(stmt, mod_name, file_name, c.id)
				}
				else {}
			}
		}
	}
	w.collect_const_fn_value_aliases_from_flat(flat)
}

fn const_fn_value_alias_key(mod_name string, name string) string {
	return '${normalize_module_name(mod_name)}:${name}'
}

fn (mut w Walker) collect_const_fn_value_aliases() {
	for file in w.files {
		mod_name := normalize_module_name(file.mod)
		prev_fn_file := w.cur_fn_file
		w.cur_fn_file = file.name
		for stmt in file.stmts {
			if !stmt_ok(stmt) {
				continue
			}
			if stmt is ast.ConstDecl {
				for field in stmt.fields {
					if field.name == '' {
						continue
					}
					if field.value is ast.Ident
						&& w.ident_resolves_to_fn_value(field.value.name, mod_name) {
						w.const_fn_value_aliases[const_fn_value_alias_key(mod_name, field.name)] = field.value.name
					} else if field.value is ast.SelectorExpr {
						target := w.selector_fn_value_target(field.value)
						if target != '' {
							w.const_fn_value_aliases[const_fn_value_alias_key(mod_name, field.name)] = target
						}
					}
				}
			}
		}
		w.cur_fn_file = prev_fn_file
	}
}

fn (mut w Walker) collect_const_fn_value_aliases_from_flat(flat &ast.FlatAst) {
	for fi in 0 .. flat.files.len {
		fc := flat.file_cursor(fi)
		mod_name := normalize_module_name(fc.mod())
		prev_fn_file := w.cur_fn_file
		w.cur_fn_file = fc.name()
		stmts_list := fc.stmts()
		for i in 0 .. stmts_list.len() {
			c := stmts_list.at(i)
			if c.kind() != .stmt_const_decl {
				continue
			}
			fields := c.list_at(0)
			for j in 0 .. fields.len() {
				field := fields.at(j)
				name := field.name()
				if name == '' {
					continue
				}
				value_c := field.edge(0)
				if !value_c.is_valid() {
					continue
				}
				if value_c.kind() == .expr_ident
					&& w.ident_resolves_to_fn_value(value_c.name(), mod_name) {
					w.const_fn_value_aliases[const_fn_value_alias_key(mod_name, name)] =
						value_c.name()
				} else if value_c.kind() == .expr_selector {
					target := w.selector_fn_value_target_cursor(value_c)
					if target != '' {
						w.const_fn_value_aliases[const_fn_value_alias_key(mod_name, name)] = target
					}
				}
			}
		}
		w.cur_fn_file = prev_fn_file
	}
}

fn (mut w Walker) collect_imports(imports []ast.ImportStmt) {
	for imp in imports {
		w.add_module_name(imp.name)
		if imp.alias != '' {
			w.add_module_name(imp.alias)
			// Map alias to real module name for correct symbol resolution.
			// e.g., 'import v2.ssa.optimize as ssa_optimize' → 'ssa_optimize' → 'optimize'
			real_name := if imp.name.contains('.') {
				imp.name.all_after_last('.')
			} else {
				imp.name
			}
			w.module_alias_to_real[imp.alias] = real_name
		}
	}
}

fn markused_imported_symbol_fn_name(module_name string, name string) string {
	if module_name == '' || module_name == 'main' {
		return name
	}
	return '${module_name}__${name}'
}

fn selective_import_fn_key(file_name string, name string) string {
	return '${file_name}:${name}'
}

fn (mut w Walker) collect_file_imports(file_name string, imports []ast.ImportStmt) {
	w.collect_imports(imports)
	for imp in imports {
		if imp.symbols.len == 0 {
			continue
		}
		import_mod := if imp.is_aliased { imp.name.all_after_last('.') } else { imp.alias }
		for symbol in imp.symbols {
			if symbol is ast.Ident {
				w.selective_import_fn_targets[selective_import_fn_key(file_name, symbol.name)] = markused_imported_symbol_fn_name(import_mod,
					symbol.name)
			}
		}
	}
}

fn (mut w Walker) collect_def_stmt(stmt ast.Stmt, mod_name string, file_name string, decl_id ast.FlatNodeId) {
	if !stmt_ok(stmt) {
		return
	}
	match stmt {
		ast.StructDecl {
			w.add_type_name(mod_name, stmt.name)
			w.add_struct_field_receivers(mod_name, stmt)
			w.add_struct_embedded_receivers(mod_name, stmt)
			w.add_struct_fn_fields(mod_name, stmt)
		}
		ast.EnumDecl {
			w.add_type_name(mod_name, stmt.name)
		}
		ast.InterfaceDecl {
			w.add_type_name(mod_name, stmt.name)
			w.add_interface_decl(mod_name, stmt)
		}
		ast.TypeDecl {
			w.add_type_name(mod_name, stmt.name)
		}
		ast.GlobalDecl {
			w.add_global_interface_names(mod_name, stmt)
		}
		ast.FnDecl {
			if stmt.name == '' {
				return
			}
			info := FnInfo{
				key:      decl_key(mod_name, stmt, w.env)
				mod:      mod_name
				file:     file_name
				decl:     stmt
				decl_id:  decl_id
				has_body: stmt.stmts.len > 0
			}
			w.fns << info
			idx := w.fns.len - 1
			w.index_fn(idx, info)
		}
		else {}
	}
}

fn (mut w Walker) seed_roots() bool {
	mut has_root := false
	for i, info in w.fns {
		if is_main_root(info) {
			w.mark_fn(i)
			has_root = true
		}
	}
	if has_root {
		if w.opts.minimal_runtime_roots {
			w.seed_minimal_runtime_roots()
		} else {
			w.seed_generic_specialization_roots()
			w.seed_codegen_required_roots()
			// Also seed module init() functions (called from synthesized main)
			for i, info in w.fns {
				if is_module_init(info) {
					w.mark_fn(i)
				}
			}
			w.seed_top_level_initializer_roots()
			w.seed_drop_method_roots()
		}
		return true
	}
	for i, info in w.fns {
		if is_test_root(info) || is_module_init(info) {
			w.mark_fn(i)
			has_root = true
		}
	}
	if has_root {
		if w.opts.minimal_runtime_roots {
			w.seed_minimal_runtime_roots()
		} else {
			w.seed_generic_specialization_roots()
			w.seed_codegen_required_roots()
			w.seed_top_level_initializer_roots()
			w.seed_drop_method_roots()
		}
	}
	return has_root
}

fn (mut w Walker) seed_minimal_runtime_roots() {
}

fn (mut w Walker) seed_roots_from_flat(flat &ast.FlatAst) bool {
	mut has_root := false
	for i, info in w.fns {
		if is_main_root(info) {
			w.mark_fn(i)
			has_root = true
		}
	}
	if has_root {
		if !w.opts.minimal_runtime_roots {
			w.seed_generic_specialization_roots()
			w.seed_codegen_required_roots()
			// Also seed module init() functions (called from synthesized main)
			for i, info in w.fns {
				if is_module_init(info) {
					w.mark_fn(i)
				}
			}
			w.seed_top_level_initializer_roots_from_flat(flat)
			w.seed_drop_method_roots()
		}
		return true
	}
	for i, info in w.fns {
		if is_test_root(info) || is_module_init(info) {
			w.mark_fn(i)
			has_root = true
		}
	}
	if has_root {
		if !w.opts.minimal_runtime_roots {
			w.seed_generic_specialization_roots()
			w.seed_codegen_required_roots()
			w.seed_top_level_initializer_roots_from_flat(flat)
			w.seed_drop_method_roots()
		}
	}
	return has_root
}

fn (mut w Walker) seed_codegen_required_roots() {
	for i, info in w.fns {
		if w.is_codegen_required_root(info) {
			w.mark_fn(i)
		}
	}
}

// seed_drop_method_roots marks every `Type.drop` method as live for the
// types that the ownership checker recorded in `env.drop_at_fn_exit`. The
// cleanc backend will emit synthetic `Type__drop(&var)` calls at fn-exit
// for those bindings; without seeding the methods here, dead-code elim
// would strip the bodies and leave the linker with unresolved symbols.
fn (mut w Walker) seed_drop_method_roots() {
	if w.env == unsafe { nil } {
		return
	}
	mut seen := map[string]bool{}
	for _, entries in w.env.drop_at_fn_exit {
		for entry in entries {
			if entry.type_name.len == 0 || entry.type_name in seen {
				continue
			}
			seen[entry.type_name] = true
			w.mark_method_name('drop', [entry.type_name])
		}
	}
}

fn (w &Walker) is_codegen_required_root(info FnInfo) bool {
	decl := info.decl
	if !w.opts.minimal_runtime_roots && should_always_emit_for_markused(info.file) {
		return true
	}
	if !w.opts.minimal_runtime_roots && info.mod == 'builtin' && decl.name == 'print_backtrace' {
		return true
	}
	if info.mod == 'json2' && decl.name == 'enum_uses_json_as_number' {
		return true
	}
	if info.mod == 'time' && decl.is_method && decl.receiver.typ is ast.Ident
		&& decl.receiver.typ.name == 'Duration' {
		return true
	}
	if info.mod == 'sync' {
		if decl.name in ['try_pop_priv', 'try_push_priv', 'new_channel_st', 'new_spin_lock'] {
			return true
		}
		if decl.is_method && decl.name == 'try_wait' {
			return true
		}
	}
	if is_builtin_map_file(info.file) && should_keep_builtin_map_decl(decl) {
		return true
	}
	if is_builtin_string_file(info.file) && should_keep_builtin_string_decl(decl) {
		return true
	}
	if is_builtin_array_file(info.file) && should_keep_builtin_array_decl(decl) {
		return true
	}
	if decl.name == 'str' || decl.name.ends_with('__str') {
		return true
	}
	if decl.name.starts_with('__sort_cmp_') {
		return true
	}
	if decl.is_method {
		if decl.name in ['+', '-', '*', '/', '%', '==', '!=', '<', '>', '<=', '>='] {
			return true
		}
		if decl.receiver.typ is ast.Type && decl.receiver.typ is ast.ArrayType {
			return true
		}
	}
	return false
}

pub fn should_always_emit_for_markused(path string) bool {
	if path.ends_with('.vh') {
		return true
	}
	return is_builtin_runtime_keep_file(path)
}

pub fn is_builtin_runtime_keep_file(path string) bool {
	normalized := path.replace('\\', '/')
	return normalized.ends_with('vlib/builtin/map.c.v')
		|| normalized.ends_with('vlib/builtin/builtin.c.v')
		|| normalized.ends_with('vlib/builtin/builtin.v')
		|| normalized.ends_with('vlib/builtin/cfns_wrapper.c.v')
		|| normalized.ends_with('vlib/builtin/allocation.c.v')
		|| normalized.ends_with('vlib/builtin/prealloc.c.v')
		|| normalized.ends_with('vlib/builtin/panicing.c.v')
		|| normalized.ends_with('vlib/builtin/chan_option_result.v')
		|| normalized.ends_with('vlib/builtin/int.v') || normalized.ends_with('vlib/builtin/rune.v')
		|| normalized.ends_with('vlib/builtin/float.c.v')
		|| normalized.ends_with('vlib/builtin/utf8.v')
		|| normalized.ends_with('vlib/builtin/utf8.c.v')
		|| normalized.ends_with('vlib/strings/builder.c.v')
		|| normalized.ends_with('vlib/strconv/utilities.v')
		|| normalized.ends_with('vlib/strconv/utilities.c.v')
		|| normalized.ends_with('vlib/strconv/ftoa.c.v')
		|| normalized.ends_with('vlib/strconv/f32_str.c.v')
		|| normalized.ends_with('vlib/strconv/f64_str.c.v')
		|| normalized.ends_with('vlib/math/bits/bits.v')
		|| normalized.ends_with('vlib/math/bits/bits.c.v')
		|| normalized.ends_with('vlib/sokol/memory/memory.c.v')
}

pub fn is_builtin_map_file(path string) bool {
	normalized := path.replace('\\', '/')
	return normalized.ends_with('vlib/builtin/map.v')
}

pub fn should_keep_builtin_map_decl(decl ast.FnDecl) bool {
	base_keep := decl.name in ['new_map', 'move', 'clear', 'key_to_index', 'meta_less',
		'meta_greater', 'ensure_extra_metas', 'ensure_extra_metas_grow', 'set', 'expand', 'rehash',
		'reserve', 'cached_rehash', 'get_and_set', 'get', 'get_check', 'exists', 'delete', 'keys',
		'values', 'clone', 'free', 'key', 'value', 'has_index', 'zeros_to_end', 'new_dense_array']
	return base_keep || decl.name.starts_with('map_eq_') || decl.name.starts_with('map_clone_')
		|| decl.name.starts_with('map_free_')
}

pub fn is_builtin_string_file(path string) bool {
	normalized := path.replace('\\', '/')
	return normalized.ends_with('vlib/builtin/string.v')
}

pub fn should_keep_builtin_string_decl(decl ast.FnDecl) bool {
	return decl.name in ['eq', 'plus', 'plus_two', 'substr', 'substr_unsafe', 'repeat', 'free',
		'vstring', 'vstring_with_len', 'vstring_literal', 'vstring_literal_with_len', 'runes',
		'join', 'compare_strings', 'compare_strings_by_len', 'compare_lower_strings']
}

pub fn is_builtin_array_file(path string) bool {
	normalized := path.replace('\\', '/')
	return normalized.ends_with('vlib/builtin/array.v')
}

pub fn should_keep_builtin_array_decl(decl ast.FnDecl) bool {
	return decl.name in [
		'__new_array',
		'__new_array_with_default',
		'__new_array_with_multi_default',
		'__new_array_with_array_default',
		'__new_array_with_map_default',
		'new_array_from_c_array',
		'new_array_from_c_array_no_alloc',
		'new_array_from_array_and_c_array',
		'ensure_cap',
		'repeat',
		'repeat_to_depth',
		'insert',
		'insert_many',
		'prepend',
		'prepend_many',
		'delete',
		'delete_many',
		'clear',
		'reset',
		'trim',
		'drop',
		'get_unsafe',
		'get',
		'get_with_check',
		'first',
		'last',
		'pop_left',
		'pop',
		'delete_last',
		'slice',
		'slice_ni',
		'contains',
		'clone_static_to_depth',
		'clone',
		'clone_to_depth',
		'set_unsafe',
		'set',
		'push',
		'push_many',
		'reverse_in_place',
		'reverse',
		'free',
		'sort',
		'sort_with_compare',
		'sorted_with_compare',
		'copy',
		'write_u8',
		'write_rune',
		'write_string',
		'write_ptr',
		'grow_cap',
		'grow_len',
		'pointers',
		'panic_on_negative_len',
		'panic_on_negative_cap',
	]
}

fn (mut w Walker) seed_top_level_initializer_roots() {
	for file in w.files {
		mod_name := normalize_module_name(file.mod)
		for stmt in file.stmts {
			if !stmt_ok(stmt) {
				continue
			}
			match stmt {
				ast.ConstDecl, ast.GlobalDecl {
					w.walk_stmt(stmt, mod_name)
				}
				else {}
			}
		}
	}
}

fn (mut w Walker) seed_top_level_initializer_roots_from_flat(flat &ast.FlatAst) {
	for i in 0 .. flat.files.len {
		fc := flat.file_cursor(i)
		mod_name := normalize_module_name(fc.mod())
		stmts := fc.stmts()
		for j in 0 .. stmts.len() {
			stmt_c := stmts.at(j)
			match stmt_c.kind() {
				.stmt_const_decl, .stmt_global_decl {
					w.walk_stmt_cursor(stmt_c, mod_name)
				}
				else {}
			}
		}
	}
}

fn generic_base_name_from_key(key string) string {
	mut name := key
	bracket_idx := name.index_u8(`[`)
	if bracket_idx > 0 {
		name = name[..bracket_idx]
	}
	dot_idx := name.last_index_u8(`.`)
	if dot_idx > 0 && dot_idx < name.len - 1 {
		name = name[dot_idx + 1..]
	}
	return strip_generic_specialization_suffix(name)
}

fn (mut w Walker) seed_generic_specialization_roots() {
	if w.env == unsafe { nil } {
		return
	}
	for key, _ in w.env.generic_types {
		base_name := generic_base_name_from_key(key)
		if base_name == '' {
			continue
		}
		w.mark_lookup('fn:${base_name}')
		w.mark_lookup('mname:${base_name}')
	}
}

fn is_main_root(info FnInfo) bool {
	return !is_method_decl(info.decl) && info.mod == 'main' && info.decl.name == 'main'
}

fn is_test_root(info FnInfo) bool {
	return !is_method_decl(info.decl) && info.decl.name.starts_with('test_')
		&& info.decl.typ.params.len == 0
}

fn is_method_decl(decl ast.FnDecl) bool {
	return decl.is_method || decl.is_static
}

fn is_module_init(info FnInfo) bool {
	return !is_method_decl(info.decl) && (info.decl.name == 'init'
		|| info.decl.name == 'deinit'
		|| info.decl.name.starts_with('__v_init_consts_'))
}

fn normalize_module_name(module_name string) string {
	if module_name == '' {
		return 'main'
	}
	return module_name
}

fn sanitize_receiver_name(name string) string {
	if !string_ok(name) {
		return ''
	}
	mut out := name.trim_space()
	for out.len > 0 && (out[0] == `&` || out[0] == `?` || out[0] == `!`) {
		out = out[1..]
	}
	return out
}

fn maybe_trim_module_prefix(mod_name string, name string) string {
	if mod_name == '' || mod_name == 'main' {
		return name
	}
	prefix := '${mod_name}__'
	if name.starts_with(prefix) {
		return name[prefix.len..]
	}
	return name
}

fn add_unique_string(mut out []string, value string) {
	if value == '' {
		return
	}
	for existing in out {
		if existing == value {
			return
		}
	}
	out << value
}

fn add_unique_int(mut out []int, value int) {
	for existing in out {
		if existing == value {
			return
		}
	}
	out << value
}

fn add_unique_fn_name(mut out []string, value string) {
	if value == '' {
		return
	}
	for existing in out {
		if existing == value {
			return
		}
	}
	out << value
}

fn type_name_candidates_from_type(mod_name string, typ types.Type) []string {
	mut out := []string{}
	names := [typ.name(), typ.base_type().name()]
	for raw_name in names {
		name := sanitize_receiver_name(raw_name)
		add_unique_string(mut out, name)
		add_unique_string(mut out, maybe_trim_module_prefix(mod_name, name))
		if name.contains('__') {
			add_unique_string(mut out, name.all_after_last('__'))
		} else if mod_name != '' && mod_name != 'main' {
			add_unique_string(mut out, '${mod_name}__${name}')
		}
		// For array types like []Attribute, also add Array_Attribute form.
		if name.starts_with('[]') {
			elem := name[2..]
			add_unique_string(mut out, 'Array_${elem}')
			add_unique_string(mut out, maybe_trim_module_prefix(mod_name, 'Array_${elem}'))
			if mod_name != '' && mod_name != 'main' {
				add_unique_string(mut out, 'Array_${mod_name}__${elem}')
			}
		}
	}
	return out
}

fn type_is_fn_value(typ types.Type) bool {
	return type_is_fn_value_with_depth(typ, 0)
}

fn type_is_fn_value_with_depth(typ types.Type, depth int) bool {
	if depth > 16 || !type_ok(typ) {
		return false
	}
	match typ {
		types.FnType {
			return true
		}
		types.Alias {
			return type_is_fn_value_with_depth(typ.base_type, depth + 1)
		}
		else {
			return false
		}
	}
}

fn (w &Walker) type_name_is_fn_value(name string, mod_name string) bool {
	if w.env == unsafe { nil } || name == '' || !string_ok(name) {
		return false
	}
	mut type_names := []string{}
	add_unique_string(mut type_names, name)
	add_unique_string(mut type_names, maybe_trim_module_prefix(mod_name, name))
	if name.contains('__') {
		add_unique_string(mut type_names, name.all_after_last('__'))
	} else if mod_name != '' && mod_name != 'main' {
		add_unique_string(mut type_names, '${mod_name}__${name}')
	}
	mut scope_names := []string{}
	add_unique_string(mut scope_names, mod_name)
	if name.contains('__') {
		add_unique_string(mut scope_names, name.all_before('__'))
	}
	add_unique_string(mut scope_names, 'builtin')
	for scope_name in scope_names {
		if scope_name == '' {
			continue
		}
		if scope := w.env.get_scope(scope_name) {
			for type_name in type_names {
				if typ := scope.lookup_type_parent(type_name, 0) {
					if type_is_fn_value(typ) {
						return true
					}
				}
			}
		}
	}
	return false
}

fn (w &Walker) type_expr_is_fn_value(expr ast.Expr, mod_name string) bool {
	match expr {
		ast.Ident {
			return w.type_name_is_fn_value(expr.name, mod_name)
		}
		ast.SelectorExpr {
			if expr.lhs is ast.Ident {
				lhs_name := expr.lhs.name
				real_mod := w.module_alias_to_real[lhs_name] or { lhs_name }
				return w.type_name_is_fn_value(expr.rhs.name, real_mod)
					|| w.type_name_is_fn_value('${real_mod}__${expr.rhs.name}', real_mod)
			}
		}
		ast.PrefixExpr {
			if expr.op == .amp {
				return w.type_expr_is_fn_value(expr.expr, mod_name)
			}
		}
		ast.ModifierExpr {
			return w.type_expr_is_fn_value(expr.expr, mod_name)
		}
		ast.GenericArgs {
			return w.type_expr_is_fn_value(expr.lhs, mod_name)
		}
		ast.GenericArgOrIndexExpr {
			return w.type_expr_is_fn_value(expr.lhs, mod_name)
		}
		ast.Type {
			match expr {
				ast.FnType {
					return true
				}
				ast.PointerType {
					return w.type_expr_is_fn_value(expr.base_type, mod_name)
				}
				ast.GenericType {
					return w.type_expr_is_fn_value(expr.name, mod_name)
				}
				else {}
			}
		}
		else {}
	}

	return false
}

fn (w &Walker) expr_has_fn_value_type(expr ast.Expr) bool {
	if w.env == unsafe { nil } || !expr_ok(expr) {
		return false
	}
	pos := expr.pos()
	if !pos.is_valid() {
		return false
	}
	if typ := w.env.get_expr_type(pos.id) {
		return type_is_fn_value(typ)
	}
	return false
}

fn (w &Walker) cursor_has_fn_value_type(c ast.Cursor) bool {
	if w.env == unsafe { nil } || !c.is_valid() {
		return false
	}
	pos := c.pos()
	if !pos.is_valid() {
		return false
	}
	if typ := w.env.get_expr_type(pos.id) {
		return type_is_fn_value(typ)
	}
	return false
}

fn add_receiver_name_candidates(mut out []string, mod_name string, raw_name string) {
	name := sanitize_receiver_name(raw_name)
	if name == '' {
		return
	}
	add_unique_string(mut out, name)
	add_unique_string(mut out, maybe_trim_module_prefix(mod_name, name))
	if name.contains('__') {
		add_unique_string(mut out, name.all_after_last('__'))
	} else if mod_name != '' && mod_name != 'main' {
		add_unique_string(mut out, '${mod_name}__${name}')
	}
}

fn receiver_type_expr_name(expr ast.Expr) string {
	match expr {
		ast.Ident {
			return expr.name
		}
		ast.SelectorExpr {
			return expr.rhs.name
		}
		ast.PrefixExpr {
			if expr.op == .amp {
				return receiver_type_expr_name(expr.expr)
			}
		}
		ast.ModifierExpr {
			return receiver_type_expr_name(expr.expr)
		}
		ast.GenericArgs {
			return receiver_type_expr_name(expr.lhs)
		}
		ast.GenericArgOrIndexExpr {
			return receiver_type_expr_name(expr.lhs)
		}
		ast.Type {
			match expr {
				ast.PointerType {
					return receiver_type_expr_name(expr.base_type)
				}
				ast.ArrayType {
					elem_name := receiver_type_expr_name(expr.elem_type)
					if elem_name != '' {
						return 'Array_${elem_name}'
					}
				}
				ast.ArrayFixedType {
					elem_name := receiver_type_expr_name(expr.elem_type)
					len_name := receiver_fixed_array_len_name(expr.len)
					if elem_name != '' && len_name != '' {
						return 'Array_fixed_${elem_name}_${len_name}'
					}
				}
				ast.GenericType {
					return receiver_type_expr_name(expr.name)
				}
				else {}
			}
		}
		else {}
	}

	return ''
}

fn receiver_fixed_array_len_name(expr ast.Expr) string {
	match expr {
		ast.BasicLiteral {
			return expr.value
		}
		ast.Ident {
			return expr.name
		}
		else {
			return expr.name()
		}
	}
}

fn type_expr_receiver_candidates(mod_name string, expr ast.Expr) []string {
	mut out := []string{}
	match expr {
		ast.SelectorExpr {
			if expr.lhs is ast.Ident {
				add_receiver_name_candidates(mut out, mod_name, expr.rhs.name)
				add_unique_string(mut out, '${expr.lhs.name}__${expr.rhs.name}')
			}
		}
		ast.PrefixExpr {
			if expr.op == .amp {
				return type_expr_receiver_candidates(mod_name, expr.expr)
			}
		}
		ast.ModifierExpr {
			return type_expr_receiver_candidates(mod_name, expr.expr)
		}
		ast.GenericArgs {
			return type_expr_receiver_candidates(mod_name, expr.lhs)
		}
		ast.GenericArgOrIndexExpr {
			return type_expr_receiver_candidates(mod_name, expr.lhs)
		}
		ast.Type {
			match expr {
				ast.PointerType {
					return type_expr_receiver_candidates(mod_name, expr.base_type)
				}
				ast.GenericType {
					return type_expr_receiver_candidates(mod_name, expr.name)
				}
				else {}
			}
		}
		else {}
	}

	name := receiver_type_expr_name(expr)
	if name != '' {
		add_receiver_name_candidates(mut out, mod_name, name)
	}
	return out
}

fn type_expr_receiver_candidates_cursor(c ast.Cursor, mod_name string) []string {
	mut out := []string{}
	if !c.is_valid() {
		return out
	}
	match c.kind() {
		.expr_selector {
			lhs_c := c.edge(0)
			rhs_c := c.edge(1)
			if lhs_c.is_valid() && lhs_c.kind() == .expr_ident && rhs_c.is_valid() {
				add_receiver_name_candidates(mut out, mod_name, rhs_c.name())
				add_unique_string(mut out, '${lhs_c.name()}__${rhs_c.name()}')
			}
		}
		.expr_prefix, .expr_modifier, .typ_pointer, .typ_generic {
			return type_expr_receiver_candidates_cursor(c.edge(0), mod_name)
		}
		.expr_ident {
			add_receiver_name_candidates(mut out, mod_name, c.name())
		}
		else {}
	}

	return out
}

fn receiver_names_from_decl(mod_name string, decl ast.FnDecl, env &types.Environment) []string {
	mut out := []string{}
	add_receiver_name_candidates(mut out, mod_name, receiver_type_expr_name(decl.receiver.typ))
	match decl.receiver.typ {
		ast.Ident {
			name := sanitize_receiver_name(decl.receiver.typ.name)
			add_unique_string(mut out, name)
			add_unique_string(mut out, maybe_trim_module_prefix(mod_name, name))
			if mod_name != '' && mod_name != 'main' {
				add_unique_string(mut out, '${mod_name}__${name}')
			}
		}
		ast.PrefixExpr {
			if decl.receiver.typ.expr is ast.Ident {
				name := sanitize_receiver_name(decl.receiver.typ.expr.name)
				add_unique_string(mut out, name)
				add_unique_string(mut out, maybe_trim_module_prefix(mod_name, name))
				if mod_name != '' && mod_name != 'main' {
					add_unique_string(mut out, '${mod_name}__${name}')
				}
			}
		}
		else {}
	}

	// Method on []ElemType — receiver name is Array_ElemType.
	// Use the string name which includes [] prefix for array types.
	recv_name := decl.receiver.typ.name()
	if recv_name.starts_with('[]') {
		elem_name := recv_name[2..]
		if elem_name.len > 0 {
			arr_name := 'Array_${elem_name}'
			add_unique_string(mut out, arr_name)
			add_unique_string(mut out, maybe_trim_module_prefix(mod_name, arr_name))
			if mod_name != '' && mod_name != 'main' {
				add_unique_string(mut out, 'Array_${mod_name}__${elem_name}')
			}
		}
	}
	pos := decl.receiver.typ.pos()
	if env != unsafe { nil } && pos.is_valid() {
		if receiver_type := env.get_expr_type(pos.id) {
			for name in type_name_candidates_from_type(mod_name, receiver_type) {
				add_unique_string(mut out, name)
			}
		}
	}
	return out
}

fn receiver_primary_name(mod_name string, decl ast.FnDecl, env &types.Environment) string {
	names := receiver_names_from_decl(mod_name, decl, env)
	if names.len > 0 {
		return names[0]
	}
	return 'unknown'
}

fn normalize_method_name(name string) string {
	if !string_ok(name) {
		return ''
	}
	return match name {
		'+' { 'plus' }
		'-' { 'minus' }
		'*' { 'mul' }
		'/' { 'div' }
		'%' { 'mod' }
		'==' { 'eq' }
		'!=' { 'ne' }
		'<' { 'lt' }
		'>' { 'gt' }
		'<=' { 'le' }
		'>=' { 'ge' }
		else { name }
	}
}

fn (w &Walker) lookup_fn_scope(info FnInfo) &types.Scope {
	if w.env == unsafe { nil } {
		return unsafe { nil }
	}
	decl := info.decl
	mod_name := info.mod
	if !is_method_decl(decl) {
		if scope := w.env.get_fn_scope(mod_name, decl.name) {
			return scope
		}
		return unsafe { nil }
	}
	receivers := receiver_names_from_decl(mod_name, decl, w.env)
	for recv in receivers {
		if scope := w.env.get_fn_scope(mod_name, '${recv}__${decl.name}') {
			return scope
		}
	}
	return unsafe { nil }
}

fn (mut w Walker) add_lookup(key string, idx int) {
	if key == '' {
		return
	}
	if key !in w.lookup {
		w.lookup[key] = [idx]
		return
	}
	mut values := w.lookup[key]
	add_unique_int(mut values, idx)
	w.lookup[key] = values
}

fn (mut w Walker) index_fn(idx int, info FnInfo) {
	decl := info.decl
	mod_name := info.mod
	if is_method_decl(decl) {
		mut method_names := []string{}
		add_unique_string(mut method_names, decl.name)
		add_unique_string(mut method_names, normalize_method_name(decl.name))
		receivers := receiver_names_from_decl(mod_name, decl, w.env)
		for method_name in method_names {
			w.add_lookup('mname:${method_name}', idx)
			for receiver_name in receivers {
				w.add_method_receiver(receiver_name, idx)
				if receiver_name.contains('__') {
					w.add_method_receiver(receiver_name.all_after_last('__'), idx)
				}
				w.add_lookup('meth:${receiver_name}:${method_name}', idx)
				w.add_lookup('fn:${receiver_name}__${method_name}', idx)
				w.add_lookup('mod:${mod_name}:${receiver_name}__${method_name}', idx)
			}
		}
		return
	}
	w.add_lookup('fn:${decl.name}', idx)
	w.add_lookup('mod:${mod_name}:${decl.name}', idx)
	if mod_name != '' && mod_name != 'main' && mod_name != 'builtin' {
		w.add_lookup('fn:${mod_name}__${decl.name}', idx)
	}
}

fn (mut w Walker) add_module_name(name string) {
	if name == '' {
		return
	}
	w.module_names[name] = true
	if name.contains('.') {
		w.module_names[name.all_after_last('.')] = true
	}
}

fn (mut w Walker) add_type_name(mod_name string, name string) {
	if name == '' {
		return
	}
	w.type_names[name] = true
	trimmed := maybe_trim_module_prefix(mod_name, name)
	w.type_names[trimmed] = true
	if mod_name != '' && mod_name != 'main' {
		w.type_names['${mod_name}__${trimmed}'] = true
	}
}

fn (mut w Walker) add_interface_name(mod_name string, name string) {
	if name == '' {
		return
	}
	w.interface_type_names[name] = true
	trimmed := maybe_trim_module_prefix(mod_name, name)
	w.interface_type_names[trimmed] = true
	if mod_name != '' && mod_name != 'main' {
		w.interface_type_names['${mod_name}__${trimmed}'] = true
	}
}

fn (mut w Walker) add_interface_decl(mod_name string, decl ast.InterfaceDecl) {
	w.add_interface_name(mod_name, decl.name)
	mut method_names := []string{}
	for field in decl.fields {
		if field.is_interface_method && field.typ is ast.Type && field.typ is ast.FnType {
			add_unique_fn_name(mut method_names, field.name)
			add_unique_fn_name(mut method_names, normalize_method_name(field.name))
		}
	}
	mut embedded_names := []string{}
	for embedded in decl.embedded {
		for embedded_name in type_expr_receiver_candidates(mod_name, embedded) {
			add_unique_string(mut embedded_names, embedded_name)
		}
	}
	mut iface_names := []string{}
	add_unique_string(mut iface_names, decl.name)
	add_unique_string(mut iface_names, maybe_trim_module_prefix(mod_name, decl.name))
	if mod_name != '' && mod_name != 'main' {
		trimmed := maybe_trim_module_prefix(mod_name, decl.name)
		add_unique_string(mut iface_names, '${mod_name}__${trimmed}')
	}
	for iface_name in iface_names {
		if method_names.len > 0 {
			if iface_name in w.interface_method_names {
				mut existing := w.interface_method_names[iface_name]
				for method_name in method_names {
					add_unique_fn_name(mut existing, method_name)
				}
				w.interface_method_names[iface_name] = existing
			} else {
				w.interface_method_names[iface_name] = method_names.clone()
			}
		}
		if embedded_names.len > 0 {
			mut existing := w.interface_embedded_names[iface_name] or { []string{} }
			for embedded_name in embedded_names {
				add_unique_string(mut existing, embedded_name)
			}
			w.interface_embedded_names[iface_name] = existing
		}
	}
}

fn (mut w Walker) add_struct_field_receivers(mod_name string, decl ast.StructDecl) {
	mut struct_names := []string{}
	add_receiver_name_candidates(mut struct_names, mod_name, decl.name)
	for field in decl.fields {
		field_receivers := type_expr_receiver_candidates(mod_name, field.typ)
		if field_receivers.len == 0 {
			continue
		}
		for struct_name in struct_names {
			key := '${struct_name}:${field.name}'
			mut existing := w.struct_field_receivers[key] or { []string{} }
			for receiver in field_receivers {
				add_unique_string(mut existing, receiver)
			}
			w.struct_field_receivers[key] = existing
		}
	}
}

fn (mut w Walker) add_struct_embedded_receivers(mod_name string, decl ast.StructDecl) {
	if decl.embedded.len == 0 {
		return
	}
	mut struct_names := []string{}
	add_receiver_name_candidates(mut struct_names, mod_name, decl.name)
	for embedded in decl.embedded {
		embedded_receivers := type_expr_receiver_candidates(mod_name, embedded)
		if embedded_receivers.len == 0 {
			continue
		}
		for struct_name in struct_names {
			mut existing := w.struct_embedded_receivers[struct_name] or { []string{} }
			for receiver in embedded_receivers {
				add_unique_string(mut existing, receiver)
			}
			w.struct_embedded_receivers[struct_name] = existing
		}
	}
}

fn (mut w Walker) add_struct_fn_fields(mod_name string, decl ast.StructDecl) {
	mut struct_names := []string{}
	add_receiver_name_candidates(mut struct_names, mod_name, decl.name)
	for field in decl.fields {
		if !w.type_expr_is_fn_value(field.typ, mod_name) {
			continue
		}
		for struct_name in struct_names {
			w.struct_fn_fields['${struct_name}:${field.name}'] = true
		}
	}
}

fn (w &Walker) init_field_is_fn_value(init_typ ast.Expr, field_name string, mod_name string) bool {
	if field_name == '' {
		return false
	}
	for struct_name in type_expr_receiver_candidates(mod_name, init_typ) {
		if w.struct_fn_fields['${struct_name}:${field_name}'] {
			return true
		}
	}
	return false
}

fn (w &Walker) init_field_is_fn_value_cursor(init_typ ast.Cursor, field_name string, mod_name string) bool {
	if field_name == '' {
		return false
	}
	for struct_name in type_expr_receiver_candidates_cursor(init_typ, mod_name) {
		if w.struct_fn_fields['${struct_name}:${field_name}'] {
			return true
		}
	}
	return false
}

fn (mut w Walker) add_global_interface_names(mod_name string, decl ast.GlobalDecl) {
	for field in decl.fields {
		iface_name := w.interface_name_from_type_expr(field.typ, mod_name)
		if iface_name == '' {
			continue
		}
		w.global_interface_names[field.name] = iface_name
		if mod_name != '' && mod_name != 'main' && mod_name != 'builtin' {
			w.global_interface_names['${mod_name}__${field.name}'] = iface_name
		}
	}
}

fn (mut w Walker) add_method_receiver(receiver_name string, idx int) {
	if receiver_name == '' {
		return
	}
	if receiver_name !in w.methods_by_receiver {
		w.methods_by_receiver[receiver_name] = [idx]
		return
	}
	mut values := w.methods_by_receiver[receiver_name]
	add_unique_int(mut values, idx)
	w.methods_by_receiver[receiver_name] = values
}

fn (mut w Walker) mark_fn(idx int) {
	if idx < 0 || idx >= w.fns.len {
		return
	}
	info := w.fns[idx]
	key := info.key
	was_used := key in w.used_keys
	if !was_used {
		w.used_keys[key] = true
		w.mark_generic_binding_receiver_methods(info)
	}
	if idx in w.queued_fn_indices {
		return
	}
	w.queued_fn_indices[idx] = true
	if info.has_body {
		w.queue << idx
	}
}

fn (mut w Walker) mark_generic_binding_receiver_methods(info FnInfo) {
	if w.env == unsafe { nil } || info.decl.typ.generic_params.len == 0 {
		return
	}
	for key, bindings_list in w.env.generic_types {
		if generic_base_name_from_key(key) != info.decl.name {
			continue
		}
		for bindings in bindings_list {
			for _, concrete in bindings {
				receivers := type_name_candidates_from_type(info.mod, concrete)
				w.mark_all_methods_for_receivers(receivers)
			}
		}
	}
}

fn (w &Walker) lookup_count(key string) int {
	if !string_ok(key) {
		return 0
	}
	if key in w.lookup {
		return w.lookup[key].len
	}
	return 0
}

fn (mut w Walker) mark_lookup(key string) {
	if !string_ok(key) {
		return
	}
	if key in w.lookup {
		for idx in w.lookup[key] {
			w.mark_fn(idx)
		}
	}
}

fn called_fn_name_candidates(name string) []string {
	mut out := []string{}
	if name == '' || !string_ok(name) {
		return out
	}
	add_unique_string(mut out, name)
	generic_base := strip_generic_specialization_suffix(name)
	if generic_base != name {
		add_unique_string(mut out, generic_base)
	}
	if name == 'builtin__new_array_from_c_array_noscan' {
		add_unique_string(mut out, 'new_array_from_c_array')
		return out
	}
	if name == 'builtin__new_array_from_array_and_c_array' {
		add_unique_string(mut out, 'new_array_from_array_and_c_array')
		return out
	}
	if name == 'builtin__array_push_noscan' || name == 'builtin__array__push_noscan' {
		add_unique_string(mut out, 'array__push_noscan')
		return out
	}
	if name.starts_with('builtin__') {
		base := name['builtin__'.len..]
		add_unique_string(mut out, base)
	}
	if name == 'gen_v__new_gen' {
		add_unique_string(mut out, 'v__new_gen')
	}
	if name.starts_with('strings__Builder__') {
		method_name := name.all_after_last('__')
		add_unique_string(mut out, 'array__${method_name}')
	}
	return out
}

fn strip_generic_specialization_suffix(name string) string {
	if idx := name.index('_T_') {
		if idx > 0 {
			return name[..idx]
		}
	}
	return name
}

fn should_mark_ident_as_fn(name string) bool {
	if name == '' || !string_ok(name) {
		return false
	}
	return name.starts_with('map_') || name.starts_with('map__') || name.starts_with('new_map')
		|| name.starts_with('array__') || name.starts_with('IError_') || name.starts_with('Error__')
		|| name.starts_with('MessageError__') || name.starts_with('_option_')
		|| name.starts_with('_result_')
}

fn (w &Walker) ident_resolves_to_fn_value(name string, mod_name string) bool {
	if name == '' || !string_ok(name) || name == 'C' || name in w.module_names
		|| w.is_cast_type_name(name) {
		return false
	}
	if w.cur_fn_scope != unsafe { nil } {
		if obj := w.cur_fn_scope.lookup_parent(name, 0) {
			return obj is types.Fn
		}
	}
	candidates := called_fn_name_candidates(name)
	if _ := w.exact_mangled_fn_lookup_key(name) {
		return true
	}
	for candidate in candidates {
		if _ := w.current_module_fn_lookup_key(candidate, mod_name) {
			return true
		}
	}
	for candidate in candidates {
		if _ := w.selective_import_fn_lookup_key(candidate) {
			return true
		}
	}
	for candidate in candidates {
		if w.lookup_count('mod:${mod_name}:${candidate}') > 0
			|| w.lookup_count('fn:${candidate}') > 0 {
			return true
		}
		if !candidate.contains('__') && mod_name != '' && mod_name != 'main'
			&& mod_name != 'builtin' && w.lookup_count('fn:${mod_name}__${candidate}') > 0 {
			return true
		}
	}
	return false
}

fn (w &Walker) current_module_fn_lookup_key(name string, mod_name string) ?string {
	key := 'mod:${mod_name}:${name}'
	if w.lookup_count(key) > 0 {
		return key
	}
	return none
}

fn (w &Walker) exact_mangled_fn_lookup_key(name string) ?string {
	if !name.contains('__') {
		return none
	}
	key := 'fn:${name}'
	if w.lookup_count(key) > 0 {
		return key
	}
	return none
}

fn (w &Walker) selective_import_fn_lookup_key(name string) ?string {
	if w.cur_fn_file == '' {
		return none
	}
	if target := w.selective_import_fn_targets[selective_import_fn_key(w.cur_fn_file, name)] {
		key := 'fn:${target}'
		if w.lookup_count(key) > 0 {
			return key
		}
	}
	return none
}

fn (mut w Walker) mark_fn_name(name string, mod_name string) {
	if name == '' || !string_ok(name) || !string_ok(mod_name) {
		return
	}
	if w.opts.minimal_runtime_roots && name in ['array__eq', 'builtin__array__eq'] {
		w.mark_fn_name('map_map_eq', 'builtin')
	}
	candidates := called_fn_name_candidates(name)
	if key := w.exact_mangled_fn_lookup_key(name) {
		w.mark_lookup(key)
		return
	}
	for candidate in candidates {
		if key := w.current_module_fn_lookup_key(candidate, mod_name) {
			w.mark_lookup(key)
			return
		}
	}
	for candidate in candidates {
		if key := w.selective_import_fn_lookup_key(candidate) {
			w.mark_lookup(key)
			return
		}
	}
	for candidate in candidates {
		w.mark_lookup('mod:${mod_name}:${candidate}')
		w.mark_lookup('fn:${candidate}')
		if !candidate.contains('__') && mod_name != '' && mod_name != 'main'
			&& mod_name != 'builtin' {
			w.mark_lookup('fn:${mod_name}__${candidate}')
		}
	}
}

fn (mut w Walker) mark_method_name(name string, receivers []string) {
	if name == '' || !string_ok(name) {
		return
	}
	normalized := normalize_method_name(name)
	for receiver in receivers {
		if !string_ok(receiver) {
			continue
		}
		for candidate in receiver_lookup_candidates(receiver) {
			w.mark_lookup('meth:${candidate}:${name}')
			if normalized != name {
				w.mark_lookup('meth:${candidate}:${normalized}')
			}
		}
	}
}

fn (w &Walker) method_lookup_count(name string, receivers []string) int {
	if name == '' || !string_ok(name) {
		return 0
	}
	normalized := normalize_method_name(name)
	mut count := 0
	for receiver in receivers {
		if !string_ok(receiver) {
			continue
		}
		for candidate in receiver_lookup_candidates(receiver) {
			count += w.lookup_count('meth:${candidate}:${name}')
			if normalized != name {
				count += w.lookup_count('meth:${candidate}:${normalized}')
			}
		}
	}
	return count
}

fn receiver_lookup_candidates(receiver string) []string {
	mut out := []string{}
	if !string_ok(receiver) {
		return out
	}
	add_unique_string(mut out, receiver)
	if receiver.contains('__') {
		add_unique_string(mut out, receiver.all_after_last('__'))
	}
	if receiver.starts_with('Array_') || receiver.starts_with('Array_fixed_') {
		add_unique_string(mut out, 'array')
	}
	if receiver.starts_with('Map_') {
		add_unique_string(mut out, 'map')
	}
	if receiver.starts_with('_option_') {
		add_unique_string(mut out, '_option')
	}
	if receiver.starts_with('_result_') {
		add_unique_string(mut out, '_result')
	}
	return out
}

fn (mut w Walker) mark_method_name_fallback(name string) {
	if name == '' || !string_ok(name) {
		return
	}
	normalized := normalize_method_name(name)
	// Check count first to avoid pulling in everything for common names.
	count := w.lookup_count('mname:${name}') +
		if normalized != name { w.lookup_count('mname:${normalized}') } else { 0 }
	if count == 0 || count > 64 {
		return
	}
	w.mark_lookup('mname:${name}')
	if normalized != name {
		w.mark_lookup('mname:${normalized}')
	}
}

fn infix_operator_may_use_method(op token.Token) bool {
	return op in [.plus, .minus, .mul, .div, .mod, .eq, .ne, .lt, .gt, .le, .ge, .pipe, .xor]
}

fn (mut w Walker) mark_infix_operator_method(expr ast.InfixExpr, mod_name string) {
	if !infix_operator_may_use_method(expr.op) {
		return
	}
	method_name := expr.op.str()
	receivers := w.receiver_candidates_for_expr(expr.lhs, mod_name)
	if receivers.len > 0 {
		w.mark_method_name(method_name, receivers)
		return
	}
	w.mark_method_name_fallback(method_name)
}

fn (mut w Walker) mark_all_methods_for_receivers(receivers []string) {
	for receiver in receivers {
		if receiver in w.methods_by_receiver {
			for idx in w.methods_by_receiver[receiver] {
				w.mark_fn(idx)
			}
		}
		if receiver.contains('__') {
			short_name := receiver.all_after_last('__')
			if short_name in w.methods_by_receiver {
				for idx in w.methods_by_receiver[short_name] {
					w.mark_fn(idx)
				}
			}
		}
	}
}

fn (mut w Walker) mark_comptime_method_loop_receivers(stmt ast.Stmt, mod_name string) {
	if stmt !is ast.ForStmt {
		return
	}
	if stmt.init !is ast.ForInStmt {
		return
	}
	for_in := stmt.init as ast.ForInStmt
	if for_in.expr !is ast.SelectorExpr {
		return
	}
	sel := for_in.expr as ast.SelectorExpr
	if sel.rhs.name != 'methods' {
		return
	}
	mut receivers := []string{}
	for receiver in type_expr_receiver_candidates(mod_name, sel.lhs) {
		add_unique_string(mut receivers, receiver)
	}
	for receiver in w.receiver_candidates_for_expr(sel.lhs, mod_name) {
		add_unique_string(mut receivers, receiver)
	}
	w.mark_all_methods_for_receivers(receivers)
}

fn (mut w Walker) mark_comptime_method_loop_receivers_cursor(stmt ast.Cursor, mod_name string) {
	if !stmt.is_valid() || stmt.kind() != .stmt_for {
		return
	}
	init := stmt.edge(0)
	if !init.is_valid() || init.kind() != .stmt_for_in {
		return
	}
	expr := init.edge(2)
	if !expr.is_valid() || expr.kind() != .expr_selector {
		return
	}
	rhs := expr.edge(1)
	if !rhs.is_valid() || rhs.name() != 'methods' {
		return
	}
	lhs := expr.edge(0)
	mut receivers := []string{}
	for receiver in type_expr_receiver_candidates_cursor(lhs, mod_name) {
		add_unique_string(mut receivers, receiver)
	}
	for receiver in w.receiver_candidates_for_cursor(lhs, mod_name) {
		add_unique_string(mut receivers, receiver)
	}
	w.mark_all_methods_for_receivers(receivers)
}

fn (w &Walker) embedded_receivers_for(receivers []string) []string {
	mut out := []string{}
	mut seen := map[string]bool{}
	mut stack := []string{}
	for receiver in receivers {
		if receiver == '' || receiver in seen {
			continue
		}
		seen[receiver] = true
		stack << receiver
	}
	for stack.len > 0 {
		receiver := stack[stack.len - 1]
		stack.delete(stack.len - 1)
		embedded := w.struct_embedded_receivers[receiver] or { continue }
		for embedded_receiver in embedded {
			add_unique_string(mut out, embedded_receiver)
			if embedded_receiver !in seen {
				seen[embedded_receiver] = true
				stack << embedded_receiver
			}
		}
	}
	return out
}

fn ierror_wrapper_base_from_ident(name string) string {
	if !name.starts_with('IError_') {
		return ''
	}
	prefix_len := 'IError_'.len
	if name.ends_with('_type_name_wrapper') {
		return name[prefix_len..name.len - '_type_name_wrapper'.len]
	}
	if name.ends_with('_msg_wrapper') {
		return name[prefix_len..name.len - '_msg_wrapper'.len]
	}
	if name.ends_with('_code_wrapper') {
		return name[prefix_len..name.len - '_code_wrapper'.len]
	}
	return ''
}

fn (mut w Walker) mark_ierror_wrapper_dependencies(name string, mod_name string) {
	base := ierror_wrapper_base_from_ident(name)
	if base == '' {
		return
	}
	w.mark_fn_name('${base}__msg', mod_name)
	w.mark_fn_name('${base}__code', mod_name)
	w.mark_fn_name('Error__code', mod_name)
}

fn (w &Walker) interface_name_from_expr(expr ast.Expr, mod_name string) string {
	match expr {
		ast.Ident {
			name := sanitize_receiver_name(expr.name)
			if name in w.interface_type_names {
				return name
			}
			if mod_name != '' && mod_name != 'main' {
				qualified := '${mod_name}__${name}'
				if qualified in w.interface_type_names {
					return qualified
				}
			}
		}
		ast.SelectorExpr {
			if expr.lhs is ast.Ident {
				mut candidates := []string{}
				add_unique_string(mut candidates, expr.rhs.name)
				add_unique_string(mut candidates, '${expr.lhs.name}__${expr.rhs.name}')
				if mod_name != '' && mod_name != 'main' {
					add_unique_string(mut candidates, '${mod_name}__${expr.rhs.name}')
				}
				for candidate in candidates {
					if candidate in w.interface_type_names {
						return candidate
					}
				}
			}
		}
		else {}
	}

	return ''
}

// interface_name_from_cursor mirrors interface_name_from_expr but reads its
// inputs from a Cursor instead of a decoded ast.Expr. interface_name_from_expr
// only ever inspects Ident and SelectorExpr variants, so the cursor version
// only needs to dispatch on .expr_ident / .expr_selector.
fn (w &Walker) interface_name_from_cursor(c ast.Cursor, mod_name string) string {
	if !c.is_valid() {
		return ''
	}
	match c.kind() {
		.expr_ident {
			name := sanitize_receiver_name(c.name())
			if name in w.interface_type_names {
				return name
			}
			if mod_name != '' && mod_name != 'main' {
				qualified := '${mod_name}__${name}'
				if qualified in w.interface_type_names {
					return qualified
				}
			}
		}
		.expr_selector {
			lhs_c := c.edge(0)
			rhs_c := c.edge(1)
			if lhs_c.is_valid() && lhs_c.kind() == .expr_ident && rhs_c.is_valid() {
				lhs_name := lhs_c.name()
				rhs_name := rhs_c.name()
				mut candidates := []string{}
				add_unique_string(mut candidates, rhs_name)
				add_unique_string(mut candidates, '${lhs_name}__${rhs_name}')
				if mod_name != '' && mod_name != 'main' {
					add_unique_string(mut candidates, '${mod_name}__${rhs_name}')
				}
				for candidate in candidates {
					if candidate in w.interface_type_names {
						return candidate
					}
				}
			}
		}
		else {}
	}

	return ''
}

fn (mut w Walker) mark_interface_conversion_methods(target_expr ast.Expr, value_expr ast.Expr, mod_name string) {
	iface_name := w.interface_name_from_expr(target_expr, mod_name)
	if iface_name == '' {
		return
	}
	w.mark_interface_conversion_methods_for_name(iface_name, value_expr, mod_name)
}

fn (w &Walker) interface_name_from_type_expr(expr ast.Expr, mod_name string) string {
	match expr {
		ast.Ident, ast.SelectorExpr {
			return w.interface_name_from_expr(expr, mod_name)
		}
		ast.PrefixExpr {
			if expr.op == .amp {
				return w.interface_name_from_type_expr(expr.expr, mod_name)
			}
		}
		ast.ModifierExpr {
			return w.interface_name_from_type_expr(expr.expr, mod_name)
		}
		ast.Type {
			match expr {
				ast.PointerType {
					return w.interface_name_from_type_expr(expr.base_type, mod_name)
				}
				ast.GenericType {
					return w.interface_name_from_type_expr(expr.name, mod_name)
				}
				else {}
			}
		}
		else {}
	}

	return ''
}

fn (w &Walker) interface_name_from_type(typ types.Type) string {
	if !type_ok(typ) {
		return ''
	}
	match typ {
		types.Interface {
			return typ.name
		}
		types.Pointer {
			if !type_ok(typ.base_type) {
				return ''
			}
			return w.interface_name_from_type(typ.base_type)
		}
		types.Alias {
			if !type_ok(typ.base_type) {
				return ''
			}
			return w.interface_name_from_type(typ.base_type)
		}
		else {}
	}

	return ''
}

fn (w &Walker) interface_name_from_expr_type(expr ast.Expr, mod_name string) string {
	if expr is ast.Ident {
		if iface_name := w.global_interface_names[expr.name] {
			return iface_name
		}
		if mod_name != '' && mod_name != 'main' && mod_name != 'builtin' {
			qualified := '${mod_name}__${expr.name}'
			if iface_name := w.global_interface_names[qualified] {
				return iface_name
			}
		}
	}
	if w.env != unsafe { nil } {
		pos := expr.pos()
		if pos.is_valid() {
			if typ := w.env.get_expr_type(pos.id) {
				return w.interface_name_from_type(typ)
			}
		}
	}
	return ''
}

fn (w &Walker) interface_method_names_for(iface_name string) []string {
	mut out := []string{}
	mut seen := map[string]bool{}
	mut stack := []string{}
	add_unique_string(mut stack, iface_name)
	for stack.len > 0 {
		current := stack[stack.len - 1]
		stack.delete(stack.len - 1)
		if current == '' || current in seen {
			continue
		}
		seen[current] = true
		method_names := w.interface_method_names[current] or { []string{} }
		for method_name in method_names {
			add_unique_fn_name(mut out, method_name)
		}
		embedded_names := w.interface_embedded_names[current] or { []string{} }
		for embedded_name in embedded_names {
			if embedded_name !in seen {
				add_unique_string(mut stack, embedded_name)
			}
		}
	}
	return out
}

fn (mut w Walker) mark_assignment_interface_conversion(lhs ast.Expr, rhs ast.Expr, mod_name string) {
	iface_name := w.interface_name_from_expr_type(lhs, mod_name)
	if iface_name == '' {
		return
	}
	w.mark_interface_conversion_methods_for_name(iface_name, rhs, mod_name)
}

// interface_name_from_expr_type_cursor mirrors interface_name_from_expr_type
// but reads from a Cursor without decoding the underlying Expr. Most lhs
// nodes are Idents, so the global_interface_names lookup short-circuits
// before any env call.
fn (w &Walker) interface_name_from_expr_type_cursor(c ast.Cursor, mod_name string) string {
	if !c.is_valid() {
		return ''
	}
	if c.kind() == .expr_ident {
		name := c.name()
		if iface_name := w.global_interface_names[name] {
			return iface_name
		}
		if mod_name != '' && mod_name != 'main' && mod_name != 'builtin' {
			qualified := '${mod_name}__${name}'
			if iface_name := w.global_interface_names[qualified] {
				return iface_name
			}
		}
	}
	if w.env != unsafe { nil } {
		pos := c.pos()
		if pos.is_valid() {
			if typ := w.env.get_expr_type(pos.id) {
				return w.interface_name_from_type(typ)
			}
		}
	}
	return ''
}

// mark_assignment_interface_conversion_cursor is the cursor analogue of
// mark_assignment_interface_conversion. The lhs cursor is inspected for
// the interface-type fast path; only when iface_name is non-empty does
// the rhs cursor get handed to the by-cursor receiver lookup.
fn (mut w Walker) mark_assignment_interface_conversion_cursor(lhs_c ast.Cursor, rhs_c ast.Cursor, mod_name string) {
	iface_name := w.interface_name_from_expr_type_cursor(lhs_c, mod_name)
	if iface_name == '' {
		return
	}
	w.mark_interface_conversion_methods_for_name_cursor(iface_name, rhs_c, mod_name)
}

// mark_interface_conversion_methods_cursor mirrors mark_interface_conversion_methods
// but accepts the value as a Cursor. target_expr stays a legacy Expr because
// callers (e.g. stmt_return) already hold one — cur_fn_decl.typ.return_type.
fn (mut w Walker) mark_interface_conversion_methods_cursor(target_expr ast.Expr, value_c ast.Cursor, mod_name string) {
	iface_name := w.interface_name_from_expr(target_expr, mod_name)
	if iface_name == '' {
		return
	}
	w.mark_interface_conversion_methods_for_name_cursor(iface_name, value_c, mod_name)
}

// mark_result_error_return_methods_cursor is the cursor analogue of
// mark_result_error_return_methods. Only fires when the function's return
// type is ResultType — in that case the receiver candidates for the
// returned value are pulled from the cursor, and 'msg' / 'code' are marked
// on each.
fn (mut w Walker) mark_result_error_return_methods_cursor(return_type ast.Expr, value_c ast.Cursor, mod_name string) {
	if return_type is ast.Type && return_type is ast.ResultType {
		receivers := w.receiver_candidates_for_cursor(value_c, mod_name)
		if receivers.len == 0 {
			return
		}
		w.mark_method_name('msg', receivers)
		w.mark_method_name('code', receivers)
	}
}

fn (mut w Walker) mark_interface_conversion_methods_for_name(iface_name string, value_expr ast.Expr, mod_name string) {
	receivers := w.receiver_candidates_for_expr(value_expr, mod_name)
	if receivers.len == 0 {
		return
	}
	method_names := w.interface_method_names_for(iface_name)
	if method_names.len > 0 {
		embedded_receivers := w.embedded_receivers_for(receivers)
		for method_name in method_names {
			w.mark_method_name(method_name, receivers)
			w.mark_method_name(method_name, embedded_receivers)
		}
	}
}

fn (mut w Walker) mark_call_arg_interface_conversions(expr ast.CallExpr, mod_name string) {
	mut marked := false
	if w.env == unsafe { nil } {
		w.mark_call_arg_interface_conversions_from_decls(expr.lhs, expr.args, mod_name)
		return
	}
	if lhs_type := w.call_lhs_fn_type(expr.lhs, mod_name) {
		param_types := lhs_type.get_param_types()
		param_offset := if expr.lhs is ast.SelectorExpr { 1 } else { 0 }
		for i, arg in expr.args {
			param_idx := i + param_offset
			if param_idx >= param_types.len {
				break
			}
			param_type := param_types[param_idx]
			if param_type is types.Interface {
				w.mark_interface_conversion_methods_for_name((param_type as types.Interface).name,
					arg, mod_name)
				marked = true
			}
		}
	}
	if !marked {
		w.mark_call_arg_interface_conversions_from_decls(expr.lhs, expr.args, mod_name)
	}
}

fn (mut w Walker) mark_call_or_cast_arg_interface_conversion(expr ast.CallOrCastExpr, mod_name string) {
	mut marked := false
	if w.env == unsafe { nil } {
		w.mark_call_arg_interface_conversions_from_decls(expr.lhs, [expr.expr], mod_name)
		return
	}
	if lhs_type := w.call_lhs_fn_type(expr.lhs, mod_name) {
		param_types := lhs_type.get_param_types()
		param_offset := if expr.lhs is ast.SelectorExpr { 1 } else { 0 }
		if param_offset < param_types.len {
			param_type := param_types[param_offset]
			if param_type is types.Interface {
				w.mark_interface_conversion_methods_for_name((param_type as types.Interface).name,
					expr.expr, mod_name)
				marked = true
			}
		}
	}
	if !marked {
		w.mark_call_arg_interface_conversions_from_decls(expr.lhs, [expr.expr], mod_name)
	}
}

fn (w &Walker) add_lookup_indices(key string, mut out []int) {
	if !string_ok(key) {
		return
	}
	if key in w.lookup {
		for idx in w.lookup[key] {
			add_unique_int(mut out, idx)
		}
	}
}

fn (w &Walker) add_fn_name_indices(name string, mod_name string, mut out []int) {
	if name == '' || !string_ok(name) || !string_ok(mod_name) {
		return
	}
	candidates := called_fn_name_candidates(name)
	if key := w.exact_mangled_fn_lookup_key(name) {
		w.add_lookup_indices(key, mut out)
		return
	}
	for candidate in candidates {
		if key := w.current_module_fn_lookup_key(candidate, mod_name) {
			w.add_lookup_indices(key, mut out)
			return
		}
	}
	for candidate in candidates {
		if key := w.selective_import_fn_lookup_key(candidate) {
			w.add_lookup_indices(key, mut out)
			return
		}
	}
	for candidate in candidates {
		w.add_lookup_indices('mod:${mod_name}:${candidate}', mut out)
		w.add_lookup_indices('fn:${candidate}', mut out)
		if !candidate.contains('__') && mod_name != '' && mod_name != 'main'
			&& mod_name != 'builtin' {
			w.add_lookup_indices('fn:${mod_name}__${candidate}', mut out)
		}
	}
}

fn (w &Walker) add_method_name_indices(name string, receivers []string, mut out []int) {
	if name == '' || !string_ok(name) {
		return
	}
	normalized := normalize_method_name(name)
	for receiver in receivers {
		if !string_ok(receiver) {
			continue
		}
		for candidate in receiver_lookup_candidates(receiver) {
			w.add_lookup_indices('meth:${candidate}:${name}', mut out)
			if normalized != name {
				w.add_lookup_indices('meth:${candidate}:${normalized}', mut out)
			}
		}
	}
}

fn (w &Walker) call_lhs_decl_indices(lhs ast.Expr, mod_name string) []int {
	mut out := []int{}
	if !expr_ok(lhs) {
		return out
	}
	match lhs {
		ast.Ident {
			w.add_fn_name_indices(lhs.name, mod_name, mut out)
		}
		ast.GenericArgs {
			return w.call_lhs_decl_indices(lhs.lhs, mod_name)
		}
		ast.GenericArgOrIndexExpr {
			return w.call_lhs_decl_indices(lhs.lhs, mod_name)
		}
		ast.SelectorExpr {
			method_name := lhs.rhs.name
			if !string_ok(method_name) {
				return out
			}
			if lhs.lhs is ast.Ident {
				left_name := lhs.lhs.name
				if !string_ok(left_name) {
					return out
				}
				if left_name == 'C' {
					w.add_fn_name_indices(method_name, mod_name, mut out)
					return out
				}
				if left_name in w.module_names {
					real_mod := w.module_alias_to_real[left_name] or { left_name }
					w.add_fn_name_indices('${real_mod}__${method_name}', mod_name, mut out)
					return out
				}
				if left_name in w.type_names {
					w.add_method_name_indices(method_name,
						[left_name, '${mod_name}__${left_name}'], mut out)
					return out
				}
			}
			if lhs.lhs is ast.SelectorExpr {
				type_sel := lhs.lhs as ast.SelectorExpr
				if type_sel.lhs is ast.Ident {
					mod_ident := type_sel.lhs.name
					type_name := type_sel.rhs.name
					if !string_ok(mod_ident) || !string_ok(type_name) {
						return out
					}
					if mod_ident in w.module_names || mod_ident in w.type_names {
						mut candidates := []string{cap: 3}
						candidates << type_name
						candidates << '${mod_name}__${type_name}'
						candidates << '${mod_ident}__${type_name}'
						w.add_method_name_indices(method_name, candidates, mut out)
						return out
					}
				}
			}
			receivers := w.receiver_candidates_for_expr(lhs.lhs, mod_name)
			if receivers.len > 0 {
				w.add_method_name_indices(method_name, receivers, mut out)
				if out.len > 0 {
					return out
				}
			}
			normalized := normalize_method_name(method_name)
			w.add_lookup_indices('mname:${method_name}', mut out)
			if normalized != method_name {
				w.add_lookup_indices('mname:${normalized}', mut out)
			}
		}
		else {}
	}

	return out
}

fn (mut w Walker) mark_call_arg_interface_conversions_from_decls(lhs ast.Expr, args []ast.Expr, mod_name string) {
	decl_indices := w.call_lhs_decl_indices(lhs, mod_name)
	if decl_indices.len == 0 {
		return
	}
	for idx in decl_indices {
		if idx < 0 || idx >= w.fns.len {
			continue
		}
		info := w.fns[idx]
		params := info.decl.typ.params
		arg_offset := if lhs is ast.Ident && info.decl.is_method && !info.decl.is_static {
			1
		} else {
			0
		}
		for i, param in params {
			arg_idx := i + arg_offset
			if arg_idx >= args.len {
				break
			}
			iface_name := w.interface_name_from_expr(param.typ, info.mod)
			if iface_name == '' {
				continue
			}
			w.mark_interface_conversion_methods_for_name(iface_name, args[arg_idx], mod_name)
		}
	}
}

fn (mut w Walker) mark_string_interpolation_str_dependency(expr ast.Expr, mod_name string) {
	receivers := w.receiver_candidates_for_expr(expr, mod_name)
	if receivers.len == 0 {
		return
	}
	w.mark_method_name('str', receivers)
	for receiver in receivers {
		w.mark_fn_name('${receiver}__str', mod_name)
	}
}

fn (mut w Walker) mark_result_error_return_methods(return_type ast.Expr, expr ast.Expr, mod_name string) {
	if return_type is ast.Type && return_type is ast.ResultType {
		receivers := w.receiver_candidates_for_expr(expr, mod_name)
		if receivers.len == 0 {
			return
		}
		w.mark_method_name('msg', receivers)
		w.mark_method_name('code', receivers)
	}
}

// call_lhs_fn_type_cursor mirrors call_lhs_fn_type but reads from a Cursor.
// receiver_candidates_for_expr still needs a decoded Expr for the
// SelectorExpr branch — only that subtree is decoded, and only when env
// lookup falls through to it.
fn (w &Walker) call_lhs_fn_type_cursor(c ast.Cursor, mod_name string) ?types.FnType {
	if !c.is_valid() {
		return none
	}
	if w.env != unsafe { nil } {
		if lhs_type := w.env.get_expr_type(c.pos().id) {
			if lhs_type is types.FnType {
				return lhs_type as types.FnType
			}
		}
		match c.kind() {
			.expr_ident {
				if fn_type := w.env.lookup_fn(mod_name, c.name()) {
					return fn_type
				}
			}
			.expr_selector {
				inner_lhs := c.edge(0)
				rhs_c := c.edge(1)
				if inner_lhs.is_valid() && rhs_c.is_valid() {
					receivers := w.receiver_candidates_for_cursor(inner_lhs, mod_name)
					for receiver in receivers {
						if fn_type := w.env.lookup_method(receiver, rhs_c.name()) {
							return fn_type
						}
					}
				}
			}
			else {}
		}
	}
	if c.kind() == .expr_ident && w.cur_fn_scope != unsafe { nil } {
		if obj := w.cur_fn_scope.lookup_parent(c.name(), 0) {
			typ := obj.typ()
			if typ is types.FnType {
				return typ as types.FnType
			}
		}
	}
	return none
}

// call_lhs_decl_indices_cursor mirrors call_lhs_decl_indices with cursor
// input. Same shape: Ident / GenericArgs / GenericArgOrIndexExpr / Selector
// — the only ast-decoded subtree is lhs.lhs for SelectorExpr's
// receiver_candidates_for_expr fallback.
fn (w &Walker) call_lhs_decl_indices_cursor(c ast.Cursor, mod_name string) []int {
	mut out := []int{}
	if !c.is_valid() {
		return out
	}
	match c.kind() {
		.expr_ident {
			w.add_fn_name_indices(c.name(), mod_name, mut out)
		}
		.expr_generic_args {
			return w.call_lhs_decl_indices_cursor(c.edge(0), mod_name)
		}
		.expr_generic_arg_or_index {
			return w.call_lhs_decl_indices_cursor(c.edge(0), mod_name)
		}
		.expr_selector {
			rhs_c := c.edge(1)
			if !rhs_c.is_valid() {
				return out
			}
			method_name := rhs_c.name()
			if !string_ok(method_name) {
				return out
			}
			lhs_c := c.edge(0)
			if lhs_c.is_valid() && lhs_c.kind() == .expr_ident {
				left_name := lhs_c.name()
				if !string_ok(left_name) {
					return out
				}
				if left_name == 'C' {
					w.add_fn_name_indices(method_name, mod_name, mut out)
					return out
				}
				if left_name in w.module_names {
					real_mod := w.module_alias_to_real[left_name] or { left_name }
					w.add_fn_name_indices('${real_mod}__${method_name}', mod_name, mut out)
					return out
				}
				if left_name in w.type_names {
					w.add_method_name_indices(method_name,
						[left_name, '${mod_name}__${left_name}'], mut out)
					return out
				}
			}
			if lhs_c.is_valid() && lhs_c.kind() == .expr_selector {
				inner_lhs := lhs_c.edge(0)
				inner_rhs := lhs_c.edge(1)
				if inner_lhs.is_valid() && inner_lhs.kind() == .expr_ident && inner_rhs.is_valid() {
					mod_ident := inner_lhs.name()
					type_name := inner_rhs.name()
					if !string_ok(mod_ident) || !string_ok(type_name) {
						return out
					}
					if mod_ident in w.module_names || mod_ident in w.type_names {
						mut candidates := []string{cap: 3}
						candidates << type_name
						candidates << '${mod_name}__${type_name}'
						candidates << '${mod_ident}__${type_name}'
						w.add_method_name_indices(method_name, candidates, mut out)
						return out
					}
				}
			}
			receivers := w.receiver_candidates_for_cursor(lhs_c, mod_name)
			if receivers.len > 0 {
				w.add_method_name_indices(method_name, receivers, mut out)
				if out.len > 0 {
					return out
				}
			}
			normalized := normalize_method_name(method_name)
			w.add_lookup_indices('mname:${method_name}', mut out)
			if normalized != method_name {
				w.add_lookup_indices('mname:${normalized}', mut out)
			}
		}
		else {}
	}

	return out
}

// mark_call_arg_interface_conversions_cursor / _from_decls_cursor are the
// cursor analogues of mark_call_arg_interface_conversions /
// mark_call_arg_interface_conversions_from_decls. Args are only decoded on
// demand (when the param type is an interface), so non-interface params do
// not pay any decode cost.
fn (mut w Walker) mark_call_arg_interface_conversions_cursor(call_c ast.Cursor, mod_name string) {
	lhs_c := call_c.edge(0)
	ec := call_c.edge_count()
	args_len := ec - 1
	mut marked := false
	if w.env != unsafe { nil } {
		if lhs_type := w.call_lhs_fn_type_cursor(lhs_c, mod_name) {
			param_types := lhs_type.get_param_types()
			param_offset := if lhs_c.is_valid() && lhs_c.kind() == .expr_selector {
				1
			} else {
				0
			}
			for i in 0 .. args_len {
				param_idx := i + param_offset
				if param_idx >= param_types.len {
					break
				}
				param_type := param_types[param_idx]
				if param_type is types.Interface {
					arg_c := call_c.edge(1 + i)
					w.mark_interface_conversion_methods_for_name_cursor((param_type as types.Interface).name,
						arg_c, mod_name)
					marked = true
				}
			}
		}
	} else {
		w.mark_call_arg_interface_conversions_from_decls_cursor(call_c, mod_name)
		return
	}
	if !marked {
		w.mark_call_arg_interface_conversions_from_decls_cursor(call_c, mod_name)
	}
}

fn (mut w Walker) mark_call_arg_interface_conversions_from_decls_cursor(call_c ast.Cursor, mod_name string) {
	lhs_c := call_c.edge(0)
	decl_indices := w.call_lhs_decl_indices_cursor(lhs_c, mod_name)
	if decl_indices.len == 0 {
		return
	}
	args_len := call_c.edge_count() - 1
	lhs_is_ident := lhs_c.is_valid() && lhs_c.kind() == .expr_ident
	for idx in decl_indices {
		if idx < 0 || idx >= w.fns.len {
			continue
		}
		info := w.fns[idx]
		params := info.decl.typ.params
		arg_offset := if lhs_is_ident && info.decl.is_method && !info.decl.is_static {
			1
		} else {
			0
		}
		for i, param in params {
			arg_idx := i + arg_offset
			if arg_idx >= args_len {
				break
			}
			iface_name := w.interface_name_from_expr(param.typ, info.mod)
			if iface_name == '' {
				continue
			}
			arg_c := call_c.edge(1 + arg_idx)
			w.mark_interface_conversion_methods_for_name_cursor(iface_name, arg_c, mod_name)
		}
	}
}

// mark_call_or_cast_arg_interface_conversion_cursor is the cursor analogue
// of mark_call_or_cast_arg_interface_conversion. The "expr" edge (call_c.edge(1))
// is treated as a single-arg list; it is decoded only when the param type
// is an interface.
fn (mut w Walker) mark_call_or_cast_arg_interface_conversion_cursor(call_c ast.Cursor, mod_name string) {
	lhs_c := call_c.edge(0)
	expr_c := call_c.edge(1)
	mut marked := false
	if w.env != unsafe { nil } {
		if lhs_type := w.call_lhs_fn_type_cursor(lhs_c, mod_name) {
			param_types := lhs_type.get_param_types()
			param_offset := if lhs_c.is_valid() && lhs_c.kind() == .expr_selector {
				1
			} else {
				0
			}
			if param_offset < param_types.len {
				param_type := param_types[param_offset]
				if param_type is types.Interface {
					w.mark_interface_conversion_methods_for_name_cursor((param_type as types.Interface).name,
						expr_c, mod_name)
					marked = true
				}
			}
		}
	} else {
		decl_indices := w.call_lhs_decl_indices_cursor(lhs_c, mod_name)
		w.mark_call_or_cast_arg_interface_from_decl_indices_cursor(decl_indices, lhs_c, expr_c,
			mod_name)
		return
	}
	if !marked {
		decl_indices := w.call_lhs_decl_indices_cursor(lhs_c, mod_name)
		w.mark_call_or_cast_arg_interface_from_decl_indices_cursor(decl_indices, lhs_c, expr_c,
			mod_name)
	}
}

fn (mut w Walker) mark_call_or_cast_arg_interface_from_decl_indices_cursor(decl_indices []int, lhs_c ast.Cursor, expr_c ast.Cursor, mod_name string) {
	if decl_indices.len == 0 || !expr_c.is_valid() {
		return
	}
	lhs_is_ident := lhs_c.is_valid() && lhs_c.kind() == .expr_ident
	for idx in decl_indices {
		if idx < 0 || idx >= w.fns.len {
			continue
		}
		info := w.fns[idx]
		params := info.decl.typ.params
		arg_offset := if lhs_is_ident && info.decl.is_method && !info.decl.is_static {
			1
		} else {
			0
		}
		if arg_offset >= params.len {
			continue
		}
		param := params[arg_offset]
		iface_name := w.interface_name_from_expr(param.typ, info.mod)
		if iface_name == '' {
			continue
		}
		w.mark_interface_conversion_methods_for_name_cursor(iface_name, expr_c, mod_name)
	}
}

fn (w &Walker) call_lhs_fn_type(lhs ast.Expr, mod_name string) ?types.FnType {
	if !expr_ok(lhs) {
		return none
	}
	if w.env != unsafe { nil } {
		if lhs_type := w.env.get_expr_type(lhs.pos().id) {
			if lhs_type is types.FnType {
				return lhs_type as types.FnType
			}
		}
		if lhs is ast.Ident {
			if fn_type := w.env.lookup_fn(mod_name, lhs.name) {
				return fn_type
			}
		}
		if lhs is ast.SelectorExpr {
			receivers := w.receiver_candidates_for_expr(lhs.lhs, mod_name)
			for receiver in receivers {
				if fn_type := w.env.lookup_method(receiver, lhs.rhs.name) {
					return fn_type
				}
			}
		}
	}
	if lhs is ast.Ident && w.cur_fn_scope != unsafe { nil } {
		if obj := w.cur_fn_scope.lookup_parent(lhs.name, 0) {
			typ := obj.typ()
			if typ is types.FnType {
				return typ as types.FnType
			}
		}
	}
	return none
}

fn (w &Walker) current_param_receiver_candidates(name string, mod_name string) []string {
	mut out := []string{}
	if name == '' || w.cur_fn_decl.name == '' {
		return out
	}
	for param in w.cur_fn_decl.typ.params {
		if param.name != name {
			continue
		}
		for receiver in type_expr_receiver_candidates(mod_name, param.typ) {
			add_unique_string(mut out, receiver)
		}
		generic_name := receiver_type_expr_name(param.typ)
		if generic_name == '' || w.env == unsafe { nil } {
			continue
		}
		for bindings in w.current_fn_generic_bindings() {
			if concrete := bindings[generic_name] {
				for receiver in type_name_candidates_from_type(mod_name, concrete) {
					add_unique_string(mut out, receiver)
				}
			}
		}
	}
	return out
}

fn (w &Walker) current_receiver_candidates(name string, mod_name string) []string {
	mut out := []string{}
	if name == '' || w.cur_fn_decl.name == '' || !is_method_decl(w.cur_fn_decl)
		|| w.cur_fn_decl.receiver.name != name {
		return out
	}
	for receiver in type_expr_receiver_candidates(mod_name, w.cur_fn_decl.receiver.typ) {
		add_unique_string(mut out, receiver)
	}
	generic_name := receiver_type_expr_name(w.cur_fn_decl.receiver.typ)
	if generic_name == '' || w.env == unsafe { nil } {
		return out
	}
	for bindings in w.current_fn_generic_bindings() {
		if concrete := bindings[generic_name] {
			for receiver in type_name_candidates_from_type(mod_name, concrete) {
				add_unique_string(mut out, receiver)
			}
		}
	}
	return out
}

fn (w &Walker) current_fn_generic_bindings() []map[string]types.Type {
	if w.env == unsafe { nil } || w.cur_fn_decl.name == '' {
		return []map[string]types.Type{}
	}
	mut out := []map[string]types.Type{}
	for key, bindings_list in w.env.generic_types {
		if generic_base_name_from_key(key) != w.cur_fn_decl.name {
			continue
		}
		for bindings in bindings_list {
			out << bindings
		}
	}
	return out
}

// receiver_candidates_for_cursor mirrors receiver_candidates_for_expr but
// reads inputs from a Cursor instead of a decoded ast.Expr. Same shape:
// env-type lookup by pos.id first, then a structural match on cursor kind.
// PrefixExpr / ParenExpr / ModifierExpr / SelectorExpr recurse through
// edge(0); InitExpr through its typ at edge(0); CallExpr / CallOrCastExpr
// look at edge(0)'s selector.lhs without recursion.
fn (w &Walker) receiver_candidates_for_cursor(c ast.Cursor, mod_name string) []string {
	mut out := []string{}
	if !c.is_valid() {
		return out
	}
	pos := c.pos()
	if w.env != unsafe { nil } && pos.is_valid() {
		if receiver_type := w.env.get_expr_type(pos.id) {
			for name in type_name_candidates_from_type(mod_name, receiver_type) {
				add_unique_string(mut out, name)
			}
		}
	}
	match c.kind() {
		.expr_ident {
			ident_name := c.name()
			if w.env != unsafe { nil } && w.cur_fn_scope != unsafe { nil } {
				if local_type := w.env.lookup_local_var(w.cur_fn_scope, ident_name) {
					for type_name in type_name_candidates_from_type(mod_name, local_type) {
						add_unique_string(mut out, type_name)
					}
				}
			}
			name := sanitize_receiver_name(ident_name)
			for type_name in w.current_receiver_candidates(ident_name, mod_name) {
				add_unique_string(mut out, type_name)
			}
			for type_name in w.current_param_receiver_candidates(ident_name, mod_name) {
				add_unique_string(mut out, type_name)
			}
			add_unique_string(mut out, name)
			if mod_name != '' && mod_name != 'main' {
				add_unique_string(mut out, '${mod_name}__${name}')
			}
		}
		.expr_prefix, .expr_paren, .expr_modifier {
			for name in w.receiver_candidates_for_cursor(c.edge(0), mod_name) {
				add_unique_string(mut out, name)
			}
		}
		.expr_init {
			for name in w.receiver_candidates_for_cursor(c.edge(0), mod_name) {
				add_unique_string(mut out, name)
			}
		}
		.expr_call, .expr_call_or_cast {
			lhs_c := c.edge(0)
			if lhs_c.is_valid() && lhs_c.kind() == .expr_selector {
				inner_lhs := lhs_c.edge(0)
				if inner_lhs.is_valid() && inner_lhs.kind() == .expr_ident {
					type_name := inner_lhs.name()
					if type_name in w.type_names {
						add_receiver_name_candidates(mut out, mod_name, type_name)
					}
				}
			}
		}
		.expr_selector {
			lhs_c := c.edge(0)
			rhs_c := c.edge(1)
			if rhs_c.is_valid() {
				rhs_name := rhs_c.name()
				lhs_receivers := w.receiver_candidates_for_cursor(lhs_c, mod_name)
				for lhs_receiver in lhs_receivers {
					key := '${lhs_receiver}:${rhs_name}'
					if field_receivers := w.struct_field_receivers[key] {
						for field_receiver in field_receivers {
							add_unique_string(mut out, field_receiver)
						}
					}
				}
			}
		}
		else {}
	}

	return out
}

// mark_interface_conversion_methods_for_name_cursor is the cursor analogue
// of mark_interface_conversion_methods_for_name. The value_expr is replaced
// by a value cursor, so receiver_candidates_for_cursor avoids decoding.
fn (mut w Walker) mark_interface_conversion_methods_for_name_cursor(iface_name string, value_c ast.Cursor, mod_name string) {
	receivers := w.receiver_candidates_for_cursor(value_c, mod_name)
	if receivers.len == 0 {
		return
	}
	if iface_name in w.interface_method_names {
		embedded_receivers := w.embedded_receivers_for(receivers)
		for method_name in w.interface_method_names[iface_name] {
			w.mark_method_name(method_name, receivers)
			w.mark_method_name(method_name, embedded_receivers)
		}
	}
	w.mark_all_methods_for_receivers(receivers)
}

// mark_string_interpolation_str_dependency_cursor is the cursor analogue of
// mark_string_interpolation_str_dependency.
fn (mut w Walker) mark_string_interpolation_str_dependency_cursor(value_c ast.Cursor, mod_name string) {
	receivers := w.receiver_candidates_for_cursor(value_c, mod_name)
	if receivers.len == 0 {
		return
	}
	w.mark_method_name('str', receivers)
	for receiver in receivers {
		w.mark_fn_name('${receiver}__str', mod_name)
	}
}

fn (w &Walker) receiver_candidates_for_expr(expr ast.Expr, mod_name string) []string {
	mut out := []string{}
	if !expr_ok(expr) {
		return out
	}
	pos := expr.pos()
	if w.env != unsafe { nil } && pos.is_valid() {
		if receiver_type := w.env.get_expr_type(pos.id) {
			for name in type_name_candidates_from_type(mod_name, receiver_type) {
				add_unique_string(mut out, name)
			}
		}
	}
	match expr {
		ast.Ident {
			if w.env != unsafe { nil } && w.cur_fn_scope != unsafe { nil } {
				if local_type := w.env.lookup_local_var(w.cur_fn_scope, expr.name) {
					for type_name in type_name_candidates_from_type(mod_name, local_type) {
						add_unique_string(mut out, type_name)
					}
				}
			}
			name := sanitize_receiver_name(expr.name)
			for type_name in w.current_receiver_candidates(expr.name, mod_name) {
				add_unique_string(mut out, type_name)
			}
			for type_name in w.current_param_receiver_candidates(expr.name, mod_name) {
				add_unique_string(mut out, type_name)
			}
			add_unique_string(mut out, name)
			if mod_name != '' && mod_name != 'main' {
				add_unique_string(mut out, '${mod_name}__${name}')
			}
		}
		ast.PrefixExpr {
			for name in w.receiver_candidates_for_expr(expr.expr, mod_name) {
				add_unique_string(mut out, name)
			}
		}
		ast.ParenExpr {
			for name in w.receiver_candidates_for_expr(expr.expr, mod_name) {
				add_unique_string(mut out, name)
			}
		}
		ast.ModifierExpr {
			for name in w.receiver_candidates_for_expr(expr.expr, mod_name) {
				add_unique_string(mut out, name)
			}
		}
		ast.InitExpr {
			// InitExpr has a .typ that names the struct being constructed.
			for name in w.receiver_candidates_for_expr(expr.typ, mod_name) {
				add_unique_string(mut out, name)
			}
		}
		ast.CallExpr {
			if expr.lhs is ast.SelectorExpr {
				selector := expr.lhs as ast.SelectorExpr
				if selector.lhs is ast.Ident {
					type_name := selector.lhs.name
					if type_name in w.type_names {
						add_receiver_name_candidates(mut out, mod_name, type_name)
					}
				}
			}
		}
		ast.CallOrCastExpr {
			if expr.lhs is ast.SelectorExpr {
				selector := expr.lhs as ast.SelectorExpr
				if selector.lhs is ast.Ident {
					type_name := selector.lhs.name
					if type_name in w.type_names {
						add_receiver_name_candidates(mut out, mod_name, type_name)
					}
				}
			}
		}
		ast.SelectorExpr {
			lhs_receivers := w.receiver_candidates_for_expr(expr.lhs, mod_name)
			for lhs_receiver in lhs_receivers {
				key := '${lhs_receiver}:${expr.rhs.name}'
				if field_receivers := w.struct_field_receivers[key] {
					for field_receiver in field_receivers {
						add_unique_string(mut out, field_receiver)
					}
				}
			}
		}
		else {}
	}

	return out
}

fn (w &Walker) is_cast_type_name(name string) bool {
	if !string_ok(name) {
		return false
	}
	if name in builtin_cast_type_names {
		return true
	}
	return name in w.type_names
}

fn (mut w Walker) mark_call_lhs(lhs ast.Expr, mod_name string) {
	if !expr_ok(lhs) {
		return
	}
	match lhs {
		ast.Ident {
			w.mark_fn_name(lhs.name, mod_name)
			w.mark_const_fn_value_alias(lhs.name, mod_name)
		}
		ast.GenericArgs {
			w.mark_call_lhs(lhs.lhs, mod_name)
		}
		ast.GenericArgOrIndexExpr {
			w.mark_call_lhs(lhs.lhs, mod_name)
		}
		ast.SelectorExpr {
			method_name := lhs.rhs.name
			if !string_ok(method_name) {
				return
			}
			if lhs.lhs is ast.Ident {
				left_name := lhs.lhs.name
				if !string_ok(left_name) {
					return
				}
				if left_name == 'C' {
					w.mark_fn_name(method_name, mod_name)
					return
				}
				if left_name in w.module_names {
					// Resolve import aliases (e.g., 'ssa_optimize' → 'optimize')
					// so lookup finds 'optimize__func' not 'ssa_optimize__func'.
					real_mod := w.module_alias_to_real[left_name] or { left_name }
					w.mark_fn_name('${real_mod}__${method_name}', mod_name)
					return
				}
				if left_name in w.type_names {
					w.mark_method_name(method_name, [left_name, '${mod_name}__${left_name}'])
					return
				}
			}
			if lhs.lhs is ast.SelectorExpr {
				type_sel := lhs.lhs as ast.SelectorExpr
				if type_sel.lhs is ast.Ident {
					mod_ident := type_sel.lhs.name
					type_name := type_sel.rhs.name
					if !string_ok(mod_ident) || !string_ok(type_name) {
						return
					}
					if mod_ident in w.module_names || mod_ident in w.type_names {
						candidates := [
							type_name,
							'${mod_name}__${type_name}',
							'${mod_ident}__${type_name}',
						]
						w.mark_method_name(method_name, candidates)
						return
					}
				}
			}
			receivers := w.receiver_candidates_for_expr(lhs.lhs, mod_name)
			if receivers.len > 0 {
				prev_queue_len := w.queue.len
				w.mark_method_name(method_name, receivers)
				if w.queue.len > prev_queue_len {
					return
				}
			}
			w.mark_method_name_fallback(method_name)
		}
		else {}
	}
}

fn (w &Walker) const_fn_value_alias_is_shadowed(name string) bool {
	if w.cur_fn_scope == unsafe { nil } {
		return false
	}
	if obj := w.cur_fn_scope.lookup_parent(name, 0) {
		return obj !is types.Const
	}
	return false
}

fn (mut w Walker) mark_const_fn_value_alias(name string, mod_name string) bool {
	if name == '' || !string_ok(name) || !string_ok(mod_name)
		|| w.const_fn_value_alias_is_shadowed(name) {
		return false
	}
	if target := w.const_fn_value_aliases[const_fn_value_alias_key(mod_name, name)] {
		w.mark_fn_name(target, mod_name)
		return true
	}
	return false
}

// mark_call_lhs_cursor is the cursor-input analogue of mark_call_lhs. It
// handles every case structurally via cursor accesses, and only falls back
// to decoding `lhs.lhs` for the SelectorExpr receiver_candidates_for_expr
// branch — which is itself only reached when the early-return cases (C.fn,
// module.fn, Type.method, mod.Type.method) all fail.
fn (mut w Walker) mark_call_lhs_cursor(c ast.Cursor, mod_name string) {
	if !c.is_valid() {
		return
	}
	match c.kind() {
		.expr_ident {
			w.mark_fn_name(c.name(), mod_name)
			w.mark_const_fn_value_alias(c.name(), mod_name)
		}
		.expr_generic_args {
			w.mark_call_lhs_cursor(c.edge(0), mod_name)
		}
		.expr_generic_arg_or_index {
			w.mark_call_lhs_cursor(c.edge(0), mod_name)
		}
		.expr_selector {
			rhs_c := c.edge(1)
			if !rhs_c.is_valid() {
				return
			}
			method_name := rhs_c.name()
			if !string_ok(method_name) {
				return
			}
			lhs_c := c.edge(0)
			if lhs_c.is_valid() && lhs_c.kind() == .expr_ident {
				left_name := lhs_c.name()
				if !string_ok(left_name) {
					return
				}
				if left_name == 'C' {
					w.mark_fn_name(method_name, mod_name)
					return
				}
				if left_name in w.module_names {
					real_mod := w.module_alias_to_real[left_name] or { left_name }
					w.mark_fn_name('${real_mod}__${method_name}', mod_name)
					return
				}
				if left_name in w.type_names {
					w.mark_method_name(method_name, [left_name, '${mod_name}__${left_name}'])
					return
				}
			}
			if lhs_c.is_valid() && lhs_c.kind() == .expr_selector {
				inner_lhs := lhs_c.edge(0)
				inner_rhs := lhs_c.edge(1)
				if inner_lhs.is_valid() && inner_lhs.kind() == .expr_ident && inner_rhs.is_valid() {
					mod_ident := inner_lhs.name()
					type_name := inner_rhs.name()
					if !string_ok(mod_ident) || !string_ok(type_name) {
						return
					}
					if mod_ident in w.module_names || mod_ident in w.type_names {
						candidates := [
							type_name,
							'${mod_name}__${type_name}',
							'${mod_ident}__${type_name}',
						]
						w.mark_method_name(method_name, candidates)
						return
					}
				}
			}
			// Fallback: cursor-native receiver candidates — no decode.
			receivers := w.receiver_candidates_for_cursor(lhs_c, mod_name)
			if receivers.len > 0 {
				prev_queue_len := w.queue.len
				w.mark_method_name(method_name, receivers)
				if w.queue.len > prev_queue_len {
					return
				}
			}
			w.mark_method_name_fallback(method_name)
		}
		else {}
	}
}

fn (mut w Walker) mark_selector_fn_value(expr ast.SelectorExpr, mod_name string) {
	w.mark_selector_fn_value_with_fallback(expr, mod_name, false)
}

fn (mut w Walker) mark_selector_fn_value_with_fallback(expr ast.SelectorExpr, mod_name string, force_fallback bool) {
	target := w.selector_fn_value_target(expr)
	if target != '' {
		w.mark_fn_name(target, mod_name)
		return
	}
	if expr.rhs.name == '' {
		return
	}
	receivers := w.receiver_candidates_for_expr(expr.lhs, mod_name)
	if receivers.len > 0 {
		matched := w.method_lookup_count(expr.rhs.name, receivers) > 0
		w.mark_method_name(expr.rhs.name, receivers)
		if matched {
			return
		}
	}
	if force_fallback || w.expr_has_fn_value_type(ast.Expr(expr)) {
		w.mark_method_name_fallback(expr.rhs.name)
	}
}

fn (mut w Walker) mark_selector_fn_value_cursor(c ast.Cursor, mod_name string) {
	w.mark_selector_fn_value_cursor_with_fallback(c, mod_name, false)
}

fn (mut w Walker) mark_selector_fn_value_cursor_with_fallback(c ast.Cursor, mod_name string, force_fallback bool) {
	target := w.selector_fn_value_target_cursor(c)
	if target != '' {
		w.mark_fn_name(target, mod_name)
		return
	}
	rhs_c := c.edge(1)
	if !rhs_c.is_valid() {
		return
	}
	method_name := rhs_c.name()
	if method_name == '' {
		return
	}
	receivers := w.receiver_candidates_for_cursor(c.edge(0), mod_name)
	if receivers.len > 0 {
		matched := w.method_lookup_count(method_name, receivers) > 0
		w.mark_method_name(method_name, receivers)
		if matched {
			return
		}
	}
	if force_fallback || w.cursor_has_fn_value_type(c) {
		w.mark_method_name_fallback(method_name)
	}
}

fn (w &Walker) selector_fn_value_target(expr ast.SelectorExpr) string {
	if expr.lhs !is ast.Ident {
		return ''
	}
	left := expr.lhs as ast.Ident
	left_name := left.name
	if left_name == 'C' || left_name !in w.module_names {
		return ''
	}
	real_mod := w.module_alias_to_real[left_name] or { left_name }
	target := '${real_mod}__${expr.rhs.name}'
	if w.lookup_count('fn:${target}') > 0 {
		return target
	}
	return ''
}

fn (w &Walker) selector_fn_value_target_cursor(c ast.Cursor) string {
	if !c.is_valid() || c.kind() != .expr_selector {
		return ''
	}
	lhs_c := c.edge(0)
	rhs_c := c.edge(1)
	if !lhs_c.is_valid() || lhs_c.kind() != .expr_ident || !rhs_c.is_valid() {
		return ''
	}
	left_name := lhs_c.name()
	if left_name == 'C' || left_name !in w.module_names {
		return ''
	}
	real_mod := w.module_alias_to_real[left_name] or { left_name }
	target := '${real_mod}__${rhs_c.name()}'
	if w.lookup_count('fn:${target}') > 0 {
		return target
	}
	return ''
}

fn (mut w Walker) mark_fn_value_expr(expr ast.Expr, mod_name string) {
	if !w.opts.minimal_runtime_roots || !expr_ok(expr) {
		return
	}
	match expr {
		ast.Ident {
			if w.mark_const_fn_value_alias(expr.name, mod_name) {
				return
			}
			if w.ident_resolves_to_fn_value(expr.name, mod_name) {
				w.mark_fn_name(expr.name, mod_name)
			}
		}
		ast.SelectorExpr {
			w.mark_selector_fn_value(expr, mod_name)
		}
		ast.GenericArgs {
			w.mark_fn_value_expr(expr.lhs, mod_name)
		}
		ast.GenericArgOrIndexExpr {
			w.mark_fn_value_expr(expr.lhs, mod_name)
		}
		ast.ModifierExpr {
			w.mark_fn_value_expr(expr.expr, mod_name)
		}
		ast.ParenExpr {
			w.mark_fn_value_expr(expr.expr, mod_name)
		}
		else {}
	}
}

fn (mut w Walker) mark_fn_value_expr_cursor(c ast.Cursor, mod_name string) {
	if !w.opts.minimal_runtime_roots || !c.is_valid() {
		return
	}
	match c.kind() {
		.expr_ident {
			name := c.name()
			if w.mark_const_fn_value_alias(name, mod_name) {
				return
			}
			if w.ident_resolves_to_fn_value(name, mod_name) {
				w.mark_fn_name(name, mod_name)
			}
		}
		.expr_selector {
			w.mark_selector_fn_value_cursor(c, mod_name)
		}
		.expr_generic_args {
			w.mark_fn_value_expr_cursor(c.edge(0), mod_name)
		}
		.expr_generic_arg_or_index {
			w.mark_fn_value_expr_cursor(c.edge(0), mod_name)
		}
		.expr_modifier {
			w.mark_fn_value_expr_cursor(c.edge(0), mod_name)
		}
		.expr_paren {
			w.mark_fn_value_expr_cursor(c.edge(0), mod_name)
		}
		else {}
	}
}

fn (mut w Walker) walk_stmts(stmts []ast.Stmt, mod_name string) {
	for stmt in stmts {
		w.walk_stmt(stmt, mod_name)
	}
}

fn (mut w Walker) walk_stmt(stmt ast.Stmt, mod_name string) {
	if !stmt_ok(stmt) {
		return
	}
	match stmt {
		ast.AssertStmt {
			w.walk_expr(stmt.expr, mod_name)
			w.walk_expr(stmt.extra, mod_name)
		}
		ast.AssignStmt {
			for i, rhs in stmt.rhs {
				if i < stmt.lhs.len {
					w.mark_assignment_interface_conversion(stmt.lhs[i], rhs, mod_name)
				}
			}
			for expr in stmt.lhs {
				w.walk_expr(expr, mod_name)
			}
			for expr in stmt.rhs {
				w.mark_fn_value_expr(expr, mod_name)
				w.walk_expr(expr, mod_name)
			}
		}
		ast.BlockStmt {
			w.walk_stmts(stmt.stmts, mod_name)
		}
		ast.ComptimeStmt {
			w.mark_comptime_method_loop_receivers(stmt.stmt, mod_name)
			w.walk_stmt(stmt.stmt, mod_name)
		}
		ast.ConstDecl {
			for field in stmt.fields {
				w.mark_fn_value_expr(field.value, mod_name)
				w.walk_expr(field.value, mod_name)
			}
		}
		ast.DeferStmt {
			w.walk_stmts(stmt.stmts, mod_name)
		}
		ast.EnumDecl {
			for field in stmt.fields {
				w.walk_expr(field.value, mod_name)
			}
		}
		ast.ExprStmt {
			w.walk_expr(stmt.expr, mod_name)
		}
		ast.ForInStmt {
			w.walk_expr(stmt.key, mod_name)
			w.walk_expr(stmt.value, mod_name)
			w.walk_expr(stmt.expr, mod_name)
		}
		ast.ForStmt {
			w.walk_stmt(stmt.init, mod_name)
			w.walk_expr(stmt.cond, mod_name)
			w.walk_stmt(stmt.post, mod_name)
			w.walk_stmts(stmt.stmts, mod_name)
		}
		ast.GlobalDecl {
			for field in stmt.fields {
				w.walk_expr(field.typ, mod_name)
				w.walk_expr(field.value, mod_name)
			}
		}
		ast.LabelStmt {
			w.walk_stmt(stmt.stmt, mod_name)
		}
		ast.ReturnStmt {
			for expr in stmt.exprs {
				w.mark_fn_value_expr(expr, mod_name)
				w.walk_expr(expr, mod_name)
				// If the function returns an interface type and the return expr
				// is a concrete struct InitExpr, mark the concrete type's
				// interface methods so the vtable wrapper can call them.
				w.mark_interface_conversion_methods(w.cur_fn_decl.typ.return_type, expr, mod_name)
				w.mark_result_error_return_methods(w.cur_fn_decl.typ.return_type, expr, mod_name)
			}
		}
		ast.StructDecl {
			for field in stmt.fields {
				w.walk_expr(field.typ, mod_name)
				w.walk_expr(field.value, mod_name)
			}
		}
		ast.TypeDecl {
			w.walk_expr(stmt.base_type, mod_name)
			for variant in stmt.variants {
				w.walk_expr(variant, mod_name)
			}
		}
		[]ast.Attribute {
			attrs := stmt as []ast.Attribute
			for attr in attrs {
				w.walk_expr(attr.value, mod_name)
				w.walk_expr(attr.comptime_cond, mod_name)
			}
		}
		else {}
	}
}

fn (mut w Walker) walk_expr(expr ast.Expr, mod_name string) {
	if !expr_ok(expr) {
		return
	}
	match expr {
		ast.ArrayInitExpr {
			w.walk_expr(expr.typ, mod_name)
			for item in expr.exprs {
				w.mark_fn_value_expr(item, mod_name)
				w.walk_expr(item, mod_name)
			}
			w.mark_fn_value_expr(expr.init, mod_name)
			w.walk_expr(expr.init, mod_name)
			w.walk_expr(expr.cap, mod_name)
			w.walk_expr(expr.len, mod_name)
			w.walk_expr(expr.update_expr, mod_name)
		}
		ast.AsCastExpr {
			w.walk_expr(expr.expr, mod_name)
			w.walk_expr(expr.typ, mod_name)
		}
		ast.AssocExpr {
			w.walk_expr(expr.typ, mod_name)
			w.walk_expr(expr.expr, mod_name)
			for field in expr.fields {
				w.mark_fn_value_expr(field.value, mod_name)
				w.walk_expr(field.value, mod_name)
			}
		}
		ast.CallExpr {
			w.mark_call_lhs(expr.lhs, mod_name)
			w.mark_call_arg_interface_conversions(expr, mod_name)
			w.walk_expr(expr.lhs, mod_name)
			for arg in expr.args {
				w.mark_fn_value_expr(arg, mod_name)
				w.walk_expr(arg, mod_name)
			}
		}
		ast.CallOrCastExpr {
			if expr.lhs is ast.Ident && w.is_cast_type_name(expr.lhs.name) {
				w.mark_interface_conversion_methods(expr.lhs, expr.expr, mod_name)
				w.walk_expr(expr.expr, mod_name)
				return
			}
			w.mark_call_lhs(expr.lhs, mod_name)
			w.mark_call_or_cast_arg_interface_conversion(expr, mod_name)
			w.walk_expr(expr.lhs, mod_name)
			w.mark_fn_value_expr(expr.expr, mod_name)
			w.walk_expr(expr.expr, mod_name)
		}
		ast.CastExpr {
			w.mark_interface_conversion_methods(expr.typ, expr.expr, mod_name)
			w.walk_expr(expr.typ, mod_name)
			w.walk_expr(expr.expr, mod_name)
		}
		ast.ComptimeExpr {
			w.walk_expr(expr.expr, mod_name)
		}
		ast.FnLiteral {
			w.walk_stmts(expr.stmts, mod_name)
		}
		ast.GenericArgs {
			w.walk_expr(expr.lhs, mod_name)
			for arg in expr.args {
				w.walk_expr(arg, mod_name)
			}
		}
		ast.GenericArgOrIndexExpr {
			w.walk_expr(expr.lhs, mod_name)
			w.walk_expr(expr.expr, mod_name)
		}
		ast.Ident {
			w.mark_ierror_wrapper_dependencies(expr.name, mod_name)
			if (!w.opts.minimal_runtime_roots || should_mark_ident_as_fn(expr.name))
				&& !w.is_cast_type_name(expr.name) {
				w.mark_fn_name(expr.name, mod_name)
			}
		}
		ast.IfExpr {
			w.walk_expr(expr.cond, mod_name)
			w.walk_stmts(expr.stmts, mod_name)
			w.walk_expr(expr.else_expr, mod_name)
		}
		ast.IfGuardExpr {
			w.walk_stmt(ast.Stmt(expr.stmt), mod_name)
		}
		ast.IndexExpr {
			w.walk_expr(expr.lhs, mod_name)
			w.walk_expr(expr.expr, mod_name)
		}
		ast.InfixExpr {
			w.mark_infix_operator_method(expr, mod_name)
			w.walk_expr(expr.lhs, mod_name)
			w.walk_expr(expr.rhs, mod_name)
		}
		ast.InitExpr {
			w.mark_interface_conversion_methods(expr.typ, expr, mod_name)
			w.walk_expr(expr.typ, mod_name)
			for field in expr.fields {
				if field.value is ast.SelectorExpr
					&& w.init_field_is_fn_value(expr.typ, field.name, mod_name) {
					w.mark_selector_fn_value_with_fallback(field.value, mod_name, true)
				}
				w.mark_fn_value_expr(field.value, mod_name)
				w.walk_expr(field.value, mod_name)
			}
		}
		ast.KeywordOperator {
			for value in expr.exprs {
				w.walk_expr(value, mod_name)
			}
		}
		ast.LambdaExpr {
			w.walk_expr(expr.expr, mod_name)
		}
		ast.LockExpr {
			for lock_expr in expr.lock_exprs {
				w.walk_expr(lock_expr, mod_name)
			}
			for lock_expr in expr.rlock_exprs {
				w.walk_expr(lock_expr, mod_name)
			}
			w.walk_stmts(expr.stmts, mod_name)
		}
		ast.MapInitExpr {
			w.walk_expr(expr.typ, mod_name)
			for key in expr.keys {
				w.mark_fn_value_expr(key, mod_name)
				w.walk_expr(key, mod_name)
			}
			for value in expr.vals {
				w.mark_fn_value_expr(value, mod_name)
				w.walk_expr(value, mod_name)
			}
		}
		ast.MatchExpr {
			w.walk_expr(expr.expr, mod_name)
			for branch in expr.branches {
				for cond in branch.cond {
					w.walk_expr(cond, mod_name)
				}
				w.walk_stmts(branch.stmts, mod_name)
			}
		}
		ast.ModifierExpr {
			w.walk_expr(expr.expr, mod_name)
		}
		ast.OrExpr {
			w.walk_expr(expr.expr, mod_name)
			w.walk_stmts(expr.stmts, mod_name)
		}
		ast.ParenExpr {
			w.walk_expr(expr.expr, mod_name)
		}
		ast.PostfixExpr {
			w.walk_expr(expr.expr, mod_name)
		}
		ast.PrefixExpr {
			w.walk_expr(expr.expr, mod_name)
		}
		ast.RangeExpr {
			w.walk_expr(expr.start, mod_name)
			w.walk_expr(expr.end, mod_name)
		}
		ast.SelectExpr {
			w.walk_stmt(expr.stmt, mod_name)
			w.walk_stmts(expr.stmts, mod_name)
			w.walk_expr(expr.next, mod_name)
		}
		ast.SelectorExpr {
			w.mark_selector_fn_value(expr, mod_name)
			w.walk_expr(expr.lhs, mod_name)
		}
		ast.SqlExpr {
			w.walk_expr(expr.expr, mod_name)
		}
		ast.StringInterLiteral {
			for inter in expr.inters {
				w.mark_string_interpolation_str_dependency(inter.expr, mod_name)
				w.walk_expr(inter.expr, mod_name)
				w.walk_expr(inter.format_expr, mod_name)
			}
		}
		ast.Tuple {
			for value in expr.exprs {
				w.mark_fn_value_expr(value, mod_name)
				w.walk_expr(value, mod_name)
			}
		}
		ast.UnsafeExpr {
			w.walk_stmts(expr.stmts, mod_name)
		}
		ast.Type {
			match expr {
				ast.ArrayFixedType {
					w.walk_expr(expr.len, mod_name)
					w.walk_expr(expr.elem_type, mod_name)
				}
				ast.ArrayType {
					w.walk_expr(expr.elem_type, mod_name)
				}
				ast.ChannelType {
					w.walk_expr(expr.cap, mod_name)
					w.walk_expr(expr.elem_type, mod_name)
				}
				ast.FnType {
					for param in expr.params {
						w.walk_expr(param.typ, mod_name)
					}
					w.walk_expr(expr.return_type, mod_name)
				}
				ast.GenericType {
					w.walk_expr(expr.name, mod_name)
					for param in expr.params {
						w.walk_expr(param, mod_name)
					}
				}
				ast.MapType {
					w.walk_expr(expr.key_type, mod_name)
					w.walk_expr(expr.value_type, mod_name)
				}
				ast.OptionType {
					w.walk_expr(expr.base_type, mod_name)
				}
				ast.PointerType {
					w.walk_expr(expr.base_type, mod_name)
				}
				ast.ResultType {
					w.walk_expr(expr.base_type, mod_name)
				}
				ast.ThreadType {
					w.walk_expr(expr.elem_type, mod_name)
				}
				ast.TupleType {
					for typ in expr.types {
						w.walk_expr(typ, mod_name)
					}
				}
				ast.AnonStructType {
					for embedded in expr.embedded {
						w.walk_expr(embedded, mod_name)
					}
					for field in expr.fields {
						w.walk_expr(field.typ, mod_name)
						w.walk_expr(field.value, mod_name)
					}
				}
				else {}
			}
		}
		else {}
	}
}
